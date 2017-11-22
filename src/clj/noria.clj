(ns noria
  (:import [noria LCS]))

(defn get-key [x]
  (if (vector? x)
    (::key (meta x))
    (::key x)))

(defn get-type [x]
  (if (vector? x)
    (let [f (first x)]
      (when (and (not= f 'do) (not= f 'apply))
        f))
    (::type x)))

(defn assign-key [x k]
  (cond (vector? x)
        (vary-meta x assoc ::key k)
        (map? x) (assoc x ::key k)
        :else x))

(defn assign-keys [elements]
  (let [xf (fn [r-f]
             (let [indices (java.util.HashMap.)]
               (fn
                 ([] (r-f))
                 ([s e]
                  (if-let [key (get-key e)]
                    (r-f s e)
                    (let [type (get-type e)
                          idx (or (.get indices type) 0)]
                      (.put indices type (inc idx))
                      (r-f s (assign-key e [type idx])))))
                 ([s] (r-f s)))))]
    (into (if (set? elements) #{} []) (comp (filter some?) xf) elements)))

(defn user-component? [x]
  (and (vector? x)
       (fn? (first x))))

(def primitive-component? map?)

(def component-ref? nat-int?)

(defn apply? [x]
  (and (vector? x)
       (= 'apply (first x))))

(defn do? [x]
  (and (vector? x)
       (= 'do (first x))))

(defonce schema (atom {}))

(defn get-data-type [k]
  (or (::data-type (@schema k)) :simple-value))

(defn defattr [attr data]
  (swap! schema assoc attr data))

(defonce constructor-parameters (atom {}))

(defn get-constructor-parameters [k]
  (get @constructor-parameters k #{}))

(defn defconstructor [node-type attrs]
  (swap! constructor-parameters assoc node-type attrs))

(defn get-children [value]
  (cond (user-component? (::expr value)) [(::subst value)]
        (primitive-component? (::expr value)) (mapcat
                                               (fn [[attr v]]
                                                 (case (get-data-type attr)
                                                   :nodes-seq v
                                                   :node [v]
                                                   nil)) value)
        (apply? (::expr value)) (conj (::args value) (::subst value))
        (do? (::expr value)) (::children value)
        :else nil))

(defn destroy-value [value ctx]
  (->> (tree-seq (constantly true) get-children value)
       (remove (fn [v] (component-ref? (::expr value))))
       (map (fn [v]
              (when (user-component? (::expr v))
                ((::render v) (::state v)))
              v))
       (map ::result)
       (dedupe)
       (reduce (fn [ctx n]
                 (update ctx :garbage conj n)) ctx)))

(declare reconcile*)

(defn alloc-id-if-needed [id ctx]
  (if (some? id)
    [id ctx]    
    [(:next-id ctx) (update ctx :next-id inc)]))

(defn reconcile-user [ppath {::keys [subst render state result id] :as old-value} [xf & args :as expr] env ctx]
  (if (and (some? old-value) (not= (get-type expr) (get-type (::expr old-value))))
    (recur ppath nil expr env (destroy-value old-value ctx))
    (let [[id ctx] (alloc-id-if-needed id ctx)
          id-path (conj ppath id)
          render (or render
                     (xf (fn
                           ([] {::id-path id-path})
                           ([state expr] (assoc state ::expr expr))
                           ([state] state))))
          state (or state (render))
          state' (render state args)          
          [subst' ctx'] (if (and (some? old-value) (::skip-subtree? state'))
                          [subst ctx]
                          (reconcile* id-path subst (::expr state') env ctx))]
      [{::state state'
        ::expr expr
        ::id id
        ::env env
        ::id-path id-path
        ::render render
        ::subst subst'
        ::result (::result subst')}
       ctx'])))

(defn supply [ctx u]
  (update ctx :updates conj! u))

(defn update-order [parent-node attr old-nodes new-nodes ctx]
  (if (= old-nodes new-nodes)
    ctx
    (let [moved? (complement (into #{} (LCS/lcs (int-array old-nodes) (int-array new-nodes))))
          old-nodes-set (into #{} old-nodes)
          removes (into []
                        (keep
                         (fn [node]
                           (when (and (moved? node) (contains? old-nodes-set node))
                             {::update-type :remove
                              ::attr attr
                              ::node parent-node
                              ::value node})))
                        (into old-nodes-set new-nodes))
          adds (into []
                     (comp
                      (map-indexed
                       (fn [i node]
                         (when (moved? node)
                           {::update-type :add
                            ::attr attr
                            ::node parent-node
                            ::value node
                            ::index i})))
                      (filter some?))
                     new-nodes)]      
      (as-> ctx <>
        (reduce supply <> removes)
        (reduce supply <> adds)))))

(defn update-set [parent-node attr old-nodes new-nodes ctx]
  (if (= old-nodes new-nodes)
    ctx
    (let [to-add (clojure.set/difference new-nodes old-nodes)
          to-remove (clojure.set/difference old-nodes new-nodes)
          removes (into []
                        (map
                         (fn [node]
                           {::update-type :remove
                            ::attr attr
                            ::node parent-node
                            ::value node}))
                        to-remove)
          idx (count (clojure.set/intersection new-nodes old-nodes))
          adds (into []
                     (comp
                      (map-indexed
                       (fn [i node]
                         {::update-type :add
                          ::attr attr
                          ::node parent-node
                          ::value node
                          ::index (+ idx i)})))
                     to-add)]
      (as-> ctx <!>
        (reduce supply <!> removes)
        (reduce supply <!> adds)))))

(defn collect-reusable-values [old-by-keys new-keys]
  (reduce (fn [^java.util.HashMap hm [key old-value]]
            (when (not (contains? new-keys key))
              (when-let [c-type (-> old-value ::expr get-type)]
                (let [^java.util.ArrayDeque l (or (.get hm c-type)
                                                  (let [l (java.util.ArrayDeque.)]
                                                    (.put hm c-type l)
                                                    l))]
                  (.push l old-value))))
            hm)
          (java.util.HashMap.)
          old-by-keys))

(defn reconcile-by-keys [ppath old-by-keys new-exprs env ctx]
  (let [new-keys (into #{} (keep get-key) new-exprs)
        ^java.util.HashMap to-reuse (collect-reusable-values old-by-keys new-keys)
        [new-values ctx'] (reduce (fn [[new-values ctx] expr]
                                    (let [old-value (or (old-by-keys (get-key expr))
                                                        (when-let [e-type (get-type expr)]
                                                          (when-let [^java.util.ArrayDeque tr (.get to-reuse e-type)]
                                                            (.poll tr))))
                                          [new-value ctx'] (reconcile* ppath old-value expr env ctx)]
                                      [(conj! new-values new-value) ctx']))
                                  [(transient []) ctx]
                                  new-exprs)]
    [(persistent! new-values) (reduce (fn [ctx [_ values]]
                                        (reduce (fn [ctx v] (destroy-value v ctx)) ctx values)) ctx' to-reuse)]))

(defn reconcile-sequence [ppath parent-node attr old-values new-exprs env ctx]
  (let [new-exprs-with-keys (assign-keys new-exprs)
        old-values-by-keys (into {}
                                 (keep (fn [value]
                                         (when-let [k (get-key (::expr value))]
                                           [k value])))
                                 old-values)        
        [new-values ctx'] (reconcile-by-keys ppath old-values-by-keys new-exprs-with-keys env ctx)
        unordered? (set? new-exprs)
        old-nodes (into (if unordered? #{} [])
                        (map ::result)
                        old-values)
        new-nodes (into (if unordered? #{} [])
                        (map ::result)
                        new-values)
        ctx'' (if unordered?
                (update-set parent-node attr old-nodes new-nodes ctx')
                (update-order parent-node attr old-nodes new-nodes ctx'))]
    [new-values ctx'']))

(defn make-node [type constructor-data {:keys [next-node] :as ctx}]
  [next-node (-> ctx
               (update :next-node inc)
               (supply {::update-type :make-node
                        ::node next-node
                        ::type type
                        ::constructor-parameters constructor-data}))])

(defn set-attr [node attr value ctx]  
  (supply ctx {::update-type :set-attr
               ::attr attr
               ::node node
               ::value value}))

(defn reconcile-attrs [ppath old expr env ctx]
  (let [node (::result old)
        [new ctx'] (reduce
                    (fn [[res ctx] [attr new-expr]]
                      (let [old-value (get old attr)
                            [new-value ctx']
                            (case (get-data-type attr)
                              :simple-value [new-expr (if (not= old-value new-expr)
                                                        (set-attr node attr new-expr ctx)
                                                        ctx)]
                              :nodes-seq (reconcile-sequence ppath node attr old-value new-expr env ctx)
                              :node (let [[new-value ctx'] (reconcile* ppath old-value new-expr env ctx)]
                                      [new-value (if (not= (::result old-value) (::result new-value))
                                                   (set-attr node attr (::result new-value) ctx')
                                                   ctx')]))]
                        [(assoc! res attr new-value) ctx']))
                    [(transient {}) ctx]
                    (dissoc expr ::key ::type))]
    [(persistent! new) ctx']))

(defn reconcile-constructor-parameters [ppath expr env ctx]
  (let [constructor-parameter? (get-constructor-parameters (::type expr))
        [constructor-data component ctx']
        (reduce
         (fn [[constructor-data component ctx :as res] [attr expr]]
           (if (constructor-parameter? attr)
             (let [[constructor-value value-reconciled ctx']
                   (case (get-data-type attr)
                     :simple-value [expr expr ctx]
                     :node (let [[value ctx'] (reconcile* ppath nil expr env ctx)]
                             [(::result value) value ctx'])
                     :nodes-seq (let [[t-items ctx']
                                      (reduce (fn [[items-reconciled ctx] item]
                                                (let [[item-reconciled ctx'] (reconcile* ppath nil item env ctx)]
                                                  [(conj! items-reconciled item-reconciled) ctx']))
                                              [(transient []) ctx]
                                              expr)
                                      items (persistent! t-items)]
                                  [(mapv ::result items) items ctx']))]
               [(assoc! constructor-data attr constructor-value)
                (assoc! component attr value-reconciled)
                ctx'])
             res))
         [(transient {}) (transient {}) ctx]
         expr)]
    [(persistent! constructor-data) (persistent! component) ctx']))

(defn reconcile-primitive [ppath {::keys [id] :as old-value} expr env ctx]
  (let [[id ctx] (alloc-id-if-needed id ctx)
        id-path (conj ppath id)]
    (if (some? old-value)
      (if (not= (get-type (::expr old-value)) (get-type expr))
        (recur ppath nil expr env (destroy-value old-value ctx))
        (let [[reconciled-attrs ctx'] (reconcile-attrs id-path old-value expr env ctx)]
          [(assoc reconciled-attrs
                  ::id id
                  ::id-path id-path
                  ::expr expr
                  ::result (::result old-value))
           ctx']))
      (let [constructor-parameter? (get-constructor-parameters (::type expr))
            [constructor-data constructor-attrs ctx'] (reconcile-constructor-parameters id-path expr env ctx)
            [new-node ctx''] (make-node (::type expr) constructor-data ctx')
            [component ctx'''] (reconcile-attrs id-path
                                                {::result new-node}
                                                (into {} (remove (comp constructor-parameter? first)) expr)
                                                env ctx'')]
        [(merge constructor-attrs
                component
                {::result new-node
                 ::id id
                 ::env env
                 ::id-path id-path
                 ::expr expr})
         ctx''']))))

(defn reconcile-apply [ppath {::keys [subst id args] :as old-value} [_ lambda & new-args :as expr] env ctx]
  (let [[id ctx] (alloc-id-if-needed id ctx)
        id-path (conj ppath id)
        new-args-with-keys (assign-keys new-args)        
        args-by-keys (into {} (map (fn [a] [(get-key (::expr a)) a])) args)        
        [args-reconciled ctx'] (reconcile-by-keys id-path args-by-keys new-args-with-keys env ctx)        
        [args-vars env'] (reduce (fn [[args-vars env] arg-value]
                                   [(conj args-vars (:next-var env))
                                    (-> env
                                        (assoc (:next-var env) arg-value)
                                        (update :next-var inc))])
                                 [[] env]
                                 args-reconciled)        
        [subst' ctx''] (reconcile* id-path subst (apply lambda args-vars) env' ctx')]
    [{::subst subst'
      ::id id
      ::env env
      ::id-path id-path
      ::args args-reconciled
      ::result (::result subst')
      ::expr expr}
     ctx'']))

(defn reconcile-do [ppath {::keys [children id] :as old-value} [_ & new-children :as expr] env ctx]
  (let [[id ctx] (alloc-id-if-needed id ctx)
        id-path (conj ppath id)
        children-by-keys (into {} (map (fn [c] [(get-key (::expr c)) c])) children)
        new-children-with-keys (assign-keys new-children)
        [children-reconciled ctx'] (reconcile-by-keys id-path children-by-keys new-children-with-keys env ctx)]
    [{::expr expr
      ::id id
      ::env env
      ::id-path id-path
      ::result (::result (last children-reconciled))
      ::children children-reconciled}
     ctx']))

(defn composite-component? [expr]
  (or (user-component? expr)
      (primitive-component? expr)
      (do? expr)
      (apply? expr)))

(defn reconcile* [ppath old-value expr env ctx]
  (cond
    (nil? expr) [nil (if (some? old-value)
                       (destroy-value old-value ctx)
                       ctx)]
    (component-ref? expr) (let [var-value (env expr)]
                            (assert (some? var-value) {:var expr
                                                       :env env
                                                       :old-value old-value})
                            [{::expr expr
                              ::env env
                              ::result (::result var-value)} ctx])
    (user-component? expr) (reconcile-user ppath old-value expr env ctx)
    (primitive-component? expr) (reconcile-primitive ppath old-value expr env  ctx)
    (apply? expr) (reconcile-apply ppath old-value expr env ctx)
    (do? expr) (reconcile-do ppath old-value expr env ctx)
    :else (throw (ex-info "don't know how to reconcile " {:expr expr}))))

(def env-0 {:next-var 0})

(defn reconcile-with-gc [ppath old-value expr env ctx]
  (let [ctx (assoc ctx :updates (transient []))
        [new-value ctx'] (reconcile* ppath old-value expr env ctx)]
    [new-value (as-> ctx' <>
                 (dissoc <> :garbage)
                 (reduce (fn [ctx g]
                           (supply ctx {::update-type :destroy
                                        ::node g}))
                         <> (:garbage ctx'))
                 (update <> :updates persistent!))]))

(defn reconcile [old-value expr ctx]
  (reconcile-with-gc [] old-value expr env-0 ctx))

(defn update-by-id [coll id id-fn f]
  (cond
    (vector? coll) (reduce (fn [res i]
                             (if (= id (id-fn (nth coll i)))
                               (reduced (update res i f))
                               res)) coll (range (count coll)))
    (set? coll) (reduce (fn [res e]
                          (if (= (id-fn e) id)
                            (reduced (-> res
                                         (disj e)
                                         (conj (f e))))
                            res)) coll coll)
    :else (throw (ex-info "collection type is not supported" {:coll coll}))))

(defn reconcile-in* [ppath {::keys [expr id] :as old-value} [p & rest-path] state-fn ctx]
  (if (some? p)
    (let [*ctx (atom nil)
          continue #(let [[new-value ctx'] (reconcile-in* (conj ppath id)
                                                          %
                                                          rest-path
                                                          state-fn
                                                          ctx)]
                      (reset! *ctx ctx')
                      new-value)]
      [(cond
          (user-component? expr) (update old-value ::subst continue)
          (primitive-component? expr) (reduce
                                       (fn [old-value [attr v]]
                                         (case (get-data-type attr)
                                           :node (if (= p (::id v))
                                                   (reduced
                                                    (update old-value attr continue))
                                                   old-value)
                                           :nodes-seq (let [updated (update-by-id v p ::id continue)]
                                                        (if (identical? updated v)
                                                          old-value
                                                          (reduced (assoc old-value attr updated))))
                                           old-value))
                                       old-value old-value)
          (apply? expr) (if (= p (::id (::subst old-value)))
                          (update old-value ::subst continue)
                          (update old-value ::args update-by-id p ::id continue))
          (do? expr) (update old-value ::children update-by-id p ::id continue)
          :else (throw (ex-info "component not found for path" {:ppath ppath
                                                                :p p
                                                                :rest-path rest-path})))
       
       (if @*ctx @*ctx (throw (ex-info "component not found for path" {:ppath ppath
                                                                       :p p
                                                                       :rest-path rest-path
                                                                       :old-value old-value})))])
    (reconcile-with-gc ppath
                       (update old-value ::state state-fn)
                       (::expr old-value)
                       (::env old-value)
                       ctx)))

(defn reconcile-in [old-value path state-fn ctx]
  (reconcile-in* [] old-value (rest path) state-fn ctx))

(def context-0 {:next-node 0
                :next-id 0})
