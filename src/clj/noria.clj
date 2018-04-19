(ns noria
  (:require [clojure.data.int-map :as i]
            [noria.thunks :as t]
            [clojure.spec.alpha :as s])
  (:import [noria LCS]
           [noria.thunks Thunk]))

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

(defn update-order [parent-node attr old-nodes new-nodes]
  (if (= old-nodes new-nodes)
    nil
    (let [moved? (complement (into (i/int-set) (LCS/lcs (int-array old-nodes) (int-array new-nodes))))
          old-nodes-set (i/int-set old-nodes)]
      (-> []
          (into
           (keep
            (fn [node]
              (when (and (moved? node) (contains? old-nodes-set node))
                {::update-type :remove
                 ::attr attr
                 ::node parent-node
                 ::value node})))
           old-nodes)
          (into (comp
                 (map-indexed
                  (fn [i node]
                    (when (moved? node)
                      {::update-type :add
                       ::attr attr
                       ::node parent-node
                       ::value node
                       ::index i})))
                 (filter some?))
                new-nodes)))))

(defn update-set [parent-node attr old-nodes new-nodes]
  (if (= old-nodes new-nodes)
    nil
    (let [to-add (i/difference new-nodes old-nodes)
          to-remove (i/difference old-nodes new-nodes)
          idx (count (i/intersection new-nodes old-nodes))]
      (-> []
          (into 
           (map
            (fn [node]
              {::update-type :remove
               ::attr attr
               ::node parent-node
               ::value node}))
           to-remove)
          (into
           (map-indexed
            (fn [i node]
              {::update-type :add
               ::attr attr
               ::node parent-node
               ::value node
               ::index (+ idx i)}))
           to-add)))))

;; old noria

(defn get-key [x]
  (if (vector? x)
    (::key (meta x))
    (::key x)))

(defn get-type [x]
  (if (vector? x)
    (let [f (nth x 0)]
      (when (and (not= 'do f) (not= 'apply f))
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
    (into (if (set? elements) (i/int-set) []) xf elements)))

(defn user-component? [x]
  (and (vector? x)
       (fn? (nth x 0))))

(def primitive-component? map?)

(def component-ref? nat-int?)

(defn apply? [x]
  (and (vector? x)
       (= 'apply (nth x 0))))

(defn do? [x]
  (and (vector? x)
       (= 'do (nth x 0))))

(defn get-children [{::keys [expr] :as value}]
  (cond (user-component? expr) [(::subst value)]
        (primitive-component? expr) (if (or (= (::type expr) :dom/div)
                                            (= (::type expr) :dom/span))
                                      (:dom/children value)
                                      (reduce-kv (fn [s attr v]
                                                   (case (get-data-type attr)
                                                     :nodes-seq (if (nil? s) v (into s v))
                                                     :node (if (nil? s) [v] (conj s v))
                                                     s)) nil value))
        (apply? expr) (conj (::args value) (::subst value))
        (do? expr) (::children value)
        :else nil))

(defn destroy-value [ctx value]
  (update ctx :garbage
          (fn [g]
            (transduce (comp 
                        (remove (comp component-ref? ::expr))
                        (map (fn [v]
                               (when (user-component? (::expr v))
                                 ((::render v) (::state v)))
                               v))
                        (map ::result)
                        (filter some?))
                       conj!
                       g
                       (tree-seq (constantly true) get-children value)))))

(declare reconcile*)

(defn alloc-id-if-needed [id ctx]
  (if (some? id)
    [id ctx]    
    [(:next-id ctx) (update ctx :next-id inc)]))

(defn reconcile-user [ppath {::keys [subst render state result id] :as old-value} [xf & args :as expr] env ctx]
  (if (and (some? old-value) (not= (get-type expr) (get-type (::expr old-value))))
    (recur ppath nil expr env (destroy-value ctx old-value))
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
      (assert (contains? state' ::expr) {:id-path id-path :expr expr})
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

(defn reconcile-by-keys [ppath old-values new-exprs env ctx]
  (let [hs-conj! (fn [^java.util.HashSet hs v] (.add hs v) hs)
        hm-assoc! (fn [^java.util.HashMap hm [k v]] (.put hm k v) hm)
        new-exprs' (assign-keys new-exprs)
        new-keys (transduce (keep get-key)
                            (completing hs-conj!)
                            (java.util.HashSet.)
                            new-exprs')
        ^java.util.HashMap old-values-by-keys (transduce (keep (fn [value]
                                                                 (when-let [k (get-key (::expr value))]
                                                                   [k value])))
                                                         (completing hm-assoc!)
                                                         (java.util.HashMap.)
                                                         old-values)
        
        ^java.util.HashMap to-reuse (reduce (fn [^java.util.HashMap hm [key old-value]]
                                              (when (not (contains? new-keys key))
                                                (when-let [c-type (-> old-value ::expr get-type)]
                                                  (let [^java.util.ArrayDeque l (or (.get hm c-type)
                                                                                    (let [l (java.util.ArrayDeque.)]
                                                                                      (.put hm c-type l)
                                                                                      l))]
                                                    (.push l old-value))))
                                              hm)
                                            (java.util.HashMap.)
                                            (.entrySet old-values-by-keys))
        
        [new-values ctx'] (reduce (fn [[new-values ctx] expr]
                                    (let [old-value (or (get old-values-by-keys (get-key expr))
                                                        (when-let [e-type (get-type expr)]
                                                          (when-let [^java.util.ArrayDeque tr (.get to-reuse e-type)]
                                                            (.poll tr))))
                                          [new-value ctx'] (reconcile* ppath old-value expr env ctx)]
                                      (when (some? old-value)
                                        (.remove old-values-by-keys (get-key (::expr old-value))))
                                      [(conj! new-values new-value) ctx' ]))
                                  [(transient []) ctx]
                                  new-exprs')]
    [(persistent! new-values) (reduce (fn [ctx [_ val]]
                                        (destroy-value ctx val))
                                      ctx'
                                      (.entrySet old-values-by-keys))]))

(defn unordered? [new-exprs]
  (::unordered? (meta new-exprs)))

(defn reconcile-sequence [ppath parent-node attr old-values new-exprs env ctx]
  (let [[new-values ctx'] (reconcile-by-keys ppath old-values new-exprs env ctx)
        unordered? (unordered? new-exprs)
        old-nodes (into (if unordered? (i/int-set) [])
                        (keep ::result)
                        old-values)
        new-nodes (into (if unordered? (i/int-set) [])
                        (keep ::result)
                        new-values)]
    [new-values (reduce ctx' supply (if unordered?
                                      (update-set parent-node attr old-nodes new-nodes)
                                      (update-order parent-node attr old-nodes new-nodes)))]))

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

(defn reduce-keys-of-two-maps [r-f state m1 m2]
  (let [noria-key? #(and (keyword? %) (= (namespace %) "noria"))]
    (as-> state <>
      (reduce-kv (fn [state a v]
                   (if (noria-key? a)
                     state
                     (r-f state a)))
                 <> m1)
      (reduce-kv (fn [state a v]
                   (if (or (noria-key? a) (contains? m1 a))
                     state
                     (r-f state a)))
                 <> m2))))

(defn reconcile-attrs [ppath old expr env ctx]
  (let [node (::result old)
        [new ctx'] (reduce-keys-of-two-maps
                    (fn [[res ctx] attr]
                      (let [new-expr (get expr attr)
                            old-value (get old attr)
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
                    old expr)]
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
  (onair.log/info :reconciling (get-type expr))
  (let [[id ctx] (alloc-id-if-needed id ctx)
        id-path (conj ppath id)]
    (if (some? old-value)
      (if (not= (get-type (::expr old-value)) (get-type expr))
        (recur ppath nil expr env (destroy-value ctx old-value))
        (let [[reconciled-attrs ctx'] (reconcile-attrs id-path old-value expr env ctx)]
          [(assoc reconciled-attrs
                  ::id id
                  ::id-path id-path
                  ::env env
                  ::expr expr
                  ::result (::result old-value))
           ctx']))
      (let [constructor-parameter? (get-constructor-parameters (::type expr))
            [constructor-data constructor-attrs ctx'] (reconcile-constructor-parameters id-path expr env ctx)
            [new-node ctx''] (make-node (::type expr) constructor-data ctx')
            [reconciled-attrs ctx'''] (reconcile-attrs id-path
                                                       {::result new-node}
                                                       (into {} (remove (comp constructor-parameter? first)) expr)
                                                       env ctx'')]
        [(merge constructor-attrs
                reconciled-attrs
                {::result new-node
                 ::id id
                 ::env env
                 ::id-path id-path
                 ::expr expr})
         ctx''']))))

(defn reconcile-apply [ppath {::keys [subst id args] :as old-value} [_ lambda & new-args :as expr] env ctx]
  (let [[id ctx] (alloc-id-if-needed id ctx)
        id-path (conj ppath id)
        [args-reconciled ctx'] (reconcile-by-keys id-path args new-args env ctx)        
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
      ::lambda lambda
      ::id-path id-path
      ::args args-reconciled
      ::result (::result subst')
      ::expr expr}
     ctx'']))

(defn reconcile-do [ppath {::keys [children id] :as old-value} [_ & new-children :as expr] env ctx]
  (let [[id ctx] (alloc-id-if-needed id ctx)
        id-path (conj ppath id)
        [children-reconciled ctx'] (reconcile-by-keys id-path children new-children env ctx)]
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
                       (destroy-value ctx old-value)
                       ctx)]
    (component-ref? expr) (let [var-value (env expr)]
                            (assert (contains? env expr) {:var expr
                                                          :env env
                                                          :old-value old-value})
                            [{::expr expr
                              ::env env
                              ::result (::result var-value)} ctx])
    (user-component? expr) (if (and old-value (not (user-component? (::expr old-value))))
                             (recur ppath nil expr env (destroy-value ctx old-value))
                             (reconcile-user ppath old-value expr env ctx))
    (primitive-component? expr) (if (and old-value (not (primitive-component? (::expr old-value))))
                                  (recur ppath nil expr env (destroy-value ctx old-value))
                                  (reconcile-primitive ppath old-value expr env  ctx))
    (apply? expr) (if (and old-value (not (apply? (::expr old-value))))
                    (recur ppath nil expr env (destroy-value ctx old-value))
                    (reconcile-apply ppath old-value expr env ctx))
    (do? expr) (if (and old-value (not (do? (::expr old-value))))
                 (recur ppath nil expr env (destroy-value ctx old-value))
                 (reconcile-do ppath old-value expr env ctx))
    :else (throw (ex-info "don't know how to reconcile " {:expr expr}))))

(def env-0 {:next-var 0})

(def context-0 {:next-node 0
                :next-id 0})

(defn destroy-garbage [ctx]
  (transduce
   (comp (distinct)
         (map (fn [g] {::update-type :destroy
                       ::node g})))
   (completing supply)
   (dissoc ctx :garbage)
   (persistent! (:garbage ctx))))

(defn reconcile [old-value expr ctx]
  (let [ctx (assoc ctx
                   :updates (transient [])
                   :garbage (transient []))
        [new-value ctx'] (reconcile* [] old-value expr env-0 ctx)]
    [new-value (-> ctx'
                   (destroy-garbage)
                   (update :updates persistent!))]))

(defn update-by-id-with-ctx [coll id id-fn f ctx]
  (cond
    (vector? coll) (reduce (fn [[coll ctx _ :as result] i]
                             (let [old-v (nth coll i)]
                               (if (= id (id-fn old-v))
                                 (let [[new-v ctx'] (f i old-v ctx)]
                                   (reduced [(assoc coll i new-v) ctx' true]))
                                 result)))
                           [coll ctx false]
                           (range (count coll)))
    (set? coll) (reduce (fn [[coll ctx _ :as result] e]
                          (if (= (id-fn e) id)
                            (let [[new-e ctx'] (f 0 e ctx)]
                              (reduced [(-> coll (disj e) (conj new-e)) ctx' true]))
                            result))
                        [coll ctx false]
                        coll)
    :else (throw (ex-info "collection type is not supported" {:coll coll}))))

(defn update-with-ctx [coll k f ctx]
  (let [[new-value ctx'] (f (coll k) ctx)]
    [(assoc coll k new-value) ctx']))

(defn reconcile-in* [ppath {::keys [expr id] :as old-value} [p & rest-path] state-fn ctx]
  (if (some? p)
    (let [continue (fn [old-value ctx] (reconcile-in* (conj ppath id) old-value rest-path state-fn ctx))]
      (cond
        (nil? expr) [nil ctx]
        (user-component? expr)
        (update-with-ctx old-value ::subst continue ctx)
        (primitive-component? expr)
        (reduce
         (fn [[old-value ctx :as result] [attr v]]
           (case (get-data-type attr)
             :node (if (= p (::id v))
                     (let [[new-v ctx'] (continue v ctx)]
                       (reduced [(assoc old-value attr new-v)
                                 (if (not= (::result new-v) (::result v))
                                   (supply ctx' {::update-type :set-attr
                                                 ::node (::result old-value)
                                                 ::attr attr
                                                 ::value (::result new-v)})
                                   ctx')]))
                     result)
             :nodes-seq (let [[new-v ctx' found?]
                              (update-by-id-with-ctx
                               v p ::id
                               (fn [i e ctx]
                                 (let [[new-e ctx'] (continue e ctx)]
                                   [new-e (-> ctx'
                                              (cond-> (not= (::result e) (::result new-e))
                                                (-> (cond-> (some? (::result e))
                                                      (supply {::update-type :remove
                                                               ::node (::result old-value)
                                                               ::attr attr
                                                               ::value (::result e)}))
                                                    (cond-> (some? new-e)
                                                      (supply {::update-type :add
                                                               ::node (::result old-value)
                                                               ::index i
                                                               ::attr attr
                                                               ::value (::result new-e)})))))])) ctx)]
                          (if found?
                            (reduced [(assoc old-value attr new-v) ctx'])
                            result))
             result))
         [old-value ctx] old-value)
        (apply? expr)
        (if (= p (::id (::subst old-value)))
          (update-with-ctx old-value ::subst continue ctx)
          (let [[{::keys [args lambda] :as args-reconciled} ctx'] (update-with-ctx old-value ::args (fn [args ctx]
                                                                                                      (update-by-id-with-ctx args p ::id
                                                                                                                             (fn [i v ctx] (continue v ctx)) ctx))
                                                                                   ctx)]
            (if (= (map ::result (::args old-value))
                   (map ::result (::args args-reconciled)))
              [args-reconciled ctx']
              (throw (ex-info "not implemented"))
              #_(reconcile* ppath
                          args-reconciled
                          (::expr args-reconciled)
                          (::env args-reconciled)
                          ctx'))))
        (do? expr)
        (update-with-ctx old-value ::children
                         (fn [old-children ctx]
                           (update-by-id-with-ctx old-children p ::id (fn [i v ctx] (continue v ctx)) ctx)) ctx)
        :else
        (throw (ex-info "component not found for path" {:ppath ppath
                                                        :p p
                                                        :rest-path rest-path
                                                        :old-value old-value}))))
    (reconcile* ppath
                (update old-value ::state state-fn)
                (::expr old-value)
                (::env old-value)
                ctx)))

(defn reconcile-in [old-value path state-fn ctx]
  (let [[new-value ctx'] (reconcile-in* [] old-value (rest path) state-fn (assoc ctx
                                                                                 :updates (transient [])
                                                                                 :garbage (transient [])))]
    [new-value (-> ctx'
                   (destroy-garbage)
                   (update :updates persistent!))]))

;;; noria-2

(s/def ::-< (s/cat :key-spec (s/? (s/cat :kw #{:noria/key}
                                         :key any?))
                   :thunk-def any?
                   :args (s/* any?)))

(defmacro -< [& stuff]
  (when-not (s/valid? ::-< stuff)
    (throw (ex-info (s/explain-str ::-< stuff)
                    (s/explain-data ::-< stuff))))
  (let [{{key :key} :key-spec
         :keys [thunk-def args]} (s/conform ::-< stuff)
        key (or key `(quote ~(gensym)))]
    `(t/thunk* ~key ~thunk-def [~@args])))

(def deref-or-value t/deref-or-value)
(def thunk-def t/thunk-def)

(def ^:dynamic *updates* nil)
(def ^:dynamic *next-node* nil)
(def ^:dynamic *callbacks* nil)

(def node
  (t/thunk-def
   {:up-to-date? (constantly false)
    :compute
    (fn [state [type attrs]]
      (let [old-type (::type state)
            old-node (::node state)
            constructor-params (get-constructor-parameters type)
            node-id (if (= old-type type)
                      old-node
                      (let [node-id (swap! *next-node* inc)]
                        (when old-node
                          (swap! *updates* conj! {:noria/update-type :destroy
                                                  :noria/node old-node}))
                        (swap! *updates* conj! {:noria/update-type :make-node
                                                :noria/type type
                                                :noria/node node-id
                                                :noria/constructor-parameters (select-keys attrs constructor-params)})
                        node-id))]
        [(transduce
          (if (not= old-node node-id)
            (remove (comp constructor-params first))
            identity)
          (completing
           (fn [state [attr value]]
             (let [old-value (get state attr)
                   data-type (get-data-type attr)
                   new-value (t/deref-or-value value)]
               (case data-type
                 (:node :simple-value)
                 (if (not= old-value new-value)
                   (do
                     (swap! *updates* conj! {:noria/update-type :set-attr
                                             :noria/value new-value
                                             :noria/node node-id
                                             :noria/attr attr})
                     (assoc! state attr new-value))
                   state)
                
                 :callback
                 (do
                   (swap! *callbacks* assoc! [node-id attr] new-value)
                   (cond
                     (and (nil? old-value) (nil? new-value)) state
                     (some? new-value) (do
                                         (swap! *updates* conj! {:noria/update-type :set-attr
                                                                 :noria/node node-id
                                                                 :noria/attr attr
                                                                 :noria/value (if (:noria/sync (meta new-value))
                                                                                :noria-handler-sync
                                                                                :noria-handler-async)})
                                         (assoc! state attr new-value))
                     (nil? new-value) (do
                                        (swap! *updates* conj! {:noria/update-type :set-attr
                                                                :noria/node node-id
                                                                :noria/attr attr
                                                                :noria/value :-noria-handler})
                                        (dissoc! state attr))
                     :else state))
                 :nodes-seq (let [unordered? (unordered? new-value)
                                  new-nodes (into (if unordered? (i/int-set) [])
                                                  (map t/deref-or-value)
                                                  new-value)
                                  old-nodes (or old-value (if unordered? (i/int-set) []))
                                  updates (if unordered?
                                            (update-set node-id attr old-nodes new-nodes)
                                            (update-order node-id attr old-nodes new-nodes))]
                              (swap! *updates* (fn [u] (reduce conj! u updates)))
                              (assoc! state attr new-nodes)))))
           persistent!)
          (transient (assoc state
                            ::type type
                            ::node node-id))
          attrs) node-id]))
    :destroy! (fn [{::keys [node] :as state} destroy-children!]
                (doseq [[attr value] state]
                  (case (get-data-type attr)
                    :node (swap! *updates* conj! {:noria/update-type :set-attr
                                                  :noria/node node
                                                  :noria/attr attr
                                                  :noria/value nil})
                    :node-seq (doseq [n value]
                                (swap! *updates* conj! {:noria/update-type :remove
                                                        :noria/node node
                                                        :noria/attr attr
                                                        :noria/value n}))
                    nil))
                (swap! *updates* conj! {:noria/update-type :destroy
                                        :noria/node node}))}))

(defn evaluate [graph f args-vector & {:keys [dirty-set]}]
  (binding [*updates* (atom (transient []))
            *next-node* (atom (or (::next-node graph) 0))]
    (let [[graph value] (t/evaluate graph f args-vector :dirty-set dirty-set)]
      [(assoc graph ::next-node @*next-node*) (persistent! @*updates*)])))
