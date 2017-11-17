(ns noria
  (:import [noria LCS]))

(defmacro get-in* [m ks]
  (reduce (fn [r k] `(get ~r ~k)) m ks))

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

(declare reconcile*)

(defn lookup [component-id ctx]
  (if-let [component (get-in* ctx [:components component-id])]
    [component ctx]
    (let [new-component-id (:next-component-id ctx)
          new-component {::component-id new-component-id}]
      [new-component
       (-> ctx
           (update :next-component-id inc)
           (assoc-in [:components new-component-id] new-component))])))

(defn collect-reusable-components [key->component-id new-keys ctx]
  (let [get-component-type (fn [c-id]
                             (get-type (get-in* ctx [:components c-id ::element])))]
    (reduce (fn [^java.util.HashMap hm [key c-id]]
              (when (not (contains? new-keys key))
                (when-let [c-type (get-component-type c-id)]
                  (let [^java.util.ArrayDeque l (or (.get hm c-type)
                                                    (let [l (java.util.ArrayDeque.)]
                                                      (.put hm c-type l)
                                                      l))]
                    (.push l c-id))))
              hm)
            (java.util.HashMap.) key->component-id)))

(defn reconcile-by-keys [key->component-id new-elements r-f ctx]
  (let [new-keys (into #{} (map get-key) new-elements)
        ^java.util.HashMap to-reuse (collect-reusable-components key->component-id new-keys ctx)
        [reconciled ctx'] (reduce (fn [[sink ctx] e]
                                    (let [old-c-id (or (key->component-id (get-key e))
                                                       (when-let [e-type (get-type e)]
                                                         (when-let [^java.util.ArrayDeque tr (.get to-reuse e-type)]
                                                           (.poll tr))))
                                          [reconciled ctx'] (reconcile* old-c-id e r-f ctx)]
                                      [(conj! sink reconciled) ctx']))
                                  [(transient []) ctx]
                                  new-elements)]
    [(persistent! reconciled) ctx']))

(defn reconcile-order [parent-node attr old-nodes new-nodes r-f ctx]
  (if (= old-nodes new-nodes)
    ctx
    (let [moved? (complement (into #{} (LCS/lcs (int-array old-nodes) (int-array new-nodes))))
          old-nodes-set (into #{} old-nodes)
          new-nodes-set (into #{} new-nodes)
          
          to-remove (clojure.set/difference old-nodes-set new-nodes-set)
          
          removes (into []
                        (keep
                         (fn [node]
                           (when (and (moved? node) (contains? old-nodes-set node))
                             {::update-type :remove
                              ::attr attr
                              ::node parent-node
                              ::value node})))
                        (into new-nodes to-remove))
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
      (update ctx :updates (fn [updates] (as-> updates <!>
                                          (reduce r-f <!> removes)
                                          (reduce r-f <!> adds)))))))

(defn reconcile-set [parent-node attr old-nodes new-nodes r-f ctx]
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
      (update ctx :updates (fn [updates] (as-> updates <!>
                                          (reduce r-f <!> removes)
                                          (reduce r-f <!> adds)))))))

(defn get-component-key [ctx]
  (fn [c-id]
    [(get-key (get-in* ctx [:components c-id ::element])) c-id]))

(defn reconcile-sequence [parent-node attr component-ids new-elements r-f ctx]
  (let [get-node (fn [ctx] #(get-in* ctx [:components % ::node]))
        new-elements (assign-keys new-elements)
        key->component-id (into {} (map (get-component-key ctx)) component-ids)
        [new-component-ids ctx'] (reconcile-by-keys key->component-id new-elements r-f ctx)
        unordered? (set? new-elements)
        old-nodes (into (if unordered? #{} [])
                        (map (get-node ctx))
                        component-ids)
        new-nodes (into (if unordered? #{} [])
                        (map (get-node ctx'))
                        new-component-ids)
        ctx'' (if unordered?
                (reconcile-set parent-node attr old-nodes new-nodes r-f ctx')
                (reconcile-order parent-node attr old-nodes new-nodes r-f ctx'))]
    [new-component-ids ctx'']))

(defonce schema (atom {}))

(defn get-data-type [k]
  (or (get-in* @schema [k ::data-type]) :simple-value))

(defn defattr [attr data]
  (swap! schema assoc attr data))

(defonce constructor-parameters (atom {}))

(defn get-constructor-parameters [k]
  (get @constructor-parameters k #{}))

(defn defconstructor [node-type attrs]
  (swap! constructor-parameters assoc node-type attrs))

(defn make-node [type constructor-data r-f {:keys [next-id] :as ctx}]
  [next-id (-> ctx
               (update :next-id inc)
               (update :updates r-f
                       {::update-type :make-node
                        ::node next-id
                        ::type type
                        ::constructor-parameters constructor-data}))])

(defn set-attr [r-f ctx node attr value]
  (update ctx :updates r-f {::update-type :set-attr
                            ::attr attr
                            ::node node
                            ::value value}))

(defn component-node [ctx component-id]
  (get-in* ctx [:components component-id ::node]))

(defn reconcile-attrs [{::keys [node] :as component} new-element r-f ctx]
  (let [[component ctx']
        (reduce
         (fn [[component ctx] [attr new-value]]
           (let [old-value (get component attr)
                 [value-reconciled ctx']
                 (case (get-data-type attr)
                   :simple-value [new-value (if (not= old-value new-value)
                                              (set-attr r-f ctx node attr new-value)
                                              ctx)]
                   :nodes-seq (reconcile-sequence node attr old-value new-value r-f ctx)
                   :node (let [old-node (component-node ctx old-value)
                               [reconciled ctx'] (reconcile* old-value new-value r-f ctx)
                               value-node (component-node ctx' reconciled)]
                           [reconciled (if (not= value-node old-node)
                                         (set-attr r-f ctx' node attr value-node)
                                         ctx')]))]
             [(assoc! component attr value-reconciled) ctx']))
         [(transient component) ctx]
         new-element)]
    [(persistent! component) ctx']))

(defn reconcile-constructor-parameters [new-element r-f ctx]
  (let [constructor-parameter? (get-constructor-parameters (::type new-element))
        [constructor-data component ctx']
        (reduce
         (fn [[constructor-data component ctx :as res] [attr new-value]]
           (if (constructor-parameter? attr)
             (let [[constructor-value value-reconciled ctx']
                   (case (get-data-type attr)
                     :simple-value [new-value new-value ctx]
                     :node (let [[reconciled ctx'] (reconcile* nil new-value r-f ctx)]
                             [(component-node ctx' reconciled) reconciled ctx'])
                     :nodes-seq (let [[t-items ctx']
                                      (reduce (fn [[items-reconciled ctx] item]
                                                (let [[item-reconciled ctx'] (reconcile* nil item r-f ctx)]
                                                  [(conj! items-reconciled item-reconciled) ctx']))
                                              [(transient []) ctx]
                                              new-value)
                                      items (persistent! t-items)]
                                  [(mapv #(component-node ctx' %) items) items ctx']))]
               [(assoc! constructor-data attr constructor-value)
                (assoc! component attr value-reconciled)
                ctx'])
             res))
         [(transient {}) (transient {}) ctx]
         new-element)]
    [(persistent! constructor-data) (persistent! component) ctx']))

(defn reconcile-primitive [component-id new-element r-f ctx]
  (if-let [component (get-in* ctx [:components component-id])]
    (let [[component' ctx'] (reconcile-attrs component (dissoc new-element ::type ::key) r-f ctx)]
      [component-id (assoc-in ctx' [:components component-id] (assoc component' ::element new-element))])
    (let [component-id (:next-component-id ctx)
          ctx (update ctx :next-component-id inc)
          constructor-parameter? (get-constructor-parameters (::type new-element))
          [constructor-data component-with-constructor-attrs ctx'] (reconcile-constructor-parameters new-element r-f ctx)
          [new-node ctx''] (make-node (::type new-element) constructor-data r-f ctx')
          [component ctx'''] (reconcile-attrs (assoc component-with-constructor-attrs
                                                     ::node new-node
                                                     ::component-id component-id)
                                              (remove (comp constructor-parameter? first)
                                                      (dissoc new-element ::type ::key)) r-f ctx'')]
      [component-id (assoc-in ctx''' [:components component-id] (assoc component ::element new-element))])))

(defn reconcile-user [component-id [xf & args :as element] r-f ctx]
  (let [[{::keys [subst render state component-id] :as component} ctx'] (lookup component-id ctx)
        render (or render
                   (xf (fn
                         ([] {::component-id component-id})
                         ([state element] (assoc state ::element element))
                         ([state] state))))
        state (or state (render))
        state' (render state args)]
    (if (and (get-in* ctx [:components component-id]) (::skip-subtree? state'))
      [component-id (-> ctx'
                        (update ::heap conj subst)
                        (update-in [:components component-id]
                                   assoc
                                   ::state state'
                                   ::element element))]
      (let [[subst' ctx''] (reconcile* subst (::element state') r-f ctx')
            subst-component (get-in* ctx'' [:components subst'])]
        [component-id (update-in ctx''
                                 [:components component-id]
                                 assoc
                                 ::render render
                                 ::subst subst'
                                 ::state state'
                                 ::element element
                                 ::node (::node subst-component))]))))

(defn apply? [x]
  (and (vector? x)
       (= 'apply (first x))))

(defn reconcile-apply [component-id [_ lambda & new-args :as element] r-f ctx]
  (let [[{::keys [args subst component-id] :as component} ctx'] (lookup component-id ctx)
        new-args (assign-keys new-args)
        key->component-id (into {} (map (get-component-key ctx')) args)
        [args-reconciled ctx''] (reconcile-by-keys key->component-id new-args r-f ctx')
        [subst' ctx'''] (reconcile* subst (apply lambda args-reconciled) r-f ctx'')] 
    [component-id (update-in ctx'''
                             [:components component-id]
                             assoc
                             ::subst subst'
                             ::args args-reconciled
                             ::node (get-in* ctx''' [:components subst' ::node])
                             ::element element)]))

(defn do? [x]
  (and (vector? x)
       (= 'do (first x))))

(defn reconcile-do [component-id [_ & elements :as new-element] r-f ctx]
  (let [[{::keys [node children component-id] :as component} ctx] (lookup component-id ctx)
        key->component-id (into {}
                                (map (fn [c-id]
                                       [(get-key (get-in* ctx [:components c-id ::element])) c-id]))
                                children)
        elements-with-keys (assign-keys elements)
        [new-children-ids ctx'] (reconcile-by-keys key->component-id elements-with-keys r-f ctx)]
    [component-id (update-in ctx'
                             [:components component-id]
                             assoc
                             ::children new-children-ids
                             ::node (get-in* ctx' [:components (last new-children-ids) ::node])
                             ::element (into (with-meta ['do] (meta new-element)) elements-with-keys))]))

(def component-ref? nat-int?)

(defn reconcile* [component-id element r-f ctx]
  (if (component-ref? element)
    [element ctx]
    (let [heap-before (::heap ctx)
          ctx (assoc ctx ::heap #{})
          component (get-in* ctx [:components component-id])
          component-id (if (and (some? component)
                                (or (nil? element)
                                    (and (or (user-component? element)
                                             (primitive-component? element))
                                         (not= (get-type (::element component))
                                               (get-type element)))))
                         nil
                         component-id)
          [component-id ctx'] (cond
                                (nil? element) [nil ctx]
                                (user-component? element) (reconcile-user component-id element r-f ctx)
                                (primitive-component? element) (reconcile-primitive component-id element r-f ctx)
                                (apply? element) (reconcile-apply component-id element r-f ctx)
                                (do? element) (reconcile-do component-id element r-f ctx)
                                :else (throw (ex-info "don't know how to reconcile " {:element element})))
          victims (clojure.set/difference (::heap component)
                                          (::heap ctx'))]
      [component-id (-> ctx'
                        (assoc ::heap (cond-> heap-before
                                        (some? component-id) (conj component-id)))
                        (update ::victims into victims)
                        (cond-> (some? component-id) (assoc-in [:components component-id ::heap] (::heap ctx'))))])))

(defn gc [{:keys [components root]
           ::keys [victims] :as ctx}]
  (let [victims (into #{}
                      (mapcat (fn [c-id] (tree-seq (constantly true)
                                                  (comp ::heap components)
                                                  c-id)))
                      victims)
        nodes-to-destroy (into #{} (keep #(get-in* components [% ::node])) victims)]
    (doall (->> victims
                (map components)
                (filter (comp user-component? ::element))
                (map (fn [{::keys [render state]}]
                       (render state)))))
    (-> ctx
        (update :components (fn [components]
                              (reduce dissoc components victims)))
        (update :updates (fn [updates]
                           (transduce (map (fn [node] {::update-type :destroy
                                                      ::node node}))
                                      conj! updates nodes-to-destroy))))))

(defn reconcile [component-id element {:keys [root] :as ctx}]
  (let [[component-id' ctx'] (reconcile* component-id element conj! (assoc ctx
                                                                           :updates (transient [])
                                                                           ::victims #{}
                                                                           ::heap #{}))]
    [component-id'
     (-> ctx'
         (cond-> (not= component-id component-id') (update ::victims conj component-id))
         (gc)
         (update :updates persistent!)
         (dissoc ::victims ::heap))]))

(defn force-update [ctx state]
  (let [component-id (::component-id state)
        elt (get-in* ctx [:components component-id ::element])
        ctx' (assoc-in ctx [:components component-id ::state] state)
        [component-id' ctx''] (reconcile component-id elt ctx')]
    (assert (= component-id component-id'))
    ctx''))

(def context-0
  {:updates []
   :components {}
   :next-component-id 0
   :next-id 0})

(defn destroy! [ctx]
  (doall (->> (vals (:components ctx))
              (filter (comp user-component? ::element))
              (map (fn [c]
                     ((::render c) (::state c))))))
  nil)
