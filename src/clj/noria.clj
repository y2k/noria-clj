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
    (first x)
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
    (into [] (comp (filter some?) xf) elements)))

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

(defn reconcile-by-keys [key->component-id new-elements r-f ctx]
  (let [new-keys (into #{} (map get-key) new-elements)
        [reconciled ctx'] (reduce (fn [[sink ctx] e]
                                    (let [[reconciled ctx'] (reconcile* (key->component-id (get-key e)) e r-f ctx)]
                                      [(conj! sink reconciled) ctx']))
                                  [(transient []) ctx]
                                  new-elements)]
    [(persistent! reconciled) ctx']))

(defn reconcile-order [parent-node attr old-nodes new-nodes r-f ctx]
  (if (= old-nodes new-nodes)
    ctx
    (let [moved? (complement (into #{} (LCS/lcs (int-array old-nodes) (int-array new-nodes))))
          ;old-nodes-indices (into {} (map-indexed (fn [i n] [n i])) old-nodes)
          old-nodes-set (into #{} old-nodes)]
      (reduce (fn [ctx [i node]]
                (if (moved? node)
                  (-> ctx
                      (cond-> (contains? old-nodes-set node)
                        (update :updates r-f {::update-type :remove
                                              ::attr attr
                                              ::node parent-node
                                              ::value node}))
                      (update :updates r-f {::update-type :add
                                            ::attr attr
                                            ::node parent-node
                                            ::value node
                                            ::index i}))
                  ctx))
              ctx
              (map vector (range) new-nodes)))))

(defn get-component-key [ctx]
  (fn [c-id]
    [(get-key (get-in* ctx [:components c-id ::element])) c-id]))

(defn reconcile-sequence [parent-node attr component-ids new-elements r-f ctx]
  (let [get-nodes (fn [ctx] #(get-in* ctx [:components % ::node]))
        old-nodes (map (get-nodes ctx) component-ids)
        new-elements (assign-keys new-elements)
        key->component-id (into {} (map (get-component-key ctx)) component-ids)
        [new-component-ids ctx'] (reconcile-by-keys key->component-id new-elements r-f ctx)
        new-nodes (map (get-nodes ctx') new-component-ids)
        ctx'' (reconcile-order parent-node attr old-nodes new-nodes r-f ctx')]
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
                        (update ::heap clojure.set/union (get-in* ctx [:components component-id ::heap]))
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
                              (component-ref? element) [element ctx]
                              (user-component? element) (reconcile-user component-id element r-f ctx)
                              (primitive-component? element) (reconcile-primitive component-id element r-f ctx)
                              (apply? element) (reconcile-apply component-id element r-f ctx)
                              (do? element) (reconcile-do component-id element r-f ctx)
                              :else (throw (ex-info "don't know how to reconcile " {:element element})))]
    [component-id (-> ctx'
                      (update ::heap clojure.set/union heap-before)
                      (cond-> (some? component-id) (->
                                                    (update ::heap conj component-id)
                                                    (assoc-in [:components component-id ::heap] (::heap ctx')))))]))

(defn reconcile [component-id element ctx]
  (let [[component-id' ctx'] (reconcile* component-id element conj! (assoc ctx
                                                                           ::heap #{}
                                                                           :updates (transient [])))
        stale-components (clojure.set/difference
                          (get-in* ctx [:components component-id ::heap])
                          (get-in* ctx' [:components component-id ::heap]))
        nodes-to-destroy (into #{} (keep #(get-in* ctx' [:components % ::node])) stale-components)]
    (doall (->> stale-components
                (map #(get-in* ctx' [:components %]))
                (filter (comp user-component? ::element))
                (map (fn [{::keys [render state]}]
                       (render state)))))
    [component-id' (-> (update ctx' :components (fn [cs] (reduce dissoc cs stale-components)))
                       (dissoc ::heap)
                       (update :updates (fn [updates]
                                          (transduce (map (fn [node] {::update-type :destroy
                                                                      ::node node}))
                                                     conj! updates nodes-to-destroy)))
                       (update :updates persistent!))]))

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
