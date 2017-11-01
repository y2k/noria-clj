(ns noria
  (:import [noria LCS]))

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
  (persistent!
   (second (reduce (fn [[indices res] e]
                     (if (some? e)
                       (if-let [key (get-key e)]
                         [indices (conj! res e)]
                         (let [type (get-type e)
                               indices' (update indices type (fn [i] (if i (inc i) 0)))
                               idx (indices' type)]
                           [indices' (conj! res (assign-key e [type idx]))]))
                       [indices res]))
                   [{} (transient [])]
                   elements))))

(defn user-component? [x]
  (and (vector? x)
       (fn? (first x))))

(def primitive-component? map?)

(defn reduce-children [f ctx children]
  (let [[recons ctx'] (reduce (fn [[recons ctx] c]
                                (let [[r ctx'] (f c ctx)]
                                  [(conj! recons r) ctx']))
                              [(transient []) ctx]
                              children)]
    [(persistent! recons) ctx']))

(declare reconcile*)

(defn lookup [component-id ctx]
  (if-let [component ((:components ctx) component-id)]
    [component ctx]
    (let [new-component-id (:next-component-id ctx)
          new-component {::component-id new-component-id}]
      [new-component
       (-> ctx
           (update :next-component-id inc)
           (assoc-in [:components new-component-id] new-component))])))

(defn reconcile-by-keys [key->component-id new-elements r-f ctx]
  (let [new-keys (into #{} (map get-key) new-elements)]
    (reduce-children (fn [e ctx]
                       (reconcile* (key->component-id (get-key e)) e r-f ctx))
                     ctx
                     new-elements)))

(defn reconcile-order [parent-node attr old-nodes new-nodes r-f ctx]
  (if (= old-nodes new-nodes)
    ctx
    (let [moved? (complement (into #{} (LCS/lcs (int-array old-nodes) (int-array new-nodes))))
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
    [(get-key (::element ((:components ctx) c-id))) c-id]))

(defn reconcile-sequence [parent-node attr component-ids new-elements r-f ctx]
  (let [get-nodes (fn [ctx] #(::node ((:components ctx) %)))
        old-nodes (map (get-nodes ctx) component-ids)
        new-elements (assign-keys new-elements)
        key->component-id (into {} (map (get-component-key ctx)) component-ids)
        [new-component-ids ctx'] (reconcile-by-keys key->component-id new-elements r-f ctx)
        new-nodes (map (get-nodes ctx') new-component-ids)
        ctx'' (reconcile-order parent-node attr old-nodes new-nodes r-f ctx')]
    [new-component-ids ctx'']))

(defonce data-types (atom {}))

(defn get-data-type [k]
  (get @data-types k :simple-value))

(defn defattr [attr data-type]
  (swap! data-types assoc attr data-type))

(defonce constructor-parameters (atom {}))

(defn get-constructor-parameters [k]
  (get @constructor-parameters k []))

(defn defconstructor [node-type attrs]
  (swap! constructor-parameters assoc node-type attrs))

(defn make-node [element r-f {:keys [next-id] :as ctx}]
  (let [constructor-parameters (get-constructor-parameters (get-type element))]
    [next-id (-> ctx
                 (update :next-id inc)
                 (update :updates r-f
                         {::update-type :make-node
                          ::node next-id
                          ::type (get-type element)
                          ::constructor-parameters (select-keys element constructor-parameters)}))]))

(defn set-attr [r-f ctx node attr value]
  (update ctx :updates conj {::update-type :set-attr
                             ::attr attr
                             ::node node
                             ::value value}))

(defmulti reconcile-data-type (fn [data-type node attr old-value new-value r-f ctx] data-type))

(defmethod reconcile-data-type :simple-value [_ node attr old-value new-value r-f ctx]
  [new-value (if (not= old-value new-value)
               (set-attr r-f ctx node attr new-value)
               ctx)])

(defmethod reconcile-data-type :nodes-seq [_ node attr old-value new-value r-f ctx]
  (reconcile-sequence node attr old-value new-value r-f ctx))

(defmethod reconcile-data-type :node [_ node attr old-value new-value r-f ctx]
  (let [value-node (::node ((:components ctx) new-value))]
    [value-node (if (not= value-node old-value)
                  (set-attr r-f ctx node attr value-node)
                  ctx)]))

(defn reconcile-primitive [component-id new-element r-f ctx]
  (let [[{::keys [node element component-id] :as component} ctx] (lookup component-id ctx)
        [node ctx] (if (some? node)
                     [node ctx]
                     (make-node new-element r-f ctx))
        [component' ctx'] (reduce (fn [[component ctx] [attr v]]
                                    (let [data-type (get-data-type attr)
                                          [value-reconciled ctx'] (reconcile-data-type data-type node attr (component attr) v r-f ctx)]
                                      [(assoc component attr value-reconciled) ctx']))
                                  [component ctx]
                                  (dissoc new-element ::type ::key))]
    [component-id (assoc-in ctx'
                            [:components component-id]
                            (assoc component'
                                   ::node node
                                   ::element new-element))]))

(defn reconcile-user [component-id [xf & args :as element] r-f ctx]
  (let [[{::keys [subst render state component-id] :as component} ctx'] (lookup component-id ctx)
        render (or render
                   (xf (fn
                         ([] {})
                         ([state element] (assoc state ::element element))
                         ([state] state))))
        state (or state (render))
        state' (render state args)]
    (if (and ((:components ctx) component-id) (::skip-subtree? state'))
      [component-id (update-in ctx'
                               [:components component-id]
                               assoc
                               ::state state'
                               ::element element)]
      (let [[subst' ctx''] (reconcile* subst (::element state') r-f ctx')
            subst-component ((:components ctx'') subst')]
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
                             ::node (::node ((:components ctx''') subst'))
                             ::element element)]))

(defn do? [x]
  (and (vector? x)
       (= 'do (first x))))

(defn reconcile-do [component-id [_ & elements] r-f ctx]
  (let [[{::keys [node element children component-id] :as component} ctx] (lookup component-id ctx)
        key->component-id (into {}
                                (map (fn [c-id]
                                       [(get-key (::element ((:components ctx) c-id))) c-id]))
                                children)
        elements-with-keys (assign-keys elements)
        [new-children-ids ctx'] (reconcile-by-keys key->component-id elements-with-keys r-f ctx)]
    [component-id (update-in ctx'
                             [:components component-id]
                             assoc
                             ::children new-children-ids
                             ::node (::node ((:components ctx') (last new-children-ids)))
                             ::element elements-with-keys)]))

(def component-ref? nat-int?)

(defn reconcile* [component-id element r-f ctx]
  (let [component ((:components ctx) component-id)
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
    [component-id (cond-> ctx'
                    (some? component-id) (->
                                          (update ::heap conj component-id)
                                          (assoc-in [:components component-id ::heap] (::heap ctx'))))]))

(defn reconcile [component-id element r-f ctx]
  (let [[component-id' ctx'] (reconcile* component-id element r-f (assoc ctx ::heap #{}))
        stale-components (clojure.set/difference (get-in ctx [:components component-id ::heap])
                                                 (get-in ctx' [:components component-id ::heap]))
        nodes-to-destroy (into #{} (map (fn [c-id] (get-in ctx' [:components c-id ::node]))) stale-components)]
    [component-id' (-> ctx'
                       (dissoc ::heap)
                       (update :updates (fn [updates]
                                          (transduce (map (fn [node] {::update-type :destroy
                                                                     ::node node}))
                                                     r-f updates nodes-to-destroy))))]))

(def context-0
  {:updates []
   :components {}
   :next-component-id 0
   :next-id 0})
