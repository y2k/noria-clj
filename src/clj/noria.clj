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

(defn set-key [x k]
  (cond (vector? x)
        (vary-meta x assoc ::key k)
        (map? x) (assoc x ::key k)
        :else x))

(defn set-keys [elements]
  (persistent!
   (second (reduce (fn [[indices res] e]
                     (if (some? e)
                       (if-let [key (get-key e)]
                         [indices (conj! res e)]
                         (let [type (get-type e)
                               indices' (update indices type (fn [i] (if i (inc i) 0)))
                               idx (indices' type)]
                           [indices' (conj! res (set-key e [type idx]))]))
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

(declare reconcile)

(defn destroy-recursively [component-id r-f ctx]
  (let [get-children (fn [c-id] (let [c ((:components ctx) c-id)]
                                 (or (::children c)
                                     (some-> (::subst c) (vector)))))
        components-to-destroy (reverse (tree-seq get-children get-children component-id))]
    (-> ctx
        (update :updates
                (fn [updates]
                  (transduce (comp
                              (map (fn [component-id]
                                     (let [component ((:components ctx) component-id)]
                                       (if (user-component? (::element component))
                                         ((::render component) (::state component))))
                                     component-id))
                              (map (comp ::node (:components ctx)))
                              (dedupe)
                              (mapcat (fn [node]
                                        [{::update-type :remove
                                          ::node node}
                                         {::update-type :destroy
                                          ::node node}])))
                             r-f
                             updates
                             components-to-destroy)))
        (update :components (fn [components]
                              (apply dissoc components components-to-destroy))))))

(defn lookup [component-id ctx]
  (if-let [component ((:components ctx) component-id)]
    [component ctx]
    (let [new-component-id (:next-component-id ctx)
          new-component {::component-id new-component-id}]
      [new-component
       (-> ctx
           (update :next-component-id inc)
           (assoc-in [:components new-component-id] new-component))])))

(defn destroy-stale-components [key->component-id new-keys r-f ctx]
  (reduce (fn [ctx [key component-id]]
            (if (contains? new-keys key)
              ctx
              (destroy-recursively component-id r-f ctx)))
          ctx
          key->component-id))

(defn reconcile-by-keys [key->component-id new-elements r-f ctx]
  (let [new-keys (into #{} (map get-key) new-elements)
        ctx' (destroy-stale-components key->component-id new-keys r-f ctx)]
    (reduce-children (fn [e ctx]
                       (reconcile (key->component-id (get-key e)) e r-f ctx))
                     ctx'
                     new-elements)))

(defn reconcile-order [parent-node old-nodes new-nodes r-f ctx]
  (let [old-nodes (int-array old-nodes)
        new-nodes (int-array new-nodes)
        moved? (if (java.util.Arrays/equals old-nodes new-nodes)
                 (constantly false)
                 (complement (into #{} (LCS/lcs old-nodes new-nodes))))
        old-nodes-set (into #{} old-nodes)]
    (reduce (fn [ctx [i node]]
              (if (moved? node)
                (-> ctx
                    (cond-> (contains? old-nodes-set node)
                      (update :updates r-f {::update-type :remove
                                            ::node node}))
                    (update :updates r-f {::update-type :add
                                          ::parent parent-node
                                          ::child node
                                          ::index i}))
                ctx))
            ctx
            (map vector (range) new-nodes))))

(defn reconcile-sequence [parent-node component-ids new-elements r-f ctx]
  (let [get-nodes (fn [ctx]
                    #(::node ((:components ctx) %)))
        old-nodes (map (get-nodes ctx) component-ids)
        key->component-id (into {}
                                (map (fn [c-id]
                                       [(get-key (::element ((:components ctx) c-id))) c-id]))
                                component-ids)
        [new-component-ids ctx'] (reconcile-by-keys key->component-id new-elements r-f ctx)
        ctx'' (reconcile-order parent-node old-nodes (map (get-nodes ctx') new-component-ids) r-f ctx')]
    [new-component-ids ctx'']))

(defn make-node [element r-f {:keys [next-id] :as ctx}]
  [next-id (-> ctx
               (update :next-id inc)
               (update :updates r-f {::update-type :make-node
                                     ::node next-id
                                     ::type (get-type element)
                                     ::props (::props element)}))])

(defn reconcile-primitive [component-id new-element r-f ctx]
  (let [[{::keys [node element children component-id] :as component} ctx] (lookup component-id ctx)
        [node ctx] (if (some? node)
                     [node ctx]
                     (make-node new-element r-f ctx))
        [children-reconciled ctx'] (reconcile-sequence node children (set-keys (:children new-element)) r-f ctx)
        old-props (::props element)
        new-props (::props new-element)]
    [component-id
     (-> ctx'
         (update-in [:components component-id]
                    assoc
                    ::node node
                    ::element new-element
                    ::children children-reconciled)
         (cond-> 
           (and (::node component) (not= old-props new-props))
           (update :updates r-f {::update-type :update-props
                                 ::node node
                                 ::new-props new-props
                                 ::old-props old-props})))]))

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
      (let [[subst' ctx''] (reconcile subst (::element state') r-f ctx')
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
        [args-reconciled ctx''] (reduce-children (fn [[old-arg new-arg] ctx]
                                                   (reconcile old-arg new-arg r-f ctx))
                                                 ctx'
                                                 (map vector (concat args (repeat nil)) new-args))
        [subst' ctx'''] (reconcile subst (apply lambda args-reconciled) r-f ctx'')]
    [component-id (update-in ctx'''
                             [:components component-id]
                             assoc
                             ::subst subst'
                             ::args args-reconciled
                             ::node (::node ((:components ctx''') subst'))
                             ::element element)]))

(defn reconcile-vector [component-id elements r-f ctx]
  (let [[{::keys [node element children component-id] :as component} ctx] (lookup component-id ctx)
        key->component-id (into {}
                                (map (fn [c-id]
                                       [(get-key (::element ((:components ctx) c-id))) c-id]))
                                children)
        elements-with-keys (set-keys elements)
        [new-children-ids ctx'] (reconcile-by-keys key->component-id elements-with-keys r-f ctx)]
    [component-id (update-in ctx'
                             [:components component-id]
                             assoc
                             ::children new-children-ids
                             ::node (::node ((:components ctx') (last new-children-ids)))
                             ::element elements-with-keys)]))

(def component-ref? nat-int?)

(defn reconcile [component-id element r-f ctx]
  (let [component ((:components ctx) component-id)
        [component-id ctx] (if (and (some? component)
                                    (or (nil? element)
                                        (and (or (user-component? element)
                                                 (primitive-component? element))
                                             (not= (get-type (::element component))
                                                   (get-type element)))))
                                [nil (destroy-recursively component-id r-f ctx)]
                                [component-id ctx])]
    (cond
      (nil? element) [nil ctx]
      (component-ref? element) [element ctx]
      (user-component? element) (reconcile-user component-id element r-f ctx)
      (primitive-component? element) (reconcile-primitive component-id element r-f ctx)
      (apply? element) (reconcile-apply component-id element r-f ctx)
      (vector? element) (reconcile-vector component-id element r-f ctx)
      :else (throw (ex-info "don't know how to reconcile " {:element element})))))

(comment
  

  (let** [a [my-label "foo"]
          b [my-component a]]
    [{::type :set-property
      :object a
      :property :b
      :value b}
     b])
  

  [[my-label 1]
   [my-label 2]] => 

  (def my-label
    (noria.components/render (fn [x]
                               {::type :text
                                ::props {:text (str x)}})))

  (def my-lambda
    (noria.components/render
     (fn [y]
       ['apply (fn [x]
                 {::type :div
                  :children [x]})
        [my-label y]])))

  (def my-container
    (noria.components/render
     (fn [i]
       {::type :div
        :children (map (fn [i]
                         ^{::key i} [my-label i]) (range i))})))

  (def my-composite
    (noria.components/render
     (fn [i]
       (mapv (fn [i] [my-label i]) (range i)))))

  

  (let [[c-id ctx] (reconcile nil [my-lambda :x] conj {:updates []
                                                       :components {}
                                                       :next-component-id 0
                                                       :next-id 0})]
    (def c-id c-id)
    (def ctx ctx))

  (let [[c-id ctx] (reconcile nil [my-composite 2] conj {:updates []
                                                         :components {}
                                                         :next-component-id 0
                                                         :next-id 0})]
    (def c-id c-id)
    (def ctx ctx))

  (reconcile c-id [my-composite 3] conj (assoc ctx :updates []))

  (let [[c-id ctx] (reconcile nil [my-container 2] conj {:updates []
                                                         :components {}
                                                         :next-component-id 0
                                                         :next-id 0})]
    (def c-id c-id)
    (def ctx ctx))
  
  (reconcile c-id [my-container 1] conj (assoc ctx :updates []))

  (reconcile c-id [my-lambda :y] conj (assoc ctx :updates []))

  (defn bench [i j]
    (LCS/lcs i j))
  
  (let [i (int-array (range 100))
        j (int-array (range 100))]
    (time
     (do
       (bench i j)
       nil)))

  )
