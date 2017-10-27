(ns noria.noria
  (:import [noria LCS]))

(defn get-key [x]
  (if (vector? x)
    (:noria/key (meta x))
    (:noria/key x)))

(defn get-type [x]
  (if (vector? x)
    (first x)
    (:noria/type x)))

(defn set-key [x k]
  (if (vector? x)
    (vary-meta x assoc :noria/key k)
    (assoc x :noria/key k)))

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

(def user-component? vector?)

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
                                 (or (:noria/children c)
                                     (some-> (:noria/subst c) (vector)))))
        components-to-destroy (reverse (tree-seq get-children get-children component-id))]
    (-> ctx
        (update :updates
                (fn [updates]
                  (transduce (comp
                              (map (fn [component-id]
                                     (let [component ((:components ctx) component-id)]
                                       (if (user-component? (:noria/element component))
                                         ((:noria/render component) (:noria/state component))))
                                     component-id))
                              (map (comp :noria/node (:components ctx)))
                              (dedupe)
                              (mapcat (fn [node]
                                        [{:noria/update-type :remove
                                          :noria/node node}
                                         {:noria/update-type :destroy
                                          :noria/node node}])))
                             r-f
                             updates
                             components-to-destroy))))))

(defn lookup [component-id ctx]
  (if-let [component ((:components ctx) component-id)]
    [component ctx]
    (let [new-component-id (:next-component-id ctx)
          new-component {:noria/component-id new-component-id}]
      [new-component
       (-> ctx
           (update :next-component-id inc)
           (assoc-in [:components new-component-id] new-component))])))

(defn reconcile-children [parent-node children-ids new-children r-f ctx]
  (let [[children ctx] (reduce-children lookup ctx children-ids)
        component-key (comp get-key :noria/element)
        old-keys (map component-key children)
        new-keys (mapv get-key new-children)
        new-keys-set (into #{} new-keys)
        _ (assert (= (count new-keys) (count new-keys-set)) {:keys new-keys
                                                             :error "keys should be distinct"})
        moved? (if (= old-keys new-keys)
                 (constantly false)
                 (complement (into #{} (LCS/lcs (into-array Object old-keys)
                                                (into-array Object new-keys)))))
        key->component (into {}
                             (map (fn [c] [(component-key c) c]))
                             children)
        ctx-with-destroys (->> children
                              (remove #(contains? new-keys-set (component-key %)))
                              (reduce (fn [ctx c]
                                        (destroy-recursively (:noria/component-id c) r-f ctx))
                                      ctx))]
    (reduce-children
     (fn [[i child] ctx]
       (let [old-c (key->component (get-key child))
             [new-c-id ctx'] (reconcile (:noria/component-id old-c) child r-f ctx)
             new-c ((:components ctx') new-c-id)]
         [new-c-id (cond-> ctx'
                  (and (some? old-c) (moved? (get-key child)))
                  (update :updates r-f {:noria/update-type :remove
                                        :noria/node (:noria/node old-c)})
                  
                  (or (not= (:noria/node old-c) (:noria/node new-c))
                      (moved? (get-key child)))
                  (update :updates r-f {:noria/update-type :add
                                        :noria/index i
                                        :noria/child-node (:noria/node new-c)
                                        :noria/parent-node parent-node}))]))
     ctx-with-destroys
     (map vector (range) new-children))))

(defn make-node [element r-f {:keys [next-id] :as ctx}]
  [next-id (-> ctx
               (update :next-id inc)
               (update :updates r-f {:noria/update-type :make-node
                                     :noria/node next-id
                                     :noria/type (get-type element)
                                     :noria/props (:noria/props element)}))])

(defn reconcile-primitive [component-id new-element r-f ctx]
  (let [[{:noria/keys [node element children component-id] :as component} ctx] (lookup component-id ctx)
        [node ctx] (if (some? node)
                     [node ctx]
                     (make-node new-element r-f ctx))
        [children-reconciled ctx'] (reconcile-children node children (set-keys (:children new-element)) r-f ctx)
        old-props (:noria/props element)
        new-props (:noria/props new-element)]
    [component-id
     (-> ctx'
         (update-in [:components component-id]
                    assoc
                    :noria/node node
                    :noria/element new-element
                    :noria/children children-reconciled)
         (cond-> 
           (and (:noria/node component) (not= old-props new-props))
           (update :updates r-f {:noria/update-type :update-props
                                 :noria/node node
                                 :noria/new-props new-props
                                 :noria/old-props old-props})))]))

(defn reconcile-user [component-id [xf & args :as element] r-f ctx]
  (let [[{:noria/keys [subst render state component-id] :as component} ctx'] (lookup component-id ctx)
        render (or render
                   (xf (fn
                         ([] {})
                         ([state element] (assoc state ::element element))
                         ([state] state))))
        state (or state (render))
        state' (render state args)]
    (if (and ((:components ctx) component-id) (:noria/skip-subtree? state'))
      [component-id (update-in ctx'
                               [:components component-id]
                               assoc
                               :noria/state state'
                               :noria/element element)]
      (let [[subst' ctx''] (reconcile subst (::element state') r-f ctx')
            subst-component ((:components ctx'') subst')]
        [component-id (update-in ctx''
                                 [:components component-id]
                                 assoc
                                 :noria/render render
                                 :noria/subst subst'
                                 :noria/state state'
                                 :noria/element element
                                 :noria/node (:noria/node subst-component))]))))

(defn reconcile [component-id element r-f ctx]
  (let [component ((:components ctx) component-id)
        [component-id ctx] (if (and (some? component)
                                    (not= (get-type (:noria/element component))
                                          (get-type element)))
                             [nil (destroy-recursively component-id r-f ctx)]
                             [component-id ctx])]
    (if (user-component? element)
      (reconcile-user component-id element r-f ctx)
      (reconcile-primitive component-id element r-f ctx))))

(comment
  (def my-label
    (noria.components/render (fn [x]
                               {:noria/type :text
                                :noria/props {:text (str x)}})))

  (def my-container
    (noria.components/render (fn [i]
                               {:noria/type :div
                                :noria/props {:orientation :vertical}
                                :children (map (fn [i]
                                                 ^{:noria/key i} [my-label i])
                                               (range i))})))

  
  (let [[c-id ctx] (reconcile nil [my-container 1] conj {:updates []
                                                         :components {}
                                                         :next-component-id 0
                                                         :next-id 0})]
    (def c-id c-id)
    (def ctx ctx))

  

  (reconcile c-id [my-container 2] conj (assoc ctx :updates []))


  

  (reconcile (first c0) [my-label 1] conj {:updates []
                                           :next-id 1})
  )
