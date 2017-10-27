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
    (assoc :noria/key k)))

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

(defn destroy-recursively [component r-f ctx]
  (let [children (fn [c] (or (:noria/children c)
                            (some-> (:noria/subst c) (vector))))]
    (update ctx :updates
            (fn [updates]
              (transduce (comp
                          (map (fn [component]
                                 (if (user-component? (:noria/element component))
                                   ((:noria/render component) (:noria/state component)))
                                 component))
                          (map :noria/node)
                          (dedupe)
                          (mapcat (fn [node]
                                    [{:noria/update-type :remove
                                      :noria/node node}
                                     {:noria/update-type :destroy
                                      :noria/node node}])))
                         r-f
                         updates
                         (reverse (tree-seq children children component)))))))

(defn reconcile-children [parent-node children new-children r-f ctx]
  (let [component-key (comp get-key :noria/element)
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
                                        (destroy-recursively c r-f ctx)) ctx))]
    (reduce-children
     (fn [[i child] ctx]
       (let [old-c (key->component (:key child))
             [new-c ctx'] (reconcile old-c child r-f ctx)]
         [new-c (cond-> ctx'
                  (and (some? old-c) (moved? (:key child)))
                  (update :updates r-f {:noria/update-type :remove
                                        :noria/node (:noria/node old-c)})
                  
                  (or (not= (:noria/node old-c) (:noria/node new-c))
                      (moved? (:key child)))
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

(defn reconcile-primitive [{:noria/keys [node element children] :as component} new-element r-f ctx]
  (let [[node ctx] (if (some? node)
                     [node ctx]
                     (make-node new-element r-f ctx))
        [children-reconciled ctx'] (reconcile-children node children (set-keys (:children new-element)) r-f ctx)
        old-props (:noria/props element)
        new-props (:noria/props new-element)]
    [(assoc component
            :noria/node node
            :noria/element new-element
            :noria/children children-reconciled)
     (cond-> ctx'
       (and (:noria/node component) (not= old-props new-props))
       (update :updates r-f {:noria/update-type :update-props
                             :noria/node node
                             :noria/new-props new-props
                             :noria/old-props old-props}))]))

(defn reconcile-user [{:noria/keys [subst render state] :as component} [xf & args :as element] r-f ctx]
  (let [render (or render
                   (xf (fn
                         ([] {})
                         ([state element] (assoc state ::element element))
                         ([state] state))))
        state (or state (render))
        state' (render state args)]
    (if (and (some? component) (:noria/skip-subtree? state'))
      [(assoc component
              :noria/state state'
              :noria/element element) ctx]
      (let [[subst' ctx'] (reconcile subst (::element state') r-f ctx)]
        [(assoc component
                :noria/render render
                :noria/subst subst'
                :noria/state state'
                :noria/element element
                :noria/node (:noria/node subst')) ctx']))))

(defn reconcile [component element r-f ctx]
  (let [[component ctx] (if (and (some? component)
                                 (not= (get-type (:noria/element component)) (get-type element)))
                          [nil (destroy-recursively component r-f ctx)]
                          [component ctx])]
    (if (user-component? element)
      (reconcile-user component element r-f ctx)
      (reconcile-primitive component element r-f ctx))))

(comment
  (def my-label
    (noria.components/render (fn [x]
                               {:noria/type :text
                                :noria/props {:text (str x)}})))

  (def my-container
    (noria.components/render (fn [_]
                               {:noria/type :div
                                :children [^{:noria/key "xy"} [my-label 10]
                                                 ^{:noria/key "xyzw"} [my-label 12]]
                                :noria/props {:orientation :vertical}})))

  
  (reconcile nil [my-container 42] conj {:updates []
                                         :next-id 0})


  

  (reconcile (first c0) [my-label 1] conj {:updates []
                                           :next-id 1})
  )
