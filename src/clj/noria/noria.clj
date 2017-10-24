(ns noria.noria
  (:import [noria LCS]))

(defn- get-props* [[_ props]]
  (if (map? props) props nil))

(defn get-children [{[_ props & r] :elt}]
  (let [children (cond
                   (map? props) r
                   (some? props) (cons props r)
                   :else r)]
    (persistent!
     (second (reduce (fn [[indices res] e]
                       (if (some? e)
                         (if-let [key (or (:key (get-props* e)) (:key (meta e)))]
                           [indices (conj! res {:elt e :key key})]
                           (let [type (first e)
                                 indices' (update indices type (fn [i] (if i (inc i) 0)))
                                 idx (indices' type)]
                             [indices' (conj! res {:elt e :key [type idx]})]))
                         [indices res]))
                     [{} (transient [])]
                     children)))))

(defn get-props [elt-with-key]
  (get-props* (:elt elt-with-key)))

(defn get-type [{[type] :elt}] type)

(defn user-component? [elt]
   (fn? (get-type elt)))

(defn reduce-children [f ctx children]
  (let [[recons ctx'] (reduce (fn [[recons ctx] c]
                                (let [[r ctx'] (f c ctx)]
                                  [(conj! recons r) ctx']))
                              [(transient []) ctx]
                              children)]
    [(persistent! recons) ctx']))

(defn build-component [{:keys [key elt] :as element} r-f ctx]
  (if (user-component? element)
    (let [[xf & args] elt
          render (xf (fn
                       ([] {})
                       ([state element] (assoc state ::element element))
                       ([state] state)))
          _ (assert (fn? render) {:element element})
          state (render (render) args)
          [c-subst ctx'] (build-component {:elt (::element state)
                                           :key key} r-f ctx)]
      [{:noria/node (:noria/node c-subst)
        :noria/state state
        :noria/render render
        :noria/element element
        :noria/subst c-subst}
       ctx'])
    (let [new-node (:next-id ctx)
          [c-components ctx'] (reduce-children #(build-component %1 r-f %2)
                                               (update ctx :next-id inc)
                                               (get-children element))]
      
      (assert (= (count (get-children element))
                 (count (into #{} (map :key) (get-children element))))
              {:error "keys should be distinct"
               :keys (map :key (get-children element))})
      
      [{:noria/node new-node
        :noria/element element
        :noria/children c-components}
       (update ctx' :updates (fn [updates]
                               (transduce
                                (map-indexed (fn [i c]
                                               {:noria/update-type :add
                                                :noria/index i
                                                :noria/parent-node new-node
                                                :noria/child-node (:noria/node c)}))
                                r-f
                                (r-f updates
                                     {:noria/update-type :make-node
                                      :noria/node new-node
                                      :noria/node-type (get-type element)
                                      :noria/props (get-props element)})
                                c-components)))])))

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
  (let [component-key (comp :key :noria/element)
        old-keys (map component-key children)
        new-keys (mapv :key new-children)
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
             [new-c ctx'] (if (some? old-c)
                            (reconcile old-c child r-f ctx)
                            (build-component child r-f ctx))]
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

(defn reconcile-primitive [{:noria/keys [node element children] :as component} new-element r-f ctx]
  (let [new-children (get-children new-element)
        [children-reconciled ctx'] (reconcile-children node children new-children r-f ctx)
        old-props (get-props element)
        new-props (get-props new-element)]
    [(assoc component
            :noria/element new-element
            :noria/children children-reconciled)
     (cond-> ctx'
       (not= old-props new-props)
       (update :updates r-f {:noria/update-type :update-props
                             :noria/node node
                             :noria/new-props new-props
                             :noria/old-props old-props}))]))

(defn reconcile-user [{:noria/keys [subst render state] :as component} {[_ & args] :elt key :key :as element} r-f ctx]
  (let [state' (render state args)]
    (if (:noria/skip-subtree? state')
      [(assoc component
              :noria/state state'
              :noria/element element) ctx]
      (let [[subst' ctx'] (reconcile subst {:elt (::element state') :key key} r-f ctx)]
        [(assoc component
                :noria/subst subst'
                :noria/state state'
                :noria/element element
                :noria/node (:noria/node subst')) ctx']))))

(defn reconcile [component element r-f ctx]
  (if (not= (get-type (:noria/element component)) (get-type element))
    (let [[new-component ctx'] (build-component element r-f ctx)]
      [new-component (destroy-recursively component r-f ctx')])
    (if (user-component? element)
      (reconcile-user component element r-f ctx)
      (reconcile-primitive component element r-f ctx))))
