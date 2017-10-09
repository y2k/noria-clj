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

(def ^:dynamic *sink* nil)

(defn build-component [{:keys [key elt] :as element} r-f ctx]
  (if (user-component? element)
    (let [sink (atom nil)
          [xf & args] elt
          render (xf (fn
                       ([] nil)
                       ([x y] (reset! *sink* y) nil)
                       ([x] x)))
          [state subst] (binding [*sink* (atom nil)]
                          [(apply render (render) args) @*sink*])
          [c-subst ctx'] (build-component {:elt subst
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

(defn reconcile-children [parent-node children new-children r-f ctx]
  (let [component-key (comp :key :noria/element)
        old-keys (map component-key children)
        new-keys (map :key new-children)]
    (if (not= old-keys new-keys)
      (let [common (into #{} (LCS/lcs (into-array Object old-keys)
                                      (into-array Object new-keys)))
            key->component (into {}
                                 (map (fn [c] [(component-key c) c]))
                                 children)
            
            new-keys-set (into #{} new-keys)
            ctx-with-removes (update ctx :updates
                                     (fn [updates]
                                       (transduce (comp (remove #(contains? common component-key))
                                                        (mapcat
                                                         (fn [{node :noria/node :as child}]
                                                           (let [remove {:noria/update-type :remove
                                                                         :noria/node node}]
                                                             (if (contains? new-keys-set (component-key child))
                                                               [remove]
                                                               [remove {:noria/update-type :destroy
                                                                        :noria/node node}])))))
                                                  r-f updates children)))

            [children-reconciled ctx'] (reduce-children (fn [child ctx]
                                                          (if-let [old-c (key->component (:key child))]
                                                            (reconcile old-c child r-f ctx)
                                                            (build-component child r-f ctx)))
                                                        ctx-with-removes new-children)]
        [children-reconciled
         (update ctx' :updates
                 (fn [updates]
                   (transduce (keep-indexed (fn [i {elt :noria/element
                                                   child-node :noria/node}]
                                              (when-not (contains? common (:key elt))
                                                {:noria/update-type :add
                                                 :noria/index i
                                                 :noria/child-node child-node
                                                 :noria/parent-node parent-node})))
                              conj! updates children-reconciled)))])
      
      (reduce-children (fn [[child-c child-e] ctx]
                         (reconcile child-c child-e r-f ctx))
                       ctx
                       (map vector children new-children)))))

(defn reconcile-primitive [{:noria/keys [node element children] :as component} new-element r-f ctx]
  (if (not= (get-type element) (get-type new-element))
    (let [[new-component ctx'] (build-component new-element r-f ctx)]
      [new-component (update ctx' :updates (fn [updates]
                                             (-> updates
                                                 (r-f {:noria/update-type :remove
                                                       :noria/node node})
                                                 (r-f {:noria/update-type :destroy
                                                       :noria/node node}))))])
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
                               :noria/old-props old-props}))])))

(defn reconcile-user [{:noria/keys [subst render state] :as component} {[_ & args] :elt key :key :as element} r-f ctx]  
  (let [[state' subst-e] (binding [*sink* (atom ::nil)]
                         [(apply render state args)
                          @*sink*])]
    (if (not= subst-e ::nil)
      (let [[subst' ctx'] (reconcile subst {:elt subst-e :key key} r-f ctx)]
        [(assoc component
                :noria/subst subst'
                :noria/state state'
                :noria/element element
                :noria/node (:noria/node subst')) ctx'])
      [(assoc component
              :noria/state state'
              :noria/element element) ctx])))

(defn reconcile [c element r-f ctx]
  (if (user-component? element)
    (reconcile-user c element r-f ctx)
    (reconcile-primitive c element r-f ctx)))

