(ns noria.noria
  (:require [clojure.spec.alpha :as s])
  (:import [noria LCS]))

(s/def ::element-type (s/or :primitive keyword? :user fn?))
(s/def ::props (s/map-of keyword? any?))

(s/def ::primitive (s/spec (s/cat :type keyword? :props (s/? ::props) :children (s/* ::element))))
(s/def ::user (s/spec (s/cat :type ::element-type :props (s/? ::props) :children (s/* ::element))))
(s/def ::element (s/or ::primitive ::user))

(s/def ::key any?)
(s/def ::element-with-key (s/keys :req-un [::element ::key]))

(s/def :component/element ::element-with-key)
(s/def ::node any?)
(s/def :component/node ::node)

(s/def ::primitive-component (s/keys :req [:component/node
                                           :component/element
                                           :component/children]))

(s/def ::user-component (s/keys :req [:component/node
                                      :component/element
                                      :component/subst]))

(s/def ::component (s/or ::user-compomnent ::primitive-component))
(s/def :component/subst ::component)



(s/def :component/children (s/coll-of ::component))

(defmulti update-spec :update/type)
(s/def ::update (s/multi-spec update-spec :update/type))
(s/def :make-node/node ::node)
(s/def :make-node/props ::props)
(s/def :make-node/type keyword?)

(defmethod update-spec :make-node [_] (s/keys :req [:make-node/node
                                                    :make-node/props
                                                    :make-node/type]))

(s/def :update-props/node ::node)
(s/def :update-props/props-diff ::props)

(defmethod update-spec :update-props [_] (s/keys :req [:update-props/node
                                                       :update-props/props-diff]))

(s/def :remove/node ::node)
(defmethod update-spec :remove [_] (s/keys :req [:remove/node]))

(s/def :destroy/node ::node)
(defmethod update-spec :destroy [_] (s/keys :req [:destroy/node]))

(s/def :add/index nat-int?)
(s/def :add/parent ::node)
(s/def :add/child ::node)

(defmethod update-spec :add [_] (s/keys :req [:add/index
                                              :add/parent
                                              :add/child]))


(defn- get-props* [[_ props]]
  (if (map? props) props nil))

(defn get-children [{[_ props & r] :elt}]
  (let [children (cond
                   (map? props) r
                   (some? props) (cons props r)
                   :else r)]
    (persistent! (second (reduce (fn [[indices res] e]
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

(defn map-children [f ctx children]
  (let [[recons* ctx'] (reduce (fn [[recons ctx] c]
                                     (let [[r ctx'] (f c ctx)]
                                       [(conj! recons r) ctx']))
                                   [(transient []) ctx]
                                   children)]
    [(persistent! recons*) ctx']))

(def ^:dynamic *sink* nil)

(defn build-component [{[type & args] :elt key :key :as elt} ctx]
  (if (user-component? elt)
    (let [sink (atom nil)
          render (type (fn
                         ([] nil)
                         ([x y] (reset! *sink* y) nil)
                         ([x] x)))
          [state subst] (binding [*sink* (atom nil)]
                          [(apply render (render) args) @*sink*])
          [c-subst ctx'] (build-component {:elt subst
                                           :key key} ctx)]
      [{:component/node (:component/node c-subst)
        :component/state state
        :component/render render
        :component/element elt
        :component/subst c-subst}
       ctx'])
    (let [new-node (:next-id ctx)
          [c-components ctx'] (map-children build-component (update ctx :next-id inc) (get-children elt))]
      [{:component/node new-node
        :component/element elt
        :component/children c-components}
       (update ctx' :updates (fn [updates]
                               (transduce
                                (map-indexed (fn [i c]
                                               {:update/type :add
                                                :add/index i
                                                :add/parent new-node
                                                :add/child (:component/node c)}))
                                conj!
                                (conj! updates
                                       {:update/type :make-node
                                        :make-node/node new-node
                                        :make-node/type type
                                        :make-node/props (get-props elt)})
                                c-components)))])))

(declare reconcile)

(defn reconcile-children [{c-children :component/children
                           c-node :component/node :as c} new-children ctx]
  (let [old-keys (map (comp :key :component/element) c-children)
        new-keys (map :key new-children)]
    (if (not= old-keys new-keys)
      (let [common (into #{} (LCS/lcs (into-array Object old-keys) (into-array Object new-keys)))
            key->component (into {}
                                 (map (fn [c] [(:key (:component/element c)) c]))
                                 c-children)
            
            new-keys-set (into #{} new-keys)
            ctx-with-removes (update ctx :updates
                                     (fn [updates]
                                       (transduce (comp (remove #(contains? common (-> % :component/element :key)))
                                                        (mapcat
                                                         (fn [{node :component/node
                                                              elt :component/element}]
                                                           (let [remove {:update/type :remove
                                                                         :remove/node node}]
                                                             (if (contains? new-keys-set (:key elt))
                                                               [remove]
                                                               [remove {:update/type :destroy
                                                                        :destroy/node node}])))))
                                                  conj! updates c-children)))

            [children-reconciled ctx'] (map-children (fn [child ctx]
                                                       (if-let [old-c (key->component (:key child))]
                                                         (reconcile old-c child ctx)
                                                         (build-component child ctx)))
                                                     ctx-with-removes new-children)]
        [children-reconciled
         (update ctx' :updates
                 (fn [updates]
                   (transduce (keep-indexed (fn [i {elt :component/element
                                                   child-node :component/node}]
                                              (when-not (contains? common (:key elt))
                                                {:update/type :add
                                                 :add/index i
                                                 :add/child child-node
                                                 :add/parent c-node})))
                              conj! updates children-reconciled)))])
      
      (map-children (fn [[child-c child-e] ctx]
                      (reconcile child-c child-e ctx))
                    ctx
                    (map vector c-children new-children)))))

(defn reconcile-primitive [{old-elt :component/element
                            node :component/node :as c} elt ctx]
  (if (not= (get-type old-elt) (get-type elt))
    (build-component elt ctx)
    (let [new-children (get-children elt)
          [children-reconciled ctx'] (reconcile-children c new-children ctx)
          old-props (get-props old-elt)
          new-props (get-props elt)]
      [(assoc c
              :component/element elt
              :component/children children-reconciled)
       (cond-> ctx'
         (not= old-props new-props)
         (update :updates conj! {:update/type :update-props
                                 :update-props/node node
                                 :update-props/new-props new-props
                                 :update-props/old-props old-props}))])))

(defn reconcile-user [{c-subst :component/subst
                       render :component/render
                       state :component/state
                       old-elt :component/element :as c} {[_ & args] :elt key :key :as elt} ctx]  
  (let [[state' subst] (binding [*sink* (atom ::nil)]
                         [(apply render state args)
                          @*sink*])]
    (if (not= subst ::nil)
      (let [[new-c-subst ctx'] (reconcile c-subst {:elt subst :key key} ctx)]
        [(assoc c
                :component/subst new-c-subst
                :component/state state'
                :component/element elt
                :component/node (:component/node new-c-subst)) ctx'])
      [(assoc c
              :component/state state'
              :component/element elt) ctx])))

(defn reconcile [c elt ctx]
  (if (user-component? elt)
    (reconcile-user c elt ctx)
    (reconcile-primitive c elt ctx)))

