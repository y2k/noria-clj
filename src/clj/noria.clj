(ns noria
  (:require [clojure.data.int-map :as i]
            [noria.thunks :as t]
            [clojure.spec.alpha :as s])
  (:import [noria LCS]
           [noria.thunks Thunk]))

(s/def :noria/node nat-int?)
(s/def :noria/update-type #{:add :remove :make-node :set-attr :destroy})
(s/def :noria/attr keyword?)
(s/def :noria/value any?)
(s/def :noria/index nat-int?)
(s/def :noria/type keyword?)
(s/def :noria/key any?)
(s/def :noria/constructor-parameters (s/map-of keyword? any?))

(defmulti update-spec :noria/update-type)
(defmethod update-spec :add [_]
  (s/keys :req [:noria/node :noria/attr :noria/value :noria/index]))
(defmethod update-spec :remove [_]
  (s/keys :req [:noria/node :noria/attr :noria/value]))
(defmethod update-spec :make-node [_]
  (s/keys :req [:noria/node :noria/type :noria/constructor-parameters]))
(defmethod update-spec :set-attr [_]
  (s/keys :req [:noria/node :noria/attr :noria/value]))
(defmethod update-spec :destroy [_]
  (s/keys :req [:noria/node]))

(s/def :noria/update (s/multi-spec update-spec :noria/update-type))


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

(s/def ::key-spec (s/? (s/cat :kw #{:noria/key}
                              :key any?)))

(s/def ::-< (s/cat :key-spec ::key-spec
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

(s/def ::-<< (s/cat :key-spec ::key-spec
                    :expr (s/* any?)))

(defmacro -<< [& stuff]
  (let [{{key :key} :key-spec
         :keys [expr]} (s/conform ::-<< stuff)
        key (or key `(quote ~(gensym)))]
    `(t/thunk* ~key (fn [] ~@expr) [])))

(def deref-or-value t/deref-or-value)

(defn thunk-def [params]
  (let [my-up-to-date? (:up-to-date? params =)
        my-compute (:compute params)
        my-changed? (:changed? params not=)
        my-destroy! (:destroy! params identity)]
    (assert (some? my-compute))
    (reify t/ThunkDef
      (up-to-date? [this state old-arg new-arg]
        (t/with-thunks-forbidden my-up-to-date? old-arg new-arg))
      (compute [this state args]
        (my-compute state args))
      (changed? [this old-value new-value]
        (t/with-thunks-forbidden my-changed? old-value new-value))
      (destroy! [this state]
        (my-destroy! state)))))

(def ^:dynamic *updates* nil)
(def ^:dynamic *next-node* nil)
(def ^:dynamic *callbacks* nil)

(defn unordered? [new-exprs]
  (::unordered? (meta new-exprs)))

(def node
  (reify t/ThunkDef
    (up-to-date? [this state old-arg new-arg] (= old-arg new-arg))
    (compute [this state [type attrs]]
      (let [old-type (::type state)
            old-node (::node state)
            constructor-params (get-constructor-parameters type)
            reduce-keys-of-two-maps (fn [r-f state m1 m2]
                                      (as-> state <>
                                        (reduce-kv (fn [state a v]
                                                     (r-f state a v (get m2 a)))
                                                   <> m1)
                                        (reduce-kv (fn [state a v]
                                                     (if (contains? m1 a)
                                                       state
                                                       (r-f state a nil v)))
                                                   <> m2)))
            node-id (if (= old-type type)
                      old-node
                      (let [node-id (swap! *next-node* inc)]
                        (when old-node
                          (swap! *updates* conj! {:noria/update-type :destroy
                                                  :noria/node old-node}))                        
                        node-id))
            attrs'
            (if (= old-node node-id)
              (persistent!
               (reduce-keys-of-two-maps
                (fn [state attr old-value new-value]
                  (let [new-value (t/deref-or-value new-value)
                        data-type (if (or (fn? new-value) (fn? old-value))
                                    :callback
                                    (get-data-type attr))]
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
                        (swap! *callbacks* assoc-in [node-id (if (keyword? attr) (name attr) attr)] new-value)
                        (cond
                          (and (nil? old-value) (nil? new-value))
                          state

                          (and (nil? old-value) (some? new-value))
                          (do
                            (swap! *updates* conj! {:noria/update-type :set-attr
                                                    :noria/node node-id
                                                    :noria/attr attr
                                                    :noria/value (if (:noria/sync (meta new-value))
                                                                   :noria-handler-sync
                                                                   :noria-handler-async)})
                            (assoc! state attr new-value))

                          (and (some? old-value) (nil? new-value))
                          (do
                            (swap! *updates* conj! {:noria/update-type :set-attr
                                                    :noria/node node-id
                                                    :noria/attr attr
                                                    :noria/value :-noria-handler})
                            (dissoc! state attr))

                          :else state))
                      :nodes-seq (let [unordered? (unordered? new-value)
                                       new-nodes (into (if unordered? (i/int-set) [])
                                                       (keep t/deref-or-value)
                                                       new-value)
                                       old-nodes (or old-value (if unordered? (i/int-set) []))
                                       updates (if unordered?
                                                 (update-set node-id attr old-nodes new-nodes)
                                                 (update-order node-id attr old-nodes new-nodes))]
                                   (swap! *updates* (fn [u] (reduce conj! u updates)))
                                   (assoc! state attr new-nodes))))) 
                (transient (::attrs state))
                (::attrs state)
                attrs))
              (let [[attrs' constr updates]
                    (reduce (fn [[attrs constr updates] [attr value]]
                              (let [new-value (t/deref-or-value value)
                                    data-type (if (fn? new-value)
                                                :callback
                                                (get-data-type attr))]
                                (case data-type
                                  (:node :simple-value)
                                  (if (contains? constructor-params attr)
                                    [(assoc! attrs attr new-value) (assoc! constr attr new-value) updates]
                                    [(assoc! attrs attr new-value)
                                     constr
                                     (conj! updates {:noria/update-type :set-attr
                                                     :noria/value new-value
                                                     :noria/node node-id
                                                     :noria/attr attr})])
                                  
                                  :callback
                                  (do
                                    (swap! *callbacks* assoc-in [node-id (if (keyword? attr) (name attr) attr)] new-value)
                                    (let [cb (if (:noria/sync (meta new-value))
                                               :noria-handler-sync
                                               :noria-handler-async)]
                                      (if (contains? constructor-params attr)
                                        [(assoc! attrs attr cb) (assoc! constr attr cb) updates]
                                        [(assoc! attrs attr cb)
                                         constr
                                         (conj! updates {:noria/update-type :set-attr
                                                         :noria/node node-id
                                                         :noria/attr attr
                                                         :noria/value cb})])))
                                  :nodes-seq
                                  (let [new-nodes (into (if (unordered? new-value) (i/int-set) [])
                                                        (keep t/deref-or-value)
                                                        new-value)]
                                    (if (contains? constructor-params attr)
                                      [(assoc! attrs attr new-nodes) (assoc! constr attr new-nodes) updates]
                                      [(assoc! attrs attr new-nodes)
                                       constr
                                       (transduce
                                        (map-indexed (fn [i e]
                                                       {:noria/update-type :add
                                                        :noria/node node-id
                                                        :noria/attr attr
                                                        :noria/value e
                                                        :noria/index i}))
                                        conj!
                                        updates
                                        new-nodes)])))))
                            [(transient {}) (transient {}) (transient [])]
                            attrs)]
                (swap! *updates* (fn [u]
                                   (reduce conj!
                                           (conj! u
                                                  {:noria/update-type :make-node
                                                   :noria/type type
                                                   :noria/node node-id
                                                   :noria/constructor-parameters (persistent! constr)})
                                           (persistent! updates))))
                (persistent! attrs')))]
        [(assoc state
                ::attrs attrs'
                ::type type
                ::node node-id) node-id]))
    (destroy! [this {::keys [node attrs] :as state}]
      #_(doseq [[attr value] attrs]
        (case (get-data-type attr)
          :node (swap! *updates* conj! {:noria/update-type :set-attr
                                        :noria/node node
                                        :noria/attr attr
                                        :noria/value nil})
          :nodes-seq (doseq [n value]
                      (swap! *updates* conj! {:noria/update-type :remove
                                              :noria/node node
                                              :noria/attr attr
                                              :noria/value n}))
          nil))
      (swap! *updates* conj! {:noria/update-type :destroy
                              :noria/node node}))
    (changed? [this old-value new-value] (not= old-value new-value))))

(defn evaluate [graph f args-vector & {:keys [dirty-set middleware]
                                       :or {dirty-set (i/int-set)
                                            middleware identity}}]
  (binding [*updates* (atom (transient []))
            *callbacks* (atom (or (::callbacks graph) {}))
            *next-node* (atom (or (::next-node graph) 0))]
    (let [[graph value] (t/evaluate graph f args-vector
                                    :dirty-set dirty-set
                                    :middleware middleware)]
      [(assoc graph
              ::next-node @*next-node*
              ::callbacks @*callbacks*)
       (persistent! @*updates*)
       value])))
