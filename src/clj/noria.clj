(ns noria
  (:require [clojure.data.int-map :as i]
            [noria.thunks :as t]
            [clojure.spec.alpha :as s])
  (:import [noria LCS]
           [noria.thunks Thunk]
           [gnu.trove TLongHashSet TLongArrayList TObjectLongHashMap]))

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


(defonce schema (atom {:constructors {}
                       :attrs {}}))

(defn data-type [attr]
  (or (::data-type (get-in @schema [:attrs attr])) :simple-value))

(defn seq-kind [attr]
  (or (::seq-kind (get-in @schema [:attrs attr])) :vector))

(defn defattr [attr data]
  (swap! schema assoc-in [:attrs attr] data))

(defn constructor-parameters [node-type]
  (get-in @schema [:constructors node-type :attrs] #{}))

(defn default-values [node-type]
  (get-in @schema [:constructors node-type :default-values] {}))

(defn defconstructor [node-type {:keys [attrs default-values] :as opts}]
  (swap! schema assoc-in [:constructors node-type] {:attrs attrs
                                                    :default-values default-values}))


(defn update-order [parent-node attr ^TLongArrayList old-nodes ^TLongArrayList new-nodes]
  (if (= old-nodes new-nodes)
    nil
    (let [lcs (TLongHashSet. (LCS/lcs (.toNativeArray old-nodes) (.toNativeArray new-nodes)))
          res (java.util.ArrayList.)]
      (dotimes [i (.size old-nodes)]
        (let [node (.get old-nodes i)]
          (when-not (.contains lcs node)
            (.add res {::update-type :remove
                       ::attr attr
                       ::node parent-node
                       ::value node}))))
      (dotimes [i (.size new-nodes)]
        (let [node (.get new-nodes i)]
          (when-not (.contains lcs node)
            (.add res {::update-type :add
                       ::attr attr
                       ::node parent-node
                       ::value node
                       ::index i}))))
      res)))

(defn update-set [parent-node attr ^TLongHashSet old-nodes ^TLongHashSet new-nodes]
  (let [updates (java.util.ArrayList.)
        i (volatile! 0)]
    (.forEach old-nodes (reify gnu.trove.TLongProcedure
                          (^boolean execute [_ ^long node]
                           (when-not (.contains new-nodes node)
                             (.add updates {::update-type :remove
                                            ::attr attr
                                            ::node parent-node
                                            ::value node}))
                           true)))

    (.forEach new-nodes (reify gnu.trove.TLongProcedure
                          (^boolean execute [_ ^long node]
                           (when-not (.contains old-nodes node)
                             (.add updates {::update-type :add
                                            ::attr attr
                                            ::node parent-node
                                            ::value node
                                            ::index (vswap! i inc)}))
                           true)))
    updates))

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

(defn flat [coll]
  (into []
        (fn flat-xf [r-f]
          (fn
            ([] (r-f))
            ([s i]
             (let [i (noria/deref-or-value i)]
               (if (sequential? i)
                 (transduce flat-xf r-f s i)
                 (r-f s i))))
            ([s] (r-f s))))
        (noria/deref-or-value coll)))

(defn fmap [f & args]
  (apply f (map deref-or-value args)))

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

(defn fatty [thunk-def]
  (reify t/ThunkDef
      (up-to-date? [this state old-arg new-arg]
        (t/up-to-date? thunk-def state old-arg new-arg))
      (compute [this state args]
        (t/compute thunk-def state args))
      (changed? [this old-value new-value]
        true)
      (destroy! [this state]
        (t/destroy! thunk-def state))))

(def ^:dynamic *updates* nil)
(def ^:dynamic *next-node* nil)
(def ^:dynamic *callbacks* nil)

(defn unordered? [new-exprs]
  (::unordered? (meta new-exprs)))

(def node
 (with-meta
  (reify t/ThunkDef
    (up-to-date? [this state old-arg new-arg] (= old-arg new-arg))
    (compute [this state [type attrs]]
      (let [old-type (::type state)
            old-node (::node state)
            constructor-params (constructor-parameters type)
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
            init-children (fn [new-value]
                            (if (nil? new-value)
                              nil
                              (if (unordered? new-value)
                                (transduce (keep t/deref-or-value)
                                           (completing
                                            (fn [^TLongHashSet s ^long i]
                                              (.add s i)
                                              s))
                                           (TLongHashSet.)
                                           new-value)
                                (transduce (keep t/deref-or-value)
                                           (completing
                                            (fn [^TLongArrayList s ^long i]
                                              (.add s i)
                                              s))
                                           (TLongArrayList.)
                                           new-value))))
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
                                    (data-type attr))]
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
                      :nodes-seq
                      (let [new-nodes (if (nil? new-value)
                                        (if (nil? old-value)
                                          nil
                                          (if (unordered? old-value)
                                            (TLongHashSet.)
                                            (TLongArrayList.)))
                                        (init-children new-value))
                            old-nodes (or old-value (if (unordered? new-value) (TLongHashSet.) (TLongArrayList.)))
                            updates (if (and (nil? new-value)
                                             (nil? old-value))
                                      nil (if (unordered? new-value)
                                            (update-set node-id attr old-nodes new-nodes)
                                            (update-order node-id attr old-nodes new-nodes)))]
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
                                                (data-type attr))]
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
                                  (let [new-nodes (init-children new-value)]
                                    (if (contains? constructor-params attr)
                                      [(assoc! attrs attr new-nodes)
                                       (assoc! constr attr new-nodes)
                                       updates]
                                      [(assoc! attrs attr new-nodes)
                                       constr
                                       (transduce
                                        (comp
                                         (keep t/deref-or-value)
                                         (map-indexed (fn [i e]
                                                        {:noria/update-type :add
                                                         :noria/node node-id
                                                         :noria/attr attr
                                                         :noria/value e
                                                         :noria/index i})))
                                        conj!
                                        updates
                                        new-value)])))))
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
        [{::attrs attrs'
          ::type type
          ::node node-id} node-id]))
    (destroy! [this {::keys [node attrs] :as state}]
      (swap! *callbacks* dissoc node)
      #_(doseq [[attr value] attrs]
        (case (data-type attr)
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
    (changed? [this old-value new-value] (not= old-value new-value)))
    {:noria/primitive true}))

(defn evaluate [graph f args-vector & {:keys [dirty-set middleware assert?]
                                       :or {dirty-set (i/int-set)
                                            assert? false
                                            middleware identity}}]
  (binding [*updates* (atom (transient []))
            *callbacks* (atom (or (::callbacks graph) {}))
            *next-node* (atom (or (::next-node graph) 0))]
    (let [[graph value] (t/evaluate graph f args-vector
                                    :dirty-set dirty-set
                                    :middleware middleware
                                    :assert? assert?)]
      [(assoc graph
              ::next-node @*next-node*
              ::callbacks @*callbacks*)
       (persistent! @*updates*)
       value])))
