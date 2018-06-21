(ns noria.thunks
  "When comparing inputs any reasonable default can not know about input form.
  So it can not distinguish thunks from values.
  And it can not deref old thunks because they are new now
  => We must compare thunk inputs as identities
  When evaluating some parent we need to compare inputs to stop children from evaluation.
  There are two cases:
  (if (Some child dep is marked as dirty/changed)
    (there is no sense in comparing inputs with = as we know thunks are changed anyways (and up-to-date? will give false positive))
    (there is no sense in comparing current thunk's values as we know they are unchanged))
  But we need a place to stop change propagation. Change is a value. So we must compare output"
  (:require [clojure.pprint :as pprint]
            [clojure.data.int-map :as i])
  (:import [gnu.trove TLongHashSet TLongArrayList TObjectLongHashMap]))

(defprotocol ThunkDef
  (destroy! [this state])
  (compute [this state arg])
  (up-to-date? [this state old-arg new-arg])
  (changed? [this old-value new-value]))

(extend-protocol ThunkDef
  clojure.lang.AFn
  (up-to-date? [this state old-arg new-arg] (= old-arg new-arg))
  (compute [this state args]
    [state (apply this args)])
  (changed? [this old-value new-value] (not= old-value new-value))
  (destroy! [this state]))

(defprotocol Dumb)

(deftype Calc [state
               args
               value
               thunk-def                 
               ^TLongHashSet deps                 
               ^TObjectLongHashMap children-by-keys
               ^TLongArrayList children])

(deftype Frame [^TObjectLongHashMap flashbacks
                ^TLongHashSet deps                  
                ^TObjectLongHashMap children-by-keys
                ^TLongArrayList children])

(def ^{:tag 'ThreadLocal} >-frame-< (ThreadLocal.))
(defn current-frame ^Frame [] (.get >-frame-<))

(def ^{:tag 'ThreadLocal} >-graph-< (ThreadLocal.))
(defn current-graph [] (.get >-graph-<))

(def next-id (atom 0))

(defn empty-frame [flashbacks]
  (Frame. flashbacks
          (TLongHashSet.)
          (TObjectLongHashMap.)
          (TLongArrayList.)))

(defmacro new-frame [g flashbacks & body]
  `(let [old-frame# (.get >-frame-<)]
     (try
       (let [old-g# (.get >-graph-<)
             new-frame# (empty-frame ~flashbacks)]
         (.set >-graph-< ~g)
         (.set >-frame-< new-frame#)
         (let [res# (do ~@body)]         
           [res# new-frame# (.get >-graph-<)]))
       (finally
         (.set >-frame-< old-frame#)))))

(def ^:dynamic *assert?* false)

(defn t-intersects? [^TLongHashSet s1 ^TLongHashSet s2]
  (if (<= (.size s1) (.size s2))
    (let [i (.iterator s1)]
      (loop []
        (if (.hasNext i)
          (if (.contains s2 (.next i))
            true
            (recur))
          false)))
    (recur s2 s1)))

(defn t-contains? [^TLongHashSet s1 ^long e]
  (.contains s1 e))

(defn t-conj! [^TLongHashSet s ^long e]
  (.add s e)
  s)

(defn append-child! [key ^long id]
  (let [^Frame frame (current-frame)]
    (.add ^TLongArrayList (.-children frame) id)
    (.put ^TObjectLongHashMap (.-children-by-keys frame) key id)))

(deftype Thunk [^long id]
  clojure.lang.IDeref
  (deref [this]
    (when-let [frame (current-frame)]
      (t-conj! (.-deps frame) id))
    (assert (some? (current-graph)) {:error "Thunk deref outside of computation"
                                     :thunk-id id})
    (let [v (.-value ^Calc (get (::values (current-graph)) id))]
      (if (instance? Thunk v)
        (deref v)
        v)))
  java.lang.Object
  (toString [this]
    (str "[Thunk#" id "]"))
  (equals [this other]
    (and (instance? Thunk other) (= (.-id ^Thunk other) id))))

(defmethod print-method Thunk [o, ^java.io.Writer w]
  (.write w ^String (str o)))

(defmethod pprint/simple-dispatch Thunk [s]
  (pr s))

(defn reduce-int-array [^clojure.lang.IFn$OLO f init ^TLongArrayList array]
  (let [acc (volatile! init)]    
    (.forEach
     array
     (reify gnu.trove.TLongProcedure
       (^boolean execute [_ ^long id]
        (let [acc' (.invokePrim f @acc id)]
          (if (reduced? acc')
            (do
              (vreset! acc @acc')
              false)
            (do
              (vreset! acc acc')
              true))))))
    @acc))

(defn gc [graph old-children new-children]
  (let [^TLongHashSet ids (doto (TLongHashSet. (.toNativeArray ^TLongArrayList old-children))
                            (.removeAll (.toNativeArray ^TLongArrayList new-children)))]
    (update
     graph ::values
     (fn [values]
       (let [old-values (::values graph)
             middleware (::middleware graph)
             destroy-rec (fn destroy-rec [values ^long id]
                           (let [^Calc c (get old-values id)
                                 r (reduce-int-array destroy-rec
                                                     (dissoc! values id)
                                                     (.-children c))]
                             (destroy! (middleware (.-thunk-def c))
                                       (.-state c))
                             r))
             iterator (.iterator ids)]
         (loop [s (transient values)]
           (if (.hasNext iterator)                                 
             (recur (destroy-rec s (.next iterator)))
             (persistent! s))))))))

(defn reconcile-by-id [graph ^long id thunk-def args]  
  (let [^Calc calc (get (::values graph) id)
        thunk-def-wrapped ((::middleware graph) thunk-def)]
    (if (t-contains? (::up-to-date graph) id)
      graph
      (if (and (some? calc)
               (not (contains? (::dirty-set graph) id))
               (not (t-intersects? (::triggers graph) (.-deps calc)))
               (identical? thunk-def (.-thunk-def calc))
               (up-to-date? thunk-def-wrapped (.-state calc) (.-args calc) args))
        (do
          (t-conj! (::up-to-date graph) id)
          graph)
        (let [[[state' value'] ^Frame frame graph'] (new-frame
                                              graph 
                                              (if calc (.-children-by-keys calc) (TObjectLongHashMap.))
                                              (compute thunk-def-wrapped
                                                       (if calc
                                                         (.-state calc)
                                                         {:noria/id id})
                                                       args))
              deps' (.-deps frame)
              children' (.-children frame)
              children-by-keys' (.-children-by-keys frame)]
          (t-conj! (::up-to-date graph') id)
          (-> graph'
              (cond-> (and calc (not= (.-children calc) children'))
                (gc (.-children calc) children'))
              (update ::values assoc id
                      (Calc. state' args value' thunk-def deps'
                             children-by-keys'
                             children'))
              (cond-> (and (some? calc)
                           (changed? thunk-def-wrapped (.-value calc) value'))
                (update ::triggers t-conj! id))))))))

(defn reconcile-thunk [graph ^TObjectLongHashMap flashbacks thunk-def key args]
  (if-let [id (when flashbacks
                (when (.containsKey flashbacks key)
                  (.get flashbacks key)))]
    [id (reconcile-by-id graph id thunk-def args)]
    (let [graph' (update graph ::max-thunk-id inc)
          id (swap! next-id inc)]
      [id (reconcile-by-id graph' id thunk-def args)])))

(defn thunk* [key thunk-def args-vector]
  (let [[id graph'] (reconcile-thunk (current-graph)
                                     (.-flashbacks (current-frame))
                                     thunk-def
                                     key
                                     args-vector)]
    (.set >-graph-< graph')
    (append-child! key id)
    (Thunk. id)))

(defn deref-or-value [thunk-or-value]
  (if (instance? Thunk thunk-or-value)
    (deref thunk-or-value)
    thunk-or-value))

(defn traverse-graph [graph id dirty-set]
  (let [^Calc c (get (::values graph) id)]
    (if (or (contains? dirty-set id)
            (t-intersects? (::triggers graph) (.-deps c)))
      (let [graph' (reconcile-by-id graph id (.-thunk-def c) (.-args c))
            ^Calc c' (get (::values graph') id)]
        (reduce-int-array
         (fn [g ^long id]
           (traverse-graph g id dirty-set))
         graph'
         (.-children c')))
      (reduce-int-array
       (fn [g ^long c-id]
         (let [g' (traverse-graph g c-id dirty-set)]
           (if (and (t-contains? (.-deps c) c-id)
                    (t-contains? (::triggers g') c-id))
             (reduced (traverse-graph g' id dirty-set))
             g')))
       graph
       (.-children c)))))

(def graph-0 {::values (i/int-map)
              ::max-thunk-id 0})

(defn evaluate [graph f args-vector & {:keys [dirty-set middleware assert?]
                                       :or {dirty-set (i/int-set)
                                            assert? false
                                            middleware identity}}]
  (binding [*assert?* assert?]
    (let [old-graph (.get >-graph-<)
          first-run? (nil? (::root graph))
          up-to-date (TLongHashSet.)
          [root-id graph] (reconcile-thunk (assoc (or graph graph-0)
                                                  ::middleware middleware
                                                  ::dirty-set dirty-set
                                                  ::triggers (TLongHashSet.)
                                                  ::up-to-date up-to-date)
                                           (when-let [root-id (::root graph)]
                                             (doto (TObjectLongHashMap.)
                                               (.put ::root root-id))) ;; flashbacks
                                           f ::root args-vector)
          _ (when-not first-run?
              (.remove up-to-date root-id))
          graph (-> graph
                    (assoc ::root root-id)
                    (cond-> (not first-run?)
                      (traverse-graph root-id dirty-set))
                    (dissoc ::triggers
                            ::dirty-set
                            ::up-to-date))
          value (do
                  (.set >-graph-< graph)
                  15
                  (deref-or-value (.-value ^Calc (get-in graph [::values root-id]))))]
      (.set >-graph-< old-graph)
      [graph value])))

(defn with-thunks-forbidden [f & args]
  (if *assert?*
    (let [g (.get >-graph-<)
          frame (current-frame)]
      (try
        (.set >-graph-< nil)
        (.set >-frame-< nil)
        (apply f args)
        (finally
          (.set >-frame-< frame)
          (.set >-graph-< g))))
    (apply f args)))

