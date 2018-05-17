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
  (:import [gnu.trove TLongHashSet]))

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

(defrecord Calc [value
                 state
                 deps
                 thunk-def
                 args
                 children-by-keys
                 children])

(def ^:dynamic *dependencies* nil)
(def ^:dynamic *flashbacks* nil)
(def ^:dynamic *children* nil)
(def ^:dynamic *graph* nil)

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

(deftype Thunk [^long id]
  clojure.lang.IDeref
  (deref [this]
    (when *dependencies*
      (t-conj! *dependencies* id))
    (assert (some? *graph*) {:error "Thunk deref outside of computation"
                             :thunk-id id})
    (let [v (.-value ^Calc (get (::values @*graph*) id))]
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

(defn reduce-int-array [f init ^longs array]
  (let [l (alength array)]
    (loop [i 0
           acc init]
      (if (< i l)
        (let [acc' (f acc (long (aget array i)))]
          (if (reduced? acc')
            @acc'
            (recur (inc i) acc')))
        acc))))

(defn gc [graph ids]
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
           iterator (.iterator ^TLongHashSet ids)]              
       (loop [s (transient values)]
         (if (.hasNext iterator)                                 
           (recur (destroy-rec s (.next iterator)))
           (persistent! s)))))))

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
        (update graph ::up-to-date t-conj! id)
        (let [[graph' state' value' deps' children']
              (binding [*graph* (atom graph)
                        *flashbacks* (when calc
                                       (.-children-by-keys calc))
                        *dependencies* (TLongHashSet.)
                        *children* (atom (transient []))]
                (let [[state value] (compute thunk-def-wrapped
                                             (if calc
                                               (.-state calc)
                                               {:noria/id id})
                                             args)
                      graph' @*graph*]
                  (reset! *graph* nil)
                  [graph' state value *dependencies* (persistent! @*children*)]))
              ^longs children-array (let [c-c (count children')
                                          a (long-array c-c)]
                                      (loop [i 0]
                                        (if (< i c-c)
                                          (do
                                            (aset a i (long (nth (nth children' i) 1)))
                                            (recur (inc i)))
                                          a)))]
          (-> graph'
              (cond-> calc
                (gc (doto (TLongHashSet. ^longs (.-children calc))
                      (.removeAll children-array))))
              (update ::values assoc id
                        (Calc. value' state' deps' thunk-def args
                               (into {} children')
                               children-array))
              (update ::up-to-date t-conj! id)
              (cond-> (and (some? calc)
                           (changed? thunk-def-wrapped (.-value calc) value'))
                (update ::triggers t-conj! id))))))))

(defn reconcile-thunk [graph flashbacks thunk-def key args]
  (if-let [id (when flashbacks
                (get flashbacks key))]
    [id (reconcile-by-id graph id thunk-def args)]
    (let [graph' (update graph ::max-thunk-id inc)
          id (::max-thunk-id graph')]
      [id (reconcile-by-id graph' id thunk-def args)])))

(defn thunk* [key thunk-def args-vector]
  (let [[id graph'] (reconcile-thunk @*graph* *flashbacks* thunk-def key args-vector)]
    (reset! *graph* graph')
    (swap! *children* conj! [key id])
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

(defn evaluate [graph f args-vector & {:keys [dirty-set middleware]
                                       :or {dirty-set (i/int-set)
                                            middleware identity}}]
  (let [first-run? (nil? (::root graph))
        [root-id graph] (reconcile-thunk (assoc (or graph graph-0)
                                                ::middleware middleware
                                                ::dirty-set dirty-set
                                                ::triggers (TLongHashSet.)
                                                ::up-to-date (TLongHashSet.))
                                          (when-let [root-id (::root graph)]
                                            {::root root-id}) ;; flashbacks
                                          f ::root args-vector)
        graph (-> graph
                  (assoc ::root root-id)
                  (cond-> (not first-run?)
                    (traverse-graph root-id dirty-set))
                  (dissoc ::triggers
                          ::dirty-set
                          ::up-to-date))]
    [graph (binding [*graph* (atom graph)]
             (deref-or-value (.-value ^Calc (get-in graph [::values root-id]))))]))

(defn with-thunks-forbidden [f & args]
  (binding [*graph* nil
            *dependencies* nil
            *children* nil]
    (apply f args)))


