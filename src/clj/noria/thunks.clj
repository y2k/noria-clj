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
  (:import [gnu.trove TLongHashSet TLongArrayList TObjectLongHashMap TLongLongHashMap]))

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

(deftype Calc [state
               args
               value
               thunk-def                 
               ^TLongHashSet deps                 
               ^TObjectLongHashMap children-by-keys
               ^TLongArrayList children
               ^long parent-id
               ^TLongLongHashMap children-index])

(deftype Frame [^TObjectLongHashMap flashbacks
                ^TLongHashSet deps                  
                ^TObjectLongHashMap children-by-keys
                ^TLongArrayList children
                ^long parent-id])

(defrecord Graph [middleware
                  dirty-set
                  triggers
                  up-to-date
                  values
                  max-thunk-id
                  root
                  dependants
                  new-triggers])

(defn update-values [^Graph g f]
  (Graph. (.-middleware g)
          (.-dirty-set g)
          (.-triggers g)
          (.-up-to-date g)
          (f (.-values g))
          (.-max-thunk-id g)
          (.-root g)
          (.-dependants g)
          (.-new-triggers g)))

(defn update-dependants [^Graph g f]
  (Graph. (.-middleware g)
          (.-dirty-set g)
          (.-triggers g)
          (.-up-to-date g)
          (.-values g)
          (.-max-thunk-id g)
          (.-root g)
          (f (.-dependants g))
          (.-new-triggers g)))

(def ^{:tag 'ThreadLocal} >-frame-< (ThreadLocal.))
(defn current-frame ^Frame [] (.get >-frame-<))

(def ^{:tag 'ThreadLocal} >-graph-< (ThreadLocal.))
(defn ^Graph current-graph [] (.get >-graph-<))

(def next-id (atom 0))

(defn empty-frame [parent-id flashbacks]
  (Frame. flashbacks
          (TLongHashSet.)
          (TObjectLongHashMap.)
          (TLongArrayList.)
          parent-id))

(defmacro new-frame [g parent-id flashbacks & body]
  `(let [old-frame# (.get >-frame-<)]
     (try
       (let [old-g# (.get >-graph-<)
             new-frame# (empty-frame ~parent-id ~flashbacks)]
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
  (.add ^TLongHashSet s e)
  s)

(defn i-get [^clojure.data.int_map.PersistentIntMap m ^long k]
  (.get ^clojure.data.int_map.INode (.-root m) k nil))

(defn i-contains? [^clojure.data.int_map.PersistentIntSet s ^long k]
  (.contains ^clojure.data.int_map.IntSet (.-int-set s) k))

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
    (let [v (.-value ^Calc (i-get (.-values (current-graph)) id))]
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
  (let [size (.size array)]
    (loop [i 0
           acc init]
      (if (< i size)
        (let [acc' (.invokePrim f acc (.get array i))]
          (if (reduced? acc')
            @acc'
            (recur (inc i) acc')))
        acc))))

(defn t-difference [^TLongHashSet s1 ^TLongHashSet s2]
  (let [it (.iterator s1)
        r (TLongHashSet.)]
    (loop []
      (if (.hasNext it)
        (let [x (.next it)]
          (when-not (.contains s2 x)
            (.add r x))
          (recur))
        r))))

(defn extinct! [mdlwr values dependants ^long id]
  (let [^Calc c (get @values id)
        ^TLongHashSet deps (.-deps c)
        ^TLongArrayList children (.-children c)
        l (.size children)]
    (vreset! dependants
             (let [it (.iterator deps)]
               (loop [d @dependants]
                 (if (.hasNext it)
                   (let [x (.next it)]
                     (recur (update d x disj id)))
                   d))))
    (vswap! values dissoc id)
    (destroy! (mdlwr (.-thunk-def c))
              (.-state c))
    (loop [i 0]
      (when (< i l)
        (extinct! mdlwr values dependants (.get children i))
        (recur (inc i))))))

(defn gc [^Graph graph old-children new-children]
  (let [^TLongHashSet ids (doto (TLongHashSet. (.toNativeArray ^TLongArrayList old-children))
                            (.removeAll (.toNativeArray ^TLongArrayList new-children)))
        iterator (.iterator ids)
        values (volatile! (.-values graph))
        dependants (volatile! (.-dependants graph))]
    (loop []
      (when (.hasNext iterator)
        (extinct! (.-middleware graph) values dependants (.next iterator))
        (recur)))
    (Graph. (.-middleware graph)
            (.-dirty-set graph)
            (.-triggers graph)
            (.-up-to-date graph)
            @values
            (.-max-thunk-id graph)
            (.-root graph)
            @dependants
            (.-new-triggers graph))))

(defn ^Graph reconcile-by-id [^Graph graph  id parent-id thunk-def args]
  (if (t-contains? (.-up-to-date graph) id)
    graph
    (let [^Calc calc (i-get (.-values graph) id)
          thunk-def-wrapped ((.-middleware graph) thunk-def)]
      (if (and (some? calc)
               (not (i-contains? (.-dirty-set graph) id))
               (not (t-intersects? (.-triggers graph) (.-deps calc)))
               (identical? thunk-def (.-thunk-def calc))
               (up-to-date? thunk-def-wrapped (.-state calc) (.-args calc) args))
        (do
          (t-conj! (.-up-to-date graph) id)
          graph)
        (let [[[state' value'] ^Frame frame ^Graph graph'] (new-frame
                                                            graph
                                                            id
                                                            (if calc (.-children-by-keys calc) (TObjectLongHashMap.))
                                                            (compute thunk-def-wrapped
                                                                     (if calc
                                                                       (.-state calc)
                                                                       {:noria/id id})
                                                                     args))
              deps' (.-deps frame)
              ^TLongArrayList children' (.-children frame)
              children-by-keys' (.-children-by-keys frame)
              children-changed? (or (nil? calc) (not= (.-children calc) children'))]
          (t-conj! (.-up-to-date graph') id)
          (when (and (some? calc)
                     (changed? thunk-def-wrapped (.-value calc) value'))
            (t-conj! (.-triggers graph') id)
            (t-conj! (.-new-triggers graph') id))
          (-> graph' 
              (cond-> (and calc children-changed?)
                (gc (.-children calc) children'))
              (update-values (fn [values]
                               (let [^TLongHashMap children-index (if children-changed?
                                                                    (let [ci (TLongLongHashMap.)
                                                                          l (.size  children')]
                                                                      (loop [i 0]
                                                                        (if (< i l)
                                                                          (do
                                                                            (.put ci (.get children' i) i)
                                                                            (recur (inc i)))
                                                                          ci)))
                                                                    (.-children-index calc))]
                                 (assoc values id
                                        (Calc. state' args value' thunk-def deps'
                                               children-by-keys'
                                               children'
                                               parent-id
                                               children-index)))))
              (update-dependants (fn [dependants]
                                   (let [deps (when calc (.-deps calc))
                                         ^TLongHashSet new (if deps (t-difference deps' deps) deps')
                                         ^TLongHashSet old (when deps (t-difference deps deps'))
                                         d' (let [it (.iterator new)]
                                              (loop [d dependants]
                                                (if (.hasNext it)
                                                  (let [x (.next it)]
                                                    (recur (assoc d x (conj (get d x (i/int-set)) id))))
                                                  d)))]
                                     (if old
                                       (let [it (.iterator old)]
                                         (loop [d d']
                                           (if (.hasNext it)
                                             (let [x (.next it)]
                                               (recur (update d x disj id)))
                                             d)))
                                       d'))))))))))

(defn reconcile-thunk [graph ^TObjectLongHashMap flashbacks parent-id thunk-def key args]
  (if-let [id (when flashbacks
                (when (.containsKey flashbacks key)
                  (.get flashbacks key)))]
    [id (reconcile-by-id graph id parent-id thunk-def args)]
    (let [graph' (update graph :max-thunk-id inc)
          id (swap! next-id inc)]
      [id (reconcile-by-id graph' id parent-id thunk-def args)])))

(defn thunk* [key thunk-def args-vector]
  (let [frame (current-frame)
        [id graph'] (reconcile-thunk (current-graph)
                                     (.-flashbacks frame)
                                     (.-parent-id frame)
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

(defn compare-longs ^long [^long l1 ^long l2]
  (if (< l1 l2)
    -1
    (if (< l2 l1)
      1
      0)))

(defn resolve-path ^TLongArrayList [^Graph graph id]
  (let [^Calc c (get (.-values graph) id)
        pid (.-parent-id c)]
      (if (= pid -1)
        (TLongArrayList.)
        (let [^Calc pc (get (.-values graph) pid)
              id-idx (.get ^TLongLongHashMap (.-children-index pc) id)]
          (doto (resolve-path graph pid)
                (.add id-idx))))))

(defn hueverse-graph [^Graph graph ^long id ^long parent-id dirty-set]
  (let [heap (java.util.PriorityQueue. (fn [[_ ^longs p1] [_ ^longs p2]]
                                         (let [l1 (alength p1)
                                               l2 (alength p2)]
                                           (loop [i 0]
                                             (if (and (< i l1) (< i l2))
                                               (if (= (aget p1 i) (aget p2 i))
                                                 (recur (inc i))
                                                 (compare-longs (aget p1 i) (aget p2 i)))
                                               (compare-longs l1 l2))))))
        values (.-values graph)]
    (reduce (fn [_ id]
              (when (get values id)
                (let [path (resolve-path graph id)]
                  (.add heap [id (.toNativeArray path)])))) nil dirty-set)
    (loop [^Graph g graph]
      (if-let [[id path] (.poll heap)]
        (if-let [^Calc c (get (.-values g) id)]
          (let [^Graph g' (reconcile-by-id g id (.-parent-id c) (.-thunk-def c) (.-args c))
                ^TLongHashSet new-triggers (.-new-triggers g')]
            (when new-triggers
              (let [dependants (.-dependants graph)
                    it (.iterator new-triggers)]
                (loop []
                  (when (.hasNext it)
                    (let [trigger (.next it)]
                      (reduce (fn [_ dependant]
                                (.add heap [dependant (.toNativeArray (resolve-path graph dependant))]))
                              nil (get dependants trigger))
                      (recur))))))
            (recur (Graph. (.-middleware g')
                           (.-dirty-set g')
                           (.-triggers g')
                           (.-up-to-date g')
                           (.-values g')
                           (.-max-thunk-id g')
                           (.-root g')
                           (.-dependants g')
                           (TLongHashSet.))))
          (recur g))
        g))))

(def graph-0 (Graph. nil nil nil nil (i/int-map) 0 nil (i/int-map) (TLongHashSet.)))

(defn evaluate [^Graph graph f args-vector & {:keys [dirty-set middleware assert?]
                                       :or {dirty-set (i/int-set)
                                            assert? false
                                            middleware identity}}]
  (def graph graph)
  (binding [*assert?* assert?]
    (let [old-graph (.get >-graph-<)
          ^Graph graph (or graph graph-0)
          first-run? (nil? (.-root graph))
          up-to-date (TLongHashSet.)
          [root-id graph] (reconcile-thunk (assoc graph
                                                  :middleware middleware
                                                  :dirty-set dirty-set
                                                  :triggers (TLongHashSet.)
                                                  :up-to-date up-to-date)
                                           (when-let [root-id (.-root graph)]
                                             (doto (TObjectLongHashMap.)
                                               (.put ::root root-id))) ;; flashbacks
                                           -1
                                           f ::root args-vector)
          _ (when-not first-run?
              (.remove up-to-date root-id))
          graph (-> graph
                    (assoc :root root-id)
                    (cond-> (not first-run?)
                      (hueverse-graph root-id -1 dirty-set))
                    (assoc
                     :triggers nil
                     :dirty-set nil
                     :up-to-date nil))
          value (do
                  (.set >-graph-< graph)
                  15
                  (deref-or-value (.-value ^Calc (get-in graph [:values root-id]))))]
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

(comment

  (let [[container-id calc] (first (filter (fn [[id calc]] (= 345 (:noria/node (.-state calc)))) (.-values graph)))]
      (def container-calc calc)
      (def container-id container-id))

  (.-deps container-calc)

  (def editor-component-id (.-parent-id container-calc))

  (def double-spock-id 164815)
  (.-children (get (.-values graph) 164814))

  (.-children (get (.-values graph) 165254))

  (.- (get (.-values graph) ))


  (get (.-dependants graph) double-spock-id)

  )
