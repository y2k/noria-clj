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
            [clojure.data.int-map :as i]
            [clojure.spec.alpha :as s]))

(defprotocol ThunkDef
  (destroy! [this state destroy-children!])
  (compute [this state arg])
  (up-to-date? [this old-arg new-arg])
  (changed? [this old-value new-value]))

(extend-protocol ThunkDef
  clojure.lang.AFn
  (up-to-date? [this old-arg new-arg] (= old-arg new-arg))
  (compute [this state args]
    [state (apply this args)])
  (changed? [this old-value new-value] (not= old-value new-value))
  (destroy! [this state destroy-children!] (destroy-children!)))

(defn thunk-def [params]
  (let [my-up-to-date? (:up-to-date? params =)
        my-compute (:compute params)
        my-changed? (:changed? params not=)
        my-destroy! (:destroy! params (fn [state destroy-children!] (destroy-children!)))]
    (assert (some? my-compute))
    (reify ThunkDef
      (up-to-date? [this old-arg new-arg]
        (my-up-to-date? old-arg new-arg))
      (compute [this state args]
        (my-compute state args))
      (changed? [this old-value new-value]
        (my-changed? old-value new-value))
      (destroy! [this state destroy-children!]
        (my-destroy! state destroy-children!)))))

(defrecord Calc [value
                 state
                 deps
                 thunk-def
                 args
                 key
                 children-by-keys
                 children])

(def graph-0 {::values (i/int-map)
              ::triggers (i/int-set)
              ::up-to-date (i/int-set)
              ::max-thunk-id 0})

(def ^:dynamic *dependencies* nil)
(def ^:dynamic *parent-id* nil)
(def ^:dynamic *children* nil)
(def ^:dynamic *graph* nil)

(deftype Thunk [^long id]
  clojure.lang.IDeref
  (deref [this]
    (when *dependencies*
      (swap! *dependencies* conj! id))
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

(defn recall [graph parent-id key]
  (when-let [^Calc parent (get (::values graph) parent-id)]
    (get (.-children-by-keys parent) key)))

(defn gc [graph ids]
  (let [destroy (fn destroy [id]
                  (let [^Calc c (get (::values graph) id)
                        thunk-def (.-thunk-def c)
                        children (.-children c)
                        state (.-state c)]
                    (destroy! thunk-def state #(doseq [c children]
                                                 (destroy c)))))]
    (doseq [id ids] (destroy id)))

  (update graph ::values (fn [v]
                           (transduce
                            (mapcat (fn [id]
                                      (tree-seq (constantly true) #(.-children ^Calc (get (::values graph) %)) id)))
                            
                            (completing (fn [v id]
                                          (dissoc! v id))
                                        persistent!)
                            (transient v)
                            ids))))

(defn with-thunks-forbidden [f & args]
  (binding [*graph* nil
            *dependencies* nil
            *children* nil]
    (apply f args)))

(defn reconcile-by-id [graph id thunk-def key args]
  (let [^Calc calc (get (::values graph) id)]
    (if (contains? (::up-to-date graph) id)      
      graph
      (if (and (some? calc)
               (not (some (::triggers graph) (.-deps calc)))
               (identical? thunk-def (.-thunk-def calc))
               (with-thunks-forbidden up-to-date? thunk-def (.-args calc) args))
        (update graph ::up-to-date conj id)        
        (let [[graph' state' value' deps' children']
              (binding [*graph* (atom graph)
                        *parent-id* id
                        *dependencies* (atom (transient (i/int-set)))
                        *children* (atom (transient []))]
                (let [[state value] (compute thunk-def (if calc (.-state calc) {:noria/id id}) args)]
                  [@*graph* state value (persistent! @*dependencies*) (persistent! @*children*)]))]
          
          (-> graph'
              (cond-> calc
                (gc (i/difference (i/int-set (.-children calc))
                                  (into (i/int-set)
                                        (map second)
                                        children'))))
              (assoc-in [::values id]
                        (Calc. value' state' deps' thunk-def args key
                               (into {} children')
                               (into (vector-of :long) (map second) children')))
              (update ::up-to-date conj id)
              (cond-> (and (some? calc)
                           (with-thunks-forbidden changed? thunk-def (.-value calc) value'))
                (update ::triggers conj id))))))))

(defn reconcile-thunk [graph parent-id thunk-def key args]
  (if-let [id (recall graph parent-id key)]
    [id (reconcile-by-id graph id thunk-def key args)]
    (let [graph' (update graph ::max-thunk-id inc)
          id (::max-thunk-id graph')]
      [id (reconcile-by-id graph' id thunk-def key args)])))

(defn reconcile-self [graph id ^Calc c]
  (reconcile-by-id graph id (.-thunk-def c) (.-key c) (.-args c)))

(defn run [graph id dirty-set]
  (let [^Calc c (get (::values graph) id)]
    (if (or (contains? dirty-set id)
            (some #(contains? (::triggers graph) %) (.-deps c)))
      (reduce (fn [g id]
                (run g id dirty-set))
              (reconcile-self graph id c)
              (.-children c))
      (reduce (fn [g c-id]
                (let [g' (run g c-id dirty-set)]
                  (if (some #(contains? (::triggers g') %) (.-deps c))
                    (reduced (run g' id dirty-set))
                    g')))
              graph
              (.-children c)))))

(defn thunk* [key thunk-def args]
  (let [[id graph'] (reconcile-thunk @*graph* *parent-id* thunk-def key args)]
    (reset! *graph* graph')
    (swap! *children* conj! [key id])
    (Thunk. id)))

(s/def ::-< (s/cat :key-spec (s/? (s/cat :kw #{:noria/key}
                                         :key any?))
                   :thunk-def any?
                   :args (s/* any?)))

(defmacro -< [& stuff]
  (when-not (s/valid? ::-< stuff)
    (throw (ex-info (s/explain-str ::-< stuff)
                    (s/explain-data ::-< stuff))))
  (let [{{key :key} :key-spec
         :keys [thunk-def args]} (s/conform ::-< stuff) 
        key (or key `(quote ~(gensym)))]
    `(thunk* ~key ~thunk-def [~@args])))

(defn deref-or-value [thunk-or-value]
  (if (instance? Thunk thunk-or-value)
    (deref thunk-or-value)
    thunk-or-value))

(defn init-computation [f args]
  (binding [*graph* (atom graph-0)
            *parent-id* 0
            *dependencies* (atom (transient (i/int-set)))
            *children* (atom (transient []))]
    (let [^Thunk t (thunk* 0 f args)
          graph' (assoc @*graph* ::root (.-id t))]
      [graph'  (deref-or-value t)])))

(defn update-computation [graph dirty-set args]
  (binding [*graph* (atom (assoc graph
                                 ::triggers (i/int-set)
                                 ::up-to-date (i/int-set)))
            *parent-id* 0
            *dependencies* (atom (transient (i/int-set)))
            *children* (atom (transient []))]
    (let [g' (run graph (::root graph) dirty-set)]
      [g' @(Thunk. (::root graph))])))
