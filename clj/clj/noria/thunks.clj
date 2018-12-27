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
  (:require [clojure.pprint :as pprint]))

(defprotocol ThunkDef
  (destroy! [this state])
  (compute [this state arg]) ;; => [state' value]
  (up-to-date? [this state old-arg new-arg])
  (changed? [this old-value new-value]))

(defn applyv [f ^clojure.lang.IPersistentVector av]
  (case (.length av)
    0 (f)
    1 (f (.nth av 0))
    2 (f (.nth av 0) (.nth av 1))
    3 (f (.nth av 0) (.nth av 1) (.nth av 2))
    4 (f (.nth av 0) (.nth av 1) (.nth av 2) (.nth av 3))
    5 (f (.nth av 0) (.nth av 1) (.nth av 2) (.nth av 3) (.nth av 4))
    6 (f (.nth av 0) (.nth av 1) (.nth av 2) (.nth av 3) (.nth av 4) (.nth av 5))
    (apply f av)))

(extend-protocol ThunkDef
  clojure.lang.AFn
  (up-to-date? [this state old-arg new-arg] (= old-arg new-arg))
  (compute [this state args]
    (if (vector? args)
      [state (applyv this args)]
      [state (apply this args)]))
  (changed? [this old-value new-value] (not= old-value new-value))
  (destroy! [this state]))


(def ^{:tag 'ThreadLocal} >-ctx-< (ThreadLocal.))
(defn current-ctx ^noria.NoriaRT$Context [] (.get >-ctx-<))
(deftype Thunk [^long id]
  clojure.lang.IDeref
  (deref [this]
    (assert (some? (current-ctx)) {:error "Thunk deref outside of computation"
                                   :thunk-id id})
    (let [[state value] (noria.NoriaRT/read (current-ctx) id)]
      (if (instance? Thunk value)
        (deref value)
        value)))
  java.lang.Object
  (toString [this]
    (str "[Thunk#" id "]"))
  (equals [this other]
    (and (instance? Thunk other) (= (.-id ^Thunk other) id))))

(defmethod print-method Thunk [o, ^java.io.Writer w]
  (.write w ^String (str o)))

(defmethod pprint/simple-dispatch Thunk [s]
  (pr s))

(defn thunk* [key thunk-def arg]
  (Thunk. (noria.NoriaRT/reconcile (current-ctx) thunk-def key arg)))

(defn deref-or-value [thunk-or-value]
  (if (instance? Thunk thunk-or-value)
    (deref thunk-or-value)
    thunk-or-value))

(defn evaluate [graph f args-vector & {:keys [dirty-set middleware assert?]
                                       :or {dirty-set #{}
                                            assert? false
                                            middleware identity}}]
  (let [middleware-impl (reify java.util.function.Function
                          (apply [this thing]
                            (let [thing (middleware thing)]
                              (reify noria.NoriaRT$Reconciler
                                (reconcile [this ctx [state value] arg]
                                  (let [old-ctx (.get >-ctx-<)]
                                    (.set >-ctx-< ctx)
                                    (let [[state' value'] (compute thing
                                                                  (if (nil? state)
                                                                    {:noria/id (.-id (.-frame ^noria.NoriaRT$Context ctx))}
                                                                    state) arg)]
                                      (.set >-ctx-< old-ctx)
                                      (noria.NoriaRT$Propagation.
                                        [(assoc state'
                                                :noria/arg arg
                                                :noria/value value')
                                         value']
                                        (boolean (changed? thing value value'))))))
                                (needsReconcile [this [state value] arg]
                                  (not (boolean (up-to-date? thing state (:noria/arg state) arg))))
                                (destroy [this [state value]]
                                  (destroy! thing state))))))
        old-ctx (.get >-ctx-<)
        f-impl (with-meta (reify ThunkDef
                            (changed? [this old new]
                              (changed? f old new))
                            (destroy! [this state]
                              (destroy! f state))
                            (compute [this state arg]
                                     (let [[state value] (compute f state arg)]
                                       [state (deref-or-value value)]))
                            (up-to-date? [this state old new]
                              (up-to-date? f state old new)))
                 (meta f))
        ^noria.NoriaRT$Result result (cond
                                 (nil? f) (noria.NoriaRT/destroyGraph graph, middleware-impl)
                                 (some? graph) (noria.NoriaRT/revaluate ^noria.NoriaRT$DAG graph
                                                                        ^java.util.Set dirty-set
                                                                        ^java.util.function.Function middleware-impl)
                                 :else (noria.NoriaRT/evaluate ^Object f-impl
                                                               ^Object args-vector
                                                               ^java.util.function.Function middleware-impl))]
    (.set >-ctx-< old-ctx)
    (when (some? result)
      (let [[state value] (.-rootState result)]
        [(.-graph result) value]))))


