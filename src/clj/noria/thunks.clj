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

(extend-protocol ThunkDef
  clojure.lang.AFn
  (up-to-date? [this state old-arg new-arg] (= old-arg new-arg))
  (compute [this state args]
    [state (apply this args)])
  (changed? [this old-value new-value] (not= old-value new-value))
  (destroy! [this state]))


(def ^{:tag 'ThreadLocal} >-ctx-< (ThreadLocal.))
(defn current-ctx ^noria.NoriaRT$Context [] (.get >-ctx-<))
(deftype Thunk [^long id]
  clojure.lang.IDeref
  (deref [this]
    (assert (some? (current-ctx)) {:error "Thunk deref outside of computation"
                                   :thunk-id id})
    (let [v (noria.NoriaRT/read (current-ctx) id)]
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
                              (reify noria.NoriaRT$ThunkDef
                                (compute [this ctx state arg]
                                  (let [old-ctx (.get >-ctx-<)]
                                    (.set >-ctx-< ctx)
                                    (let [[state value] (compute thing (if (nil? state)
                                                                         {:noria/id (.-id (.-frame ^noria.NoriaRT$Context ctx))}
                                                                         state) arg)
                                          res (noria.NoriaRT$ThunkDef$Result. state value)]
                                      (.set >-ctx-< old-ctx)
                                      res)))
                                (isUpToDate [this state old new]
                                  (boolean (up-to-date? thing state old new)))
                                (hasChanged [this old new]
                                  (boolean (changed? thing old new)))
                                (destroy [this state]
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
        result (cond
                 (nil? f)
                 (noria.NoriaRT/extinct (noria.NoriaRT$Context. ^noria.NoriaRT$DAG graph ^java.util.Set (gnu.trove.TLongHashSet.) ^java.util.function.Function middleware-impl)
                                        (.-root ^noria.NoriaRT$DAG graph))
                 (some? graph)
                 (noria.NoriaRT/evaluate ^noria.NoriaRT$DAG graph ^java.util.Set dirty-set ^java.util.function.Function middleware-impl)
                 :else
                 (noria.NoriaRT/evaluate ^Object f-impl ^Object args-vector ^java.util.function.Function middleware-impl))]
    (.set >-ctx-< old-ctx)
    (when (some? result)
      [(.-graph result) (.-value result)])))


