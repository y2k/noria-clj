(ns noria.components)

(defn render [render-fn]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state args]
       (r-f state (apply render-fn args)))
      ([state]
       (r-f state)))))

(defn stateful [render-fn]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state args] (r-f state (apply render-fn state args)))
      ([state] (r-f state)))))

(defn conjunction [preds]
  (reduce (fn [s p] (fn [a b] (and (p a b) (s a b)))) (constantly true) preds))

(defn update-args []
  (fn [r-f]
    (fn
      ([] (assoc (r-f) ::args ::nil))
      ([state args]
       (-> (r-f state args)
           (assoc ::args args)))
      ([state] (r-f state)))))

(defn compare-args
  "Given predicate of new-args and old-args returns predicate of state and new args"
  [pred]
  (fn [{args :noria.components/args} args']
    (and (not= args :noria.components/nil)
         (pred args args'))))

(defn cache [& preds]
  (let [pred (conjunction preds)]
    (fn [r-f]
      (fn
        ([] (r-f))
        ([state args]
         (dissoc (if (and (not (::suppress-cache? state)) (pred state args))
                   state
                   (r-f state args))
                 ::suppress-cache?))
        ([state] (r-f state))))))

(defn skip-subtree
  "Preds will be called with current state and vector of new args.
  If all of them are true, render will not be called for this component and all of his subs will not be reconciled."
  [& preds]
  (let [pred (conjunction preds)]
    (comp
     (update-args)
     (fn [r-f]
       (fn
         ([] (r-f))
         ([state args]
          (r-f (assoc state
                      :noria/skip-subtree? (pred state args)) args))
         ([state] (r-f state))))
     (cache pred))))

(defn pure-if
  "Predicate will be called with two arguments: vector of old args and vector of new args. 
  If it returns true, previous state will be reused for this element (including render result).
  Then reconciliation process continues as usual"
  [pred]
  (comp
   (update-args)
   (cache (compare-args pred))))

(defn pure []
  (pure-if =))

(defn component [& pipeline]
  (comp (update-args)
        (apply comp pipeline)))

(defn mount [f]
  (fn [r-f]
    (fn
      ([] (f (r-f)))
      ([state inputs] (r-f state inputs))
      ([state] (r-f state)))))

(defn unmount [f]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state inputs] (r-f state inputs))
      ([state] (f (r-f state))))))

(defn specialize
  ([key-fn] (specialize key-fn false))
  ([key-fn save?]
   (fn [r-f]
     (fn
       ([] (r-f))
       ([state args]
        (let [key (apply key-fn args)
              state* (get-in state [::special-states key])
              state*' (r-f (assoc state* :noria/id-path (:noria/id-path state)) args)]
          (-> state
              (merge state*')
              (cond->
                  (not save?) (assoc ::special-states {}))
              (assoc-in [::special-states key] state*'))))
       ([state] (r-f state))))))
