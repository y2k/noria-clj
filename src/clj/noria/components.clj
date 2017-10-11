(ns noria.components)

(defn render [render-fn]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state args]
       (r-f state (apply render-fn args)))
      ([state]
       (r-f state)))))

(defn skip-subtree [pred]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state args]
       (r-f (assoc state
                   :noria/skip-subtree? (pred state args)) args))
      ([state] (r-f state)))))

(defn update-args []
  (fn [r-f]
    (fn
      ([] (assoc (r-f) ::args ::nil))
      ([state args]
       (-> (r-f state args)
           (assoc ::args args)))
      ([state] (r-f state)))))

(defn cache [pred]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state args]
       (if (pred state args)
         state
         (r-f state args)))
      ([state] (r-f state)))))

(defn pure-if [pred]
  (comp
   (update-args)
   (cache (fn [state args] (and (not= ::nil (::args state))
                               (pred (::args state) args))))))

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
              state*' (r-f state* args)]
          (-> state
              (merge state*')
              (cond->
                  (not save?) (assoc ::special-states {}))
              (assoc-in [::special-states key] state*'))))
       ([state] (r-f state))))))
