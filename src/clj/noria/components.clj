(ns noria.components)

(defn subtree-caching [pred]
  (fn [r-f]
    (fn
      ([] (assoc (r-f) ::args ::nil))
      ([state & args]
       (assoc (if (or (= (::args state) ::nil)
                      (not (pred args (::args state))))
                (apply r-f state args)
                state)
              ::args args))
      ([state] (r-f state)))))

(defn should-subtree-update [pred]
  (subtree-caching (complement pred)))

(defn render [render-fn]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state & args]
       (r-f state (apply render-fn args)))
      ([state] state))))

(defn pure [] (subtree-caching not=))

(defn component-caching [pred render-fn]
  (fn [r-f]
    (fn
      ([] (assoc (r-f)
                 ::args ::nil
                 ::cache nil))
      ([state & args]
       (assoc (if (or (= (::args state) ::nil)
                      (not (pred args (:args state))))
                (let [render (apply render-fn args)]
                  (assoc (r-f state render)
                         ::cache render))
                state)
              ::args args))
      ([state] (r-f state)))))

(defn should-component-update [pred render]
  (component-caching (complement pred) render))



(defn did-mount [f]
  (fn [r-f]
    (fn
      ([] (f (r-f)))
      ([state & inputs] (apply r-f state inputs))
      ([state] (r-f state)))))

(defn did-unmount [f]
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state & inputs] (apply r-f state inputs))
      ([state] (f state)))))

(comment
  (def my-component 
    (comp
     (init
      (fn [s]
        (prn "hey!")
        s))
     (subtree-caching
      (fn [[old-a old-b] [new-a new-b]]
        (= (+ old-a old-b) (+ new-a new-b))))
     (render
      (fn [a b]
        [:text {:text (str (+ a b))}]))
     (deinit
      (fn [s]
        (prn "bye!")
        s)))))
