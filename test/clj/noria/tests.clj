(ns noria.tests
  (:require [noria :refer :all]
            [clojure.test :refer :all]))

(defattr :dom/children {:noria/data-type :nodes-seq})
(defattr :dom/child {:noria/data-type :node})

(defn check-updates-match-args
  "Sequence of [args-vector expected-updates]"
  [root-function args-to-updates]
  (reduce (fn [graph [keyseq expected-updates]]
            (let [[graph' updates] (evaluate graph root-function keyseq)]
              (is (= updates expected-updates))
              graph'))
          nil
          args-to-updates))

(defn sequence [& keys]
  (-< node :div
      {:dom/children (into []
                           (map (fn [key]
                                  (-< :noria/key key
                                      node :label {:dom/text key})))
                           keys)}))


(defn split [c1 c2]
  (let [res (-< node :split
                {:dom/children [c1 c2]})]
    (-< node :listener {:dom/children [res]})
    res))

(defn sidebar-comp [shown?]
  (let [content (-< node :content {})]
    (-< node :root
        {:flex     "auto"
         :dom/children [(-< node :sidebar-buttons {})
                        (if shown?
                          (-< split (-< node :sidebar {}) content)
                          content)]})))


(deftest sidebar
  (check-updates-match-args
    sidebar-comp
    [[[true] [#:noria{:update-type :make-node, :type :content, :node 1, :constructor-parameters {}}
              #:noria{:update-type :make-node, :type :sidebar-buttons, :node 2, :constructor-parameters {}}
              #:noria{:update-type :make-node, :type :sidebar, :node 3, :constructor-parameters {}}
              #:noria{:update-type :make-node, :type :split, :node 4, :constructor-parameters {}}
              #:noria{:update-type :add, :node 4, :attr :dom/children, :value 3, :index 0}
              #:noria{:update-type :add, :node 4, :attr :dom/children, :value 1, :index 1}
              #:noria{:update-type :make-node, :type :listener, :node 5, :constructor-parameters {}}
              #:noria{:update-type :add, :node 5, :attr :dom/children, :value 4, :index 0}
              #:noria{:update-type :make-node, :type :root, :node 6, :constructor-parameters {}}
              #:noria{:update-type :set-attr, :value "auto", :node 6, :attr :flex}
              #:noria{:update-type :add, :node 6, :attr :dom/children, :value 2, :index 0}
              #:noria{:update-type :add, :node 6, :attr :dom/children, :value 4, :index 1}]]
     [[false] [#:noria{:update-type :remove, :attr :dom/children, :node 6, :value 4}
               #:noria{:update-type :add, :attr :dom/children, :node 6, :value 1, :index 1}
               #:noria{:update-type :destroy, :node 3}
               #:noria{:update-type :destroy, :node 4}
               #:noria{:update-type :destroy, :node 5}]]
     ]))

(deftest reconcile-seq
  (check-updates-match-args
   sequence
   [[["hey" "hoy"]
             [#:noria{:update-type :make-node, :node 1, :type :label, :constructor-parameters {}}
              #:noria{:update-type :set-attr, :attr :dom/text, :node 1, :value "hey"}
              #:noria{:update-type :make-node, :node 2, :type :label, :constructor-parameters {}}
              #:noria{:update-type :set-attr, :attr :dom/text, :node 2, :value "hoy"}
              #:noria{:update-type :make-node, :node 3, :type :div, :constructor-parameters {}}
              #:noria{:update-type :add, :attr :dom/children, :node 3, :value 1, :index 0}
              #:noria{:update-type :add, :attr :dom/children, :node 3, :value 2, :index 1}]]

           [["hey" "hoy"]
            []]

           [["first" "second" "third"]
            [#:noria{:update-type :make-node, :type :label, :node 4, :constructor-parameters {}}
             #:noria{:update-type :set-attr, :value "first", :node 4, :attr :dom/text}
             #:noria{:update-type :make-node, :type :label, :node 5, :constructor-parameters {}}
             #:noria{:update-type :set-attr, :value "second", :node 5, :attr :dom/text}
             #:noria{:update-type :make-node, :type :label, :node 6, :constructor-parameters {}}
             #:noria{:update-type :set-attr, :value "third", :node 6, :attr :dom/text}
             #:noria{:update-type :remove, :attr :dom/children, :node 3, :value 1}
             #:noria{:update-type :remove, :attr :dom/children, :node 3, :value 2}
             #:noria{:update-type :add, :attr :dom/children, :node 3, :value 4, :index 0}
             #:noria{:update-type :add, :attr :dom/children, :node 3, :value 5, :index 1}
             #:noria{:update-type :add, :attr :dom/children, :node 3, :value 6, :index 2}
             #:noria{:update-type :destroy, :node 1}
             #:noria{:update-type :destroy, :node 2}]]

           [["second" "first" "third"]
            [#:noria{:update-type :remove, :attr :dom/children, :node 3, :value 5}
             #:noria{:update-type :add, :attr :dom/children, :node 3, :value 5, :index 0}]]]))

(defn unordered-sequence [& keys]
  (-< node :div
      {:dom/children (with-meta
                       (into []
                             (map (fn [key]
                                    (-< :noria/key key
                                        node :label {:dom/text key})))
                             keys)
                       {:noria/unordered? true})}))

(deftest reconcile-unordered
  (check-updates-match-args
   unordered-sequence
   [[["hey" "hoy"]
     [#:noria{:update-type :make-node, :type :label, :node 1, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :value "hey", :node 1, :attr :dom/text}
      #:noria{:update-type :make-node, :type :label, :node 2, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :value "hoy", :node 2, :attr :dom/text}
      #:noria{:update-type :make-node, :type :div, :node 3, :constructor-parameters {}}
      #:noria{:update-type :add, :attr :dom/children, :node 3, :value 1, :index 0}
      #:noria{:update-type :add, :attr :dom/children, :node 3, :value 2, :index 1}]]

    [["hey" "hoy"]
     []]

    [["hoy" "hey"]
     []]

    [["oh" "hey"]
     [#:noria{:update-type :make-node, :type :label, :node 4, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :value "oh", :node 4, :attr :dom/text}
      #:noria{:update-type :remove, :attr :dom/children, :node 3, :value 2}
      #:noria{:update-type :add, :attr :dom/children, :node 3, :value 4, :index 1}
      #:noria{:update-type :destroy, :node 2}]]]))

(run-tests)

