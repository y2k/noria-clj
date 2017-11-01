(ns noria.tests
  (:require [noria :refer :all]
            [clojure.test :refer :all]
            [noria.components :refer :all]))

(defattr :dom/children :nodes-seq)
(defattr :dom/child :node)

(defn check-updates [elements]
  (reduce (fn [[c-id ctx ups] [el updates]]
            (let [[c-id' ctx'] (reconcile c-id el conj ctx)]
              (is (= updates (:updates ctx')) "wrong updates")
              [c-id' (assoc ctx' :updates []) (:updates ctx')]))
          [nil context-0 []] elements))

(t/deftest reconcile-seq
  (check-updates
   [[{:noria/type :div
      :dom/children
      [{:noria/type :div
        :noria/key :hey
        :dom/text "hey"}
       {:noria/type :div
        :noria/key :hoy
        :dom/text "hoy"}]}
     [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
      #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 1, :value "hey"}
      #:noria{:update-type :make-node, :node 2, :type :div, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 2, :value "hoy"}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 1}]]
    [{:noria/type :div
      :dom/children
      [{:noria/type :div
        :noria/key :hey
        :dom/text "hey"}
       {:noria/type :div
        :noria/key :hoy
        :dom/text "hoy"}]} []]
    [{:noria/type :div
      :dom/children [{:noria/type :div
                      :noria/key :hiy
                      :dom/text "hiy"}
                     {:noria/type :div
                      :noria/key :hoy
                      :dom/text "hoy!!"}
                     {:noria/type :div
                      :noria/key :fu
                      :dom/text "fu"}]}
     [#:noria{:update-type :make-node, :node 3, :type :div, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 3, :value "hiy"}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 2, :value "hoy!!"}
      #:noria{:update-type :make-node, :node 4, :type :div, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 4, :value "fu"}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 3, :index 0}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 4, :index 2}
      #:noria{:update-type :destroy, :node 1}]]]))

(def label
  (render
   (fn [x]
     {:noria/type :div
      :noria/text (str x)})))

(def lambda
  (render
   (fn [y]
     ['apply (fn [x]
               {:noria/type :div
                :dom/child x
                :dom/children [x]})
      [label y]])))

(deftest reconcile-lambda
  (check-updates [[[lambda "hello"]
                   [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 0, :value "hello"}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :dom/child, :node 1, :value 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 1, :value 0, :index 0}]]
                  [[lambda "hello"] []]
                  [[lambda "bye"]
                   [#:noria{:update-type :set-attr, :attr :noria/text, :node 0, :value "bye"}]]]) 
  )

(def simple-container
  (render
   (fn [i]
     {:noria/type :div
      :dom/children (map (fn [i]
                           ^{:noria/key i} [label i]) (range i))})))

(deftest reconcile-simple-list
  (check-updates [[[simple-container 2]
                   [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 1, :value "0"}
                    #:noria{:update-type :make-node, :node 2, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 2, :value "1"}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 1}]]
                  [[simple-container 2]
                   []]
                  [[simple-container 1]
                   [#:noria{:update-type :destroy, :node 2}]]
                  [[simple-container 3]
                   [#:noria{:update-type :make-node, :node 3, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 3, :value "1"}
                    #:noria{:update-type :make-node, :node 4, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 4, :value "2"}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 3, :index 1}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 4, :index 2}]]
                  [[simple-container 2]
                   [#:noria{:update-type :destroy, :node 4}]]])
  )

(def do-block
  (render
   (fn [i]
     (into ['do] (map (fn [i] [label i])) (range i)))))

(deftest reconcile-do-block
  (check-updates [[[do-block 3]
                   [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 0, :value "0"}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 1, :value "1"}
                    #:noria{:update-type :make-node, :node 2, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :noria/text, :node 2, :value "2"}]]
                  [[do-block 3] []]
                  [[do-block 1]
                   [#:noria{:update-type :destroy, :node 1}
                    #:noria{:update-type :destroy, :node 2}]]])
  )


(run-tests)


