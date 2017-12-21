(ns noria.tests
  (:require [noria :refer :all]
            [clojure.test :refer :all]
            [noria.components :refer :all]))

(defattr :dom/children {:noria/data-type :nodes-seq})
(defattr :dom/child {:noria/data-type :node})

(defn check-updates [elements]
  (reduce (fn [[old-value ctx] [el updates]]
            (let [[new-value ctx'] (reconcile old-value el ctx)]
              (is (= updates (:updates ctx')) "wrong updates")
              [new-value (dissoc ctx' :updates)]))
          [nil context-0] elements))

(deftest reconcile-seq
  (check-updates
   [[{:noria/type :div
      :dom/children
      [{:noria/type :hey
        :noria/key :hey
        :dom/text "hey"}
       {:noria/type :hoy
        :noria/key :hoy
        :dom/text "hoy"}]}
     [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
      #:noria{:update-type :make-node, :node 1, :type :hey, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 1, :value "hey"}
      #:noria{:update-type :make-node, :node 2, :type :hoy, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 2, :value "hoy"}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 1}]]
    [{:noria/type :div
      :dom/children
      [{:noria/type :hey
        :noria/key :hey
        :dom/text "hey"}
       {:noria/type :hoy
        :noria/key :hoy
        :dom/text "hoy"}]} []]
    [{:noria/type :div
      :dom/children [{:noria/type :hiy
                      :noria/key :hiy
                      :dom/text "hiy"}
                     {:noria/type :hoy
                      :noria/key :hoy
                      :dom/text "hoy!!"}
                     {:noria/type :fu
                      :noria/key :fu
                      :dom/text "fu"}]}
     [#:noria{:update-type :make-node, :node 3, :type :hiy, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 3, :value "hiy"}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 2, :value "hoy!!"}
      #:noria{:update-type :make-node, :node 4, :type :fu, :constructor-parameters {}}
      #:noria{:update-type :set-attr, :attr :dom/text, :node 4, :value "fu"}
      #:noria{:update-type :remove :attr :dom/children :node 0 :value 1}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 3, :index 0}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 4, :index 2}
      #:noria{:update-type :destroy, :node 1}]]
    [{:noria/type :div
      :dom/children [{:noria/type :hoy
                      :noria/key :hoy
                      :dom/text "hoy!!"}
                     {:noria/type :hiy
                      :noria/key :hiy
                      :dom/text "hiy"}
                     {:noria/type :fu
                      :noria/key :fu
                      :dom/text "fu"}]}
     [#:noria{:update-type :remove, :attr :dom/children, :node 0, :value 2}
      #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 0}]]]))

(def label
  (render
   (fn [x]
     {:noria/type :div
      ::text (str x)})))

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
                    #:noria{:update-type :set-attr, :attr ::text, :node 0, :value "hello"}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :add, :attr :dom/children, :node 1, :value 0, :index 0}
                    #:noria{:update-type :set-attr, :attr :dom/child, :node 1, :value 0}]]
                  [[lambda "hello"] []]
                  [[lambda "bye"]
                   [#:noria{:update-type :set-attr, :attr ::text, :node 0, :value "bye"}]]]) 
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
                    #:noria{:update-type :set-attr, :attr ::text, :node 1, :value "0"}
                    #:noria{:update-type :make-node, :node 2, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr ::text, :node 2, :value "1"}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 1}]]
                  [[simple-container 2]
                   []]
                  [[simple-container 1]
                   [#:noria{:update-type :remove :attr :dom/children :node 0 :value 2}
                    #:noria{:update-type :destroy, :node 2}]]
                  [[simple-container 3]
                   [#:noria{:update-type :make-node, :node 3, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr ::text, :node 3, :value "1"}
                    #:noria{:update-type :make-node, :node 4, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr ::text, :node 4, :value "2"}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 3, :index 1}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 4, :index 2}]]
                  [[simple-container 2]
                   [#:noria{:update-type :remove :attr :dom/children :node 0 :value 4}
                    #:noria{:update-type :destroy, :node 4}]]]))

(def do-block
  (render
   (fn [i]
     (into ['do] (map (fn [i] [label i])) (range i)))))

(deftest reconcile-do-block
  (check-updates [[[do-block 3]
                   [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr ::text, :node 0, :value "0"}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr ::text, :node 1, :value "1"}
                    #:noria{:update-type :make-node, :node 2, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr ::text, :node 2, :value "2"}]]
                  [[do-block 3] []]
                  [[do-block 1]
                   [#:noria{:update-type :destroy, :node 1}
                    #:noria{:update-type :destroy, :node 2}]]]))

(deftest simple-do
  (check-updates [[['do
                    {:noria/type :a}
                    {:noria/type :b}]
                   [#:noria{:update-type :make-node, :node 0, :type :a, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 1, :type :b, :constructor-parameters {}}]]
                  [['do
                    {:noria/type :a}
                    {:noria/type :b}] []]]))

(noria/defattr :NSWindow/contentView {:noria/data-type :node})

(def text-field
  (render
   (fn []
     {:noria/type :NSTextField
      :NSTextField/stringValue "hello"})))

(def window
  (render
   (fn []
     {:noria/type :NSWindow
      :NSWindow/contentView [text-field]})))

(defconstructor :fake/constraint #{:constraint/view1
                                   :constraint/view2})
(defconstructor :fake/label #{:label/text})
(defconstructor :fake/label2 #{:label2/text})

(def constraint
  (render
   (fn [text1 text2]
     ['apply
      (fn [view1 view2]
        ['do
          {:noria/type :fake/constraint
           :constraint/view1 view1
           :constraint/view2 view2}
          {:noria/type :Container
           :dom/children [view1 view2]}])
      {:noria/type :fake/label
       :label/text text1}
      {:noria/type :fake/label2
       :label2/text text2}])))

(deftest constructor
  (check-updates [[[constraint "hey" "hoy"]
                   [#:noria{:update-type :make-node,
                            :node 0,
                            :type :fake/label,
                            :constructor-parameters {:label/text "hey"}}
                    #:noria{:update-type :make-node,
                            :node 1,
                            :type :fake/label2,
                            :constructor-parameters {:label2/text "hoy"}}
                    #:noria{:update-type :make-node,
                            :node 2,
                            :type :fake/constraint
                            :constructor-parameters #:constraint{:view1 0, :view2 1}}
                    #:noria{:update-type :make-node,
                            :node 3,
                            :type :Container,
                            :constructor-parameters {}}
                    #:noria{:update-type :add,
                            :attr :dom/children,
                            :node 3,
                            :value 0,
                            :index 0}
                    #:noria{:update-type :add,
                            :attr :dom/children,
                            :node 3,
                            :value 1,
                            :index 1}]]
                  [[constraint "he" "ho"]
                   [#:noria{:update-type :set-attr,
                            :attr :label/text,
                            :node 0,
                            :value "he"}
                    #:noria{:update-type :set-attr,
                            :attr :label2/text,
                            :node 1, :value "ho"}]]]))

(deftest reconcile-node-attr
  (check-updates [[[window]
                   [#:noria{:update-type :make-node,
                            :node 0,
                            :type :NSWindow,
                            :constructor-parameters {}}
                    #:noria{:update-type :make-node,
                            :node 1,
                            :type :NSTextField,
                            :constructor-parameters {}}
                    #:noria{:update-type :set-attr,
                            :attr :NSTextField/stringValue,
                            :node 1,
                            :value "hello"}
                    #:noria{:update-type :set-attr,
                            :attr :NSWindow/contentView,
                            :node 0,
                            :value 1}]]]))

(deftest force-update-test
  (let [will-update-path (atom nil)
        div (fn [r-f]
              (fn
                ([] (assoc (r-f) :counter 1))
                ([r [id & children]]
                 (when (= id "will-update")
                   (reset! will-update-path (:noria/id-path r)))
                 (r-f (update r :counter inc) {:noria/type :div
                                               :counter (:counter r)
                                               :test-id id
                                               :dom/children children}))
                ([r] (r-f r))))
        elt [div "container"
             [div "sibling1"]
             [div "will-update"
              [div "some-child"]]
             [div "sibling2"]]
        [value ctx] (reconcile nil elt context-0)
        [new-value ctx'] (noria/reconcile-in value @will-update-path #(assoc % :counter 42) ctx)]
    (is (= (:updates ctx')
           [#:noria{:update-type :set-attr, :attr :counter, :node 3, :value 2}
            #:noria{:update-type :set-attr, :attr :counter, :node 2, :value 42}]))))

(def counter
  (render
   (fn [a]
     (swap! a inc)
     {:noria/type :NSTextField
      :NSTextField/stringValue "hello"})))

(def component-with-caching
  (comp
   (skip-subtree (compare-args (constantly true)))
   (render (fn [a]
             [counter a]))))

(deftest skip-subtree-works
  (let [a (atom 0)]
    (check-updates [[[component-with-caching a]
                     [#:noria{:update-type
                              :make-node,
                              :node 0,
                              :type :NSTextField,
                              :constructor-parameters {}}
                      #:noria{:update-type :set-attr,
                              :attr :NSTextField/stringValue,
                              :node 0,
                              :value "hello"}]]
                    [[component-with-caching a] []]])
    (is (= 1 @a))))

(deftest call-destroy-on-stale-components
  (let [destroyed (atom false)
        tracks-destroy (comp
                        (render (fn [] {:noria/type :text
                                        :dom/text "hello"}))
                        (unmount (fn [s] (reset! destroyed true))))
        container (render (fn [& c] {:noria/type :div
                                     :dom/children c}))
        [c-id ctx] (reconcile nil [container
                                   [tracks-destroy]] context-0)
        [c-id ctx] (reconcile c-id [container] ctx)]
    (is (= (:updates ctx) [#:noria{:update-type :remove, :attr :dom/children :node 0 :value 1}
                           #:noria{:update-type :destroy, :node 1}]))
    (is (= @destroyed true))))

(deftest reuse-with-same-type
  (check-updates [[{:noria/type :div
                    :dom/children [{:noria/type :div
                                    :x 1
                                    :noria/key 1}]}
                   [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :x, :node 1, :value 1}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}]]
                  [{:noria/type :div
                    :dom/children [{:noria/type :div
                                    :x 2
                                    :noria/key 2}]}
                   [#:noria{:update-type :set-attr, :attr :x, :node 1, :value 2}]]]))

(deftest reuse-with-same-type-check-order
  (check-updates [[{:noria/type :div
                    :dom/children [{:noria/type :div
                                    :x 1
                                    :noria/key 1}
                                   {:noria/type :div
                                    :x 2
                                    :noria/key 2}]}
                   [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :x, :node 1, :value 1}
                    #:noria{:update-type :make-node, :node 2, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :x, :node 2, :value 2}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 1}]]
                  [{:noria/type :div
                    :dom/children [{:noria/type :div
                                    :x 3
                                    :noria/key 3}
                                   {:noria/type :div
                                    :x 1
                                    :noria/key 1}]}
                   [#:noria{:update-type :set-attr, :attr :x, :node 2, :value 3}
                    #:noria{:update-type :remove, :attr :dom/children, :node 0, :value 2}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 0}]]
                  [{:noria/type :div
                    :dom/children [{:noria/type :div
                                    :x 1
                                    :noria/key 1}
                                   {:noria/type :div
                                    :x 4
                                    :noria/key 4}]}
                   [#:noria{:update-type :set-attr, :attr :x, :node 2, :value 4}
                    #:noria{:update-type :remove, :attr :dom/children, :node 0, :value 1}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}]]]))

(deftest reconcile-unordered
  (check-updates [[{:noria/type :div
                    :dom/children ^:noria/unordered? [{:noria/type :hey}
                                                      {:noria/type :hoy}]}
                   [#:noria{:update-type :make-node, :node 0, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 1, :type :hey, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 2, :type :hoy, :constructor-parameters {}}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 1, :index 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 2, :index 1}]]
                  [{:noria/type :div
                    :dom/children ^:noria/unordered?[{:noria/type :hey}
                                                     {:noria/type :hoy}]}
                   []]
                  [{:noria/type :div
                    :dom/children ^:noria/unordered?[{:noria/type :hoy}
                                                     {:noria/type :hiy}]}
                   [#:noria{:update-type :make-node, :node 3, :type :hiy, :constructor-parameters {}}
                    #:noria{:update-type :remove :attr :dom/children :node 0 :value 1}
                    #:noria{:update-type :add, :attr :dom/children, :node 0, :value 3, :index 1}
                    #:noria{:update-type :destroy, :node 1}]]
                  [{:noria/type :div
                    :dom/children ^:noria/unordered?[{:noria/type :hoy
                                                      :hoy/x 1}
                                                     {:noria/type :hiy}]}
                   [#:noria{:update-type :set-attr, :attr :hoy/x, :node 2, :value 1}]]]))

(def type-comp
  (render
   (fn [type]
     {:noria/type type})))

(def lambda-comp
  (render
   (fn [type]
     ['apply (fn [x]
               {:noria/type :div
                :dom/children [x]}) [type-comp type]])))

(deftest reconcile-lambda-2
  (check-updates [[[lambda-comp :foo]
                   [#:noria{:update-type :make-node, :node 0, :type :foo, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 1, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :add, :attr :dom/children, :node 1, :value 0, :index 0}]]
                  [[lambda-comp :bar]
                   [#:noria{:update-type :make-node, :node 2, :type :bar, :constructor-parameters {}}
                    #:noria{:update-type :remove, :attr :dom/children, :node 1, :value 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 1, :value 2, :index 0}
                    #:noria{:update-type :destroy, :node 0}]]]))

(def double-lambda
  (render
   (fn [a b]
     (prn "hey")
     ['apply (fn [x]
               (prn "hoy")
               ['apply (fn [y]
                         {:noria/type :div
                          :dom/children [x y]}) [type-comp b]]) [type-comp a]])))

(deftest nested-applies
  (check-updates [[[double-lambda :foo :bar]
                   [#:noria{:update-type :make-node, :node 0, :type :foo, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 1, :type :bar, :constructor-parameters {}}
                    #:noria{:update-type :make-node, :node 2, :type :div, :constructor-parameters {}}
                    #:noria{:update-type :add, :attr :dom/children, :node 2, :value 0, :index 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 2, :value 1, :index 1}]]
                  [[double-lambda :foo :baz]
                   [#:noria{:update-type :make-node, :node 3, :type :baz, :constructor-parameters {}}
                    #:noria{:update-type :remove, :attr :dom/children, :node 2, :value 1}
                    #:noria{:update-type :add, :attr :dom/children, :node 2, :value 3, :index 1}
                    #:noria{:update-type :destroy, :node 1}]]
                  [[double-lambda :fizz :fuzz]
                   [#:noria{:update-type :make-node, :node 4, :type :fizz, :constructor-parameters {}}                    
                    #:noria{:update-type :make-node, :node 5, :type :fuzz, :constructor-parameters {}}
                    #:noria{:update-type :remove, :attr :dom/children, :node 2, :value 0}
                    #:noria{:update-type :remove, :attr :dom/children, :node 2, :value 3}
                    #:noria{:update-type :add, :attr :dom/children, :node 2, :value 4, :index 0}
                    #:noria{:update-type :add, :attr :dom/children, :node 2, :value 5, :index 1}
                    #:noria{:update-type :destroy, :node 0}
                    #:noria{:update-type :destroy, :node 3}]]]))

(def type-from-state
  (fn [r-f]
    (fn
      ([] (r-f))
      ([state _] (r-f state {:noria/type (::type state :foo)}))
      ([state] (r-f state)))))

(deftest force-update-2
  (let [[value ctx] (reconcile nil {:noria/type :div
                                    :dom/children [[type-from-state]]} context-0)
        [new-value ctx'] (reconcile-in value [0 1] #(assoc % ::type :bar) ctx)]
    (is (= [#:noria{:update-type :make-node,
                    :node 2,
                    :type :bar,
                    :constructor-parameters {}}
            #:noria{:update-type :remove,
                    :node 0,
                    :attr :dom/children,
                    :value 1}
            #:noria{:update-type :add,
                    :node 0,
                    :index 0,
                    :attr :dom/children,
                    :value 2}
            #:noria{:update-type :destroy, :node 1}]
           (:updates ctx')))))

(deftest different-attrs
  (check-updates [[{:noria/type :foo
                    :foo/x 1}
                   [#:noria{:update-type :make-node, :node 0, :type :foo, :constructor-parameters {}}
                    #:noria{:update-type :set-attr, :attr :foo/x, :node 0, :value 1}]]
                  [{:noria/type :foo
                    :foo/y 1}
                   [#:noria{:update-type :set-attr, :attr :foo/y, :node 0, :value 1}
                    #:noria{:update-type :set-attr, :attr :foo/x, :node 0, :value nil}]]]))

(run-tests)

