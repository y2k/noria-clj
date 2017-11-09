(ns noria (:import [noria LCS]))

(defn get-key [x] (::key (if (vector? x) (meta x) x)))
(defn get-type [x] (if (vector? x) (first x) (::type x)))
(defn supply [ctx u] (update ctx :updates conj u))

(declare reconcile)

(defn reconcile-children [parent-node children new-children ctx]
  (let [component-key (comp get-key ::element)
        old-keys (map component-key children)
        new-keys (mapv get-key new-children)
        new-keys-set (into #{} new-keys)
        moved? (if (= old-keys new-keys)
                 (constantly false)
                 (complement (into #{} (LCS/lcs (into-array Object old-keys)
                                                (into-array Object new-keys)))))
        key->component (into {} (map (fn [c] [(component-key c) c])) children)
        ctx' (->> children
                  (remove #(contains? new-keys-set (component-key %)))
                  (reduce (fn [ctx c] (supply ctx {::update-type :destroy
                                                  ::node (::node c)})) ctx))]
    (reduce
     (fn [[result ctx] [i child]]
       (let [old-c (key->component (get-key child))
             [new-c ctx'] (reconcile old-c child ctx)
             child-moved? (moved? (get-key child))]
         [(conj result new-c)
          (cond-> ctx'
            (and (some? old-c) child-moved?)
            (supply {::update-type :remove
                     ::node (::node old-c)})
            (or (not= (::node old-c) (::node new-c)) child-moved?)
            (supply {::update-type :add
                     ::index i
                     ::child-node (::node new-c)
                     ::parent-node parent-node}))]))
     [[] ctx'] (map vector (range) new-children))))

(defn make-node [element {:keys [next-id] :as ctx}]
  [next-id (-> ctx
               (update :next-id inc)
               (supply {::update-type :make-node
                        ::node next-id
                        ::type (get-type element)
                        ::props (::props element)}))])

(defn reconcile-primitive [{::keys [node element children] :as component} new-element ctx]
  (let [[node ctx] (if (some? node)
                     [node ctx]
                     (make-node new-element ctx))
        [children-reconciled ctx'] (reconcile-children node children (::children new-element) ctx)
        old-props (::props element)
        new-props (::props new-element)]
    [(assoc component
            ::node node
            ::element new-element
            ::children children-reconciled)
     (cond-> ctx'
       (and (::node component) (not= old-props new-props))
       (supply {::update-type :update-props
                ::node node
                ::new-props new-props
                ::old-props old-props}))]))


(comment
  {:update-type :make-node
   :type :div
   :props {}
   :node 1}

  {:update-type :add
   :child 1
   :parent 0
   :idnex 0}

  {:update-type :remove}

  {:update-type :update-props
   :old-props {}
   :new-props {}
   :node 5}


  
  )


(defn reconcile-user [component [render & args :as element] ctx]
  (let [[subst' ctx'] (reconcile (::subst component) (apply render args) ctx)]
    [(assoc component
            ::subst subst'
            ::element element
            ::node (::node subst')) ctx']))

(defn reconcile [component element ctx]
  (let [[component ctx] (if (not= (get-type (::element component)) (get-type element))
                          [nil (if (some? component)
                                 (supply ctx {::update-type :destroy
                                              ::node (::node component)})
                                 ctx)]
                          [component ctx])]
    (if (vector? element)
      (reconcile-user component element ctx)
      (reconcile-primitive component element ctx))))


(comment
  (defn my-label [x]
    {::type :div
     ::props {:text x}})

  (defn my-container [n]
    {::type :div
     ::children (map (fn [i] ^{::key i} [my-label i]) (range n))})

  (let [[c ctx] (reconcile nil [my-container 1] {:next-id 0
                                                 :updates []})]
    (def ctx ctx)
    (def c c))

  (reconcile c [my-container 2] (assoc ctx :updates []))
  )

(comment
  :keyword
  'symbol
  
  (f x y)

  [1 2 3]

  (def x {1 2
          3 5})

  (do
    (def x 1))

  (reconcile component
             {:type :button
              :on-click (fn [] (prn "hey"))}
             ) => component'


  (reconcile nil {}) => [component updates]

  (reconcile component {}) => [component' updates']

  (defn my-table [app-delegate db]
    {:type :NSTableView
     :datasource app-delegate})

  (defn mywindow [app-delegate db]
    {:type :NSWindow
     :rootView [my-table db]})

  (let-rec** [window [my-window delegate db]
              delegate {:type :OnAirAppDelegate
                        :window window}]
    {:type :NSApplication
     :delegate delegate
     :window window})



  
  
  {}

  (defn my-button [x]
    {:type :button
     :text (str x)
     :on-click (fn []...)})

  (defn my-component [x]
    {:type :div
     :children [[my-button x]]})
  
  [my-component' 1]

  
  
  (assoc x 1 3)

  {:keyword 1}

  '(a b c)

  (list 1 2 3)

  (vector 1 2 3)

  

  )
