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
