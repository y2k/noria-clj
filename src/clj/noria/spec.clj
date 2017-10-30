(ns noria.spec
  (:require [clojure.spec.alpha :as s]))

(s/def :noria/props (s/map-of keyword? any?))
(s/def :noria/key any?)
(s/def :noria/type keyword?)
(s/def :noria/primitive-element (s/keys :req [:noria/type]
                                        :opt [:noria/key
                                              :noria/props
                                              :noria/children]))
(s/def :noria/transducer fn?)
(s/def :noria/user-element (s/spec (s/cat :type :noria/transducer :args (s/* any?))))
(s/def :noria/element (s/or :primitive :noria/primitive-element
                            :user :noria/user-element))
(s/def :noria/node nat-int?)
(s/def :noria/component (s/keys :req [:noria/node :noria/element]))
(s/def :noria/subst :noria/component)
(s/def :noria/render fn?)
(s/def :noria/state map?)
(s/def :noria/user-component (s/and :noria/component (s/keys :req [:noria/subst :noria/render :noria/state])))
(s/def :noria/children (s/coll-of :noria/component))
(s/def :noria/primitive-component (s/and :noria/component (s/keys :opt [:noria/children])))
(s/def :noria/props-diff :noria/props)
(s/def :noria/index nat-int?)
(s/def :noria/parent-node :noria/node)
(s/def :noria/child-node :noria/node)

(defmulti update-spec :noria/update-type)
(s/def :noria/update (s/multi-spec update-spec :noria/update-type))
(defmethod update-spec :add [_] (s/keys :req [:noria/index :noria/parent-node :noria/child-node]))
(defmethod update-spec :update-props [_] (s/keys :req [:noria/node :noria/props-diff]))
(defmethod update-spec :remove [_] (s/keys :req [:noria/node]))
(defmethod update-spec :destroy [_] (s/keys :req [:noria/node]))
(defmethod update-spec :make-node [_] (s/keys :req [:noria/node :noria/props :noria/node-type]))

(s/def :noria/next-id :noria/node)
(s/def :noria/updates any?)
(s/def :noria/ctx (s/keys :req-un [:noria/updates :noria/next-id]))
(s/def :noria/reducing-fn fn?)

(s/fdef noria.noria/reconcile
        :args (s/cat :component (s/nilable :noria/component)
                     :element :noria/element
                     :reducing-fn :noria/reducing-fn
                     :context :noria/ctx)
        :ret (s/cat :component :noria/component
                    :context :noria/ctx))
