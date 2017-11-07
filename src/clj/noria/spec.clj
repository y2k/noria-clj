(ns noria.spec
  (:require [clojure.spec.alpha :as s]))

(s/def :noria/node nat-int?)
(s/def :noria/component-id nat-int?)
(s/def :noria/render fn?)
(s/def :noria/subst nat-int?)
(s/def :noria/state any?)
(s/def :noria/component (s/keys :req [:noria/component-id]
                                :opt [:noria/render
                                      :noria/subst
                                      :noria/state
                                      :noria/element
                                      :noria/node]))
(s/def :noria/components (s/map-of :noria/component-id :noria/component))

(s/def :noria/update-type #{:add :remove :make-node :set-attr :destroy})
(s/def :noria/attr keyword?)
(s/def :noria/value any?)
(s/def :noria/index nat-int?)
(s/def :noria/type keyword?)
(s/def :noria/constructor-parameters:noria/constructor-patameters map?)

(defmulti update-spec :noria/update-type)
(defmethod update-spec :add [_]
  (s/keys :req [:noria/node :noria/attr :noria/value :noria/index]))
(defmethod update-spec :remove [_]
  (s/keys :req [:noria/node :noria/attr :noria/value]))
(defmethod update-spec :make-node [_]
  (s/keys :req [:noria/node :noria/type :noria/constructor-parameters]))
(defmethod update-spec :set-attr [_]
  (s/keys :req [:noria/node :noria/attr :noria/value]))
(defmethod update-spec :destroy [_]
  (s/keys :req [:noria/node]))
(s/def :noria/update (s/multi-spec update-spec :noria/update-type))

(s/def :noria/context (s/keys :req-un [:noria/components]))
