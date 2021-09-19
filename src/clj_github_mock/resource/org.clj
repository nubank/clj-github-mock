(ns clj-github-mock.resource.org
  (:require [clj-github-mock.resource.generators :as resource-gen]))

(def owner-resource
  {:resource/name :owner
   :resource/attributes
   [{:attribute/name :name
     :attribute/schema [:string {:gen/gen (resource-gen/unique-object-name)}]
     :attribute/unique :db.unique/identity
     :specmonstah/key? true}]})
