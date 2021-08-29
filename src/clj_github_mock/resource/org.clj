(ns clj-github-mock.resource.org)

(def resource
  {:resource/name :org
   :resource/db-schema {:org/name {:db/unique :db.unique/identity}}})
