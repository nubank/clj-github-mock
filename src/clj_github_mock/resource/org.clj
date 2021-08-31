(ns clj-github-mock.resource.org)

(def db-schema {:org/name {:db/unique :db.unique/identity}})
