(ns clj-github-mock.resource
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.resource.org :as org]
            [clj-github-mock.resource.database :as database]
            [clj-github-mock.db :as db]))

(defn meta-db [_initial_state]
  (db/meta-db
   [org/resource
    repo/resource
    database/tree-resource
    database/commit-resource]))

