(ns clj-github-mock.resource
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.resource.org :as org]
            [clj-github-mock.resource.git-database :as git-database]
            [clj-github-mock.db :as db]))

(defn meta-db [_initial_state]
  (db/meta-db
   [org/resource
    git-database/tree-resource
    git-database/commit-resource
    git-database/ref-resource
    repo/resource
    repo/branch-resource]))

