(ns clj-github-mock.resource
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.resource.org :as org]
            [clj-github-mock.db :as db]))

(def resources
  [org/resource
   repo/resource])

(defn meta-db [_initial_state]
  (db/meta-db resources))

