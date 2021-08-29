(ns clj-github-mock.resource
  (:require [clj-github-mock.resource.repo :as repos]
            [clj-github-mock.db :as db]))

(defn meta-db [_initial_state]
  (db/meta-db repos/model))

