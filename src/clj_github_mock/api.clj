(ns clj-github-mock.api
  (:require [clj-github-mock.api.repos :as repos]
            [clj-github-mock.db :as db]))

(defn meta-db [_initial_state]
  (db/meta-db repos/model))

