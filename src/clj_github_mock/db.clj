(ns clj-github-mock.db
  (:require [datascript.core :as d]))

(defn meta-db [tx-data]
  (-> (d/empty-db {:api/name {:db/unique :db.unique/identity}
                   :api/parent {:db/valueType :db.type/ref}
                   :entity/api {:db/valueType :db.type/ref}})
      (d/db-with tx-data)))
