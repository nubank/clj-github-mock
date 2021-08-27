(ns clj-github-mock.db
  (:require [datascript.core :as d]))

(defn- schema [meta-db]
  (apply merge (d/q '[:find [?s ...]
                      :where
                      [_ :entity/schema ?s]]
                    meta-db)))

(defn- create-conn [meta-db]
  [{:meta/meta :meta
    :meta/conn (d/create-conn (schema meta-db))}])

(defn meta-db [tx-data]
  (as-> (d/empty-db {:api/name {:db/unique :db.unique/identity}
                     :api/parent {:db/valueType :db.type/ref}
                     :entity/name {:db/unique :db.unique/identity}
                     :meta/meta {:db/unique :db.unique/identity}})
      $
    (d/db-with $ tx-data)
    (d/db-with $ (create-conn $))))

(defn meta-entity [meta-db]
  (d/entity meta-db [:meta/meta :meta]))

(defn conn [meta-db]
  (:meta/conn (meta-entity meta-db)))
