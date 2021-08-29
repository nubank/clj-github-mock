(ns clj-github-mock.db
  (:require [datascript.core :as d]
            [clj-github-mock.impl.jgit :as jgit]))

(defn- db-schema [meta-db]
  (apply merge (d/q '[:find [?s ...]
                      :where
                      [_ :resource/db-schema ?s]]
                    meta-db)))

(defn- create-meta [meta-db]
  [{:db/ident :meta
    :meta/conn (d/create-conn (db-schema meta-db))
    :meta/jgit-repo (jgit/empty-repo)}])

(defn meta-db [tx-data]
  (as-> (d/empty-db {:resource/name {:db/unique :db.unique/identity}
                     :db/ident {:db/unique :db.unique/identity}})
      $
    (d/db-with $ tx-data)
    (d/db-with $ (create-meta $))))

(defn meta-entity [meta-db]
  (d/entity meta-db [:db/ident :meta]))

(defn conn [meta-db]
  (:meta/conn (meta-entity meta-db)))

(defn jgit-repo [meta-db]
  (:meta/jgit-repo (meta-entity meta-db)))

(defn resource [meta-db resource-name]
  (d/entity meta-db [:resource/name resource-name]))

(defn lookup-fn [meta-db resource-name]
  (:resource/lookup-fn (resource meta-db resource-name)))

(defn body-fn [meta-db resource-name]
  (:resource/body-fn (resource meta-db resource-name)))

(defn post-fn [meta-db resource-name]
  (:resource/post-fn (resource meta-db resource-name)))

(defn post-schema [meta-db resource-name]
  (:resource/post-schema (resource meta-db resource-name)))

(defn patch-fn [meta-db resource-name]
  (:resource/patch-fn (resource meta-db resource-name)))

(defn patch-schema [meta-db resource-name]
  (:resource/patch-schema (resource meta-db resource-name)))

(defn delete-fn [meta-db resource-name]
  (:resource/delete-fn (resource meta-db resource-name)))

(defn list-fn [meta-db resource-name]
  (:resource/list-fn (resource meta-db resource-name)))
