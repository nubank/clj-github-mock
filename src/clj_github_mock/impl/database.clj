(ns clj-github-mock.impl.database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]))

(def repo-defaults {:default_branch "main"})

(defn- repo-datums [org-name repo-name repo-attrs]
  [{:repo/name repo-name
    :repo/org [:org/name org-name]
    :repo/attrs (merge repo-defaults repo-attrs)
    :repo/jgit (jgit/empty-repo)}])

(defn- org-datums [{org-name :name repos :repos}]
  (concat
   [{:org/name org-name}]
   (mapcat #(repo-datums org-name
                         (:name %)
                         (dissoc % :name)) repos)))

(defn create [{:keys [orgs] :as _initial-state}]
  (let [schema {:org/name {:db/unique :db.unique/identity}
                :repo/id {:db/unique :db.unique/identity}
                :repo/org {:db/valueType :db.type/ref}}
        conn (d/create-conn schema)]
    (d/transact! conn (mapcat org-datums orgs))
    conn))

(defn upsert-repo [database org-name repo-name attrs]
  (d/transact! database [{:repo/name+org [repo-name (d/entid @database [:org/name org-name])]
                          :repo/attrs (merge repo-defaults attrs)}]))

(defn find-repos [database org-name]
  (d/q
   '[:find [(pull ?r [*]) ...]
     :in $ ?org-name
     :where
     [?r :repo/org ?org]
     [?org :org/name ?org-name]]
   @database org-name))

(defn find-repo [database org-name repo-name]
  (d/q '[:find (pull ?r [*]) .
         :in $ ?org-name ?repo-name
         :where
         [?r :repo/name ?repo-name]
         [?r :repo/org ?org]
         [?org :org/name ?org-name]] @database org-name repo-name))

(defn lookup [database eid]
  (d/pull @database '[*] eid))

(defn middleware [handler conn]
  (fn [request]
    (handler (assoc request :database conn))))
