(ns clj-github-mock.impl.database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]))

(def repo-defaults {:default_branch "main"})

(defn- uuid []
  (str (java.util.UUID/randomUUID)))

(defn- repo-datums [org-name repo-name repo-attrs]
  [{:repo/id (uuid)
    :repo/name repo-name
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
                :issue/id {:db/unique :db.unique/identity}
                :repo/org {:db/valueType :db.type/ref}
                :issue/repo {:db/valueType :db.type/ref}}
        conn (d/create-conn schema)]
    (d/transact! conn (mapcat org-datums orgs))
    conn))

(defn upsert-repo [database org-name repo-name attrs]
  (d/transact! database (repo-datums org-name repo-name attrs)))

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

(def issue-defaults {:state "open"})

(defn issue-datums [repo-id issue-number issue-type issue-attrs]
  [{:issue/id (uuid)
    :issue/number issue-number
    :issue/repo [:repo/id repo-id]
    :issue/type  issue-type
    :issue/attrs (merge issue-defaults issue-attrs)}])

(defn upsert-issue [database repo-id issue-number issue-type attrs]
  (d/transact database (issue-datums repo-id issue-number issue-type attrs)))

(defn find-pull [database org-name repo-name pull-number]
  (d/q '[:find (pull ?p [*]) .
         :in $ ?org-name ?repo-name ?pull-number
         :where
         [?r :repo/name ?repo-name]
         [?r :repo/org ?org]
         [?p :issue/repo ?r]
         [?p :issue/type :pull]
         [?p :issue/number ?pull-number]
         [?org :org/name ?org-name]] @database org-name repo-name pull-number))

(defn find-pull-by-repo-id-number [database repo-id pull-number]
  (d/q '[:find (pull ?p [*]) .
         :in $ ?repo-id ?pull-number
         :where
         [?r :repo/id ?repo-id]
         [?p :issue/repo ?r]
         [?p :issue/type :pull]
         [?p :issue/number ?pull-number]
         [?org :org/name ?org-name]] @database repo-id pull-number))

(defn next-issue-number [database repo-id]
  (inc
   (ffirst (or (seq (d/q '[:find (max ?n)
                           :in $ ?repo-id
                           :where
                           [?r :repo/id ?repo-id]
                           [?i :issue/repo ?r]
                           [?i :issue/number ?n]]
                         @database repo-id))
               [[0]]))))

(or (seq #{}) [1])

(defn transact [database datums]
  (d/transact database datums))

(defn lookup [database eid]
  (d/pull @database '[*] eid))

(defn middleware [handler conn]
  (fn [request]
    (handler (assoc request :database conn))))
