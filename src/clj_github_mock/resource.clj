(ns clj-github-mock.resource
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.resource.org :as org]
            [clj-github-mock.resource.git-database :as git-database]
            [ring.middleware.params :as middleware.params]
            [reitit.ring :as ring]
            [datascript.core :as d]))

(defn- repo-datoms [org-name {:keys [name default_branch] :or {default_branch "main"}}]
  [{:repo/name name
    :repo/org {:org/name org-name}
    :repo/attrs {:default_branch default_branch}}])

(defn- org-datoms [{:keys [name repos]}]
  (into [{:org/name name}]
        (mapcat (partial repo-datoms name) repos)))

(defn- datoms [{:keys [orgs]}]
  (mapcat org-datoms orgs))

(defn conn [{:keys [db-schema] :or {db-schema {}} :as initial-state}]
  (let [result (d/create-conn (merge
                               org/db-schema
                               repo/db-schema
                               git-database/db-schema
                               db-schema))]
    (d/transact! result (datoms initial-state))
    result))

(defn conn-middleware [handler conn]
  (fn [request]
    (handler (assoc request :conn conn))))

(defn handler [conn]
  (-> (ring/ring-handler
       (ring/router (concat
                     repo/routes
                     git-database/routes))
       (ring/create-default-handler))
      (middleware.params/wrap-params)
      (conn-middleware conn)))
