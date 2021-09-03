(ns clj-github-mock.resource
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.resource.org :as org]
            [clj-github-mock.resource.git-database :as git-database]
            [ring.middleware.params :as middleware.params]
            [reitit.ring :as ring]
            [datascript.core :as d]))

; TODO use initial state
(defn conn [{:keys [db-schema] :or {db-schema {}} :as _initial-state}]
  (d/create-conn (merge
                  org/db-schema
                  repo/db-schema
                  git-database/db-schema
                  db-schema)))

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
