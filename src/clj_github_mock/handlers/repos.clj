(ns clj-github-mock.handlers.repos
  (:require [clj-github-mock.impl.database :as database]
            [clj-github-mock.impl.jgit :as jgit]
            [reitit.ring :as ring]
            [ring.middleware.params :as middleware.params]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.db :as db]))

(defn- sha? [ref]
  (and ref
       (re-find #"^[A-Fa-f0-9]{40}$" ref)))

(defn- content-sha [git-repo ref default_branch]
  (if (sha? ref)
    ref
    (let [branch (or ref default_branch)]
      (get-in (jgit/get-reference git-repo (str "refs/heads/" branch)) [:object :sha]))))

(defn get-content-handler [{{git-repo :repo/jgit
                             {:keys [default_branch]} :repo/attrs} :repo
                            {:keys [path]} :path-params
                            {:strs [ref]} :query-params}]
  (let [sha (content-sha git-repo ref default_branch)
        content (when sha (jgit/get-content git-repo sha path))]
    (if content
      {:status 200
       :body content}
      {:status 404})))

(defn repo-middleware [handler]
  (fn [{database :database {:keys [org repo]} :path-params :as request}]
    (let [repo (database/find-repo database org repo)]
      (handler (assoc request :repo repo)))))

(defn routes [meta-db]
  [["/orgs/:org/repos" {:get (handlers/list-handler meta-db :repo)
                        :post (handlers/post-handler meta-db :repo)}]
   ["/repos/:org/:repo" {:middleware [repo-middleware]}
    ["" {:get (handlers/get-handler meta-db :repo)
         :patch (handlers/patch-handler meta-db :repo)}]
    ["/git/trees" {:post (handlers/post-handler meta-db :tree)}]
    ["/git/trees/:sha" {:get (handlers/get-handler meta-db :tree)}]
    ["/git/commits" {:post (handlers/post-handler meta-db :commit)}]
    ["/git/commits/:sha" {:get (handlers/get-handler meta-db :commit)}]
    ["/git/refs" {:post (handlers/post-handler meta-db :ref)}]
    ["/git/refs/*ref" {:patch (handlers/patch-handler meta-db :ref)
                       :delete (handlers/delete-handler meta-db :ref)}]
    ["/git/ref/*ref" {:get (handlers/get-handler meta-db :ref)}]
    ["/branches/:branch" {:get (handlers/get-handler meta-db :branch)}]
    ["/contents/*path" {:get get-content-handler}]]])

(defn meta-db-middleware [handler meta-db]
  (fn [request]
    (handler (assoc request :meta-db meta-db))))

(defn handler [meta-db]
  (-> (ring/ring-handler
       (ring/router (routes meta-db))
       (ring/create-default-handler))
      (middleware.params/wrap-params)
      (database/middleware (db/conn meta-db))
      (meta-db-middleware meta-db)))
