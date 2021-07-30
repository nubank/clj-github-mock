(ns clj-github-mock.handlers.repos
  (:require [clj-github-mock.impl.database :as database]
            [clj-github-mock.impl.jgit :as jgit]
            [clojure.string :as string]
            [reitit.ring :as ring]
            [ring.middleware.params :as middleware.params]))

(defn repo-body [org-name {repo-name :repo/name attrs :repo/attrs}]
  (merge
   {:name repo-name
    :full_name (string/join "/" [org-name repo-name])}
   attrs))

(defn get-repos-handler [{database :database {:keys [org]} :path-params}]
  {:status 200
   :body (mapv (partial repo-body org) (database/find-repos database org))})

(defn post-repos-handler [{state :database
                           {:keys [org]} :path-params
                           {:keys [name] :as repo} :body}]
  (if name
    (do
      (database/upsert-repo state org name (dissoc repo :name))
      {:status 201
       :body (repo-body org (database/find-repo state org name))})
    {:status 422}))

(defn get-repo-handler [{repo :repo
                         {:keys [org]} :path-params}]
  {:status 200
   :body (repo-body org repo)})

(defn patch-repo-handler [{database :database
                           {:keys [org repo]} :path-params
                           body :body}]
  (database/upsert-repo database org repo body)
  {:status 200
   :body (repo-body org (database/find-repo database org repo))})

(defn post-tree-handler [{{git-repo :repo/jgit} :repo
                          body :body}]
  {:status 201
   :body (jgit/create-tree! git-repo body)})

(defn get-tree-handler [{{git-repo :repo/jgit} :repo
                         {:keys [sha]} :path-params}]
  (if-let [body (jgit/get-tree git-repo sha)]
    {:status 200
     :body body}
    {:status 404}))

(defn post-commit-handler [{{git-repo :repo/jgit} :repo
                            body :body}]
  {:status 201
   :body (jgit/create-commit! git-repo body)})

(defn get-commit-handler [{{git-repo :repo/jgit} :repo
                           {:keys [sha]} :path-params}]
  (if-let [body (jgit/get-commit git-repo sha)]
    {:status 200
     :body body}
    {:status 404}))

(defn post-ref-handler [{{git-repo :repo/jgit} :repo
                         body :body}]
  {:status 201
   :body (jgit/create-reference! git-repo body)})

(defn patch-ref-handler [{{git-repo :repo/jgit} :repo
                          {:keys [ref]} :path-params
                          body :body}]
  {:status 200
   :body (jgit/create-reference! git-repo (merge {:ref (str "refs/" ref)}
                                                 body))})

(defn get-ref-handler [{{git-repo :repo/jgit} :repo
                        {:keys [ref]} :path-params}]
  (if-let [body (jgit/get-reference git-repo (str "refs/" ref))]
    {:status 200
     :body body}
    {:status 404}))

(defn delete-ref-handler [{{git-repo :repo/jgit} :repo
                           {:keys [ref]} :path-params}]
  (jgit/delete-reference! git-repo (str "refs/" ref))
  {:status 204})

(defn get-branch-handler [{{git-repo :repo/jgit} :repo
                           {:keys [branch]} :path-params}]
  (if-let [branch (jgit/get-branch git-repo branch)]
    {:status 200
     :body branch}
    {:status 404}))

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

(defn- pull-body [pull org repo]
  (merge {:number (:issue/number pull)
          :repo (repo-body (:org/name org) repo)}
         (:issue/attrs pull)))

(defn post-pull-handler [{database :database
                          org :org
                          {repo-id :repo/id :as repo} :repo
                          body :body}]
  (let [next-issue-number (database/next-issue-number database repo-id)]
    (database/upsert-issue database repo-id next-issue-number :pull body)
    {:status 201
     :body (pull-body (database/find-pull-by-repo-id-number database repo-id next-issue-number) org repo)}))

(defn get-pull-handler [{database :database
                         org :org
                         {repo-id :repo/id :as repo} :repo
                         {:keys [number]} :path-params}]
  {:status 200
   :body (pull-body (database/find-pull-by-repo-id-number database repo-id (Integer/parseInt number)) org repo)})

(defn repo-middleware [handler]
  (fn [{database :database {:keys [org repo]} :path-params :as request}]
    (let [org (database/lookup database [:org/name org])
          repo (database/find-repo database (:org/name org) repo)]
      (handler (assoc request :org org :repo repo)))))

(def routes
  [["/orgs/:org/repos" {:get get-repos-handler
                        :post post-repos-handler}]
   ["/repos/:org/:repo" {:middleware [repo-middleware]}
    ["" {:get get-repo-handler
         :patch patch-repo-handler}]
    ["/git/trees" {:post post-tree-handler}]
    ["/git/trees/:sha" {:get get-tree-handler}]
    ["/git/commits" {:post post-commit-handler}]
    ["/git/commits/:sha" {:get get-commit-handler}]
    ["/git/refs" {:post post-ref-handler}]
    ["/git/refs/*ref" {:patch patch-ref-handler
                       :delete delete-ref-handler}]
    ["/git/ref/*ref" {:get get-ref-handler}]
    ["/branches/:branch" {:get get-branch-handler}]
    ["/contents/*path" {:get get-content-handler}]
    ["/pulls"
     ["" {:post post-pull-handler}]
     ["/:number" {:get get-pull-handler}]]]])

(defn handler [database]
  (-> (ring/ring-handler
       (ring/router routes)
       (ring/create-default-handler))
      (middleware.params/wrap-params)
      (database/middleware database)))
