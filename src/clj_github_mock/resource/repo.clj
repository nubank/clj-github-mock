(ns clj-github-mock.resource.repo
  (:require [clojure.string :as string]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.impl.jgit :as jgit]
            [base64-clj.core :as base64]))

(def default_branch "main")

(def db-schema {:repo/org+name {:db/tupleAttrs [:repo/org :repo/name]
                                :db/type :db.type/tuple
                                :db/unique :db.unique/identity}
                :repo/org {:db/type :db.type/ref}})

(defn repo-middleware [handler]
  (fn [{conn :conn {:keys [org repo]} :path-params :as request}]
    (let [db (d/db conn)
          org-id (d/entid db [:org/name org])
          repo (d/entity db [:repo/org+name [org-id repo]])]
      (handler (assoc request :repo repo)))))

(defn repo-key [db {{:keys [org repo]} :path-params}]
  [:repo/org+name [(d/entid db [:org/name org]) repo]])

(defn repo-body [repo]
  (merge
   {:id (:repo/id repo)
    :name (:repo/name repo)
    :full_name (string/join "/" [(-> repo :repo/org :org/name) (:repo/name repo)])}
   (:repo/attrs repo)))

(defn repo-post [_ {{:keys [org]} :path-params
                    body :body}]
  {:repo/name (:name body)
   :repo/org [:org/name org]
   :repo/default_branch (get body :default_branch default_branch)
   :repo/full_name (string/join "/" [org (:name body)])
   :repo/jgit (jgit/empty-repo)})

(defn repo-patch [db {{:keys [org repo]} :path-params
                      body :body}]
  (let [key [(d/entid db [:org/name org]) repo]]
    {:repo/org+name key
     :repo/default_branch (get body :default_branch default_branch)}))

(defn repo-list [db {{:keys [org]} :path-params}]
  (->> (d/q '[:find [?r ...]
              :in $ ?org
              :where
              [?o :org/name ?org]
              [?r :repo/org ?o]] db org)
       (map #(d/entity db %))))

(def repo-resource
  {:lookup-fn (handlers/db-lookup-fn repo-key)
   :body-fn repo-body
   :post-schema [:map
                 [:path-params [:map
                                [:org :string]]]
                 [:body [:map
                         [:name :string]]]]
   :post-fn (handlers/db-transact-fn repo-post)
   :patch-fn (handlers/db-transact-fn repo-patch)
   :patch-schema [:map
                  [:path-params [:map
                                 [:org :string]
                                 [:repo :string]]]]
   :list-fn (handlers/db-list-fn repo-list)})

(defn branch-key [db {{:keys [org repo branch]} :path-params}]
  [:ref/repo+ref [(d/entid db [:repo/org+name [(d/entid db [:org/name org]) repo]]) (str "refs/heads/" branch)]])

(defn branch-body [branch]
  {:name (string/replace (:ref/ref branch) "refs/heads/" "")
   :commit {:sha (:ref/sha branch)
            :commit (-> (jgit/get-commit (:repo/jgit (:ref/repo branch)) (:ref/sha branch))
                        (dissoc :sha))}})

(def branch-resource
  {:lookup-fn (handlers/db-lookup-fn branch-key)
   :body-fn branch-body})

(defn- sha? [ref]
  (and ref
       (re-find #"^[A-Fa-f0-9]{40}$" ref)))

(defn- content-sha [db {repo-id :db/id {:keys [default_branch]} :repo/attrs} ref]
  (if (sha? ref)
    ref
    (let [branch (or ref default_branch)]
      (:ref/sha (d/entity db [:ref/repo+ref [repo-id (str "refs/heads/" branch)]])))))

(defn content-lookup [{{git-repo :repo/jgit :as repo} :repo
                       {:keys [path]} :path-params
                       {:strs [ref]} :query-params
                       conn :conn}]
  (when-let [sha (content-sha @conn repo ref)]
    (when (jgit/path-exists? git-repo sha path)
      {:repo repo
       :sha  sha
       :path path})))

(defn content-body [{{git-repo :repo/jgit} :repo :keys [path sha]}] 
  {:type "file"
   :path path
   :content (base64/encode
             (jgit/get-content git-repo sha path)
             "UTF-8")})

(def content-resource
  {:lookup-fn content-lookup
   :body-fn content-body})

(def routes
  [["/orgs/:org/repos" {:get (handlers/list-handler repo-resource)
                        :post (handlers/post-handler repo-resource)}]
   ["/repos/:org/:repo" {:middleware [repo-middleware]}
    ["" {:get (handlers/get-handler repo-resource)
         :patch (handlers/patch-handler repo-resource)}]
    ["/branches/:branch" {:get (handlers/get-handler branch-resource)}]
    ["/contents/*path" {:get (handlers/get-handler content-resource)}]]])
