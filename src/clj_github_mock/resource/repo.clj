(ns clj-github-mock.resource.repo
  (:require [medley.core :as m]
            [clojure.string :as string]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.impl.jgit :as jgit]))

(def repo-defaults {:default_branch "main"})

(def db-schema {:repo/name+org {:db/tupleAttrs [:repo/name :repo/org]
                                :db/type :db.type/tuple
                                :db/unique :db.unique/identity}
                :repo/org {:db/type :db.type/ref}})

(defn repo-middleware [handler]
  (fn [{conn :conn {:keys [org repo]} :path-params :as request}]
    (let [db (d/db conn)
          org-id (d/entid db [:org/name org])
          repo (d/entity db [:repo/name+org [repo org-id]])]
      (handler (assoc request :repo repo)))))

(defn repo-key [db {{:keys [org repo]} :path-params}]
  [:repo/name+org [repo (d/entid db [:org/name org])]])

(defn repo-body [repo]
  (merge
   {:name (:repo/name repo)
    :full_name (string/join "/" [(-> repo :repo/org :org/name) (:repo/name repo)])}
   (:repo/attrs repo)))

(defn repo-post [_ {{:keys [org]} :path-params
                    body :body}]
  {:repo/name (:name body)
   :repo/org [:org/name org]
   :repo/attrs (merge repo-defaults (m/remove-keys #{:name} body))
   :repo/jgit (jgit/empty-repo)})

(defn repo-patch [db {{:keys [org repo]} :path-params
                      body :body}]
  (let [key [repo (d/entid db [:org/name org])]]
    {:repo/name+org key
     :repo/attrs (merge (-> (d/entity db [:repo/name+org key]) :repo/attrs)
                        body)}))

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
  [:ref/repo+ref [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) (str "refs/heads/" branch)]])

(defn branch-body [branch]
  {:name (string/replace (:ref/ref branch) "refs/heads/" "")
   :commit {:sha (:ref/sha branch)}})

; TODO complete commit object
(def branch-resource
  {:lookup-fn (handlers/db-lookup-fn branch-key)
   :body-fn branch-body})

(defn- sha? [ref]
  (and ref
       (re-find #"^[A-Fa-f0-9]{40}$" ref)))

(defn- content-sha [git-repo ref default_branch]
  (if (sha? ref)
    ref
    (let [branch (or ref default_branch)]
      (get-in (jgit/get-reference git-repo (str "refs/heads/" branch)) [:object :sha]))))

(def content-resource
  {:lookup-fn (fn [{{git-repo :repo/jgit
                     {:keys [default_branch]} :repo/attrs} :repo
                    {:keys [path]} :path-params
                    {:strs [ref]} :query-params}]
                (let [sha (content-sha git-repo ref default_branch)]
                  (when sha (jgit/get-content git-repo sha path))))
   :body-fn identity})

(def routes
  [["/orgs/:org/repos" {:get (handlers/list-handler repo-resource)
                        :post (handlers/post-handler repo-resource)}]
   ["/repos/:org/:repo" {:middleware [repo-middleware]}
    ["" {:get (handlers/get-handler repo-resource)
         :patch (handlers/patch-handler repo-resource)}]
    ["/branches/:branch" {:get (handlers/get-handler branch-resource)}]
    ["/contents/*path" {:get (handlers/get-handler content-resource)}]]])
