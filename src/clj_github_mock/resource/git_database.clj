(ns clj-github-mock.resource.git-database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.resource.repo :as repo]))

(def db-schema {:ref/repo+ref {:db/tupleAttrs [:ref/repo :ref/ref]
                               :db/unique :db.unique/identity}
                :ref/repo {:db/type :db.type/ref}})

(defn tree-body [tree]
  (jgit/get-tree (-> tree :tree/repo :repo/jgit) (:tree/sha tree)))

(defn tree-lookup [{{:keys [sha]} :path-params
                    {jgit-repo :repo/jgit :as repo} :repo}]
  (when (jgit/object-exists? jgit-repo sha)
    {:tree/repo repo
     :tree/sha sha}))

(defn tree-post [{{jgit-repo :repo/jgit :as repo} :repo
                  body :body}]
  {:tree/repo repo
   :tree/sha (:sha (jgit/create-tree! jgit-repo body))})

(def tree-resource
  {:body-fn tree-body
   :lookup-fn tree-lookup
   :post-fn tree-post
   :post-schema [:map
                 [:path-params [:map
                                [:org :string]
                                [:repo :string]]]
                 [:body [:map
                         [:tree [:vector
                                 [:map
                                  [:path :string]
                                  [:mode [:enum "100644" "100755" "040000"]]
                                  [:type [:enum "blob" "tree"]]
                                  [:sha {:optional true} :string]
                                  [:content {:optional true} :string]]]]
                         [:base_tree {:optional true} :string]]]]})

(defn commit-body [commit]
  (jgit/get-commit (-> commit :commit/repo :repo/jgit) (:commit/sha commit)))

(defn commit-lookup [{{:keys [sha]} :path-params
                      {jgit-repo :repo/jgit :as repo} :repo}]
  (when (jgit/object-exists? jgit-repo sha)
    {:commit/repo repo
     :commit/sha sha}))

(defn commit-post [{{jgit-repo :repo/jgit :as repo} :repo
                    body :body}]
  {:commit/repo repo
   :commit/sha (:sha (jgit/create-commit! jgit-repo body))})

(def commit-resource
  {:body-fn commit-body
   :lookup-fn commit-lookup
   :post-fn commit-post
   :post-schema [:map
                 [:path-params [:map
                                [:org :string]
                                [:repo :string]]]
                 [:body [:map
                         [:message :string]
                         [:tree :string]
                         [:parents {:optional true} [:vector :string]]]]]})

(defn ref-key [db {{:keys [org repo ref]} :path-params}]
  [:ref/repo+ref [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) (str "refs/" ref)]])

(defn ref-body [ref]
  {:ref (:ref/ref ref)
   :object {:type :commit
            :sha (:ref/sha ref)}})

(defn ref-post [_ {:keys [repo body]}]
  {:ref/repo (:db/id repo)
   :ref/ref (:ref body)
   :ref/sha (:sha body)})

(defn ref-patch [_ {:keys [repo body]
                    {:keys [ref]} :path-params}]
  {:ref/repo+ref [(:db/id repo) (str "refs/" ref)]
   :ref/sha (:sha body)})

; TODO enforce update using jgit
(def ref-resource {:body-fn ref-body
                   :lookup-fn (handlers/db-lookup-fn ref-key)
                   :post-fn (handlers/db-transact-fn ref-post)
                   :post-schema [:map
                                 [:path-params [:map
                                                [:org :string]
                                                [:repo :string]]]
                                 [:body [:map
                                         [:ref :string]
                                         [:sha :string]]]]
                   :patch-fn (handlers/db-transact-fn ref-patch)
                   :patch-schema [:map
                                  [:path-params [:map
                                                 [:org :string]
                                                 [:repo :string]
                                                 [:ref :string]]]
                                  [:body [:map
                                          [:sha :string]]]]
                   :delete-fn (handlers/db-delete-fn ref-key)})

(def routes
  [["/repos/:org/:repo" {:middleware [repo/repo-middleware]}

    ["/git/trees" {:post (handlers/post-handler tree-resource)}]
    ["/git/trees/:sha" {:get (handlers/get-handler tree-resource)}]
    ["/git/commits" {:post (handlers/post-handler commit-resource)}]
    ["/git/commits/:sha" {:get (handlers/get-handler commit-resource)}]
    ["/git/refs" {:post (handlers/post-handler ref-resource)}]
    ["/git/refs/*ref" {:patch (handlers/patch-handler ref-resource)
                       :delete (handlers/delete-handler ref-resource)}]
    ["/git/ref/*ref" {:get (handlers/get-handler ref-resource)}]]])