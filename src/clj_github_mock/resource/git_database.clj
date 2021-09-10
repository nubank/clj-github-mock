(ns clj-github-mock.resource.git-database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.resource.repo :as repo]))

(def db-schema {:tree/repo+sha {:db/tupleAttrs [:tree/repo :tree/sha]
                                :db/unique :db.unique/identity}
                :tree/repo {:db/type :db.type/ref}
                :tree/base {:db/type :db.type/ref}
                :commit/repo+sha {:db/tupleAttrs [:commit/repo :commit/sha]
                                  :db/unique :db.unique/identity}
                :commit/repo {:db/type :db.type/ref}
                :commit/tree {:db/type :db.type/ref}
                :commit/parents {:db/type :db.type/ref
                                 :db/cardinality :db.cardinality/many}
                :ref/repo+ref {:db/tupleAttrs [:ref/repo :ref/ref]
                               :db/unique :db.unique/identity}
                :ref/repo {:db/type :db.type/ref}
                :ref/commit {:db/type :db.type/ref}})

(defn tree-body [tree]
  (jgit/get-tree (-> tree :tree/repo :repo/jgit) (:tree/sha tree)))

(defn tree-key [_ {{:keys [sha]} :path-params
                   repo :repo}]
  [:tree/repo+sha [(:db/id repo) sha]])

(defn tree-post [_ {{jgit-repo :repo/jgit :as repo} :repo
                    body :body}]
  {:tree/repo (:db/id repo)
   :tree/sha (jgit/create-tree! jgit-repo body)})

(def tree-resource
  {:body-fn tree-body
   :lookup-fn (handlers/db-lookup-fn tree-key)
   :post-fn (handlers/db-transact-fn tree-post)
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

(defn commit-key [_ {{:keys [sha]} :path-params
                     repo :repo}]
  [:commit/repo+sha [(:db/id repo) sha]])

(defn commit-post [_ {{jgit-repo :repo/jgit :as repo} :repo
                      body :body}]
  {:commit/repo (:db/id repo)
   :commit/sha (jgit/create-commit! jgit-repo body)})

(def commit-resource
  {:body-fn commit-body
   :lookup-fn (handlers/db-lookup-fn commit-key)
   :post-fn (handlers/db-transact-fn commit-post)
   :post-schema [:map
                 [:path-params [:map
                                [:org :string]
                                [:repo :string]]]
                 [:body [:map
                         [:message :string]
                         [:tree :string]
                         [:parents {:optional true} [:vector :string]]]]]})

(defn ref-key [db {{:keys [org repo ref]} :path-params}]
  [:ref/repo+ref [(d/entid db [:repo/org+name [(d/entid db [:org/name org]) repo]]) (str "refs/" ref)]])

(defn ref-body [ref]
  {:ref (:ref/ref ref)
   :object {:type :commit
            :sha (-> ref :ref/commit :commit/sha)}})

(defn ref-post [db {:keys [repo body]}]
  {:ref/repo (:db/id repo)
   :ref/ref (:ref body)
   :ref/commit (d/entid db [:commit/repo+sha [(:db/id repo) (:sha body)]])})

(defn ref-patch [db {:keys [repo body]
                     {:keys [ref]} :path-params}]
  (let [key [(:db/id repo) (str "refs/" ref)]
        current-sha (-> (d/entity db [:ref/repo+ref key]) :ref/commit :commit/sha)
        new-sha (:sha body)
        base (jgit/merge-base (:repo/jgit repo) current-sha new-sha)]
    (if (or (:force body) (= current-sha base))
      {:ref/repo+ref key
       :ref/commit (d/entid db [:commit/repo+sha [(:db/id repo) (:sha body)]])}
      (throw (ex-info "not fast forward" {})))))

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
