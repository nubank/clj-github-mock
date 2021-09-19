(ns clj-github-mock.resource.git-database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.resource.generators :as resource-gen]
            [clojure.test.check.generators :as gen]))

(defn tree-items [{:tree/keys [repo sha]}]
  (:tree (jgit/get-tree (:repo/jgit repo) sha)))

(defn tree-gen [tree]
  (gen/let [content resource-gen/github-tree]
    (assoc tree :tree/content content)))

(defn tree-transact [db tree]
  (let [git-repo (-> (d/entity db (:tree/repo tree)) :repo/jgit)
        sha (jgit/create-tree! git-repo {:tree (:tree/content tree)})]
    [(-> tree
         (assoc :tree/sha sha)
         (dissoc :tree/content))]))

(def tree-resource
  {:resource/name :tree
   :specmonstah/bind-gen-fn tree-gen
   :specmonstah/transact-fn tree-transact
   :resource/attributes
   [{:attribute/name :id
     :attribute/schema :uuid
     :attribute/unique :db.unique/identity
     :attribute/auto-gen? true
     :attribute/internal? true
     :specmonstah/key? true}
    {:attribute/name :repo
     :attribute/ref {:resource/name :repo}
     :attribute/internal? true}
    {:attribute/name :sha}
    {:attribute/name :repo+sha
     :attribute/tuppleAttrs [:tree/repo :tree/sha]
     :attribute/unique :db.unique/identity
     :attribute/internal? true}
    {:attribute/name :tree
     :attribute/formula tree-items}]})

(defn tree-key [db {{:keys [owner repo sha]} :path-params}]
  [:tree/repo+sha [(d/entid db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]]) sha]])

(defn tree-post [db {{:keys [owner repo]} :path-params
                     body :body}]
  (let [{repo-id :db/id jgit-repo :repo/jgit} (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])]
    {:repo repo-id
     :sha (jgit/create-tree! jgit-repo body)}))

(defn commit-message [{:commit/keys [repo sha]}]
  (:message (jgit/get-commit (:repo/jgit repo) sha)))

(defn commit-transact [db commit]
  (let [git-repo (-> (d/entity db (:commit/repo commit)) :repo/jgit)
        tree (d/entity db (:commit/tree commit))
        sha (jgit/create-commit! git-repo {:tree (:tree/sha tree)
                                           :message (:commit/message commit)})]
    [(assoc commit :commit/sha sha)]))

(defn commit-gen [commit]
  (gen/let [message gen/string]
    (assoc commit :commit/message message)))

(def commit-resource
  {:resource/name :commit
   :specmonstah/bind-gen-fn commit-gen
   :specmonstah/transact-fn commit-transact
   :resource/attributes
   [{:attribute/namespace :object
     :attribute/name :node_id
     :attribute/schema :uuid
     :attribute/unique :db.unique/identity
     :attribute/auto-gen? true
     :specmonstah/key? true}
    {:attribute/name :repo
     :attribute/ref {:resource/name :repo}
     :attribute/internal? true}
    {:attribute/name :sha}
    {:attribute/name :repo+sha
     :attribute/tuppleAttrs [:commit/repo :commit/sha]
     :attribute/unique :db.unique/identity
     :attribute/internal? true}
    {:attribute/name :tree
     :attribute/ref {:resource/name :tree}}
    {:attribute/name :message
     :attribute/formula commit-message}
    {:attribute/name :parents
     :attribute/ref {:resource/name :commit}
     :attribute/cardinality :db.cardinality/many}]})

(defn commit-key [db {{:keys [owner repo sha]} :path-params}]
  [:commit/repo+sha [(d/entid db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]]) sha]])

(defn commit-post [db {{:keys [owner repo]} :path-params body :body}]
  (let [{repo-id :db/id jgit-repo :repo/jgit} (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])]
    {:repo repo-id
     :sha (jgit/create-commit! jgit-repo body)
     :tree (d/entid db [:tree/repo+sha [repo-id (:tree body)]])
     :parents (map #(d/entid db [:commit/repo+sha [repo-id %]]) (:parents body))}))

(defn ref-full-name [{:ref/keys [type name]}]
  (format
   "refs/%s/%s"
   (case type
     :branch "heads"
     :tag "tags")
   name))

(defn ref-object [{:ref/keys [commit]}]
  {:type :commit
   :sha (:commit/sha commit)})

(def ref-resource
  {:resource/name :ref
   :resource/attributes
   [{:attribute/name :node_id
     :attribute/schema :uuid
     :attribute/unique :db.unique/identity
     :attribute/auto-gen? true
     :specmonstah/key? true}
    {:attribute/name :repo
     :attribute/ref {:resource/name :repo}
     :attribute/internal? true}
    {:attribute/name :name
     :attribute/schema [:string {:gen/gen (resource-gen/unique-object-name)}]}
    {:attribute/name :type
     :attribute/schema [:enum :branch :tag]
     :attribute/internal? true}
    {:attribute/name :repo+type+name
     :attribute/tuppleAttrs [:ref/repo :ref/type :ref/name]
     :attribute/unique :db.unique/identity
     :attribute/internal? true}
    {:attribute/name :commit
     :attribute/ref {:resource/name :commit}
     :attribute/internal? true}
    {:attribute/name :ref
     :attribute/formula ref-full-name}
    {:attribute/name :object
     :attribute/formula ref-object}]})

(defn ref-parts [ref-full-name]
  (let [[_ type-name name] (re-find #"refs/(.*)/(.*)" ref-full-name)]
    {:name name
     :type (case type-name
             "heads" :branch
             "tags" :tag)}))

(defn ref-key [db {{:keys [owner repo ref]} :path-params}]
  (let [{:keys [type name]} (ref-parts (str "refs/" ref))]
    [:ref/repo+type+name [(d/entid db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])
                          type
                          name]]))

(defn ref-post [db {{:keys [owner repo]} :path-params body :body}]
  (let [{repo-id :db/id} (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])
        {:keys [name type]} (ref-parts (:ref body))]
    {:repo repo-id
     :name name
     :type type
     :commit (d/entid db [:commit/repo+sha [repo-id (:sha body)]])}))

(defn ref-patch [db {{:keys [owner repo ref]} :path-params body :body}]
  (let [{:keys [type name]} (ref-parts (str "refs/" ref))
        {repo-id :db/id jgit-repo :repo/jgit} (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])
        key [repo-id type name]
        current-sha (-> (d/entity db [:ref/repo+type+name key]) :ref/commit :commit/sha)
        new-sha (:sha body)
        base (jgit/merge-base jgit-repo current-sha new-sha)]
    (if (or (:force body) (= current-sha base))
      {:repo+type+name key
       :commit (d/entid db [:commit/repo+sha [repo-id (:sha body)]])}
      (throw (ex-info "not fast forward" {})))))

(defn routes [meta-db]
  [["/repos/:owner/:repo"
    ["/git/trees" {:post (handlers/post-resource-handler meta-db :tree tree-post)}]
    ["/git/trees/:sha" {:get (handlers/get-resource-handler meta-db :tree tree-key)}]
    ["/git/commits" {:post (handlers/post-resource-handler meta-db :commit commit-post)}]
    ["/git/commits/:sha" {:get (handlers/get-resource-handler meta-db :commit commit-key)}]
    ["/git/refs" {:post (handlers/post-resource-handler meta-db :ref ref-post)}]
    ["/git/refs/*ref" {:patch (handlers/patch-resource-handler meta-db :ref ref-key ref-patch)
                       :delete (handlers/delete-resource-handler meta-db :ref ref-key)}]
    ["/git/ref/*ref" {:get (handlers/get-resource-handler meta-db :ref ref-key)}]]])
