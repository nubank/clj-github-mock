(ns clj-github-mock.resource.git-database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.resource.generators :as resource-gen]
            [clojure.test.check.generators :as gen]))

(def object-resource
  {:resource/name :object
   :resource/abstract? true
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
    {:attribute/name :type
     :attribute/internal? true
     :attribute/schema [:enum "blob" "commit" "tree"]}
    {:attribute/name :repo+type+sha
     :attribute/tuppleAttrs [:object/repo :object/type :object/sha]
     :attribute/unique :db.unique/identity
     :attribute/internal? true}]})

(def blob-resource
  {:resource/name :blob
   :resource/extends {:resource/name :object}
   :resource/attributes
   [{:attribute/name :utf-8-content
     :attribute/internal? true}]})

(defn tree-item-sha [tree-item]
  (-> tree-item :tree-item/object :object/sha))

(defn tree-item-type [tree-item]
  (-> tree-item :tree-item/object :object/type))

(def tree-item-resource
  {:resource/name :tree-item
   :resource/attributes
   [{:attribute/name :path}
    {:attribute/name :mode}
    {:attribute/name :object
     :attribute/internal? true
     :attribute/ref {:resource/name :object}}
    {:attribute/name :sha
     :attribute/formula tree-item-sha}
    {:attribute/name :type
     :attribute/formula tree-item-type}]})

(defn tree-gen [tree]
  (gen/let [content resource-gen/github-tree]
    (assoc tree :tree/content content)))

(defn tree-transact [db tree]
  [(merge
    (dissoc tree :tree/content)
    (jgit/create-tree-datoms! (d/entity db (:object/repo tree)) {:tree (:tree/content tree)}))])

(def tree-resource
  {:resource/name :tree
   :resource/extends {:resource/name :object}
   :specmonstah/bind-gen-fn tree-gen
   :specmonstah/transact-fn tree-transact
   :resource/attributes
   [{:attribute/name :tree
     :attribute/ref {:resource/name :tree-item}
     :attribute/cardinality :db.cardinality/many}]})

(defn tree-key [db {{:keys [owner repo sha]} :path-params}]
  [:object/repo+type+sha [(d/entid db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]]) "tree" sha]])

(defn tree-post [db {{:keys [owner repo]} :path-params
                     body :body}]
  (jgit/create-tree-datoms!
   (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])
   body))

(defn commit-transact [db commit]
  (let [tree (d/entity db (:commit/tree commit))]
    [(merge
      commit
      (jgit/create-commit-datoms!
       (d/entity db (:object/repo commit))
       (:db/id tree) 
       {:tree (:object/sha tree)
        :message (:commit/message commit)}))]))

(def commit-resource
  {:resource/name :commit
   :resource/extends {:resource/name :object}
   :specmonstah/transact-fn commit-transact
   :resource/attributes
   [{:attribute/name :tree
     :attribute/ref {:resource/name :tree}}
    {:attribute/name :message
     :attribute/schema :string}
    {:attribute/name :parents
     :attribute/ref {:resource/name :commit}
     :attribute/cardinality :db.cardinality/many}]})

(defn commit-key [db {{:keys [owner repo sha]} :path-params}]
  [:object/repo+type+sha [(d/entid db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]]) "commit" sha]])

(defn commit-post [db {{:keys [owner repo]} :path-params {:keys [tree] :as body} :body}]
  (let [repo (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])]
    (jgit/create-commit-datoms!
     repo 
     (d/entid db [:object/repo+type+sha [(:db/id repo) "tree" tree]])
     body)))

(defn ref-full-name [{:ref/keys [type name]}]
  (format
   "refs/%s/%s"
   (case type
     :branch "heads"
     :tag "tags")
   name))

(defn ref-object [{:ref/keys [commit]}]
  {:type :commit
   :sha (:object/sha commit)})

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
     :commit (d/entid db [:object/repo+type+sha [repo-id "commit" (:sha body)]])}))

(defn ref-patch [db {{:keys [owner repo ref]} :path-params body :body}]
  (let [{:keys [type name]} (ref-parts (str "refs/" ref))
        {repo-id :db/id jgit-repo :repo/jgit} (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])
        key [repo-id type name]
        current-sha (-> (d/entity db [:ref/repo+type+name key]) :ref/commit :object/sha)
        new-sha (:sha body)
        base (jgit/merge-base jgit-repo current-sha new-sha)]
    (if (or (:force body) (= current-sha base))
      {:repo+type+name key
       :commit (d/entid db [:object/repo+type+sha [repo-id "commit" (:sha body)]])}
      (throw (ex-info "not fast forward" {})))))

(defn routes [meta-db]
  (let [tree-resource (d/entity meta-db [:resource/name :tree])
        commit-resource (d/entity meta-db [:resource/name :commit])]
    [["/repos/:owner/:repo"
      ["/git/trees" (handlers/post-handler {:post-fn (handlers/db-transact-fn tree-post)
                                            :post-schema (handlers/post-schema tree-resource)
                                            :body-fn (handlers/resource-body-fn tree-resource)})]
      ["/git/trees/:sha" {:get (handlers/get-resource-handler meta-db :tree tree-key)}]
      ["/git/commits" {:post (handlers/post-handler {:post-fn (handlers/db-transact-fn commit-post)
                                                     :post-schema (handlers/post-schema commit-resource)
                                                     :body-fn (handlers/resource-body-fn commit-resource)}) }]
      ["/git/commits/:sha" {:get (handlers/get-resource-handler meta-db :commit commit-key)}]
      ["/git/refs" {:post (handlers/post-resource-handler meta-db :ref ref-post)}]
      ["/git/refs/*ref" {:patch (handlers/patch-resource-handler meta-db :ref ref-key ref-patch)
                         :delete (handlers/delete-resource-handler meta-db :ref ref-key)}]
      ["/git/ref/*ref" {:get (handlers/get-resource-handler meta-db :ref ref-key)}]]]))
