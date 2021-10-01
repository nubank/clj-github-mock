(ns clj-github-mock.resource.repo
  (:require [clojure.string :as string]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]
            [clj-github-mock.impl.jgit :as jgit]
            [base64-clj.core :as base64]
            [clj-github-mock.resource.generators :as resource-gen]
            [medley.core :as m]))

(defn full-name [repo]
  (string/join "/" [(-> repo :repo/owner :owner/name) (:repo/name repo)]))

(def repo-resource
  {:resource/name :repo
   :resource/attributes
   [{:attribute/name :id
     :attribute/schema :int
     :attribute/unique :db.unique/identity
     :attribute/auto-gen? true}
    {:attribute/name :node_id
     :attribute/schema :uuid
     :attribute/unique :db.unique/identity
     :attribute/auto-gen? true
     :specmonstah/key? true}
    {:attribute/name :name
     :attribute/schema [:string {:gen/gen (resource-gen/unique-object-name)}]
     :attribute/required? true}
    {:attribute/name :full_name
     :attribute/formula full-name}
    {:attribute/name :owner
     :attribute/ref {:resource/name :owner}}
    {:attribute/name :owner+name
     :attribute/internal? true
     :attribute/unique :db.unique/identity
     :attribute/tuppleAttrs [:repo/owner :repo/name]}
    {:attribute/name :private
     :attribute/default false}
    {:attribute/name :default_branch
     :attribute/default "main"}
    {:attribute/name :jgit
     :attribute/default #(jgit/empty-repo)
     :attribute/internal? true}]})

(defn repo-key [db {{:keys [owner repo]} :path-params}]
  [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])

(defn repo-post [db {{:keys [org]} :path-params body :body}]
  (assoc body :owner (d/entid db [:owner/name org])))

(defn repo-patch [db {{:keys [owner repo]} :path-params body :body}]
  (assoc body :owner+name [(d/entid db [:owner/name owner]) repo]))

(defn repo-list [db {{:keys [org]} :path-params}]
  (->> (d/q '[:find [?r ...]
              :in $ ?org
              :where
              [?o :owner/name ?org]
              [?r :repo/owner ?o]] db org)
       (map #(d/entity db %))))

(defn- content->tree [content]
  (mapv
   (fn [[path file-content]]
     {:path path :mode "100644" :type "blob" :content file-content})
   content))

(defn- create-commit [repo content]
  (let [tree (jgit/create-tree-datoms! repo {:tree (content->tree content)})]
    (jgit/create-commit-datoms!
     repo
     tree
     {:tree (:object/sha tree)
      :message "initial commit"})))

(defn branch-transact [db {:branch/keys [name repo content base] :as branch}]
  [(-> {:db/id (:db/id branch)
        :ref/name name
        :ref/type :branch
        :ref/repo repo}
       (m/assoc-some :ref/commit (when-not base
                                   (create-commit (d/entity db repo) content))))])

(defn branch-update [db {:branch/keys [name repo content base] :as branch}]
  (if base
    [{:ref/repo+type+name [(d/entid db repo) :branch name]
      :ref/commit (-> (d/entity db [:ref/repo+type+name [(d/entid db repo) :branch base]]) :ref/commit :db/id)}]
    []))

; TODO support branch generation (ie. add schemas to name and context so user is not required to set those)
(def branch-resource
  {:resource/name :branch
   :specmonstah/transact-fn branch-transact
   :specmonstah/update-fn branch-update
   :resource/attributes
   [{:attribute/name :name}
    {:attribute/name :repo
     :attribute/ref {:resource/name :repo}}
    {:attribute/name :content}]})

(defn branch-key [db {{:keys [owner repo branch]} :path-params}]
  [:ref/repo+type+name [(d/entid db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]]) :branch branch]])

(defn branch-body [ref]
  {:name (:ref/name ref)
   :commit {:sha (-> ref :ref/commit :object/sha)
            :commit (-> (jgit/get-commit (:repo/jgit (:ref/repo ref)) (-> ref :ref/commit :object/sha))
                        (dissoc :sha))}})

(defn- sha? [ref]
  (and ref
       (re-find #"^[A-Fa-f0-9]{40}$" ref)))

(defn- content-sha [db {repo-id :db/id default_branch :repo/default_branch} ref]
  (if (sha? ref)
    ref
    (let [branch (or ref default_branch)]
      (:object/sha (:ref/commit (d/entity db [:ref/repo+type+name [repo-id :branch branch]]))))))

(defn content-lookup [{{:keys [owner repo path]} :path-params
                       {:strs [ref]} :query-params
                       conn :conn}]
  (let [db @conn]
    (when-let [{git-repo :repo/jgit :as repo} (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])]
      (when-let [sha (content-sha db repo ref)]
        (when (jgit/path-exists? git-repo sha path)
          {:repo repo
           :sha  sha
           :path path})))))

(defn content-body [{{git-repo :repo/jgit} :repo :keys [path sha]}]
  {:type "file"
   :path path
   :content (base64/encode
             (jgit/get-content git-repo sha path)
             "UTF-8")})

(defn commit-body [commit]
  {:sha (:object/sha commit)
   :message (:commit/message commit)})

(defn content-put [db {{:keys [owner repo path]} :path-params
                        {:keys [content message branch]} :body}]
  (let [repo' (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])
        branch' (d/entity db [:ref/repo+type+name [(:db/id repo') :branch (or branch (:repo/default_branch repo'))]])
        tree (jgit/create-tree-datoms! repo' {:tree [{:path path :type "blob" :mode "100644" :content (base64/decode content "UTF-8")}]
                                              :base_tree (-> branch' :ref/commit :commit/tree :object/sha)})
        commit (jgit/create-commit-datoms! repo' tree {:message message
                                                       :tree (:object/sha tree)
                                                       :parents [(-> branch' :ref/commit :object/sha)]})]
    {:ref/repo+type+name [(:db/id repo') :branch (or branch (:repo/default_branch repo'))]
     :ref/commit commit}))

;TODO fix content sha (should be sha of the blob and not commit)
(defn content-put-body [ref {{:keys [owner repo path]} :path-params conn :conn}]
  (let [db @conn
        repo' (d/entity db [:repo/owner+name [(d/entid db [:owner/name owner]) repo]])]
    {:content (content-body {:repo repo' :path path :sha (-> ref :ref/commit :object/sha)})
     :commit (commit-body (:ref/commit ref))}))

(defn routes [meta-db]
  [["/orgs/:org/repos" {:get (handlers/list-resource-handler meta-db :repo repo-list)
                        :post (handlers/post-resource-handler meta-db :repo repo-post)}]
   ["/repos/:owner/:repo"
    ["" {:get (handlers/get-resource-handler meta-db :repo repo-key)
         :patch (handlers/patch-resource-handler meta-db :repo repo-key repo-patch)}]
    ["/branches/:branch" {:get (handlers/get-handler {:lookup-fn (handlers/db-lookup-fn branch-key)
                                                      :body-fn branch-body})}]
    ["/contents/*path" {:get (handlers/get-handler {:lookup-fn content-lookup
                                                    :body-fn content-body})
                        :put (handlers/put-handler {:put-fn (handlers/db-transact-fn content-put)
                                                     :body-fn content-put-body})}]]])
