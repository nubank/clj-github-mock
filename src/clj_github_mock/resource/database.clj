(ns clj-github-mock.resource.database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clj-github-mock.db :as db]))

(def tree-resource
  {:resource/name :tree
   :resource/db-schema {:tree/repo+sha {:db/tupleAttrs [:tree/repo :tree/sha]
                                        :db/unique :db.unique/identity}
                        :tree/repo {:db/type :db.type/ref}}
   :resource/body-fn (fn [meta-db _ tree]
                       (jgit/get-tree (db/jgit-repo meta-db) (:tree/sha tree)))
   :resource/lookup-fn (fn [db {{:keys [org repo sha]} :path-params}]
                         [:tree/repo+sha [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) sha]])
   :resource/post-fn (fn [db {{:keys [org repo]} :path-params
                              meta-db :meta-db
                              body :body}]
                       {:tree/repo [:repo/name+org [repo (d/entid db [:org/name org])]]
                        :tree/sha (:sha (jgit/create-tree! (db/jgit-repo meta-db) body))})
   :resource/post-schema [:map
                          [:body [:map
                                  [:tree [:vector
                                          [:map
                                           [:path :string]
                                           [:mode [:enum "100644" "100755" "040000"]]
                                           [:type [:enum "blob" "tree"]]
                                           [:sha {:optional true} :string]
                                           [:content {:optional true} :string]]]]
                                  [:base_tree {:optional true} :string]]]]})
