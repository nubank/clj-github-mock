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

(def commit-resource
  {:resource/name :commit
   :resource/db-schema {:commit/repo+sha {:db/tupleAttrs [:commit/repo :commit/sha]
                                          :db/unique :db.unique/identity}
                        :commit/repo {:db/type :db.type/ref}}
   :resource/body-fn (fn [meta-db _ commit]
                       (jgit/get-commit (db/jgit-repo meta-db) (:commit/sha commit)))
   :resource/lookup-fn (fn [db {{:keys [org repo sha]} :path-params}]
                         [:commit/repo+sha [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) sha]])
   :resource/post-fn (fn [db {{:keys [org repo]} :path-params
                              meta-db :meta-db
                              body :body}]
                       {:commit/repo [:repo/name+org [repo (d/entid db [:org/name org])]]
                        :commit/sha (:sha (jgit/create-commit! (db/jgit-repo meta-db) body))})
   :resource/post-schema [:map
                          [:path-params [:map
                                         [:org :string]
                                         [:repo :string]]]
                          [:body [:map
                                  [:message :string]
                                  [:tree :string]
                                  [:parents {:optional true} [:vector :string]]]]]})

; TODO enforce update using jgit
(def ref-resource
  {:resource/name :ref
   :resource/db-schema {:ref/repo+ref {:db/tupleAttrs [:ref/repo :ref/ref]
                                       :db/unique :db.unique/identity}
                        :ref/repo {:db/type :db.type/ref}}
   :resource/body-fn (fn [_ _ ref]
                       {:ref (:ref/ref ref)
                        :object {:type :commit
                                 :sha (:ref/sha ref)}})
   :resource/lookup-fn (fn [db {{:keys [org repo ref]} :path-params}]
                         [:ref/repo+ref [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) (str "refs/" ref)]])
   :resource/post-fn (fn [db {{:keys [org repo]} :path-params
                              body :body}]
                       {:ref/repo [:repo/name+org [repo (d/entid db [:org/name org])]]
                        :ref/ref (:ref body)
                        :ref/sha (:sha body)})
   :resource/post-schema [:map
                          [:path-params [:map
                                         [:org :string]
                                         [:repo :string]]]
                          [:body [:map
                                  [:ref :string]
                                  [:sha :string]]]]
   :resource/patch-fn (fn [db {{:keys [org repo ref]} :path-params
                               body :body}]
                        {:ref/repo+ref [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) (str "refs/" ref)]
                         :ref/sha (:sha body)})
   :resource/patch-schema [:map
                           [:path-params [:map
                                          [:org :string]
                                          [:repo :string]
                                          [:ref :string]]]
                           [:body [:map
                                   [:sha :string]]]]})
