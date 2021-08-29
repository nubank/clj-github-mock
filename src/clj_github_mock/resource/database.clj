(ns clj-github-mock.resource.database
  (:require [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clj-github-mock.handlers :as handlers]))

(def tree-resource
  {:resource/name :tree
   :resource/body-fn (fn [_ tree]
                       (jgit/get-tree (-> tree :tree/repo :repo/jgit) (:tree/sha tree)))
   :resource/lookup-fn (fn [_ {{:keys [sha]} :path-params
                               {jgit-repo :repo/jgit :as repo} :repo}]
                         (when (jgit/get-tree jgit-repo sha)
                           {:tree/repo repo
                            :tree/sha sha}))
   :resource/post-fn (fn [_ {{jgit-repo :repo/jgit :as repo} :repo
                             body :body}]
                       {:tree/repo repo
                        :tree/sha (:sha (jgit/create-tree! jgit-repo body))})
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
   :resource/body-fn (fn [_ commit]
                       (jgit/get-commit (-> commit :commit/repo :repo/jgit) (:commit/sha commit)))
   :resource/lookup-fn (fn [_ {{:keys [sha]} :path-params
                               {jgit-repo :repo/jgit :as repo} :repo}]
                         (when (jgit/get-commit jgit-repo sha)
                           {:commit/repo repo
                            :commit/sha sha}))
   :resource/post-fn (fn [_ {{jgit-repo :repo/jgit :as repo} :repo
                             body :body}]
                       {:commit/repo repo
                        :commit/sha (:sha (jgit/create-commit! jgit-repo body))})
   :resource/post-schema [:map
                          [:path-params [:map
                                         [:org :string]
                                         [:repo :string]]]
                          [:body [:map
                                  [:message :string]
                                  [:tree :string]
                                  [:parents {:optional true} [:vector :string]]]]]})

(defn ref-key [db {{:keys [org repo ref]} :path-params}]
  [:ref/repo+ref [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) (str "refs/" ref)]])

; TODO enforce update using jgit
(def ref-resource
  {:resource/name :ref
   :resource/db-schema {:ref/repo+ref {:db/tupleAttrs [:ref/repo :ref/ref]
                                       :db/unique :db.unique/identity}
                        :ref/repo {:db/type :db.type/ref}}
   :resource/body-fn (fn [_ ref]
                       {:ref (:ref/ref ref)
                        :object {:type :commit
                                 :sha (:ref/sha ref)}})
   :resource/lookup-fn (handlers/db-lookup-fn ref-key)
   :resource/post-fn (handlers/db-transact-fn (fn [db {{:keys [org repo]} :path-params
                                                       body :body}]
                                                {:ref/repo [:repo/name+org [repo (d/entid db [:org/name org])]]
                                                 :ref/ref (:ref body)
                                                 :ref/sha (:sha body)}))
   :resource/post-schema [:map
                          [:path-params [:map
                                         [:org :string]
                                         [:repo :string]]]
                          [:body [:map
                                  [:ref :string]
                                  [:sha :string]]]]
   :resource/patch-fn (handlers/db-transact-fn (fn [db {{:keys [org repo ref]} :path-params
                                                        body :body}]
                                                 {:ref/repo+ref [(d/entid db [:repo/name+org [repo (d/entid db [:org/name org])]]) (str "refs/" ref)]
                                                  :ref/sha (:sha body)}))
   :resource/patch-schema [:map
                           [:path-params [:map
                                          [:org :string]
                                          [:repo :string]
                                          [:ref :string]]]
                           [:body [:map
                                   [:sha :string]]]]
   :resource/delete-fn (handlers/db-delete-fn ref-key)})
