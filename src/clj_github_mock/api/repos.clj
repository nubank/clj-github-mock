(ns clj-github-mock.api.repos
  (:require [medley.core :as m]
            [clj-github-mock.impl.jgit :as jgit]
            [clojure.string :as string]
            [datascript.core :as d]))

(def repo-defaults {:default_branch "main"})

(def model
  [{:entity/name :org
    :entity/schema {:org/name {:db/unique :db.unique/identity}}}
   {:entity/name :repo
    :entity/schema {:repo/name+org {:db/tupleAttrs [:repo/name :repo/org]
                                    :db/type :db.type/tuple
                                    :db/unique :db.unique/identity}
                    :repo/org {:db/type :db.type/ref}}
    :entity/lookup-fn (fn [db {{:keys [org repo]} :path-params}]
                        [:repo/name+org [repo (d/entid db [:org/name org])]])
    :entity/body-fn (fn [_ _ repo]
                      (merge
                       {:name (:repo/name repo)
                        :full_name (string/join "/" [(-> repo :repo/org :org/name) (:repo/name repo)])}
                       (:repo/attrs repo)))
    :entity/post-schema [:map
                         [:body [:map
                                 [:name :string]]]]
    :entity/post-fn (fn [_ {{:keys [org]} :path-params
                            body :body}]
                      {:repo/name (:name body)
                       :repo/org [:org/name org]
                       :repo/attrs (merge repo-defaults (m/remove-keys #{:name} body))
                       :repo/jgit (jgit/empty-repo)})}])
