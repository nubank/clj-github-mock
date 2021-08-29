(ns clj-github-mock.resource.repo
  (:require [medley.core :as m]
            [clj-github-mock.impl.jgit :as jgit]
            [clojure.string :as string]
            [datascript.core :as d]))

(def repo-defaults {:default_branch "main"})

(def resource
  {:resource/name :repo
   :resource/db-schema {:repo/name+org {:db/tupleAttrs [:repo/name :repo/org]
                                        :db/type :db.type/tuple
                                        :db/unique :db.unique/identity}
                        :repo/org {:db/type :db.type/ref}}
   :resource/lookup-fn (fn [db {{:keys [org repo]} :path-params}]
                         [:repo/name+org [repo (d/entid db [:org/name org])]])
   :resource/body-fn (fn [_ _ repo]
                       (merge
                        {:name (:repo/name repo)
                         :full_name (string/join "/" [(-> repo :repo/org :org/name) (:repo/name repo)])}
                        (:repo/attrs repo)))
   :resource/post-schema [:map
                          [:body [:map
                                  [:name :string]]]]
   :resource/post-fn (fn [_ {{:keys [org]} :path-params
                             body :body}]
                       {:repo/name (:name body)
                        :repo/org [:org/name org]
                        :repo/attrs (merge repo-defaults (m/remove-keys #{:name} body))
                        :repo/jgit (jgit/empty-repo)})
   :resource/patch-fn (fn [db {{:keys [org repo]} :path-params
                               body :body}]
                        (let [key [repo (d/entid db [:org/name org])]]
                          {:repo/name+org key
                           :repo/attrs (merge (-> (d/entity db [:repo/name+org key]) :repo/attrs)
                                              body)}))
   :resource/list-fn (fn [db {{:keys [org]} :path-params}]
                       (->> (d/q '[:find [?r ...]
                                   :in $ ?org
                                   :where
                                   [?o :org/name ?org]
                                   [?r :repo/org ?o]] db org)
                            (map #(d/entity db %))))})
