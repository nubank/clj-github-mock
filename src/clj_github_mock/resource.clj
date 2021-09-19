(ns clj-github-mock.resource
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.resource.org :as org]
            [clj-github-mock.resource.git-database :as git-database]
            [clj-github-mock.handlers :as handlers]
            [ring.middleware.params :as middleware.params]
            [reitit.ring :as ring]
            [datascript.core :as d]
            [ring.middleware.json :as middleware.json]
            [medley.core :as m]))

(def meta-db-schema
  {:resource/name {:db/unique :db.unique/identity
                   :db/doc "The name of the resource"}
   :resource/attributes {:db/type :db.type/ref
                         :db/cardinality :db.cardinality/many
                         :db/doc "The attributes of the resource"}
   :attribute/name {:db/doc "The name of the attribute"}
   :attribute/schema {:db/doc "A malli schema that will be used to validate the attribute"}
   :attribute/unique {:db/doc "Either :identity or :value as in datomic"}
   :attribute/auto-gen? {:db/doc "`true` if value will be auto generated, the schema will be used to generate the value and uniquenes is taken into account"}
   :attribute/required? {:db/doc "`true` if attribute is required"}
   :attribute/formula {:db/doc "A function that receives the entity as parameter and calculates the value of the attribute"}
   :attribute/ref {:db/type :db.type/ref
                   :db/doc "The name of the resource to which the attribute refers"}
   :attribute/internal? {:db/doc "`true` if the attribute is only for internal use (i.e. will not show up in responses)"}
   :attribute/tuppleAttrs {:db/doc "As in datomic"}
   :attribute/default {:db/doc "The default value of the attribute, can either be a value or a no-arg function whose return will be the default value"}
   :attribute/cardinality {:db/doc "Cardinality of the attribute as in datomic. Defaults to :db.cardinality/one"}})

(defn meta-db []
  (-> (d/empty-db meta-db-schema)
      (d/db-with [org/owner-resource
                  repo/repo-resource
                  git-database/tree-resource
                  git-database/commit-resource
                  git-database/ref-resource])))

(defn resources [meta-db]
  (->> (d/q '[:find [?e ...]
              :where
              [?e :resource/name]]
            meta-db)
       (map (partial d/entity meta-db))))

(defn attribute-schema [{:attribute/keys [ref tuppleAttrs unique cardinality]}]
  (-> {}
      (m/assoc-some :db/type (cond ref :db.type/ref tuppleAttrs :db.type/tuple))
      (m/assoc-some :db/tupleAttrs tuppleAttrs)
      (m/assoc-some :db/unique unique)
      (m/assoc-some :db/cardinality cardinality)))

(defn resource-db-schema [resource]
  (reduce
   (fn [result attribute]
     (assoc result
            (handlers/attribute-full-name resource attribute)
            (attribute-schema attribute)))
   {}
   (:resource/attributes resource)))

(defn db-schema [meta-db]
  (apply merge (map resource-db-schema (resources meta-db))))

(defn- repo-datoms [org-name {:keys [name default_branch] :or {default_branch "main"}}]
  [{:repo/name name
    :repo/owner {:owner/name org-name}
    :repo/attrs {:default_branch default_branch}}])

(defn- org-datoms [{:keys [name repos]}]
  (into [{:owner/name name}]
        (mapcat (partial repo-datoms name) repos)))

(defn- datoms [{:keys [orgs]}]
  (mapcat org-datoms orgs))

(defn conn [meta-db initial-state]
  (let [result (d/create-conn (db-schema meta-db))]
    (d/transact! result (datoms initial-state))
    result))

(defn conn-middleware [handler conn]
  (fn [request]
    (handler (assoc request :conn conn))))

(defn handler [meta-db conn]
  (-> (ring/ring-handler
       (ring/router (concat
                     (repo/routes meta-db)
                     (git-database/routes meta-db)))
       (ring/create-default-handler))
      (middleware.params/wrap-params)
      (conn-middleware conn)))

(defn json-handler [meta-db conn]
  (-> (handler meta-db conn)
      (middleware.json/wrap-json-body {:keywords? true})
      (middleware.json/wrap-json-response)))
