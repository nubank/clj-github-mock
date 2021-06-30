(ns clj-github-mock.generators
  (:require [reifyhealth.specmonstah.core :as sm]
            [reifyhealth.specmonstah.spec-gen :as sg]
            [malli.generator :as mg]
            [clj-github-mock.impl.database :as database]
            [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clj-github-mock.handlers.repos :as repos]))

(defn unique-name-gen []
  (let [names (atom #{})]
    (gen/such-that
     #(let [result (@names %)]
        (swap! names conj %)
        (not result))
     (gen/not-empty gen/string-alphanumeric))))

(defn schema []
  {:org {:prefix :o
         :datastore :datascript
         :malli-schema [:map
                        [:org/name [:string {:gen/gen (unique-name-gen)}]]]}
   :repo {:prefix :r
          :datastore :datascript
          :malli-schema [:map
                         [:repo/id :uuid]
                         [:repo/name [:string {:gen/gen (unique-name-gen)}]]
                         [:repo/attrs [:map
                                       [:default_branch [:= "main"]]]]
                         [:repo/jgit [:any {:gen/fmap (fn [_] (jgit/empty-repo))}]]]
          :relations {:repo/org [:org :org/name]}}
   :blob {:prefix :b
          :datastore :jgit
          :malli-schema [:map
                         [:content :string]]
          :relations {:blob/repo [:repo :repo/id]}}
   })

(defn malli-gen-ent-val
  [{{:keys [rnd size]} :gen-options :as ent-db} {:keys [ent-name]}]
  (let [{:keys [malli-schema]} (sm/ent-schema ent-db ent-name)
        generator (mg/generator malli-schema)]
    (rose/root (gen/call-gen generator rnd size))))

(defn foreign-key-ent [[_ foreign-key-attr :as path] foreign-key-val]
  (cond
    ; TODO: use constraints to detect if it is a multi relationship
    (vector? foreign-key-val) (into #{} (map (partial foreign-key-ent path) foreign-key-val))
    :else {foreign-key-attr foreign-key-val}))

(defn assoc-ent-at-foreign-keys [db {:keys [ent-type malli-gen]}]
  (reduce
   (fn [acc [attr relation-path]]
     (update acc attr (partial foreign-key-ent relation-path)))
   malli-gen
   (-> db :schema ent-type :relations)))

(def malli-gen [malli-gen-ent-val
                sg/spec-gen-merge-overwrites
                sg/spec-gen-assoc-relations])

(defn ent-db-malli-gen
  [ent-db query]
  (-> (sm/add-ents ent-db query)
      (sm/visit-ents-once :malli-gen malli-gen)))

(defn datastore [{:keys [schema]} {:keys [ent-type]}]
  (:datastore (ent-type schema)))

(defn insert-jgit [database ent-db {:keys [ent-type malli-gen]}]
  (let [repo (:repo/jgit (database/lookup database [:repo/id (:blob/repo malli-gen)]))]
    (merge malli-gen
           (jgit/create-blob! repo malli-gen))))

(defn insert-datascript [database ent-db {:keys [malli-gen] :as ent-attrs}]
  (let [datoms (assoc-ent-at-foreign-keys ent-db ent-attrs)]
    (d/transact! database [datoms])
    malli-gen))

(defn insert [database ent-db ent-attrs]
  (case (datastore ent-db ent-attrs)
    :datascript (insert-datascript database ent-db ent-attrs)
    :jgit (insert-jgit database ent-db ent-attrs)))

(defn ent-data [ent-db ent]
  (:inserted-data (sm/ent-attrs ent-db ent)))

(defn ent-attrs-map [ent-db]
  (let [ents (sm/ents ent-db)]
    (zipmap ents
            (map (partial ent-data ent-db) ents))))

(defn ents-attrs-map [ent-db]
  (let [ents-by-type (sm/ents-by-type ent-db)]
    (zipmap (keys ents-by-type)
            (map #(map (partial ent-data ent-db) %)
                 (vals ents-by-type)))))

(defn database-gen [query]
  (gen/->Generator
   (fn [rnd size]
     (let [database (database/create {})
           ent-db (-> (ent-db-malli-gen {:schema (schema)
                                        :gen-options {:rnd rnd
                                                      :size size}}
                                       query)
                     (sm/visit-ents-once :inserted-data (partial insert database)))]
       (rose/pure
        (merge
         {:handler (repos/handler database)
          :database database
          :ent-db ent-db
          :ents (ents-attrs-map ent-db)}
         (ent-attrs-map ent-db)))))))
