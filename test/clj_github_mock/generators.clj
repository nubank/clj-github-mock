(ns clj-github-mock.generators
  (:require [reifyhealth.specmonstah.core :as sm]
            [reifyhealth.specmonstah.spec-gen :as sg]
            [malli.generator :as mg]
            [clj-github-mock.impl.database :as database]
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
  {:org {:prefix :org
         :malli-schema [:map
                        [:org/name [:string {:gen/gen (unique-name-gen)}]]]}
   :repo {:prefix :repo
          :malli-schema [:map
                         [:repo/name [:string {:gen/gen (unique-name-gen)}]]
                         [:repo/attrs [:map
                                       [:default_branch [:= "main"]]]]]
          :relations {:repo/org [:org :org/name]}}})

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

(defn assoc-ent-at-foreign-keys [db {:keys [ent-type visit-val]}]
  (reduce
   (fn [acc [attr relation-path]]
     (update acc attr (partial foreign-key-ent relation-path)))
   visit-val
   (-> db :schema ent-type :relations)))

(def malli-gen [malli-gen-ent-val
                sg/spec-gen-merge-overwrites
                sg/spec-gen-assoc-relations
                assoc-ent-at-foreign-keys])

(defn ent-db-malli-gen
  [ent-db query]
  (-> (sm/add-ents ent-db query)
      (sm/visit-ents-once :malli-gen malli-gen)))

(defn insert [database _ {:keys [malli-gen]}]
  (d/transact! database [malli-gen]))

(defn database-gen [query]
  (gen/->Generator
   (fn [rnd size]
     (let [database (database/create {})]
       (rose/pure
        {:handler (repos/handler database)
         :database database
         :ent-db (-> (ent-db-malli-gen {:schema (schema)
                                        :gen-options {:rnd rnd
                                                      :size size}}
                                       query)
                     (sm/visit-ents-once :inserted-data (partial insert database)))})))))
