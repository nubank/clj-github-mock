(ns clj-github-mock.generators
  (:require [reifyhealth.specmonstah.core :as sm]
            [reifyhealth.specmonstah.spec-gen :as sg]
            [malli.generator :as mg]
            [clj-github-mock.impl.database :as database]
            [clj-github-mock.impl.jgit :as jgit]
            [datascript.core :as d]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.random :as random]
            [clj-github-mock.handlers.repos :as repos]
            [clojure.walk :as walk]
            [lambdaisland.regal.generator :as regal-gen]))

(defn unique-name-gen []
  (let [names (atom #{})]
    (gen/such-that
     #(let [result (@names %)]
        (swap! names conj %)
        (not result))
     (gen/not-empty gen/string-alphanumeric))))

(def ref-name (regal-gen/gen [:+ :word]))

(def blob gen/string-ascii)

(defn flatten-obj [[obj-name node :as entry]]
  (if (string? node)
    entry
    (into
     {}
     (map (fn [[child-name child-node]]
            [(str obj-name "/" child-name) child-node])
          (val entry)))))

(defn tree->github-tree [tree]
  (if (string? tree)
    {:tree []}
    {:tree
     (->> (walk/postwalk
           (fn [node]
             (if (map? node)
               (into {} (map flatten-obj node))
               node))
           #tap tree)
          (mapv (fn [[path content]]
                  {:path path
                   :mode "100644"
                   :type "blob"
                   :content content})))}))

(def github-tree
  (gen/fmap tree->github-tree
            (gen/recursive-gen
             #(gen/map ref-name % {:min-elements 1})
             blob)))

(defn schema []
  {:org {:prefix :org
         :datastore :datascript
         :malli-schema [:map
                        [:org/name [:string {:gen/gen (unique-name-gen)}]]]}
   :repo {:prefix :repo
          :datastore :datascript
          :malli-schema [:map
                         [:repo/id :uuid]
                         [:repo/name [:string {:gen/gen (unique-name-gen)}]]
                         [:repo/attrs [:map
                                       [:default_branch [:= "main"]]]]
                         [:repo/jgit [:any {:gen/fmap (fn [_] (jgit/empty-repo))}]]]
          :relations {:repo/org [:org :org/name]}}
   :tree {:prefix :tree
          :datastore :jgit
          :malli-schema [:map
                         [:tree [:any {:gen/gen github-tree}]]]
          :relations {:tree/repo [:repo :repo/id]}}})

(defn malli-create-gen
  [ent-db]
  (update ent-db :schema
          #(->> (map (fn [[ent-name {:keys [malli-schema] :as ent-spec}]]
                       [ent-name (assoc ent-spec :malli-gen (mg/generator malli-schema))]) %)
                (into {}))))

(defn malli-gen-ent-val
  [{{:keys [rnd-state size]} :gen-options :as ent-db} {:keys [ent-name]}]
  (let [{:keys [malli-gen]} (sm/ent-schema ent-db ent-name)
        [rnd1 rnd2] (random/split @rnd-state)]
    (reset! rnd-state rnd2)
    (rose/root (gen/call-gen malli-gen rnd1 size))))

(defn foreign-key-ent [[_ foreign-key-attr :as path] foreign-key-val]
  (cond
    ; TODO: use constraints to detect if it is a multi relationship
    (vector? foreign-key-val) (into #{} (map (partial foreign-key-ent path) foreign-key-val))
    :else {foreign-key-attr foreign-key-val}))

(defn assoc-ent-at-foreign-keys [db {:keys [ent-type spec-gen]}]
  (reduce
   (fn [acc [attr relation-path]]
     (update acc attr (partial foreign-key-ent relation-path)))
   spec-gen
   (-> db :schema ent-type :relations)))

(def malli-gen [malli-gen-ent-val
                sg/spec-gen-merge-overwrites
                sg/spec-gen-assoc-relations])

(defn ent-db-malli-gen
  [ent-db query]
  (-> (malli-create-gen ent-db)
      (sm/add-ents query)
      (sm/visit-ents-once :spec-gen malli-gen)))

(defn datastore [{:keys [schema]} {:keys [ent-type]}]
  (:datastore (ent-type schema)))

(defn insert-jgit [database ent-db {:keys [ent-type spec-gen]}]
  (let [repo (:repo/jgit (database/lookup database [:repo/id (:tree/repo spec-gen)]))]
    (merge spec-gen
           (jgit/create-tree! repo (:tree spec-gen)))))

(defn insert-datascript [database ent-db {:keys [spec-gen] :as ent-attrs}]
  (let [datoms (assoc-ent-at-foreign-keys ent-db ent-attrs)]
    (d/transact! database [datoms])
    spec-gen))

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
                                         :gen-options {:rnd-state (atom rnd)
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
