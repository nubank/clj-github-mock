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
            [lambdaisland.regal.generator :as regal-gen]
            [medley.core :refer [assoc-some]]))

(defn unique-name []
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
    [{:path "some-file"
      :mode "100644"
      :type "blob"
      :content tree}]
    (->> (walk/postwalk
          (fn [node]
            (if (map? node)
              (into {} (map flatten-obj node))
              node))
          tree)
         (mapv (fn [[path content]]
                 {:path path
                  :mode "100644"
                  :type "blob"
                  :content content})))))

(def github-tree
  (gen/fmap tree->github-tree
            (gen/recursive-gen
             #(gen/map ref-name % {:min-elements 1})
             blob)))

(def github-commit (mg/generator [:map
                                  [:message :string]]))

(defn update-gen [tree]
  (gen/let [item (gen/elements tree)
            new-content (gen/not-empty gen/string)]
    (assoc item :content new-content)))

(defn delete-gen [tree]
  (gen/let [item (gen/elements tree)]
    (-> item
        (dissoc :content)
        (assoc :sha nil))))

(defn changes [github-tree]
  (gen/vector-distinct-by :path (gen/one-of [(update-gen github-tree) (delete-gen github-tree)]) {:min-elements 1}))

(defn github-tree-changes [base-github-tree]
  (if base-github-tree
    (changes base-github-tree)
    github-tree))

(defn commit
  ([repo]
   (commit repo nil))
  ([repo parent-commit-sha]
   (let [base-tree (when parent-commit-sha
                     (as-> (jgit/get-commit repo parent-commit-sha) $
                       (jgit/get-flatten-tree repo (-> $ :tree :sha))))]
     (gen/let [tree (github-tree-changes (:tree base-tree))
               gc github-commit]
       (as-> (jgit/create-tree! repo (assoc-some {:tree tree} :base_tree (:sha base-tree))) $
         (jgit/create-commit! repo (assoc-some gc :tree (:sha $) :parents (when parent-commit-sha [parent-commit-sha]))))))))

(defn- commit-history [repo base-commit num-commits]
  (if (= 0 num-commits)
    (gen/return base-commit)
    (gen/let [next-commit (commit repo (:sha base-commit))]
      (commit-history repo next-commit (dec num-commits)))))

(defn branch
  ([repo]
   (branch repo nil))
  ([repo base-branch]
   (gen/let [num-commits (gen/fmap inc (gen/scale #(/ % 10) gen/nat))
             last-commit (commit-history repo (when base-branch (-> (jgit/get-branch repo base-branch) :commit :sha)) num-commits)
             branch-name ref-name]
     (jgit/create-reference! repo {:ref (str "refs/heads/" branch-name) :sha (:sha last-commit)})
     (jgit/get-branch repo branch-name))))

(defn schema []
  {:org {:prefix :org
         :datastore :datascript
         :malli-schema [:map
                        [:org/name [:string {:gen/gen (unique-name)}]]]}
   :repo {:prefix :repo
          :datastore :datascript
          :malli-schema [:map
                         [:repo/id :uuid]
                         [:repo/name [:string {:gen/gen (unique-name)}]]
                         [:repo/attrs [:map
                                       [:default_branch [:= "main"]]]]
                         [:repo/jgit [:any {:gen/fmap (fn [_] (jgit/empty-repo))}]]]
          :relations {:repo/org [:org :org/name]}}
   :tree {:prefix :tree
          :datastore :jgit
          :malli-schema [:map
                         [:tree/id :uuid]
                         [:tree [:any {:gen/gen github-tree}]]]
          :relations {:tree/repo [:repo :repo/id]}}
   :commit {:prefix :commit
            :datastore :jgit
            :malli-schema [:map
                           [:commit/id :uuid]
                           [:commit [:any {:gen/gen github-commit}]]]
            :relations {:commit/tree [:tree :tree/id]}
                :constraints {:commit/tree #{:uniq}}}
   :branch {:prefix :branch
            :datastore :jgit
            :malli-schema [:map
                           [:ref [:string {:gen/gen (gen/fmap #(str "refs/heads/" %) (unique-name))}]]]
            :relations {:branch/commit [:commit :commit/id]}}})

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

(defn insert-jgit [database {:keys [trees commits]} {:keys [ent-type spec-gen]}]
  (case ent-type
    :tree
    (let [repo (:repo/jgit (database/lookup database [:repo/id (:tree/repo spec-gen)]))
          tree (merge spec-gen
                      (jgit/create-tree! repo spec-gen))]
      (swap! trees assoc (:tree/id spec-gen) tree)
      tree)
    :commit
    (let [tree (get @trees (:commit/tree spec-gen))
          repo (:repo/jgit (database/lookup database [:repo/id (:tree/repo tree)]))
          commit (merge spec-gen
                        (jgit/create-commit! repo (assoc (:commit spec-gen) :tree (:sha tree))))]
      (swap! commits assoc (:commit/id spec-gen) commit)
      commit)
    :branch
    (let [commit (get @commits (:branch/commit spec-gen))
          tree (get @trees (:commit/tree commit))
          repo (:repo/jgit (database/lookup database [:repo/id (:tree/repo tree)]))]
      (merge spec-gen
             (jgit/create-reference! repo (assoc spec-gen :sha (:sha commit)))))))

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

(defn database [query]
  (gen/->Generator
   (fn [rnd size]
     (let [database (database/create {})
           ent-db (-> (ent-db-malli-gen {:schema (schema)
                                         :gen-options {:rnd-state (atom rnd)
                                                       :size size}
                                         :trees (atom {})
                                         :commits (atom {})}
                                       query)
                     (sm/visit-ents-once :inserted-data (partial insert database)))]
       (rose/pure
        (merge
         {:handler (repos/handler database)
          :database database
          :ent-db ent-db
          :ents (ents-attrs-map ent-db)}
         (ent-attrs-map ent-db)))))))
