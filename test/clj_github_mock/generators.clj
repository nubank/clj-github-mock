(ns clj-github-mock.generators
  (:require [clj-github-mock.handlers.repos :as repos]
            [clj-github-mock.impl.jgit :as jgit]
            [clj-github-mock.resource :as resource]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [clojure.walk :as walk]
            [datascript.core :as d]
            [lambdaisland.regal.generator :as regal-gen]
            [malli.generator :as mg]
            [medley.core :refer [assoc-some map-keys map-vals]]
            [reifyhealth.specmonstah.core :as sm]
            [reifyhealth.specmonstah.spec-gen :as sg]
            [clj-github-mock.db :as db]))

(def object-name
  "Generates a name for objects like org names, repo names, branches, tags, etc."
  (regal-gen/gen [:+ :word]))

(defn unique-object-name
  "Creates a object-name generator that returns unique names."
  []
  (let [names (atom #{})]
    (gen/such-that
     #(let [result (@names %)]
        (swap! names conj %)
        (not result))
     object-name)))

(def blob
  "Generates a string that can be used as a blob content."
  gen/string-ascii)

(defn- flatten-map-tree-entry [[obj-name node]]
  (if (:type node)
    {obj-name node}
    (map-keys #(str obj-name "/" %) node)))

(defn- map-tree->github-tree [map-tree]
  (->> (walk/postwalk
        (fn [node]
          (if (and (map? node) (not (:type node)))
            (apply merge (map flatten-map-tree-entry node))
            node))
        map-tree)
       (mapv (fn [[path tree-object]]
               (assoc tree-object :path path)))))

(def ^:private modes-frequency {"100644" 95 "100755" 5})

(def ^:private github-tree-object
  (mg/generator [:map
                 [:type [:= "blob"]]
                 [:mode (into [:enum {:gen/gen (gen/frequency (mapv #(vector (val %) (gen/return (key %))) modes-frequency))} (keys modes-frequency)])]
                 [:content [:string {:gen/gen blob}]]]))

(def github-tree
  "Generates a vector of github tree objects. A github tree object is a map
  representing a file with the following fields:

  - `:path`: a string with the path of the object, the path can be nested,
             in which case it will be separated by slashes
  - `:type`: this generator only creates object of the \"blob\" type
  - `:mode`: either `100644` for normal files and `100755` for executable files
             note: although the generator can return files with executable mode, the
             content of the file is not actualy something that can be executed
  - `:content`: a string with the content of the file"
  (gen/let [map-tree (gen/recursive-gen
                      #(gen/map object-name % {:min-elements 1})
                      github-tree-object)
            root-name (if (:type map-tree) object-name (gen/return nil))]
    (if root-name
      [(assoc map-tree :path root-name)]
      (map-tree->github-tree map-tree))))

(defn- update-gen [github-tree]
  (gen/let [item (gen/elements github-tree)
            new-content (gen/not-empty gen/string)]
    (assoc item :content new-content)))

(defn- delete-gen [github-tree]
  (gen/let [item (gen/elements github-tree)]
    (-> item
        (dissoc :content)
        (assoc :sha nil))))

; TODO support creating new files
(defn github-tree-changes
  "Creates a generator that given a github tree generates changes in that tree.
  The changes are themselves a github tree."
  [github-tree]
  (if (empty? github-tree)
    (gen/return github-tree)
    (gen/vector-distinct-by :path (gen/frequency [[9 (update-gen github-tree)] [1 (delete-gen github-tree)]]) {:min-elements 1})))

(defn tree
  "Creates a generator that given a jgit repository generates a tree in it.
  If passed the sha of a tree in the repository, generates a tree that is
  derived from the one whose sha was passed.

  Note: the generator in not purely functional since a jgit repository is mutable."
  ([repo]
   (tree repo nil))
  ([repo base-tree-sha]
   (let [{:keys [tree]} (when base-tree-sha
                          (jgit/get-flatten-tree repo base-tree-sha))]
     (gen/let [new-tree (if tree (github-tree-changes tree) github-tree)]
       (jgit/create-tree! repo (assoc-some {:tree new-tree} :base_tree base-tree-sha))))))

(defn commit
  "Creates a generator that given a jgit repository generates a commit in it.
  If passed the sha of a commit in the repository, generates a commit that
  is derived from the one whose sha was passed.

  Note: the generator is not purely functional since a jgit repository is mutable."
  ([repo]
   (commit repo nil))
  ([repo parent-commit-sha]
   (let [base-tree-sha (when parent-commit-sha (-> (jgit/get-commit repo parent-commit-sha) :tree :sha))]
     (gen/let [tree (tree repo base-tree-sha)
               message gen/string]
       (jgit/create-commit! repo (assoc-some {:message message :tree (:sha tree)} :parents (when parent-commit-sha [parent-commit-sha])))))))

(defn- commit-history [repo base-commit num-commits]
  (if (= 0 num-commits)
    (gen/return base-commit)
    (gen/let [next-commit (commit repo (:sha base-commit))]
      (commit-history repo next-commit (dec num-commits)))))

(defn branch
  "Creates a generator that given a jgit repository generates in it a branch pointing to a sequence of commits.
  The generator can be customized with the following options:
  - `:name`: the name of the branch, if not set a random name is generated
  - `num-commits`: the number of commits to be generated, if not set a random number of commits is generated
  - `base-branch`: the generated branch will be derived from the base-branch, if not set an orphan branch is generated

  Note: the generator is not purely functional since a jgit repository is mutable"
  [repo & {:keys [name num-commits base-branch]}]
  (gen/let [branch-name (if name (gen/return name) object-name)
            num-commits (if num-commits (gen/return num-commits) (gen/fmap inc (gen/scale #(/ % 10) gen/nat)))
            last-commit (commit-history repo (when base-branch (-> (jgit/get-branch repo base-branch) :commit)) num-commits)]
    (jgit/create-reference! repo {:ref (str "refs/heads/" branch-name) :sha (:sha last-commit)})
    (jgit/get-branch repo branch-name)))

(defn random-file
  "Creates a generator that given a jgit repository and a branch name randomly selects a file contained in that branch.
  The file is returned as a github tree object.

  Note: the generator is not purely functional since a jgit repository is mutable"
  [repo branch-name]
  (let [branch (jgit/get-branch repo branch-name)
        commit (jgit/get-commit repo (-> branch :commit :sha))
        {:keys [tree]} (jgit/get-flatten-tree repo (-> commit :tree :sha))]
    (gen/elements tree)))

(defn- schema [jgit-repo]
  {:org {:prefix :org
         :malli-schema [:map
                        [:org/name [:string {:gen/gen (unique-object-name)}]]]}
   :repo {:prefix :repo
          :malli-schema [:map
                         [:repo/name [:string {:gen/gen (unique-object-name)}]]
                         [:repo/attrs [:map
                                       [:default_branch [:= "main"]]]]
                         [:repo/jgit [:any {:gen/fmap (fn [_] (jgit/empty-repo))}]]]
          :relations {:repo/org [:org :org/name]}}
   :tree {:prefix :tree
          :malli-schema [:map
                         [:tree/sha [:string {:gen/gen (gen/fmap :sha (tree jgit-repo))}]]]
          :relations {:tree/repo [:repo :repo/name+org]}}})

(defn- malli-create-gen
  [ent-db]
  (update ent-db :schema #(map-vals (fn [{:keys [malli-schema] :as ent-spec}]
                                      (assoc ent-spec :malli-gen (mg/generator malli-schema))) %)))

(defn- malli-gen-ent-val
  [{{:keys [rnd-state size]} :gen-options :as ent-db} {:keys [ent-name]}]
  (let [{:keys [malli-gen]} (sm/ent-schema ent-db ent-name)
        [rnd1 rnd2] (random/split @rnd-state)]
    (reset! rnd-state rnd2)
    (rose/root (gen/call-gen malli-gen rnd1 size))))

(defn- foreign-key-ent [[_ foreign-key-attr :as path] foreign-key-val]
  (cond
    ; TODO: use constraints to detect if it is a multi relationship
    (vector? foreign-key-val) (set (map (partial foreign-key-ent path) foreign-key-val))
    :else {foreign-key-attr foreign-key-val}))

(defn- assoc-ent-at-foreign-keys [db {:keys [ent-type spec-gen]}]
  (reduce
   (fn [acc [attr relation-path]]
     (update acc attr (partial foreign-key-ent relation-path)))
   spec-gen
   (-> db :schema ent-type :relations)))

(def ^:private malli-gen [malli-gen-ent-val
                          sg/spec-gen-merge-overwrites
                          sg/spec-gen-assoc-relations])

(defn- ent-db-malli-gen
  [ent-db query]
  (-> (malli-create-gen ent-db)
      (sm/add-ents query)
      (sm/visit-ents-once :spec-gen malli-gen)))

(defn- insert-datascript [database ent-db {:keys [spec-gen] :as ent-attrs}]
  (let [datoms (assoc-ent-at-foreign-keys ent-db ent-attrs)]
    (d/transact! database [datoms])
    spec-gen))

(defn- insert [database ent-db ent-attrs]
  (insert-datascript database ent-db ent-attrs))

(defn- ent-data [ent-db ent]
  (:inserted-data (sm/ent-attrs ent-db ent)))

(defn- ent-attrs-map [ent-db]
  (let [ents (sm/ents ent-db)]
    (zipmap ents
            (map (partial ent-data ent-db) ents))))

(defn- ents-attrs-map [ent-db]
  (let [ents-by-type (sm/ents-by-type ent-db)]
    (zipmap (keys ents-by-type)
            (map #(map (partial ent-data ent-db) %)
                 (vals ents-by-type)))))

(defn database
  "Creates a generator that given a specmonstah query generates a `clj-github-mock.database`.
  The `schema` var in this namespace contains the specmonstah schema used by this generator.
  The generator returns a map from the ent names to ent attributes plus the following attributes:
  - `:handler`: a `clj-github-mock.repos/handler` pointing to the generated database
  - `:database`: the datascript connection to the database
  - `:ent-db`: the ent-db generated by specmonstah
  - `:ents`: a map from ent type to a collection of the ents generated for that type"
  [query]
  (gen/->Generator
   (fn [rnd size]
     (let [meta-db (resource/meta-db {})
           database (db/conn meta-db)
           ent-db (-> (ent-db-malli-gen {:schema (schema (db/jgit-repo meta-db))
                                         :gen-options {:rnd-state (atom rnd)
                                                       :size size}}
                                        query)
                      (sm/visit-ents-once :inserted-data (partial insert database)))]
       (rose/pure
        (merge
         {:handler (repos/handler meta-db)
          :database database
          :ent-db ent-db
          :ents (ents-attrs-map ent-db)}
         (ent-attrs-map ent-db)))))))
