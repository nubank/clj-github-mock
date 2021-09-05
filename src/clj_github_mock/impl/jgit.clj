(ns clj-github-mock.impl.jgit
  (:require [clojure.set :as set]
            [clojure.string :as string])
  (:import [org.eclipse.jgit.internal.storage.dfs DfsRepositoryDescription InMemoryRepository]
           [org.eclipse.jgit.lib AnyObjectId CommitBuilder Constants FileMode ObjectId ObjectReader PersonIdent TreeFormatter]
           [org.eclipse.jgit.revwalk RevCommit RevWalk]
           [org.eclipse.jgit.revwalk.filter RevFilter]
           [org.eclipse.jgit.treewalk TreeWalk]))

(defn empty-repo []
  (InMemoryRepository. (DfsRepositoryDescription.)))

(defn- new-inserter [repo]
  (-> repo (.getObjectDatabase) (.newInserter)))

(defmacro with-inserter [[inserter repo] & body]
  `(let [~inserter (new-inserter ~repo)
         result# (do ~@body)]
     (.flush ~inserter)
     result#))

(defn- new-reader [repo]
  (-> repo (.getObjectDatabase) (.newReader)))

(defn- load-object [reader object-id]
  (let [object-loader (.open reader object-id)]
    (.getBytes object-loader)))

(defn- insert-blob [inserter {:keys [content]}]
  (.insert inserter Constants/OBJ_BLOB (.getBytes content "UTF-8")))

(defn create-blob! [repo content]
  (with-inserter [inserter repo]
    (let [object-id (insert-blob inserter {:content content})]
      (ObjectId/toString object-id))))

(defn get-blob [repo sha]
  (String. (load-object (new-reader repo) (ObjectId/fromString sha)) "UTF-8"))

(def ^:private github-mode->file-mode {"100644" FileMode/REGULAR_FILE
                                       "100755" FileMode/EXECUTABLE_FILE
                                       "040000" FileMode/TREE
                                       "160000" FileMode/GITLINK
                                       "120000" FileMode/SYMLINK})

(def ^:private file-mode->github-mode (set/map-invert github-mode->file-mode))

(def ^:private file-mode->github-type {FileMode/REGULAR_FILE "blob"
                                       FileMode/EXECUTABLE_FILE "blob"
                                       FileMode/TREE "tree"
                                       FileMode/GITLINK "commit"
                                       FileMode/SYMLINK "blob"})

(defn- tree-walk-seq [tree-walk]
  (lazy-seq
   (when (.next tree-walk)
     (cons {:path (.getPathString tree-walk)
            :mode (file-mode->github-mode (.getFileMode tree-walk))
            :type (file-mode->github-type (.getFileMode tree-walk))
            :sha (ObjectId/toString (.getObjectId tree-walk 0))}
           (tree-walk-seq tree-walk)))))

(defn tree-walk [repo sha]
  (doto (TreeWalk. repo)
    (.reset (ObjectId/fromString sha))))

(defn split-path [path]
  (string/split path #"/"))

(defn path-sha [repo base_tree path]
  (when base_tree
    (when-let [tree-walk (TreeWalk/forPath repo path (into-array ObjectId [(ObjectId/fromString base_tree)]))]
      (ObjectId/toString (.getObjectId tree-walk 0)))))

(defn leaf-item? [item]
  (not (map? item)))

(defn tree-items->tree-map [tree-items]
  (->> (reduce (fn [acc {:keys [path] :as item}]
                 (let [path-split (split-path path)
                       item-path (last path-split)]
                   (update-in acc path-split conj (assoc item :path item-path))))
               {} tree-items)))

(defn tree-map->tree-items [tree-map repo base_tree]
  (mapcat
   (fn [[path item]]
     (if-not (leaf-item? item)
       (let [tree-sha (path-sha repo base_tree path)]
         [{:path path
           :type "tree"
           :mode "040000"
           :base_tree tree-sha
           :content (if (map? item)
                      (tree-map->tree-items item repo tree-sha)
                      item)}])
       item))
   tree-map))

(declare insert-tree)

(defn content->object-id [{:keys [type content base_tree] :as blob} repo inserter]
  (case type
    "blob" (insert-blob inserter blob)
    "tree" (insert-tree repo inserter content base_tree)))

(defn append-tree-item [{:keys [path mode sha content] :as item} repo tree-formatter inserter]
  (when (or content sha)
    (.append tree-formatter
             path
             (github-mode->file-mode mode)
             (if sha
               (ObjectId/fromString sha)
               (content->object-id item repo inserter)))))

(defn- merge-trees [base-tree new-tree]
  (-> (merge (group-by :path base-tree)
             (group-by :path new-tree))
      vals
      flatten))

(defn insert-tree [repo inserter tree base_tree]
  (let [tree-formatter (TreeFormatter.)
        tree' (if base_tree
                (merge-trees (tree-walk-seq (tree-walk repo base_tree))
                             tree)
                tree)]
    (doseq [tree-item tree']
      (append-tree-item tree-item repo tree-formatter inserter))
    (.insertTo tree-formatter inserter)))

(defn get-tree [repo sha]
  {:sha sha
   :tree (into [] (tree-walk-seq (tree-walk repo sha)))})

(defn- concat-path [base-path path]
  (if base-path (str base-path "/" path) path))

(defn- flatten-tree [repo base-sha base-path]
  (let [{:keys [tree]} (get-tree repo base-sha)]
    (mapcat (fn [{:keys [path type sha] :as tree-item}]
              (if (= "tree" type)
                (flatten-tree repo sha (concat-path base-path path))
                [(-> tree-item
                     (assoc :content (get-blob repo (:sha tree-item)))
                     (update :path (partial concat-path base-path))
                     (dissoc :sha))]))
            tree)))

(defn get-flatten-tree [repo sha]
  {:sha sha
   :tree (into [] (flatten-tree repo sha nil))})

(defn create-tree! [repo {:keys [tree base_tree]}]
  (let [final-tree (-> (tree-items->tree-map tree)
                       (tree-map->tree-items repo base_tree))]
    (with-inserter [inserter repo]
      (ObjectId/toString (insert-tree repo inserter final-tree base_tree)))))

;; only for testing - start
(declare tree-content)

(defn- tree-item-content [repo {:keys [type sha]}]
  (case type
    "tree" (tree-content repo sha)
    "blob" (get-blob repo sha)))

(defn tree-content [repo sha]
  (let [tree (tree-walk-seq (tree-walk repo sha))]
    (->> tree
         (map (fn [{:keys [path] :as tree-item}]
                [path (tree-item-content repo tree-item)]))
         (into {}))))
;; only for testing -  end

(defn get-commit [repo sha]
  (let [bytes (load-object (new-reader repo) (ObjectId/fromString sha))
        commit (RevCommit/parse bytes)]
    {:sha sha
     :message (.getFullMessage commit)
     :tree {:sha (-> commit (.getTree) (.getId) (.getName))}
     :parents (mapv #(hash-map :sha (ObjectId/toString (.getId %))) (.getParents commit))}))

(defn create-commit! [repo {:keys [tree message parents]}]
  (with-inserter [inserter repo]
    (let [commit-builder (doto (CommitBuilder.)
                           (.setMessage message)
                           (.setTreeId (ObjectId/fromString tree))
                           (.setAuthor (PersonIdent. "me" "me@example.com"))
                           (.setCommitter (PersonIdent. "me" "me@example.com"))
                           (.setParentIds (into-array ObjectId (map #(ObjectId/fromString %) parents))))]
      (ObjectId/toString (.insert inserter commit-builder)))))

(defn- object-id [reader sha path]
  (let [commit (RevCommit/parse (load-object reader (ObjectId/fromString sha)))
        tree-id (-> commit (.getTree) (.getId))
        tree-walk (TreeWalk/forPath ^ObjectReader reader ^String path (into-array AnyObjectId [tree-id]))]
    (when tree-walk (.getObjectId tree-walk 0))))

(defn get-content [repo sha path]
  (let [reader (new-reader repo)]
    (when-let [id (object-id repo sha path)]
      (String. (load-object reader id) "UTF-8"))))

(defn path-exists? [repo sha path]
  (boolean (object-id (new-reader repo) sha path)))

(defn object-exists? [repo sha]
  (-> repo (.getObjectDatabase) (.has (ObjectId/fromString sha))))

(defn merge-base [repo sha1 sha2]
  (let [rev-walk (RevWalk. repo)
        commit1 (.parseCommit rev-walk (ObjectId/fromString sha1))
        commit2 (.parseCommit rev-walk (ObjectId/fromString sha2))
        _ (doto rev-walk
            (.setRevFilter RevFilter/MERGE_BASE)
            (.markStart commit1)
            (.markStart commit2))
        base (.next rev-walk)]
    (when (and base (nil? (.next rev-walk)))
      (-> base (.toObjectId) (ObjectId/toString)))))
