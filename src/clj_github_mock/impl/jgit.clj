(ns clj-github-mock.impl.jgit
  (:require [clj-github-mock.impl.base64 :as base64]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import [org.eclipse.jgit.internal.storage.dfs DfsRepositoryDescription InMemoryRepository]
           [org.eclipse.jgit.lib AnyObjectId CommitBuilder Constants FileMode ObjectId
                                 ObjectInserter ObjectReader PersonIdent Repository TreeFormatter]
           [org.eclipse.jgit.revwalk RevCommit]
           [org.eclipse.jgit.treewalk TreeWalk]))

(set! *warn-on-reflection* true)

(defn empty-repo []
  (InMemoryRepository. (DfsRepositoryDescription.)))

(defn- new-inserter ^ObjectInserter [^Repository repo]
  (-> repo (.getObjectDatabase) (.newInserter)))

(defmacro with-inserter [[inserter repo] & body]
  `(let [~inserter (new-inserter ~repo)
         result# (do ~@body)]
     (.flush ~inserter)
     result#))

(defn- new-reader [^Repository repo]
  (-> repo (.getObjectDatabase) (.newReader)))

(defn- load-object [^ObjectReader reader ^ObjectId object-id]
  (let [object-loader (.open reader object-id)]
    (.getBytes object-loader)))

(defn- insert-blob [^ObjectInserter inserter {:keys [content]}]
  (let [^bytes bs (if (bytes? content) content (.getBytes ^String content "UTF-8"))]
    (.insert inserter Constants/OBJ_BLOB bs)))

(defn create-blob! [repo blob]
  (with-inserter [inserter repo]
    (let [object-id (insert-blob inserter blob)]
      {:sha (ObjectId/toString object-id)})))

(defn get-blob [repo sha]
  (let [content (load-object (new-reader repo) (ObjectId/fromString sha))]
    {:content (base64/encode-bytes->str content)}))

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

(defn- tree-walk-seq [^TreeWalk tree-walk]
  (lazy-seq
   (when (.next tree-walk)
     (cons {:path (.getPathString tree-walk)
            :mode (file-mode->github-mode (.getFileMode tree-walk))
            :type (file-mode->github-type (.getFileMode tree-walk))
            :sha (ObjectId/toString (.getObjectId tree-walk 0))}
           (tree-walk-seq tree-walk)))))

(defn tree-walk [^Repository repo sha]
  (doto (TreeWalk. repo)
    (.reset (ObjectId/fromString sha))))

(defn split-path [path]
  (string/split path #"/"))

(defn path-sha [^Repository repo base_tree ^String path]
  (when base_tree
    (when-let [tree-walk (TreeWalk/forPath repo path ^"[Lorg.eclipse.jgit.lib.AnyObjectId;" (into-array ObjectId [(ObjectId/fromString base_tree)]))]
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

(defn content->object-id ^ObjectId [{:keys [type content base_tree] :as blob} repo inserter]
  (case type
    "blob" (insert-blob inserter blob)
    "tree" (insert-tree repo inserter content base_tree)))

(defn append-tree-item [{:keys [^String path mode sha content] :as item} repo ^TreeFormatter tree-formatter inserter]
  (when (or content sha)
    (.append tree-formatter
             path
             ^FileMode (github-mode->file-mode mode)
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
                [(-> (merge tree-item (get-blob repo (:sha tree-item)))
                     ; NOTE: when reading the flattened tree, contents are always assumed to be a String
                     ;       (needed for backwards compatibility)
                     (update :content #(if (string/blank? %) % (base64/decode-str->str %)))
                     (update :path (partial concat-path base-path))
                     (dissoc :sha))]))
            tree)))

(defn get-flatten-tree [repo sha]
  {:sha sha
   :tree (into [] (flatten-tree repo sha nil))})

(defn create-tree! [repo {:keys [tree base_tree]}]
  (let [final-tree (-> (tree-items->tree-map tree)
                       (tree-map->tree-items repo base_tree))]
    (let [sha (with-inserter [inserter repo]
                (ObjectId/toString (insert-tree repo inserter final-tree base_tree)))]
      (get-tree repo sha))))

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
     :parents (mapv #(hash-map :sha (ObjectId/toString (.getId ^RevCommit %))) (.getParents commit))}))

(defn create-commit! [repo {:keys [tree message parents]}]
  (let [commit-id (with-inserter [inserter repo]
                    (let [commit-builder (doto (CommitBuilder.)
                                           (.setMessage message)
                                           (.setTreeId (ObjectId/fromString tree))
                                           (.setAuthor (PersonIdent. "me" "me@example.com"))
                                           (.setCommitter (PersonIdent. "me" "me@example.com"))
                                           (.setParentIds ^"[Lorg.eclipse.jgit.lib.ObjectId;" (into-array ObjectId (map #(ObjectId/fromString %) parents))))]
                      (.insert inserter commit-builder)))]
    (get-commit repo (ObjectId/toString commit-id))))

(defn get-reference [^Repository repo ^String ref-name]
  (when-let [ref (.exactRef repo ref-name)]
    {:ref ref-name
     :object {:type "commit"
              :sha (ObjectId/toString (.getObjectId ref))}}))

(defn create-reference! [^Repository repo {:keys [^String ref sha]}]
  (let [ref-update  (.updateRef repo ref)]
    (doto ref-update
      (.setNewObjectId (ObjectId/fromString sha))
      (.update))
    (get-reference repo ref)))

(defn delete-reference! [^Repository repo ^String ref]
  (.delete
   (doto (.updateRef repo ref) (.setForceUpdate true))))

(defn get-branch [^Repository repo ^String branch]
  (when-let [ref (.findRef repo branch)]
    (let [commit (get-commit repo (ObjectId/toString (.getObjectId ref)))]
      {:name branch
       :commit {:sha (:sha commit)
                :commit (dissoc commit :sha)}})))

(defn get-content [repo sha path]
  (let [reader (new-reader repo)
        commit (RevCommit/parse (load-object reader (ObjectId/fromString sha)))
        tree-id (-> commit (.getTree) (.getId))
        tree-walk (TreeWalk/forPath ^ObjectReader reader ^String path ^"[Lorg.eclipse.jgit.lib.AnyObjectId;" (into-array AnyObjectId [tree-id]))
        object-id (when tree-walk (.getObjectId tree-walk 0))]
    (when object-id
      (let [content (load-object reader object-id)]
        {:type "file"
         :path path
         :content (base64/encode-bytes->str content)}))))
