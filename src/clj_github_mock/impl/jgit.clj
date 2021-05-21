(ns clj-github-mock.impl.jgit
  (:require [base64-clj.core :as base64]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [org.eclipse.jgit.internal.storage.dfs DfsRepositoryDescription InMemoryRepository]
           [org.eclipse.jgit.lib AnyObjectId CommitBuilder Constants FileMode ObjectId ObjectReader PersonIdent TreeFormatter]
           [org.eclipse.jgit.revwalk RevCommit]
           [org.eclipse.jgit.treewalk TreeWalk]
           [java.util.zip ZipEntry ZipOutputStream]
           [java.io ByteArrayInputStream ByteArrayOutputStream]))

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

(defn create-blob! [repo blob]
  (with-inserter [inserter repo]
    (let [object-id (insert-blob inserter blob)]
      {:sha (ObjectId/toString object-id)})))

(defn get-blob [repo sha]
  (let [content (String. (load-object (new-reader repo) (ObjectId/fromString sha)) "UTF-8")]
    {:content (base64/encode content "UTF-8")}))

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
     :parents (mapv #(hash-map :sha (ObjectId/toString (.getId %))) (.getParents commit))}))

(defn create-commit! [repo {:keys [tree message parents]}]
  (let [commit-id (with-inserter [inserter repo]
                    (let [commit-builder (doto (CommitBuilder.)
                                           (.setMessage message)
                                           (.setTreeId (ObjectId/fromString tree))
                                           (.setAuthor (PersonIdent. "me" "me@example.com"))
                                           (.setCommitter (PersonIdent. "me" "me@example.com"))
                                           (.setParentIds (into-array ObjectId (map #(ObjectId/fromString %) parents))))]
                      (.insert inserter commit-builder)))]
    (get-commit repo (ObjectId/toString commit-id))))

(defn get-reference [repo ref-name]
  (when-let [ref (.exactRef repo ref-name)]
    {:ref ref-name
     :object {:type "commit"
              :sha (ObjectId/toString (.getObjectId ref))}}))

(defn create-reference! [repo {:keys [ref sha]}]
  (let [ref-update  (.updateRef repo ref)]
    (doto ref-update
      (.setNewObjectId (ObjectId/fromString sha))
      (.update))
    (get-reference repo ref)))

(defn delete-reference! [repo ref]
  (.delete
   (doto (.updateRef repo ref) (.setForceUpdate true))))

(defn get-branch [repo branch]
  (when-let [ref (.findRef repo branch)]
    (let [commit (get-commit repo (ObjectId/toString (.getObjectId ref)))]
      {:name branch
       :commit {:sha (:sha commit)
                :commit (dissoc commit :sha)}})))

(defn get-content [repo sha path]
  (let [reader (new-reader repo)
        commit (RevCommit/parse (load-object reader (ObjectId/fromString sha)))
        tree-id (-> commit (.getTree) (.getId))
        tree-walk (TreeWalk/forPath ^ObjectReader reader ^String path (into-array AnyObjectId [tree-id]))
        object-id (when tree-walk (.getObjectId tree-walk 0))]
    (when object-id
      (let [content (String. (load-object reader object-id) "UTF-8")]
        {:type "file"
         :path path
         :content (base64/encode content "UTF-8")}))))

(defn- get-blob-byte-array [repo sha]
  (load-object (new-reader repo) (ObjectId/fromString sha)))

(defn- zip-blob! [repo parent-path zout {:keys [path sha]}]
  (let [entry (ZipEntry. (str parent-path path))]
    (.putNextEntry zout entry)
    (io/copy (get-blob-byte-array repo sha) zout)))

(declare ^:private zip-tree-seq!)

(defn- zip-tree! [repo parent-path zout {:keys [path sha]}]
  (let [full-path (str parent-path path "/")
        entry (ZipEntry. full-path)]
    (.putNextEntry zout entry)
    (.closeEntry zout)
    (zip-tree-seq! repo full-path zout (tree-walk-seq (tree-walk repo sha)))))

(defn- zip-tree-seq! [repo parent-path zout tree-seq]
  (doseq [{:keys [type] :as item} tree-seq]
    (case type
      "tree" (zip-tree! repo parent-path zout item)
      "blob" (zip-blob! repo parent-path zout item))))

(defn get-commit-zipball [repo org repo-name sha]
  (with-open [out (ByteArrayOutputStream.)
              zout (ZipOutputStream. out)]
    (let [parent-path (str org "-" repo-name "-" sha "/")
          {:keys [tree]} (get-commit repo sha)
          tree-seq (tree-walk-seq (tree-walk repo (:sha tree)))]
      (.putNextEntry zout (ZipEntry. parent-path))
      (.closeEntry zout)
      (zip-tree-seq! repo parent-path zout tree-seq)
      (.finish zout)
      (ByteArrayInputStream. (.toByteArray out)))))
