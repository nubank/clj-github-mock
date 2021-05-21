(ns clj-github-mock.impl.jgit-test
  (:require  [base64-clj.core :as base64]
             [clj-github-mock.impl.jgit :as sut]
             [clojure.java.io :as io]
             [clojure.test :refer :all]
             [clojure.test.check.clojure-test :refer [defspec]]
             [clojure.test.check.generators :as gen]
             [clojure.test.check.properties :as prop]
             [clojure.walk :as walk]
             [editscript.core :as editscript]
             [lambdaisland.regal.generator :as rg]
             [malli.generator :as mg]
             [matcher-combinators.standalone :refer [match?]])
  (:import [java.util.zip ZipInputStream]
           [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn decode-base64 [content]
  (if (empty? content)
    content
    (base64/decode content "UTF-8")))

(defspec blob-is-added-to-repo
  (prop/for-all
   [content gen/string]
   (let [repo (sut/empty-repo)
         {:keys [sha]} (sut/create-blob! repo {:content content})]
     (= content
        (decode-base64 (:content (sut/get-blob repo sha)))))))

(defn flatten-obj [[obj-name node :as entry]]
  (if (string? node)
    entry
    (into
     {}
     (map (fn [[child-name child-node]]
            [(str obj-name "/" child-name) child-node])
          (val entry)))))

(defn tree->github-tree [tree]
  (->> (walk/postwalk
        (fn [node]
          (if (map? node)
            (into {} (map flatten-obj node))
            node))
        tree)
       (map (fn [[path content]]
              {:path path
               :mode "100644"
               :type "blob"
               :content content}))))

(def object-name [:re {:gen/gen (rg/gen [:+ [:class [\a \z]]])} #"\w+"])

(def github-tree-gen
  (gen/fmap
   tree->github-tree
   (mg/generator [:schema {:registry {::file-content :string
                                      ::dir [:and
                                             [:map-of object-name [:ref ::node]]
                                             [:fn seq]]
                                      ::node [:or ::file-content ::dir]
                                      ::root ::dir}}
                  ::root])))

(defspec tree-is-added-to-repo
  10
  (prop/for-all
   [tree github-tree-gen]
   (let [repo (sut/empty-repo)
         {:keys [sha]} (sut/create-tree! repo {:tree tree})]
     (match? {:sha sha}
             (sut/get-tree repo sha)))))

(defn delete-gen [tree]
  (gen/let [item (gen/elements tree)]
    (-> item
        (dissoc :content)
        (assoc :sha nil))))

(defn update-gen [tree]
  (gen/let [item (gen/elements tree)
            new-content (gen/not-empty gen/string)]
    (assoc item :content new-content)))

(def github-tree+changes-gen
  (gen/let [tree github-tree-gen
            changes (gen/vector-distinct-by :path (gen/one-of [(update-gen tree) (delete-gen tree)]) {:min-elements 1})]
    {:tree tree :changes changes}))

(defn changes->edits [changes]
  (->> changes
       (mapv (fn [{:keys [path content]}]
               (if content
                 [(into (sut/split-path path) [:content]) :r (base64/encode content "UTF-8")]
                 [(sut/split-path path) :-])))
       (editscript/edits->script)))

(defspec create-tree-preserves-base-tree
  10
  (prop/for-all
   [{:keys [tree changes]} github-tree+changes-gen]
   (let [repo (sut/empty-repo)
         {base_tree :sha} (sut/create-tree! repo {:tree tree})
         content-before (sut/tree-content repo base_tree)
         {sha :sha} (sut/create-tree! repo {:base_tree base_tree :tree changes})
         content-after (sut/tree-content repo sha)]
     (= content-after
        (editscript/patch content-before (changes->edits changes))))))

(defspec commit-is-added-to-repo
  10
  (prop/for-all
   [tree github-tree-gen
    message gen/string]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree tree})
         {:keys [sha]} (sut/create-commit! repo {:tree tree-sha :message message :parents []})]
     (match? {:sha sha
              :message message}
             (sut/get-commit repo sha)))))

(defspec create-commit-sets-parent
  10
  (prop/for-all
   [{:keys [tree changes]} github-tree+changes-gen
    message gen/string]
   (let [repo (sut/empty-repo)
         {parent-tree-sha :sha} (sut/create-tree! repo {:tree tree})
         {parent-sha :sha} (sut/create-commit! repo {:tree parent-tree-sha :message message :parents []})
         {tree-sha :sha} (sut/create-tree! repo {:tree changes :base_tree parent-tree-sha})
         {:keys [sha]} (sut/create-commit! repo {:tree tree-sha :message message :parents [parent-sha]})]
     (= (sut/get-commit repo parent-sha)
        (sut/get-commit repo (get-in (sut/get-commit repo sha) [:parents 0 :sha]))))))

(defspec reference-is-added-to-repo
  (prop/for-all
   [ref (gen/fmap #(str "refs/heads/" %) (gen/not-empty gen/string-alphanumeric))]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree []})
         {sha :sha} (sut/create-commit! repo {:tree tree-sha :message "test"})]
     (sut/create-reference! repo {:ref ref :sha sha})
     (= {:object {:sha sha
                  :type "commit"}
         :ref ref}
        (sut/get-reference repo ref)))))

(defspec reference-can-be-deleted
  (prop/for-all
   [ref (gen/fmap #(str "refs/heads/" %) (gen/not-empty gen/string-alphanumeric))]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree [{:path "a" :mode "100644" :type "blob" :content "a"}]})
         {sha :sha} (sut/create-commit! repo {:tree tree-sha :message "test"})]
     (sut/create-reference! repo {:ref ref :sha sha})
     (sut/delete-reference! repo ref)
     (nil? (sut/get-reference repo ref)))))

(defspec can-get-branch-info
  (prop/for-all
   [branch (gen/not-empty gen/string-alphanumeric)]
   (let [ref (str "refs/heads/" branch)
         repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree []})
         {sha :sha} (sut/create-commit! repo {:tree tree-sha :message "test"})]
     (sut/create-reference! repo {:ref ref :sha sha})
     (= {:name branch
         :commit {:sha sha
                  :commit {:message "test"
                           :parents []
                           :tree {:sha tree-sha}}}}
        (sut/get-branch repo branch)))))

(defspec can-get-content
  10
  (prop/for-all
   [tree github-tree-gen]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree tree})
         {:keys [sha]} (sut/create-commit! repo {:tree tree-sha :message "test" :parents []})]
     (every? #(= {:type "file"
                  :path (:path %)
                  :content (base64/encode (:content %) "UTF-8")}
                 (sut/get-content repo sha (:path %)))
             tree))))

(defn- slurp-entry [zip-input-stream]
  (with-open [out (ByteArrayOutputStream.)]
    (io/copy zip-input-stream out)
    (String. (.toByteArray out) "UTF-8")))

(defn- tree-item [zip-input-stream]
  (when-let [entry (.getNextEntry zip-input-stream)]
    (if (.isDirectory entry)
      {:directory? true}
      {:path (->> (.getName entry) (re-find #"^[\w|-]+\/(.*)") second)
       :mode "100644"
       :type "blob"
       :content (slurp-entry zip-input-stream)})))

(defn- zip->tree [zip-input-stream]
  (lazy-seq
   (if-let [entry (tree-item zip-input-stream)]
     (if (:directory? entry)
       (zip->tree zip-input-stream)
       (cons entry (zip->tree zip-input-stream)))
     [])))

(defn- is->tree [input-stream]
  (zip->tree (ZipInputStream. input-stream)))

(defspec can-get-commit-zipball
  10
  (prop/for-all
   [tree github-tree-gen]
   (= (set tree)
      (let [repo (sut/empty-repo)
                 {tree-sha :sha} (sut/create-tree! repo {:tree tree})
                 {:keys [sha]} (sut/create-commit! repo {:tree tree-sha :message "test" :parents []})]
             (-> (sut/get-commit-zipball repo "nubank" "some-repo" sha)
                 is->tree
                 set)))))
