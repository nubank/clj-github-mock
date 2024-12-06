(ns clj-github-mock.impl.jgit-test
  (:require  [clj-github-mock.generators :as mock-gen]
             [clj-github-mock.impl.base64 :as base64]
             [clj-github-mock.impl.jgit :as sut]
             [clojure.test.check.clojure-test :refer [defspec]]
             [clojure.test.check.generators :as gen]
             [clojure.test.check.properties :as prop]
             [editscript.core :as editscript]
             [matcher-combinators.matchers :as matchers]
             [matcher-combinators.standalone :refer [match?]])
  (:import (java.util Arrays)))

(defn decode-base64 [content]
  (if (empty? content)
    content
    (base64/decode-str->str content)))

(defspec string-blob-is-added-to-repo
  (prop/for-all
   [content gen/string]
   (let [repo (sut/empty-repo)
         {:keys [sha]} (sut/create-blob! repo {:content content})]
     (= content
        (decode-base64 (:content (sut/get-blob repo sha)))))))

(defspec binary-blob-is-added-to-repo
  (prop/for-all
   [^bytes content gen/bytes]
   (let [repo (sut/empty-repo)
         {:keys [sha]} (sut/create-blob! repo {:content (base64/encode-bytes->str content)
                                               :encoding "base64"})]
     (Arrays/equals content
                    (base64/decode-str->bytes (:content (sut/get-blob repo sha)))))))

(defspec tree-is-added-to-repo
  (prop/for-all
   [tree mock-gen/github-tree]
   (let [repo (sut/empty-repo)
         {:keys [sha]} (sut/create-tree! repo {:tree tree})]
     (match? {:sha sha
              :tree (matchers/in-any-order tree)}
             (sut/get-flatten-tree repo sha)))))

(def github-tree+changes-gen
  (gen/let [tree mock-gen/github-tree
            changes (mock-gen/github-tree-changes tree)]
    {:tree tree :changes changes}))

(defn changes->edits [changes]
  (->> changes
       (mapv (fn [{:keys [path content]}]
               (if content
                 [(into (sut/split-path path) [:content]) :r (base64/encode-str->str content)]
                 [(sut/split-path path) :-])))
       (editscript/edits->script)))

(defspec create-tree-preserves-base-tree
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
  (prop/for-all
   [tree mock-gen/github-tree
    message gen/string]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree tree})
         {:keys [sha]} (sut/create-commit! repo {:tree tree-sha :message message :parents []})]
     (match? {:sha sha
              :message message}
             (sut/get-commit repo sha)))))

(defspec create-commit-sets-parent
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
   [ref (gen/fmap #(str "refs/heads/" %) mock-gen/object-name)]
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
   [ref (gen/fmap #(str "refs/heads/" %) mock-gen/object-name)]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree [{:path "a" :mode "100644" :type "blob" :content "a"}]})
         {sha :sha} (sut/create-commit! repo {:tree tree-sha :message "test"})]
     (sut/create-reference! repo {:ref ref :sha sha})
     (sut/delete-reference! repo ref)
     (nil? (sut/get-reference repo ref)))))

(defspec can-get-branch-info
  (prop/for-all
   [branch mock-gen/object-name]
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
  (prop/for-all
   [tree mock-gen/github-tree]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree tree})
         {:keys [sha]} (sut/create-commit! repo {:tree tree-sha :message "test" :parents []})]
     (every? #(= {:type "file"
                  :path (:path %)
                  :encoding "base64"
                  :content (base64/encode-str->str (:content %))}
                 (sut/get-content repo sha (:path %)))
             tree))))
