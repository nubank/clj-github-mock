(ns clj-github-mock.impl.jgit-test
  (:require  [base64-clj.core :as base64]
             [clj-github-mock.generators :as mock-gen]
             [clj-github-mock.impl.jgit :as sut]
             [clojure.test.check.clojure-test :refer [defspec]]
             [clojure.test.check.generators :as gen]
             [clojure.test.check.properties :as prop]
             [editscript.core :as editscript]
             [matcher-combinators.matchers :as matchers]
             [matcher-combinators.standalone :refer [match?]]))

(defspec blob-is-added-to-repo
  (prop/for-all
   [content gen/string]
   (let [repo (sut/empty-repo)
         sha (sut/create-blob! repo content)]
     (= content
        (sut/get-blob repo sha)))))

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
                 [(sut/split-path path) :r content]
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

(defspec can-get-content
  (prop/for-all
   [tree mock-gen/github-tree]
   (let [repo (sut/empty-repo)
         {tree-sha :sha} (sut/create-tree! repo {:tree tree})
         {:keys [sha]} (sut/create-commit! repo {:tree tree-sha :message "test" :parents []})]
     (every? #(= (:content %)
                 (sut/get-content repo sha (:path %)))
             tree))))
