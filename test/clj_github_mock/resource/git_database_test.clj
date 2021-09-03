(ns clj-github-mock.resource.git-database-test
  (:require [clj-github-mock.resource.git-database :as git-database]
            [clojure.test :refer [deftest is]]
            [clj-github-mock.generators :as mock-gen]
            [clj-github-mock.impl.jgit :as jgit]
            [clojure.test.check.generators :as gen]))

(deftest tree-body-test
  (let [{:keys [repo0]} (mock-gen/gen-ents {:repo [[1]]})
        {:keys [sha]} (jgit/create-tree! (:repo/jgit repo0)
                                         {:tree [{:path "some-file" :type "blob" :mode "100644" :content "content"}]})]
    (is (= {:sha sha
            :tree [{:path "some-file" :type "blob" :mode "100644" :sha "6b584e8ece562ebffc15d38808cd6b98fc3d97ea"}]}
           (git-database/tree-body {:tree/repo repo0
                                    :tree/sha sha})))))

(deftest tree-lookup-test
  (let [{:keys [repo0]} (mock-gen/gen-ents {:repo [[1]]})
        {:keys [sha]} (gen/generate (mock-gen/tree (:repo/jgit repo0)))]
    (is (= {:tree/repo repo0
            :tree/sha sha}
           (git-database/tree-lookup {:path-params {:sha sha}
                                      :repo repo0})))
    (is (nil? (git-database/tree-lookup {:path-params {:sha "6b584e8ece562ebffc15d38808cd6b98fc3d97ea"}
                                         :repo repo0})))))

(deftest tree-post-test
  (let [{:keys [repo0]} (mock-gen/gen-ents {:repo [[1]]})
        tree (gen/generate mock-gen/github-tree)
        result (git-database/tree-post {:repo repo0
                                        :body {:tree tree}})]
    (is (jgit/object-exists? (:repo/jgit (:tree/repo result)) (:tree/sha result)))))

