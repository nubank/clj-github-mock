(ns clj-github-mock.resource.git-database-test
  (:require [clj-github-mock.resource.git-database :as git-database]
            [clojure.test :refer [deftest is]]
            [clj-github-mock.generators :as mock-gen]
            [clj-github-mock.impl.jgit :as jgit]
            [matcher-combinators.test :refer [match?]]
            [ring.mock.request :as mock]
            [datascript.core :as d])
  (:import [clojure.lang ExceptionInfo]))

(deftest get-tree-test
  (let [{{:keys [org0 repo0 tree0]} :ents handler :handler} (mock-gen/gen-ents {:tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})]
    (is (match? {:body {:sha "1b3303a1a927c3cf37df3deb03a5fd38df26c051"
                        :tree [{:path "some-file" :type "blob" :mode "100644" :sha "6b584e8ece562ebffc15d38808cd6b98fc3d97ea"}]}}
                (handler (mock/request :get (format "/repos/%s/%s/git/trees/%s" (:org/name org0) (:repo/name repo0) (:tree/sha tree0))))))))

(deftest post-tree-test
  (let [{{:keys [org0 repo0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:repo [[1]]})]
    (handler (-> (mock/request :post (format "/repos/%s/%s/git/trees" (:org/name org0) (:repo/name repo0)))
                 (assoc :body {:tree [{:path "some-file" :type "blob" :mode "100644" :content "content"}]})))
    (is (match? {:tree/sha "1b3303a1a927c3cf37df3deb03a5fd38df26c051"}
                (d/pull @conn '[*] [:tree/repo+sha [(:db/id repo0) "1b3303a1a927c3cf37df3deb03a5fd38df26c051"]])))))

(deftest commit-body-test
  (let [{{:keys [repo0]} :ents} (mock-gen/gen-ents {:repo [[1]]})
        commit0 (mock-gen/gen-commit (:repo/jgit repo0))
        tree (mock-gen/gen-tree (:repo/jgit repo0) (-> commit0 :tree :sha))
        sha (jgit/create-commit! (:repo/jgit repo0) {:tree (:sha tree)
                                                     :message "message"
                                                     :parents [(:sha commit0)]})]
    (is (= {:sha sha
            :message "message"
            :tree {:sha (:sha tree)}
            :parents [{:sha (:sha commit0)}]}
           (git-database/commit-body {:commit/repo repo0
                                      :commit/sha sha})))))

(deftest commit-lookup-test
  (let [{{:keys [repo0]} :ents} (mock-gen/gen-ents {:repo [[1]]})
        {:keys [sha]} (mock-gen/gen-commit (:repo/jgit repo0))]
    (is (= {:commit/repo repo0
            :commit/sha sha}
           (git-database/commit-lookup {:path-params {:sha sha}
                                        :repo repo0})))
    (is (nil? (git-database/commit-lookup {:path-params {:sha "6b584e8ece562ebffc15d38808cd6b98fc3d97ea"}
                                           :repo repo0})))))

(deftest commit-post-test
  (let [{{:keys [repo0]} :ents} (mock-gen/gen-ents {:repo [[1]]})
        parent (mock-gen/gen-commit (:repo/jgit repo0))
        tree (mock-gen/gen-tree (:repo/jgit repo0) (-> parent :tree :sha))
        result (git-database/commit-post {:repo repo0
                                          :body {:tree (:sha tree)
                                                 :message "message"
                                                 :parents [(:sha parent)]}})]
    (is (jgit/object-exists? (:repo/jgit (:commit/repo result)) (:commit/sha result)))))

(deftest ref-key-test
  (let [{{:keys [repo0]} :ents db :db} (mock-gen/gen-ents {:repo [[1]]})]
    (is (= [:ref/repo+ref [(:db/id repo0) "refs/heads/my-branch"]]
           (git-database/ref-key db {:path-params {:org (-> repo0 :repo/org :org/name)
                                                   :repo (:repo/name repo0)
                                                   :ref "heads/my-branch"}})))))

(deftest ref-body-test
  (is (= {:ref "refs/heads/my-branch"
          :object {:type :commit
                   :sha "some-sha"}}
         (git-database/ref-body {:ref/ref "refs/heads/my-branch"
                                 :ref/sha "some-sha"}))))

(deftest ref-post-test
  (let [{{:keys [repo0]} :ents db :db} (mock-gen/gen-ents {:repo [[1]]})]
    (is (= {:ref/repo (:db/id repo0)
            :ref/ref "refs/heads/my-branch"
            :ref/sha "some-sha"}
           (git-database/ref-post db {:repo repo0
                                      :body {:ref "refs/heads/my-branch"
                                             :sha "some-sha"}})))))

(deftest ref-patch-test
  (let [{{:keys [repo0 branch0]} :ents db :db} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:ref/ref "refs/heads/my-branch"}}]]})
        {:keys [sha]} (mock-gen/gen-commit (:repo/jgit repo0) (:ref/sha branch0))]
    (is (= {:ref/repo+ref [(:db/id repo0) "refs/heads/my-branch"]
            :ref/sha sha}
           (git-database/ref-patch db {:repo repo0
                                       :path-params {:ref "heads/my-branch"}
                                       :body {:sha sha}})))
    (let [{:keys [sha]} (mock-gen/gen-commit (:repo/jgit repo0))]
      (is (thrown?
           ExceptionInfo
           (git-database/ref-patch db {:repo repo0
                                       :path-params {:ref "heads/my-branch"}
                                       :body {:sha sha}}))))
    (let [{:keys [sha]} (mock-gen/gen-commit (:repo/jgit repo0))]
      (is (= {:ref/repo+ref [(:db/id repo0) "refs/heads/my-branch"]
              :ref/sha sha}
             (git-database/ref-patch db {:repo repo0
                                         :path-params {:ref "heads/my-branch"}
                                         :body {:sha sha
                                                :force true}}))))))
