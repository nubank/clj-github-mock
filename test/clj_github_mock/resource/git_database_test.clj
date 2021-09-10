(ns clj-github-mock.resource.git-database-test
  (:require [clj-github-mock.resource.git-database :as git-database]
            [clojure.test :refer [deftest is]]
            [clj-github-mock.generators :as mock-gen]
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

(deftest get-commit-test
  (let [{{:keys [org0 repo0 commit0]} :ents handler :handler} (mock-gen/gen-ents {:commit [[1 {:spec-gen {:commit/message "message"}}]]})]
    (is (match? {:body {:sha (:commit/sha commit0)
                        :message "message"}}
                (handler (mock/request :get (format "/repos/%s/%s/git/commits/%s" (:org/name org0) (:repo/name repo0) (:commit/sha commit0))))))))

(deftest post-commit-test
  (let [{{:keys [org0 repo0 tree0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})
        {{:keys [sha]} :body} (handler (-> (mock/request :post (format "/repos/%s/%s/git/commits" (:org/name org0) (:repo/name repo0)))
                                           (assoc :body {:tree (:tree/sha tree0)
                                                         :message "message"})))]
    (is (match? {:commit/sha sha}
                (d/pull @conn '[*] [:commit/repo+sha [(:db/id repo0) sha]])))))

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
