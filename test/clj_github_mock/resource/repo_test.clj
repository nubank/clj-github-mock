(ns clj-github-mock.resource.repo-test
  (:require [clj-github-mock.generators :as mock-gen]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]
            [matcher-combinators.matchers :as matchers]
            [ring.mock.request :as mock]
            [datascript.core :as d]))

(deftest list-repo-test
  (let [{{:keys [owner0 repo0 repo1]} :ents handler :handler} (mock-gen/gen-ents {:repo [[2]]})]
    (is (match? {:body (matchers/in-any-order [{:node_id (:repo/node_id repo0)}
                                               {:node_id (:repo/node_id repo1)}])}
                (handler (mock/request :get (format "/orgs/%s/repos" (:owner/name owner0))))))))

(deftest create-repo-test
  (testing "defaults"
    (let [{{:keys [owner0]} :ents handler :handler conn :conn} (mock-gen/gen-ents {:owner [[1]]})]
      (handler (-> (mock/request :post (format "/orgs/%s/repos" (:owner/name owner0)))
                   (assoc :body {:name "my-repo"})))
      (is (match? {:repo/name "my-repo"
                   :repo/default_branch "main"}
                  (d/pull @conn '[*] [:repo/owner+name [(:db/id owner0) "my-repo"]]))))))

(deftest get-repo-test
  (let [{{:keys [owner0 repo0]} :ents handler :handler} (mock-gen/gen-ents {:owner [[1 {:spec-gen {:owner/name "nubank"}}]]
                                                                          :repo [[1 {:spec-gen {:repo/name "my-repo"}}]]})]
    (is (match?
         {:body {:node_id (:repo/node_id repo0)
                 :full_name "nubank/my-repo"}}
         (handler (mock/request :get (format "/repos/%s/%s" (:owner/name owner0) (:repo/name repo0))))))))

(deftest patch-repo-test
  (let [{{:keys [owner0 repo0]} :ents handler :handler conn :conn} (mock-gen/gen-ents {:repo [[1]]})]
    (handler (-> (mock/request :patch (format "/repos/%s/%s" (:owner/name owner0) (:repo/name repo0)))
                 (assoc :body {:default_branch "some-branch"})))
    (is (match? {:repo/default_branch "some-branch"}
                (d/pull @conn '[*] (:db/id repo0))))))

(deftest get-branch-test
  (let [{{:keys [owner0 repo0]} :ents handler :handler} (mock-gen/gen-ents {:ref [[1 {:spec-gen {:ref/name "my-branch"
                                                                                                 :ref/type :branch}}]]})]
    (is (match? {:body {:name "my-branch"}}
                (handler (mock/request :get (format "/repos/%s/%s/branches/my-branch" (:owner/name owner0) (:repo/name repo0))))))))

(deftest get-content-test
  (let [{{:keys [owner0 repo0 ref0]} :ents handler :handler} (mock-gen/gen-ents {:ref [[1 {:spec-gen {:ref/name "main"
                                                                                                         :ref/type :branch}}]]
                                                                                  :tree [[1 {:spec-gen {:tree/content [{:path "some-file" :mode "100644" :type "blob" :content "some-content"}]}}]]})]
    (is (match? {:body {:type "file"
                        :path "some-file"
                        :content "c29tZS1jb250ZW50"}}
                (handler (mock/request :get (format "/repos/%s/%s/contents/some-file" (:owner/name owner0) (:repo/name repo0))))))
    (is (match? {:body {:type "file"
                        :path "some-file"
                        :content "c29tZS1jb250ZW50"}}
                (handler (-> (mock/request :get (format "/repos/%s/%s/contents/some-file" (:owner/name owner0) (:repo/name repo0)))
                             (mock/query-string {"ref" (:object/sha (:ref/commit ref0))})))))
    (is (match? {:body {:type "file"
                        :path "some-file"
                        :content "c29tZS1jb250ZW50"}}
                (handler (-> (mock/request :get (format "/repos/%s/%s/contents/some-file" (:owner/name owner0) (:repo/name repo0)))
                             (mock/query-string {"ref" "main"})))))))
