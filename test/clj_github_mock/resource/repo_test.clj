(ns clj-github-mock.resource.repo-test
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.generators :as mock-gen]
            [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match?]]
            [matcher-combinators.matchers :as matchers]
            [base64-clj.core :as base64]
            [ring.mock.request :as mock]
            [datascript.core :as d]))

(deftest list-repo-test
  (let [{{:keys [org0 repo0 repo1]} :ents handler :handler} (mock-gen/gen-ents {:repo [[2]]})]
    (is (match? {:body (matchers/in-any-order [{:id (:repo/id repo0)}
                                               {:id (:repo/id repo1)}])}
                (handler (mock/request :get (format "/orgs/%s/repos" (:org/name org0))))))))

(deftest create-repo-test
  (testing "defaults"
    (let [{{:keys [org0]} :ents handler :handler conn :conn} (mock-gen/gen-ents {:org [[1]]})]
      (handler (-> (mock/request :post (format "/orgs/%s/repos" (:org/name org0)))
                   (assoc :body {:name "my-repo"})))
      (is (match? {:repo/name "my-repo"
                   :repo/default_branch "main"}
                  (d/pull @conn '[*] [:repo/org+name [(:db/id org0) "my-repo"]])))))
  (testing "full_name"
    (let [{{:keys [org0]} :ents handler :handler conn :conn} (mock-gen/gen-ents {:org [[1 {:spec-gen {:org/name "nubank"}}]]})]
      (handler (-> (mock/request :post (format "/orgs/%s/repos" (:org/name org0)))
                   (assoc :body {:name "my-repo"})))
      (is (match? {:repo/full_name "nubank/my-repo"}
                  (d/pull @conn '[*] [:repo/org+name [(:db/id org0) "my-repo"]]))))))

(deftest get-repo-test
  (let [{{:keys [org0 repo0]} :ents handler :handler} (mock-gen/gen-ents {:repo [[1]]})]
    (is (match?
         {:body {:id (:repo/id repo0)}}
         (handler (mock/request :get (format "/repos/%s/%s" (:org/name org0) (:repo/name repo0))))))))

(deftest patch-repo-test
  (let [{{:keys [org0 repo0]} :ents handler :handler conn :conn} (mock-gen/gen-ents {:repo [[1]]})]
    (handler (-> (mock/request :patch (format "/repos/%s/%s" (:org/name org0) (:repo/name repo0)))
                 (assoc :body {:default_branch "some-branch"})))
    (is (match? {:repo/default_branch "some-branch"}
                (d/pull @conn '[*] (:db/id repo0))))))

(deftest get-branch-test
  (let [{{:keys [org0 repo0 branch0]} :ents handler :handler} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:ref/ref "refs/heads/my-branch"}}]]})]
    (is (match? {:body {:name "my-branch"}}
                (handler (mock/request :get (format "/repos/%s/%s/branches/my-branch" (:org/name org0) (:repo/name repo0))))))))

(deftest content-lookup-test
  (let [{{:keys [repo0 branch0]} :ents conn :conn} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/content [{:path "some-file" :mode "100644" :type "blob" :content "some-content"}]}}]]})]
    (is (= {:repo repo0
            :sha (:ref/sha branch0)
            :path "some-file"}
           (repo/content-lookup {:repo repo0
                                 :conn conn
                                 :path-params {:path "some-file"}
                                 :query-params {"ref" (:ref/sha branch0)}})))
    (is (= {:repo repo0
            :sha (:ref/sha branch0)
            :path "some-file"}
           (repo/content-lookup {:repo repo0
                                 :conn conn
                                 :path-params {:path "some-file"}
                                 :query-params {"ref" (second (re-find #"refs/heads/(.*)" (:ref/ref branch0)))}})))
    (is (nil? (repo/content-lookup {:repo repo0
                                    :conn conn
                                    :path-params {:path "some-file"}
                                    :query-params {"ref" "unknown-branch"}})))
    (is (nil? (repo/content-lookup {:repo repo0
                                    :conn conn
                                    :path-params {:path "unknown-file"}
                                    :query-params {"ref" (:ref/sha branch0)}})))))

(deftest content-body-test
  (let [{{:keys [repo0 branch0]} :ents} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/content [{:path "some-file" :mode "100644" :type "blob" :content "some-content"}]}}]]})]
    (is (= {:type "file"
            :path "some-file"
            :content (base64/encode "some-content" "UTF-8")}
           (repo/content-body {:repo repo0
                               :sha (:ref/sha branch0)
                               :path "some-file"})))))
