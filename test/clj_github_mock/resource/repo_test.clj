(ns clj-github-mock.resource.repo-test
  (:require [clj-github-mock.resource.repo :as repo]
            [clj-github-mock.generators :as mock-gen]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.test :refer [match?]]
            [matcher-combinators.matchers :as matchers]
            [datascript.core :as d]
            [base64-clj.core :as base64]))

(deftest repo-key-test
  (let [{:keys [org0 db]} (mock-gen/gen-ents {:org [[1]]})]
    (is (= [:repo/name+org ["repo" (:db/id org0)]]
           (repo/repo-key db {:path-params {:org (:org/name org0)
                                            :repo "repo"}})))))

(deftest repo-body-test
  (is (match? {:name "repo"
               :attr "value"}
              (repo/repo-body {:repo/name "repo"
                               :repo/attrs {:attr "value"}})))
  (is (match? {:full_name "org/repo"}
              (repo/repo-body {:repo/name "repo"
                               :repo/org {:org/name "org"}}))))

(deftest repo-post-test
  (is (match? {:repo/name "repo"
               :repo/org [:org/name "org"]
               :repo/jgit (fn [e] (instance? org.eclipse.jgit.lib.Repository e))}
              (repo/repo-post nil {:path-params {:org "org"}
                                   :body {:name "repo"}})))
  (is (match? {:repo/attrs {:default_branch "main"}}
              (repo/repo-post nil {:body {}})))
  (is (match? {:repo/attrs {:default_branch "some-branch"}}
              (repo/repo-post nil {:body {:default_branch "some-branch"}})))
  (is (match? {:repo/attrs {:name matchers/absent}}
              (repo/repo-post nil {:body {:name "repo"}}))))

(deftest repo-patch-test
  (let [{:keys [org0 db]} (mock-gen/gen-ents {:org [[:org0]]})]
    (is (match? {:repo/name+org ["repo" (:db/id org0)]}
                (repo/repo-patch db {:path-params {:org (:org/name org0)
                                                   :repo "repo"}}))))
  (let [{:keys [org0 repo0 db]} (mock-gen/gen-ents {:repo [[1 {:spec-gen {:repo/attrs {:some-attr "value"}}}]]})]
    (is (match? {:repo/attrs {:some-attr "value"}}
                (repo/repo-patch db {:path-params {:org (:org/name org0)
                                                   :repo (:repo/name repo0)}}))))
  (let [{:keys [org0 repo0 db]} (mock-gen/gen-ents {:repo [[1 {:spec-gen {:repo/attrs {:some-attr "value"
                                                                                       :another-attr "value"}}}]]})]
    (is (match? {:repo/attrs {:some-attr "value"
                              :another-attr "changed"}}
                (repo/repo-patch db {:path-params {:org (:org/name org0)
                                                   :repo (:repo/name repo0)}
                                     :body {:another-attr "changed"}})))))

(deftest repo-list-test
  (let [{:keys [org0 repo0 repo1 db]} (mock-gen/gen-ents {:repo [[2]]})]
    (is (= (set (map :db/id [repo0 repo1])) 
           (set (map :db/id (repo/repo-list db {:path-params {:org (:org/name org0)}})))))))

(deftest branch-key-test
  (let [{:keys [repo0 db]} (mock-gen/gen-ents {:repo [[1]]})]
    (is (= [:ref/repo+ref [(:db/id repo0) "refs/heads/my-branch"]]
           (repo/branch-key db {:path-params {:org (-> repo0 :repo/org :org/name)
                                              :repo (:repo/name repo0)
                                              :branch "my-branch"}})))))

(deftest branch-body-test
  (is (match? {:name "my-branch"
               :commit {:sha "some-sha"}}
              (repo/branch-body {:ref/ref "refs/heads/my-branch"
                                 :ref/sha "some-sha"}))))

(deftest content-lookup-test
  (let [{:keys [repo0 branch0 database]} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/content [{:path "some-file" :mode "100644" :type "blob" :content "some-content"}]}}]]})]
    (is (= {:repo repo0
            :sha (:ref/sha branch0)
            :path "some-file"}
           (repo/content-lookup {:repo repo0
                                 :conn database
                                 :path-params {:path "some-file"}
                                 :query-params {"ref" (:ref/sha branch0)}})))
    (is (= {:repo repo0
            :sha (:ref/sha branch0)
            :path "some-file"}
           (repo/content-lookup {:repo repo0
                                 :conn database
                                 :path-params {:path "some-file"}
                                 :query-params {"ref" (second (re-find #"refs/heads/(.*)" (:ref/ref branch0)))}})))
    (is (nil? (repo/content-lookup {:repo repo0
                                    :conn database
                                    :path-params {:path "some-file"}
                                    :query-params {"ref" "unknown-branch"}})))
    (is (nil? (repo/content-lookup {:repo repo0
                                    :conn database
                                    :path-params {:path "unknown-file"}
                                    :query-params {"ref" (:ref/sha branch0)}})))))

(deftest content-body-test
  (let [{:keys [repo0 branch0]} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/content [{:path "some-file" :mode "100644" :type "blob" :content "some-content"}]}}]]})]
    (is (= {:type "file"
            :path "some-file"
            :content (base64/encode "some-content" "UTF-8")}
           (repo/content-body {:repo repo0
                               :sha (:ref/sha branch0)
                               :path "some-file"})))))
