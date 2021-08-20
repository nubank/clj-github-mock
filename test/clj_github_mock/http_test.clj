(ns clj-github-mock.http-test
  (:require [clj-github-mock.http :as http]
            [clj-github-mock.db :as db]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.clj-test]))

(deftest routes-test
  (is (match? [["/parent" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "parent"}}]}]]
              (http/routes (db/meta-db [{:api/name "parent"
                                         :api/path "/parent"}]))))
  (is (match? [["/parent" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "parent"}}]}
                ["/child" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "child"}}]}]]]
              (http/routes (db/meta-db [{:api/name "parent"
                                         :api/path "/parent"}
                                        {:api/name "child"
                                         :api/path "/child"
                                         :api/parent {:api/name "parent"}}]))))
  (is (match? [["/parent" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "parent"}}]}
                ["/child1" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "child1"}}]}]
                ["/child2" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "child2"}}]}]]]
              (http/routes (db/meta-db [{:api/name "parent"
                                         :api/path "/parent"}
                                        {:api/name "child1"
                                         :api/path "/child1"
                                         :api/parent {:api/name "parent"}}
                                        {:api/name "child2"
                                         :api/path "/child2"
                                         :api/parent {:api/name "parent"}}]))))
  (is (match? [["/parent" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "parent"}}
                                          :another-interceptor]}]]
              (http/routes (db/meta-db [{:api/name "parent"
                                         :api/path "/parent"
                                         :api/config {:interceptors [:another-interceptor]}}]))))
  (is (match? [["/parent" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "parent"}}]}
                ["/sub-route" ]
                ["/child" {:interceptors [{:name ::http/api-interceptor
                                           :api {:api/name "child"}}]}]]]
              (http/routes (db/meta-db [{:api/name "parent"
                                         :api/path "/parent"
                                         :api/routes [["/sub-route"]]}
                                        {:api/name "child"
                                         :api/path "/child"
                                         :api/parent {:api/name "parent"}}])))))
