(ns clj-github-mock.http-test
  (:require [clj-github-mock.http :as http]
            [clj-github-mock.db :as db]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.clj-test]
            [matcher-combinators.matchers :as m]))

(deftest routes-test
  (is (= [["/parent" {}]]
         (http/routes (db/meta-db [{:api/name "parent"
                                    :api/path "/parent"}]))))
  (is (= [["/parent" {}
           ["/child" {}]]]
         (http/routes (db/meta-db [{:api/name "parent"
                                    :api/path "/parent"}
                                   {:api/name "child"
                                    :api/path "/child"
                                    :api/parent {:api/name "parent"}}]))))
  (is (= [["/parent" {}
           ["/child1" {}]
           ["/child2" {}]]]
         (http/routes (db/meta-db [{:api/name "parent"
                                    :api/path "/parent"}
                                   {:api/name "child1"
                                    :api/path "/child1"
                                    :api/parent {:api/name "parent"}}
                                   {:api/name "child2"
                                    :api/path "/child2"
                                    :api/parent {:api/name "parent"}}]))))
  (is (= [["/parent" {:middleware []}]]
         (http/routes (db/meta-db [{:api/name "parent"
                                    :api/path "/parent"
                                    :api/config {:middleware []}}])))))
