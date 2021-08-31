(ns clj-github-mock.handlers-test
  (:require [clj-github-mock.handlers :as handlers]
            [clojure.test :refer [deftest is testing]]))

(deftest post-handler-test
  (let [handler (handlers/post-handler {:post-fn :body 
                                        :post-schema [:map
                                                      [:body [:map
                                                              [:name :string]]]]
                                        :body-fn identity})]
    (testing "creates entity"
      (is (= {:status 201
              :body {:name "my-repo"
                     :some-attr "some-value"}}
             (handler {:body {:name "my-repo"
                              :some-attr "some-value"}}))))
    (testing "validates request"
      (is (= {:status 422
              :body {:body {:name ["missing required key"]}}}
             (handler {:body {:some-attr "some-value"}}))))))

(deftest get-handler-test
  (let [handler (handlers/get-handler {:body-fn identity
                                       :lookup-fn (fn [{{:keys [repo]} :path-params}]
                                                    (get {"my-repo" {:attr "value"}} repo))})]
    (testing "returns result"
      (is (= {:status 200
              :body {:attr "value"}}
             (handler {:path-params {:repo "my-repo"}}))))
    (testing "returns 404 if entity does not exist"
      (is (= {:status 404}
             (handler {:path-params {"repo" "unknown"}}))))))

(deftest patch-handler-test
  (let [repos {"my-repo" {:attr1 "value1"
                          :attr2 "value2"}}
        handler (handlers/patch-handler {:body-fn identity
                                         :lookup-fn (fn [{{:keys [repo]} :path-params}]
                                                      (get repos repo))
                                         :patch-schema [:map
                                                        [:body [:map
                                                                [:attr1 :string]]]]
                                         :patch-fn (fn [{{:keys [repo]} :path-params
                                                         body :body}]
                                                      (merge (get repos repo) body))})]
    (testing "updates the entity"
      (is (= {:status 200
              :body {:attr1 "changed"
                     :attr2 "value2"}}
             (handler {:path-params {:repo "my-repo"}
                       :body {:attr1 "changed"}}))))
    (testing "returns 404 if entity does not exist"
      (is (= {:status 404}
             (handler {:path-params {:repo "unknown"}}))))
    (testing "validates the request"
      (is (= {:status 422
              :body {:body {:attr1 ["missing required key"]}}}
             (handler {:path-params {:repo "my-repo"}
                       :body {}}))))))

(deftest list-handler-test
  (let [handler (handlers/list-handler {:list-fn (fn [_]
                                                   [{:name "repo1"}
                                                    {:name "repo2"}])
                                        :body-fn identity})]
    (testing "return results"
      (is (= {:status 200
              :body [{:name "repo1"}
                     {:name "repo2"}]}
             (handler {}))))))

(deftest delete-handler-test
  (let [repos (atom {"repo" {:attr "value"}})
        handler (handlers/delete-handler {:delete-fn (fn [{{:keys [repo]} :path-params}]
                                                       (swap! repos dissoc repo))})]
    (testing "deletes entity"
      (is (= {:status 204}
             (handler {:path-params {:repo "repo"}})))
      (is (empty? @repos)))))
