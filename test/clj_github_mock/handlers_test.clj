(ns clj-github-mock.handlers-test
  (:require [clj-github-mock.handlers :as handlers]
            [clojure.test :refer [deftest is testing]]
            [datascript.core :as d]))

(deftest db-transact-fn-test
  (let [conn (d/create-conn {:db/ident {:db/unique :db.unique/identity}})
        transaction-fn (fn [_ {:keys [body]}]
                         {:db/ident :entity
                          :body body})
        transact-fn (handlers/db-transact-fn transaction-fn)
        result (transact-fn {:conn conn
                             :body "body"})
        expected (d/entity @conn [:db/ident :entity])]
    (is (= expected result))))

(deftest db-lookup-fn-test
  (let [conn (d/create-conn {:db/ident {:db/unique :db.unique/identity}})
        key-fn (fn [_ {:keys [key]}] key)
        lookup-fn (handlers/db-lookup-fn key-fn)]
    (d/transact! conn [{:db/ident :entity}])
    (is (= (d/entity @conn [:db/ident :entity])
           (lookup-fn {:conn conn
                       :key [:db/ident :entity]})))))

(deftest db-list-fn-test
  (let [conn (d/create-conn {})
        query-fn (fn [db request]
                   [db request])
        list-fn (handlers/db-list-fn query-fn)]
    (is (= [@conn {:conn conn}]
           (list-fn {:conn conn})))))

(deftest db-delete-fn-test
  (let [conn (d/create-conn {:db/ident {:db/unique :db.unique/identity}})
        key-fn (fn [_ {:keys [key]}] key)
        delete-fn (handlers/db-delete-fn key-fn)]
    (d/transact conn [{:db/ident :entity}])
    (delete-fn {:conn conn :key [:db/ident :entity]})
    (is (nil? (d/entity @conn [:db/ident :entity])))))

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
             (handler {:body {:some-attr "some-value"}})))))
  (testing "catches ex-info"
    (let [handler (handlers/post-handler {:post-fn (fn [_]
                                                     (throw (ex-info "failure" {})))})]
      (is (= {:status 422
              :body {:error "failure"}}
             (handler {})))))
  (testing "does not catch other exceptions"
    (let [handler (handlers/post-handler {:post-fn (fn [_]
                                                     (throw (RuntimeException.)))})]
      (is (thrown?
           RuntimeException
           (handler {}))))))

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
                       :body {}})))))
  (testing "catches ex-info"
    (let [handler (handlers/patch-handler {:lookup-fn (constantly true)
                                           :patch-fn (fn [_]
                                                       (throw (ex-info "failure" {})))})]
      (is (= {:status 422
              :body {:error "failure"}}
             (handler {})))))
  (testing "does not catch other exceptions"
    (let [handler (handlers/patch-handler {:lookup-fn (constantly true)
                                           :patch-fn (fn [_]
                                                       (throw (RuntimeException.)))})]
      (is (thrown?
           RuntimeException
           (handler {}))))))

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
        handler (handlers/delete-handler {:lookup-fn (fn [{{:keys [repo]} :path-params}]
                                                       (get @repos repo))
                                          :delete-fn (fn [{{:keys [repo]} :path-params}]
                                                       (swap! repos dissoc repo))})]
    (testing "deletes entity"
      (is (= {:status 204}
             (handler {:path-params {:repo "repo"}})))
      (is (empty? @repos)))
    (testing "404 if entity does not exist"
      (is (= {:status 404}
             (handler {:path-params {:repo "unknown"}}))))))
