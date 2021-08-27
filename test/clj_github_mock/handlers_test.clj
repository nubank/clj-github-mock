(ns clj-github-mock.handlers-test
  (:require [clj-github-mock.handlers :as handlers]
            [clj-github-mock.db :as db]
            [clojure.test :refer [deftest is testing]]
            [medley.core :as m]
            [datascript.core :as d]))

(deftest post-handler-test
  (let [meta-db (db/meta-db [{:entity/name :repo
                              :entity/schema {:repo/name {:db/unique :db.unique/identity}}
                              :entity/post-fn (fn [_ {:keys [body]}]
                                                {:repo/name (:name body)
                                                 :repo/data (m/remove-keys #{:name} body)})
                              :entity/post-schema [:map
                                                   [:body [:map
                                                           [:name :string]]]]
                              :entity/body-fn (fn [_ _ repo]
                                                (merge
                                                 {:name (:repo/name repo)}
                                                 (:repo/data repo)))}])
        handler (handlers/post-handler meta-db :repo)]
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
  (let [meta-db (db/meta-db [{:entity/name :repo
                              :entity/schema {:repo/name {:db/unique :db.unique/identity}}
                              :entity/body-fn (fn [_ _ repo]
                                                {:name (:repo/name repo)
                                                 :attr (:repo/attr repo)})
                              :entity/lookup-fn (fn [_ {{:keys [repo]} :path-params}]
                                                  [:repo/name repo])}])
        _ (d/transact! (db/conn meta-db) [{:repo/name "my-repo"
                                           :repo/attr "value"}])
        handler (handlers/get-handler meta-db :repo)]
    (testing "returns result"
      (is (= {:status 200
              :body {:name "my-repo"
                     :attr "value"}}
             (handler {:path-params {:repo "my-repo"}}))))
    (testing "returns 404 if entity does not exist"
      (is (= {:status 404}
             (handler {:path-params {"repo" "unknown"}}))))))

(deftest patch-handler-test
  (let [meta-db (db/meta-db [{:entity/name :repo
                              :entity/schema {:repo/name {:db/unique :db.unique/identity}}
                              :entity/body-fn (fn [_ _ repo]
                                                {:name (:repo/name repo)
                                                 :attr (:repo/attr repo)})
                              :entity/lookup-fn (fn [_ {{:keys [repo]} :path-params}]
                                                  [:repo/name repo])
                              :entity/patch-fn (fn [_ {{:keys [repo]} :path-params
                                                       body :body}]
                                                 {:repo/name repo
                                                  :repo/attr (:attr body)})
                              :entity/patch-schema [:map
                                                    [:body [:map
                                                            [:attr :string]]]]}])
        _ (d/transact! (db/conn meta-db) [{:repo/name "my-repo"
                                           :repo/attr "value"}])
        handler (handlers/patch-handler meta-db :repo)]
    (testing "updates the entity"
      (is (= {:status 200
              :body {:name "my-repo"
                     :attr "changed"}}
             (handler {:path-params {:repo "my-repo"}
                       :body {:attr "changed"}}))))
    (testing "returns 404 if entity does not exist"
      (is (= {:status 404}
             (handler {:path-params {:repo "unknown"}}))))
    (testing "validates the request"
      (is (= {:status 422
              :body {:body {:attr ["missing required key"]}}}
             (handler {:path-params {:repo "my-repo"}
                       :body {}}))))))
