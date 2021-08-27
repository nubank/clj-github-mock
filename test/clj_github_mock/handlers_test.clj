(ns clj-github-mock.handlers-test
  (:require [clj-github-mock.handlers :as handlers]
            [clj-github-mock.db :as db]
            [clojure.test :refer [deftest is testing]]
            [medley.core :as m]
            [matcher-combinators.test :refer [match?]]))

(deftest post-handler-test
  (let [meta-db (db/meta-db [{:entity/name :repo
                              :entity/schema {:repo/name {:db/unique :db.unique/identity}}
                              :entity/post-fn (fn [_ {:keys [body]}]
                                                {:repo/name (:name body) 
                                                 :repo/data (m/remove-keys #{:name} body)})
                              :entity/post-schema [:map
                                                   [:body [:map
                                                           [:name :string]]]]
                              :entity/body-fn (fn [_ repo]
                                                (merge
                                                 {:name (:repo/name repo)}
                                                 (:repo/data repo)))}])]
    (testing "creates entity"
      (let [handler (handlers/post-handler meta-db :repo)]
        (is (= {:status 201
                :body {:name "my-repo"
                       :some-attr "some-value"}}
               (handler {:body {:name "my-repo"
                                :some-attr "some-value"}})))))
    (testing "validates request"
      (let [handler (handlers/post-handler meta-db :repo)]
        (is (match? {:status 422
                     :body {:name ["missing required key"]}}
               (handler {:body {:some-attr "some-value"}})))))))
