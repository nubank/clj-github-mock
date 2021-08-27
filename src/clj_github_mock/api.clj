(ns clj-github-mock.api
  (:require [clj-github-mock.api.repos :as repos]
            [clj-github-mock.db :as db]
            [clj-github-mock.http :as http]
            [clj-github-mock.impl.database :as database]
            [reitit.ring :as ring]
            [ring.middleware.params :as middleware.params]))

(defn handler [database]
  (let [meta-db (db/meta-db repos/model)]
    (-> (ring/ring-handler
         (ring/router (http/routes meta-db))
         (ring/create-default-handler))
        (middleware.params/wrap-params)
        (database/middleware database))))
