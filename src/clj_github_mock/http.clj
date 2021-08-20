(ns clj-github-mock.http
  (:require [datascript.core :as d]))

(defn- api-routes [api]
  (apply conj
         [(:api/path api) (get api :api/config {})]
         (map api-routes (sort-by :db/id (:api/_parent api)))))

(defn routes [db]
  (map #(api-routes (d/entity db %)) 
       (d/q '[:find [?a ...] 
              :where
              [?a :api/name _]
              [(missing? $ ?a :api/parent)]]
            db)))
