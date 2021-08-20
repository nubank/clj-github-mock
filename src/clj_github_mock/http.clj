(ns clj-github-mock.http
  (:require [datascript.core :as d]))

(defn api-interceptor [api]
  {:name ::api-interceptor
   :api (into {} (d/touch api))
   :enter (fn [context]
            (update context :request assoc :api api))})

(defn- api-routes [api]
  (apply conj
         [(:api/path api) (-> api
                              (get :api/config {})
                              (update :interceptors #(into [(api-interceptor api)] %)))]
         (concat
          (:api/routes api)
          (map api-routes (sort-by :db/id (:api/_parent api))))))

(defn routes [db]
  (map #(api-routes (d/entity db %)) 
       (d/q '[:find [?a ...] 
              :where
              [?a :api/name _]
              [(missing? $ ?a :api/parent)]]
            db)))
