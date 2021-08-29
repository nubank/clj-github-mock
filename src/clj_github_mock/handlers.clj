(ns clj-github-mock.handlers
  (:require [datascript.core :as d]
            [clj-github-mock.db :as db]
            [malli.core :as m]
            [malli.error :as me]))

(defn db-transact-fn [transaction-fn]
  (fn [meta-db request]
    (let [conn (db/conn meta-db)
          {{:strs [eid]} :tempids db :db-after} (d/transact! conn [[:db.fn/call #(vector (merge
                                                                                              (transaction-fn % request)
                                                                                              {:db/id "eid"}))]])]
      (d/entity db eid))))

(defn db-lookup-fn [key-fn]
  (fn [meta-db request]
    (let [conn (db/conn meta-db)
          db (d/db conn)]
      (d/entity db (key-fn db request)))))

(defn db-list-fn [query-fn]
  (fn [meta-db request]
    (let [conn (db/conn meta-db)
          db (d/db conn)]
      (query-fn db request))))

(defn db-delete-fn [key-fn]
  (fn [meta-db request]
    (let [conn (db/conn meta-db)
          db (d/db conn)]
      (d/transact! conn [[:db.fn/retractEntity (key-fn db request)]]))))

(defn post-handler [meta-db resource-name]
  (fn [request]
    (let [post-schema (or (db/post-schema meta-db resource-name) :any)
          error (-> (m/explain post-schema request)
                    (me/humanize))]
      (if-not error
        (let [post-fn (db/post-fn meta-db resource-name)
              body-fn (db/body-fn meta-db resource-name) 
              result (post-fn meta-db request)]
          {:status 201
           :body (body-fn meta-db result)})
        {:status 422
         :body error}))))

(defn get-handler [meta-db resource-name]
  (fn [request]
    (let [lookup-fn (db/lookup-fn meta-db resource-name)
          object (lookup-fn meta-db request)
          body-fn (db/body-fn meta-db resource-name)]
      (if object
        {:status 200
         :body (body-fn meta-db object)}
        {:status 404}))))

(defn patch-handler [meta-db resource-name]
  (fn [request]
    (let [lookup-fn (db/lookup-fn meta-db resource-name)]
      (if (lookup-fn meta-db request)
        (let [patch-schema (or (db/patch-schema meta-db resource-name) :any)
              error (-> (m/explain patch-schema request)
                        (me/humanize))]
          (if-not error
            (let [patch-fn (db/patch-fn meta-db resource-name)
                  body-fn (db/body-fn meta-db resource-name)
                  result (patch-fn meta-db request)]
              {:status 200
               :body (body-fn meta-db result)})
            {:status 422
             :body error}))
        {:status 404}))))

(defn list-handler [meta-db resource-name]
  (fn [request]
    (let [list-fn (db/list-fn meta-db resource-name)
          body-fn (db/body-fn meta-db resource-name)
          objects (list-fn meta-db request)
          results (mapv #(body-fn meta-db %) objects)]
      {:status 200
       :body results})))

(defn delete-handler [meta-db resource-name]
  (fn [request]
    (let [delete-fn (db/delete-fn meta-db resource-name)]
      (delete-fn meta-db request)
      {:status 204})))
