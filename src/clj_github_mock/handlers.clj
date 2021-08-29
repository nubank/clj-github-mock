(ns clj-github-mock.handlers
  (:require [datascript.core :as d]
            [clj-github-mock.db :as db]
            [malli.core :as m]
            [malli.error :as me]))

(defn post-handler [meta-db resource-name]
  (fn [request]
    (let [conn (db/conn meta-db)
          post-schema (or (db/post-schema meta-db resource-name) :any)
          error (-> (m/explain post-schema request)
                    (me/humanize))]
      (if-not error
        (let [post-fn (db/post-fn meta-db resource-name)
              body-fn (db/body-fn meta-db resource-name) 
              {{:strs [eid]} :tempids db :db-after} (d/transact! conn [[:db.fn/call #(vector (merge
                                                                                              (post-fn % request)
                                                                                              {:db/id "eid"}))]])]
          {:status 201
           :body (body-fn meta-db db (d/entity db eid))})
        {:status 422
         :body error}))))

(defn get-handler [meta-db resource-name]
  (fn [request]
    (let [conn (db/conn meta-db)
          db @conn
          lookup-fn (db/lookup-fn meta-db resource-name)
          object (d/entity db (lookup-fn db request))
          body-fn (db/body-fn meta-db resource-name)]
      (if object
        {:status 200
         :body (body-fn meta-db db object)}
        {:status 404}))))

(defn patch-handler [meta-db resource-name]
  (fn [request]
    (let [conn (db/conn meta-db)
          lookup-fn (db/lookup-fn meta-db resource-name)
          db @conn]
      (if (d/entid db (lookup-fn db request))
        (let [patch-schema (or (db/patch-schema meta-db resource-name) :any)
              error (-> (m/explain patch-schema request)
                        (me/humanize))]
          (if-not error
            (let [patch-fn (db/patch-fn meta-db resource-name)
                  body-fn (db/body-fn meta-db resource-name)
                  {{:strs [eid]} :tempids db-after :db-after} (d/transact! conn [[:db.fn/call #(vector (merge (patch-fn % request)
                                                                                                              {:db/id "eid"}))]])]
              {:status 200
               :body (body-fn meta-db db (d/entity db-after eid))})
            {:status 422
             :body error}))
        {:status 404}))))

(defn list-handler [meta-db resource-name]
  (fn [request]
    (let [conn (db/conn meta-db)
          db @conn
          list-fn (db/list-fn meta-db resource-name)
          body-fn (db/body-fn meta-db resource-name)
          objects (list-fn db request)
          results (mapv #(body-fn meta-db db %) objects)]
      {:status 200
       :body results})))
