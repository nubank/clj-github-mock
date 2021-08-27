(ns clj-github-mock.handlers
  (:require [datascript.core :as d]
            [clj-github-mock.db :as db]
            [malli.core :as m]
            [malli.error :as me]))

(defn post-handler [meta-db ent-name]
  (fn [request]
    (let [conn (db/conn meta-db) 
          entity (d/entity meta-db [:entity/name ent-name])
          post-schema (or (:entity/post-schema entity) :any)
          error (-> (m/explain post-schema request)
                            (me/humanize))]
      (if-not error
        (let [post-fn (:entity/post-fn entity)
              body-fn (:entity/body-fn entity)
              {{:strs [eid]} :tempids db :db-after} (d/transact! conn [[:db.fn/call #(vector (merge
                                                                                              (post-fn % request)
                                                                                              {:db/id "eid"}))]])]
          {:status 201
           :body (body-fn meta-db db (d/entity db eid))})
        (merge 
         {:status 422}
         error)))))

(defn get-handler [meta-db ent-name]
  (fn [request]
    (let [conn (db/conn meta-db)
          entity (d/entity meta-db [:entity/name ent-name])
          db @conn
          lookup-fn (:entity/lookup-fn entity)
          object (d/entity db (lookup-fn db request))
          body-fn (:entity/body-fn entity)]
      (if object
        {:status 200
         :body (body-fn meta-db db object)}
        {:status 404}))))
