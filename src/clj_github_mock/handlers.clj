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
          error #tap (-> (m/explain post-schema request)
                            (me/humanize))]
      (if-not error
        (let [post-fn (:entity/post-fn entity)
              body-fn (:entity/body-fn entity)
              {{:strs [eid]} :tempids db :db-after} (d/transact! conn [[:db.fn/call #(vector (merge
                                                                                              (post-fn % request)
                                                                                              {:db/id "eid"}))]])]
          {:status 201
           :body (body-fn meta-db (d/entity db eid))})
        (merge 
         {:status 422}
         error)))))
