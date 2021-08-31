(ns clj-github-mock.handlers
  (:require [datascript.core :as d]
            [malli.core :as m]
            [malli.error :as me]))

(defn db-transact-fn [transaction-fn]
  (fn [{:keys [conn] :as request}]
    (let [{{:strs [eid]} :tempids db :db-after} (d/transact! conn [[:db.fn/call #(vector (merge
                                                                                          (transaction-fn % request)
                                                                                          {:db/id "eid"}))]])]
      (d/entity db eid))))

(defn db-lookup-fn [key-fn]
  (fn [{:keys [conn] :as request}]
    (let [db (d/db conn)]
      (d/entity db (key-fn db request)))))

(defn db-list-fn [query-fn]
  (fn [{:keys [conn] :as request}]
    (let [db (d/db conn)]
      (query-fn db request))))

(defn db-delete-fn [key-fn]
  (fn [{:keys [conn] :as request}]
    (let [db (d/db conn)]
      (d/transact! conn [[:db.fn/retractEntity (key-fn db request)]]))))

(defn post-handler [{:keys [post-fn post-schema body-fn] :or {post-schema :any}}]
  (fn [request]
    (let [error (-> (m/explain post-schema request)
                    (me/humanize))]
      (if-not error
        (let [result (post-fn request)]
          {:status 201
           :body (body-fn result)})
        {:status 422
         :body error}))))

(defn get-handler [{:keys [lookup-fn body-fn]}]
  (fn [request]
    (let [object (lookup-fn request)]
      (if object
        {:status 200
         :body (body-fn object)}
        {:status 404}))))

(defn patch-handler [{:keys [lookup-fn patch-fn patch-schema body-fn] :or {patch-schema :any}}]
  (fn [request]
    (if (lookup-fn request)
      (let [error (-> (m/explain patch-schema request)
                      (me/humanize))]
        (if-not error
          (let [result (patch-fn request)]
            {:status 200
             :body (body-fn result)})
          {:status 422
           :body error}))
      {:status 404})))

(defn list-handler [{:keys [list-fn body-fn]}]
  (fn [request]
    (let [objects (list-fn request)
          results (mapv #(body-fn %) objects)]
      {:status 200
       :body results})))

(defn delete-handler [{:keys [delete-fn]}]
  (fn [request]
    (delete-fn request)
    {:status 204}))
