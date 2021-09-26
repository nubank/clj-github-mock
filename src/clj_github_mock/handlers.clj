(ns clj-github-mock.handlers
  (:require [datascript.core :as d]
            [malli.core :as malli]
            [malli.error :as malli.error]
            [medley.core :as m])
  (:import [clojure.lang ExceptionInfo]))

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
    (let [error (-> (malli/explain post-schema request)
                    (malli.error/humanize))]
      (if-not error
        (try
          (let [result (post-fn request)]
            {:status 201
             :body (body-fn result)})
          (catch ExceptionInfo e
            {:status 422
             :body {:error (ex-message e)}}))
        {:status 422
         :body error}))))

; TODO return 201 when creating
(defn put-handler [{:keys [put-fn put-schema body-fn] :or {put-schema :any}}]
  (fn [request]
    (let [error (-> (malli/explain put-schema request)
                    (malli.error/humanize))]
      (if-not error
        (try
          (let [result (put-fn request)]
            {:status 200
             :body (body-fn result request)})
          (catch ExceptionInfo e
            {:status 422
             :body {:error (ex-message e)}}))
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
      (let [error (-> (malli/explain patch-schema request)
                      (malli.error/humanize))]
        (if-not error
          (try
            (let [result (patch-fn request)]
              {:status 200
               :body (body-fn result)})
            (catch ExceptionInfo e
              {:status 422
               :body {:error (ex-message e)}}))
          {:status 422
           :body error}))
      {:status 404})))

(defn list-handler [{:keys [list-fn body-fn]}]
  (fn [request]
    (let [objects (list-fn request)
          results (mapv #(body-fn %) objects)]
      {:status 200
       :body results})))

(defn delete-handler [{:keys [lookup-fn delete-fn]}]
  (fn [request]
    (if (lookup-fn request)
      (do
        (delete-fn request)
        {:status 204})
      {:status 404})))

(defn attribute-full-name [attribute]
  (keyword (name (-> attribute :resource/_attributes first :resource/name))
           (name (:attribute/name attribute))))

(defn all-attributes [resource]
  (concat (:resource/attributes resource)
          (if-let [parent (:resource/extends resource)]
            (all-attributes parent)
            [])))

(defn resource-attribute [resource attribute-name]
  (m/find-first
   #(= attribute-name (:attribute/name %))
   (:resource/attributes resource)))

(declare unmarshall-attr)

(defn unmarshall-tupple [resource {:attribute/keys [tuppleAttrs]} db value]
  (mapv (fn [[tupple-attr tupple-val]]
          (unmarshall-attr resource tupple-attr db tupple-val))
        (as-> (map #(resource-attribute resource (keyword (name %))) tuppleAttrs) $
          (interleave $ value)
          (partition 2 $))))

(defn unmarshall-attr [resource {:attribute/keys [tuppleAttrs] :as attribute} db value]
  (cond
    tuppleAttrs (unmarshall-tupple resource attribute db value)
    :else value))

(defn unmarshall [resource db payload]
  (m/map-kv
   (fn [k v]
     (let [attr (resource-attribute resource k)]
       [(attribute-full-name attr)
        (unmarshall-attr resource attr db v)]))
   payload))

(defn resource-transact-fn [resource handler]
  (fn [db request]
    (unmarshall resource db (handler db request))))

(defn post-attribute-schema [{:attribute/keys [name required? schema]}]
  [name {:optional (not required?)} (or schema :any)])

(defn patch-attribute-schema [{:attribute/keys [name schema]}]
  [name {:optional true} (or schema :any)])

(defn payload-schema [resource attribute-fn]
  `[:map
    [:body [:map
            ~@(->> (map attribute-fn (:resource/attributes resource))
                   (remove nil?)
                   (into []))]]])

(defn post-schema [resource]
  (payload-schema resource post-attribute-schema))

(defn patch-schema [resource]
  (payload-schema resource patch-attribute-schema))

(declare resource-body-fn)

(defn marshall-value [{:attribute/keys [ref]} value]
  (cond
    ref (apply (resource-body-fn ref) [value])
    :else value))

(defn marshall-attr [{:attribute/keys [internal? formula cardinality] :as attribute} entity]
  (when-not internal?
    (if formula
      (formula entity)
      (when-let [value (get entity (attribute-full-name attribute))]
        (if (= :db.cardinality/many cardinality)
          (mapv (partial marshall-value attribute) value)
          (marshall-value attribute value))))))

(defn resource-body-fn [resource]
  (fn [entity]
    (reduce
     (fn [result attribute]
       (if-let [v (marshall-attr attribute entity)]
         (assoc result (:attribute/name attribute) v)
         result))
     {}
     (all-attributes resource))))

(defn default-value [default]
  (if (fn? default)
    (default)
    default))

(defn add-defaults [resource handler]
  (fn [db request]
    (let [payload (handler db request)]
      (reduce
       (fn [result {:attribute/keys [name default]}]
         (if (and default (not (get result name)))
           (assoc result name (default-value default))
           result))
       payload
       (:resource/attributes resource)))))

(defn post-resource-handler [meta-db resource-name handler]
  (let [resource (d/entity meta-db [:resource/name resource-name])]
    (post-handler {:post-fn (db-transact-fn (resource-transact-fn resource (add-defaults resource handler))) ; TODO handle auto-gen
                   :post-schema (post-schema resource)
                   :body-fn (resource-body-fn resource)})))

(defn patch-resource-handler [meta-db resource-name key-fn handler]
  (let [resource (d/entity meta-db [:resource/name resource-name])]
    (patch-handler {:patch-fn (db-transact-fn (resource-transact-fn resource handler))
                    :patch-schema (patch-schema resource)
                    :body-fn (resource-body-fn resource)
                    :lookup-fn (db-lookup-fn key-fn)})))

(defn get-resource-handler [meta-db resource-name key-fn]
  (let [resource (d/entity meta-db [:resource/name resource-name])]
    (get-handler {:lookup-fn (db-lookup-fn key-fn)
                  :body-fn (resource-body-fn resource)})))

(defn delete-resource-handler [_meta-db _resource key-fn]
  (delete-handler {:lookup-fn (db-lookup-fn key-fn)
                   :delete-fn (db-delete-fn key-fn)}))

(defn list-resource-handler [meta-db resource-name list-fn]
  (let [resource (d/entity meta-db [:resource/name resource-name])]
    (list-handler {:list-fn (db-list-fn list-fn)
                   :body-fn (resource-body-fn resource)})))
