(ns clj-github-mock.generators
  (:require [clojure.test.check.generators :as gen]
            [malli.generator :as malli-gen]
            [medley.core :as m]
            [clj-github-mock.resource :as resource]
            [clj-github-mock.handlers :as handlers]
            [reifyhealth.specmonstah.core :as sm]
            [reifyhealth.specmonstah.spec-gen :as sg]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.random :as random]
            [datascript.core :as d]))

(defn- spec-gen-ent-val
  [{{:keys [rnd-state size]} :gen-options :as ent-db} {:keys [ent-name]}]
  (let [{:keys [generator]} (sm/ent-schema ent-db ent-name)
        [rnd1 rnd2] (random/split @rnd-state)]
    (reset! rnd-state rnd2)
    (rose/root (gen/call-gen generator rnd1 size))))

(def ^:private spec-gen [spec-gen-ent-val
                         sg/spec-gen-merge-overwrites
                         sg/spec-gen-assoc-relations])

(defn- ent-db-malli-gen
  [ent-db query]
  (-> ent-db
      (sm/add-ents query)
      (sm/visit-ents-once :spec-gen spec-gen)))

(defn- foreign-key-ent [[_ foreign-key-attr :as path] foreign-key-val]
  (cond
    ; TODO: use constraints to detect if it is a multi relationship
    (vector? foreign-key-val) (set (map (partial foreign-key-ent path) foreign-key-val))
    :else [foreign-key-attr foreign-key-val]))

(defn- assoc-ent-at-foreign-keys [db {:keys [ent-type spec-gen]}]
  (reduce
   (fn [acc [attr relation-path]]
     (update acc attr (partial foreign-key-ent relation-path)))
   spec-gen
   (-> db :schema ent-type :relations)))

(defn- no-op-transact [_ datoms]
  [datoms])

(defn- insert-datascript [database ent-db {:keys [ent-type] :as ent-attrs}]
  (let [transact-fn (or (-> ent-db :schema ent-type :transact-fn) no-op-transact)
        datoms (-> (assoc-ent-at-foreign-keys ent-db ent-attrs)
                   (assoc :db/id "eid"))
        {{:strs [eid]} :tempids db :db-after} (d/transact! database [[:db.fn/call transact-fn datoms]])]
    (d/entity db eid)))

(defn- insert [database ent-db ent-attrs]
  (insert-datascript database ent-db ent-attrs))

(defn- attribute->malli-schema [{:attribute/keys [schema default]}]
  (cond
    default [:= (handlers/default-value default)]
    schema schema))

(defn- attributes-full-names [resource attributes]
  (map (partial handlers/attribute-full-name resource) attributes))

(defn- resource->malli-schema [{:resource/keys [attributes] :as resource}]
  `[:map
    ~@(->> (zipmap (attributes-full-names resource attributes)
                   (map attribute->malli-schema attributes))
           (m/remove-vals nil?)
           (into []))])

(defn- resource->generator [resource]
  (let [schema (resource->malli-schema resource)
        malli-gen (malli-gen/generator schema)]
    (if-let [bind-gen-fn (:specmonstah/bind-gen-fn resource)]
      (gen/bind malli-gen bind-gen-fn)
      malli-gen)))

(defn- specmonstah-key [{:resource/keys [attributes]}]
  (m/find-first :specmonstah/key? attributes))

; TODO support cardinality many
(defn- attribute->relation [{:attribute/keys [ref cardinality]}]
  (when (and ref (not= :db.cardinality/many cardinality))
    [(:resource/name ref) (handlers/attribute-full-name ref (specmonstah-key ref))]))

(defn- resource->relations [{:resource/keys [attributes] :as resource}]
  (->> (zipmap (attributes-full-names resource attributes)
               (map attribute->relation attributes))
       (m/remove-vals nil?)))

(defn- resource->specmonstah-schema [resource]
  (-> {:prefix (:resource/name resource)
       :generator (resource->generator resource)}
      (m/assoc-some :relations (resource->relations resource))
      (m/assoc-some :transact-fn (:specmonstah/transact-fn resource))))

(defn- meta-db->specmonstah-schema [meta-db]
  (->> (resource/resources meta-db)
       (map (fn [resource]
              [(:resource/name resource) (resource->specmonstah-schema resource)]))
       (into {})))

(defn- ent-data [ent-db ent]
  (:inserted-data (sm/ent-attrs ent-db ent)))

(defn- ent-attrs-map [ent-db]
  (let [ents (sm/ents ent-db)]
    (zipmap ents
            (map (partial ent-data ent-db) ents))))

(defn ents [query]
  (gen/->Generator
   (fn [rnd size]
     (let [meta-db (resource/meta-db)
           conn (resource/conn meta-db {})
           schema (meta-db->specmonstah-schema meta-db)
           ent-db (-> (ent-db-malli-gen {:schema schema
                                         :gen-options {:rnd-state (atom rnd)
                                                       :size size}}
                                        query)
                      (sm/visit-ents-once :inserted-data (partial insert conn)))]
       (rose/pure
        (merge
         {:conn conn
          :handler (resource/handler meta-db conn)
          :ent-db ent-db
          :db @conn
          :ents (ent-attrs-map ent-db)}))))))

(defn gen-ents [query]
  (gen/generate (ents query)))
