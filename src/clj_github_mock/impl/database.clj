(ns clj-github-mock.impl.database
  (:require [datascript.core :as d]))

(defn find-repo [database org-name repo-name]
  (d/q '[:find (pull ?r [*]) .
         :in $ ?org-name ?repo-name
         :where
         [?r :repo/name ?repo-name]
         [?r :repo/org ?org]
         [?org :org/name ?org-name]] @database org-name repo-name))

(defn middleware [handler conn]
  (fn [request]
    (handler (assoc request :database conn))))
