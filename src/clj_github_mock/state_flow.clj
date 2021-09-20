(ns clj-github-mock.state-flow
  (:require [state-flow.labs.state :as state]
            [org.httpkit.fake :as fake]
            [state-flow.api :as flow]
            [clj-github-mock.resource :as mock-resource]
            [ring.mock.request :as mock.request]
            [clj-github-mock.generators1 :as mock-gen]
            [datascript.core :as d]))

(defn httpkit-fake-handler
  "Creates a `ring-handler` that is compatible with `http-kit-fake`. Receives the same
  options as `ring-handler."
  [meta-db conn]
  (let [handler (mock-resource/json-handler meta-db conn)]
    (fn [_ {:keys [method url body headers] :as req} _]
      (handler (merge (reduce
                       (fn [req [header value]]
                         (mock.request/header req header value))
                       (-> (mock.request/request method url)
                           (mock.request/body body))
                       headers)
                      (dissoc req :body :headers))))))

(defmacro flow [& flows]
  `(flow/flow
    "github-outer"
    [:let [meta-db# (mock-resource/meta-db)
           conn# (mock-resource/conn meta-db# {})]]
    (flow/swap-state assoc ::conn conn# ::meta-db meta-db#)
    (state/wrap-with
     (fn [f#]
       (fake/with-fake-http
         [#"^https://api.github.com/.*" (httpkit-fake-handler meta-db# conn#)]
         (f#)))
     (flow/flow "github-inner"
                ~@flows))))

(defn gen-ents [query]
  (flow/get-state
   (fn [{conn ::conn meta-db ::meta-db}]
     (mock-gen/gen-ents conn meta-db query))))

(defn q [datalog & args]
  (flow/get-state
   (fn [{conn ::conn}]
     (apply d/q datalog @conn args))))
