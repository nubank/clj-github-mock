(ns clj-github-mock.state-flow
  (:require [state-flow.labs.state :as state]
            [org.httpkit.fake :as fake]
            [state-flow.api :as flow]
            [clj-github-mock.resource :as mock-resource]
            [ring.mock.request :as mock.request]
            [clj-github-mock.generators :as mock-gen]
            [datascript.core :as d]))

(defn httpkit-fake-handler
  "Creates a `ring-handler` that is compatible with `http-kit-fake`. Receives the same
  options as `ring-handler."
  [conn]
  (let [handler (mock-resource/handler conn)]
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
    [:let [schema# (mock-gen/schema)
           conn# (mock-gen/conn schema#)]]
    (flow/swap-state assoc ::conn conn# ::schema schema#)
    (state/wrap-with
     (fn [f#]
       (fake/with-fake-http
         [#"^https://api.github.com/.*" (httpkit-fake-handler conn#)]
         (f#)))
     (flow/flow "github-inner"
                ~@flows))))

(defn gen-ents [query]
  (flow/get-state
   (fn [{conn ::conn schema ::schema}]
     (mock-gen/gen-ents conn schema query))))

(defn q [datalog & args]
  (flow/get-state
   (fn [{conn ::conn}]
     (apply d/q datalog @conn args))))
