(ns clj-github-mock.core
  (:require [clj-github-mock.resource :as api]
            [ring.mock.request :as mock]))

(defn ring-handler
  "Creates a ring like handler that emulates the github api.
  Receives a map of options that configure the handler.

  Options:
  - `:initial-state`: a map containing information about organizations
  and repositories that will form the initial state of the emulator.

  Example:
  ```
  {:orgs [{:name \"nubank\" :repos [{:name \"some-repo\" :default_branch \"master\"}]}]}
  ```

  `default_branch` is optional and will default to \"main\".
  "
  [{:keys [initial-state] :as _opts}]
  (-> (api/conn initial-state)
      (api/json-handler)))

(defn httpkit-fake-handler
  "Creates a `ring-handler` that is compatible with `http-kit-fake`. Receives the same
  options as `ring-handler."
  [opts]
  (let [handler (ring-handler opts)]
    (fn [_ {:keys [method url body headers] :as req} _]
      (handler (merge (reduce
                       (fn [req [header value]]
                         (mock/header req header value))
                       (-> (mock/request method url)
                           (mock/body body))
                       headers)
                      (dissoc req :body :headers))))))
