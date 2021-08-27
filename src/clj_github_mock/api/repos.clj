(ns clj-github-mock.api.repos
  (:require [clj-github-mock.handlers.repos :as repos]))

(def model
  [{:api/name "org-repos"
    :api/path "/orgs/:org/repos"
    :api/routes [["" {:get repos/get-repos-handler
                      :post repos/post-repos-handler}]]}
   {:api/name "repos"
    :api/path "/repos/:org/:repo"
    :api/config {:middleware [repos/repo-middleware]}
    :api/routes [["" {:get repos/get-repo-handler
                      :patch repos/patch-repo-handler}]
                 ["/git/trees" {:post repos/post-tree-handler}]
                 ["/git/trees/:sha" {:get repos/get-tree-handler}]
                 ["/git/commits" {:post repos/post-commit-handler}]
                 ["/git/commits/:sha" {:get repos/get-commit-handler}]
                 ["/git/refs" {:post repos/post-ref-handler}]
                 ["/git/refs/*ref" {:patch repos/patch-ref-handler
                                    :delete repos/delete-ref-handler}]
                 ["/git/ref/*ref" {:get repos/get-ref-handler}]
                 ["/branches/:branch" {:get repos/get-branch-handler}]
                 ["/contents/*path" {:get repos/get-content-handler}]]}])
