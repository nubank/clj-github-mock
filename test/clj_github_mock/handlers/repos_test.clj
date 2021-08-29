(ns clj-github-mock.handlers.repos-test
  (:require [base64-clj.core :as base64]
            [clj-github-mock.generators :as mock-gen]
            [clj-github-mock.impl.database :as database]
            [clj-github-mock.impl.jgit :as jgit]
            [clojure.data :as data]
            [clojure.string :as string]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [matcher-combinators.standalone :refer [match?]]
            [matcher-combinators.test]
            [ring.mock.request :as mock]
            [datascript.core :as d]))

(defn org-repos-path [org-name]
  (str "/orgs/" org-name "/repos"))

(def list-org-repos-response-schema
  [:map
   [:status [:= 200]]
   [:body [:vector
           [:map
            [:name :string]
            [:full_name :string]
            [:default_branch :string]]]]])

(defn list-org-repos-request [org-name]
  (mock/request :get (org-repos-path org-name)))

(defspec list-org-repos-respects-response-schema
  (prop/for-all
   [{:keys [handler ent-db org0]} (mock-gen/database {:repo [[3]]})]
   (m/validate list-org-repos-response-schema
               (handler (list-org-repos-request (:org/name org0))))))

(defspec list-org-repos-return-all-repos
  (prop/for-all
   [{:keys [handler ent-db ents]} (mock-gen/database {:org [[:org1]
                                                            [:org2]
                                                            [:org3]]
                                                      :repo [[2 {:refs {:repo/org :org1}}]
                                                             [2 {:refs {:repo/org :org2}}]
                                                             [2 {:refs {:repo/org :org3}}]]})]
   (match? (set (map :repo/name (:repo ents)))
           (set (map :name (mapcat #(:body (handler (list-org-repos-request (:org/name %)))) (:org ents)))))))

(def create-org-repo-response-schema
  [:map
   [:status [:= 201]]
   [:body [:map
           [:name :string]
           [:full_name :string]
           [:default_branch :string]]]])

(defn create-org-repo-request [org body]
  (-> (mock/request :post (org-repos-path org))
      (assoc :body body)))

(defspec create-org-repo-respects-response-schema
  (prop/for-all
   [{:keys [handler org0]} (mock-gen/database {:org [[1]]})
    repo-name mock-gen/object-name]
   (m/validate
    create-org-repo-response-schema
    (handler (create-org-repo-request (:org/name org0) {:name repo-name})))))

(defspec create-org-repo-requires-a-name
  (prop/for-all
   [{:keys [handler org0]} (mock-gen/database {:org [[1]]})]
   (match? {:status 422}
           (handler (create-org-repo-request (:org/name org0) {})))))

(defspec create-org-repo-adds-repo-to-the-org
  (prop/for-all
   [{:keys [handler database org0]} (mock-gen/database {:org [[1]]})
    repo-name mock-gen/object-name]
   (handler (create-org-repo-request (:org/name org0) {:name repo-name}))
   (database/find-repo database (:org/name org0) repo-name)))

(def get-org-repo-response-schema
  [:map
   [:status [:= 200]]
   [:body [:map
           [:name :string]
           [:full_name :string]
           [:default_branch :string]]]])

(defn org-repo-path [org-name repo-name]
  (string/join "/" ["/repos" org-name repo-name]))

(defn get-org-repo-request [org-name repo-name]
  (mock/request :get (org-repo-path org-name repo-name)))

(defspec get-org-repo-respects-response-schema
  (prop/for-all
   [{:keys [handler org0 repo0]} (mock-gen/database {:repo [[1]]})]
   (m/validate
    get-org-repo-response-schema
    (handler (get-org-repo-request (:org/name org0) (:repo/name repo0))))))

(defn update-org-repo-request [org-name repo-name body]
  (-> (mock/request :patch (org-repo-path org-name repo-name))
      (assoc :body body)))

(defspec update-org-repo-respects-response-schema
  (prop/for-all
   [{:keys [handler org0 repo0]} (mock-gen/database {:repo [[1]]})]
   (m/validate
    get-org-repo-response-schema
    (handler (update-org-repo-request (:org/name org0) (:repo/name repo0) {:random-attr "random-value"})))))

(defspec update-org-repo-only-updates-set-fields
  (prop/for-all
   [{:keys [handler org0 repo0]} (mock-gen/database {:repo [[1]]})]
   (let [repo-before (:body (handler (get-org-repo-request (:org/name org0) (:repo/name repo0))))
         _ (handler (update-org-repo-request (:org/name org0) (:repo/name repo0) {:visibility "private"}))
         repo-after (:body (handler (get-org-repo-request (:org/name org0) (:repo/name repo0))))]
     (match? [nil {:visibility "private"} any?]
             (data/diff repo-before repo-after)))))

(defn trees-path [org repo]
  (str "/repos/" org "/" repo "/git/trees"))

(defn tree-sha-path [org repo tree-sha]
  (str (trees-path org repo) "/" tree-sha))

(defn create-tree-request [org repo body]
  (->
   (mock/request :post (trees-path org repo))
   (assoc :body body)))

(defn get-tree-request [org repo tree-sha]
  (mock/request :get (tree-sha-path org repo tree-sha)))

(defspec create-tree-adds-tree-to-repo
  (prop/for-all
   [{:keys [handler database org0 repo0]} (mock-gen/database {:repo [[1]]})
    tree mock-gen/github-tree]
   (let [{{:keys [sha]} :body} (handler (create-tree-request (:org/name org0) (:repo/name repo0) {:tree tree}))
         db @database]
     (d/entid db [:tree/repo+sha [(d/entid db [:repo/name+org [(:repo/name repo0)
                                                               (d/entid db [:org/name (:org/name org0)])]])
                                  sha]]))))

(def get-tree-response-schema
  [:map
   [:status [:= 200]]
   [:body [:map
           [:sha :string]
           [:tree [:vector
                   [:map
                    [:path :string]
                    [:mode :string]
                    [:type :string]
                    [:sha :string]]]]]]])

(defspec get-tree-respects-response-schema
  (prop/for-all
   [{:keys [handler org0 repo0 tree0]} (mock-gen/database {:tree [[1]]})]
   (m/validate get-tree-response-schema
               (handler (get-tree-request (:org/name org0) (:repo/name repo0) (:tree/sha tree0))))))

(defn commits-path [org repo]
  (str "/repos/" org "/" repo "/git/commits"))

(defn commit-sha-path [org repo sha]
  (str (commits-path org repo) "/" sha))

(defn create-commit-request [org repo body]
  (-> (mock/request :post (commits-path org repo))
      (assoc :body body)))

(defn get-commit-request [org repo sha]
  (mock/request :get (commit-sha-path org repo sha)))

(defspec create-commit-adds-commit-to-repo
  (prop/for-all
   [{:keys [handler database org0 repo0 tree]} (gen/let [{:keys [repo0] :as database} (mock-gen/database {:repo [[1]]})
                                                tree (mock-gen/tree (:repo/jgit repo0))]
                                        (assoc database :tree tree))
    message gen/string]
   (let [{{:keys [sha]} :body} (handler (create-commit-request (:org/name org0)
                                                               (:repo/name repo0)
                                                               {:message message
                                                                :tree (:sha tree)}))
         db @database]
     (d/entid db [:commit/repo+sha [(d/entid db [:repo/name+org [(:repo/name repo0)
                                                               (d/entid db [:org/name (:org/name org0)])]])
                                  sha]]))))

(def get-commit-response-schema
  [:map
   [:status [:= 200]
    :body [:map
           [:sha :string]
           [:tree [:map
                   [:sha :string]]]
           [:message :string]]]])

(defspec get-commit-respects-response-schema
  (prop/for-all
   [{:keys [handler org0 repo0 commit0]} (mock-gen/database {:commit [[1]]})]
   (m/validate get-commit-response-schema
               (handler (get-commit-request (:org/name org0) (:repo/name repo0) (:commit/sha commit0))))))

(defn ref-path [org repo ref]
  (str "/repos/" org "/" repo "/git/ref/" ref))

(defn refs-path [org repo]
  (str "/repos/" org "/" repo "/git/refs"))

(defn refs-ref-path [org repo ref]
  (str (refs-path org repo) "/" ref))

(defn create-ref-request [org repo body]
  (-> (mock/request :post (refs-path org repo))
      (assoc :body body)))

(defn update-ref-request [org repo ref body]
  (-> (mock/request :patch (refs-ref-path org repo ref))
      (assoc :body body)))

(defn delete-ref-request [org repo ref]
  (mock/request :delete (refs-ref-path org repo ref)))

(defspec create-ref-adds-ref-to-repo
  (prop/for-all
   [{:keys [handler org0 repo0 commit]} (gen/let [{:keys [repo0] :as database} (mock-gen/database {:repo [[1]]})
                                                  commit (mock-gen/commit (:repo/jgit repo0))]
                                          (assoc database :commit commit))
    ref (gen/fmap #(str "refs/heads/" %) mock-gen/object-name)]
   (handler (create-ref-request (:org/name org0) (:repo/name repo0) {:ref ref
                                                                     :sha (:sha commit)}))
   (-> repo0 :repo/jgit (jgit/get-reference ref))))

(defspec update-ref-updates-the-ref
  (prop/for-all
   [{:keys [handler org0 repo0 branch commit]} (gen/let [{:keys [repo0] :as database} (mock-gen/database {:repo [[1]]})
                                                         branch (mock-gen/branch (:repo/jgit repo0))
                                                         commit (mock-gen/commit (:repo/jgit repo0) (-> branch :commit :sha))]
                                                 (assoc database :branch branch :commit commit))]
   (handler (update-ref-request (:org/name org0) (:repo/name repo0) (str "heads/" (:name branch)) {:sha (:sha commit)}))
   (= (:sha commit) (-> repo0 :repo/jgit (jgit/get-reference (str "refs/heads/" (:name branch))) :object :sha))))

(defspec delete-ref-removes-ref-from-repo
  (prop/for-all
   [{:keys [handler org0 repo0 branch]} (gen/let [{:keys [repo0] :as database} (mock-gen/database {:repo [[1]]})
                                                  branch (mock-gen/branch (:repo/jgit repo0))]
                                          (assoc database :branch branch))]
   (handler (delete-ref-request (:org/name org0) (:repo/name repo0) (str "heads/" (:name branch))))
   (nil? (-> repo0 :repo/jgit (jgit/get-reference (str "refs/heads/" (:name branch)))))))

(defn branch-path [org repo branch]
  (str "/repos/" org "/" repo "/branches/" branch))

(defn get-branch-request [org repo branch]
  (mock/request :get (branch-path org repo branch)))

(defspec get-branch-returns-branch-info
  (prop/for-all
   [{:keys [handler org0 repo0 branch]} (gen/let [{:keys [repo0] :as database} (mock-gen/database {:repo [[1]]})
                                                  branch (mock-gen/branch (:repo/jgit repo0))]
                                          (assoc database :branch branch))]
   (= {:status 200
       :body branch}
      (handler (get-branch-request (:org/name org0) (:repo/name repo0) (:name branch))))))

(defn contents-path [org repo path]
  (str "/repos/" org "/" repo "/contents/" path))

(defn get-content-request
  ([org repo path]
   (mock/request :get (contents-path org repo path)))
  ([org repo path ref]
   (mock/request :get (contents-path org repo path) {"ref" ref})))

(defspec get-content-returns-content
  (prop/for-all
   [{:keys [handler org0 repo0 branch file]} (gen/let [{:keys [repo0] :as database} (mock-gen/database {:repo [[1]]})
                                                       branch (mock-gen/branch (:repo/jgit repo0) :num-commits 1)
                                                       file (mock-gen/random-file (:repo/jgit repo0) (:name branch))]
                                               (assoc database :branch branch :file file))]
   (= {:status 200
       :body {:type "file"
              :path (:path file)
              :content (base64/encode (:content file) "UTF-8")}}
      (handler (get-content-request (:org/name org0) (:repo/name repo0) (:path file) (-> branch :commit :sha))))))

(defspec get-content-supports-refs
  (prop/for-all
   [{:keys [handler org0 repo0 file branch]} (gen/let [{:keys [repo0] :as database} (mock-gen/database {:repo [[1]]})
                                                       branch (mock-gen/branch (:repo/jgit repo0) :num-commits 1)
                                                       file (mock-gen/random-file (:repo/jgit repo0) (:name branch))]
                                               (assoc database :branch branch :file file))]
   (= {:status 200
       :body {:type "file"
              :path (:path file)
              :content (base64/encode (:content file) "UTF-8")}}
      (handler (get-content-request (:org/name org0) (:repo/name repo0) (:path file) (:name branch))))))

(defspec get-content-supports-default-branch
  (prop/for-all
   [{:keys [handler org0 repo0 file]} (gen/let [branch-name mock-gen/object-name
                                                {:keys [repo0] :as database} (mock-gen/database {:repo [[1 {:spec-gen {:repo/attrs {:default_branch branch-name}}}]]})
                                                _ (mock-gen/branch (:repo/jgit repo0) :name branch-name :num-commits 1)
                                                file (mock-gen/random-file (:repo/jgit repo0) branch-name)]
                                        (assoc database :file file))]
   (= {:status 200
       :body {:type "file"
              :path (:path file)
              :content (base64/encode (:content file) "UTF-8")}}
      (handler (get-content-request (:org/name org0) (:repo/name repo0) (:path file))))))
