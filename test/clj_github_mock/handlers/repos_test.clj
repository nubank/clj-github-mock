(ns clj-github-mock.handlers.repos-test
  (:require [base64-clj.core :as base64]
            [cheshire.core :as json]
            [clj-github-mock.core :refer [ring-handler]]
            [clj-http.client :as http]
            [clj-http.fake :refer [with-fake-routes]]
            [clojure.data :as data]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [lambdaisland.regal.generator :as rg]
            [malli.core :as m]
            [malli.generator :as mg]
            [matcher-combinators.standalone :refer [match?]]
            [matcher-combinators.test]
            [clj-github-mock.generators :as mock-gen]
            [ring.mock.request :as mock]
            [clj-github-mock.impl.jgit :as jgit]
            [clj-github-mock.impl.database :as database]))

(defn github-url [endpoint]
  (str "https://api.github.com" endpoint))

(defmacro with-handler [initial-state & body]
  `(with-fake-routes {#"https://api.github.com/.*" (ring-handler {:initial-state ~initial-state})}
     ~@body))

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn request [base-request]
  (http/request (deep-merge {:method :get
                             :headers {"Content-Type" "application/json"}
                             :as :json
                             :throw-exceptions false}
                            (-> base-request
                                (assoc :url (github-url (:path base-request)))
                                (update :body json/encode)
                                (dissoc :path)))))

(def object-name [:re {:gen/gen (rg/gen [:+ [:class [\a \z]]])} #"\w+"])

(def object-name-gen (mg/generator object-name))

(defn repo-gen
  ([]
   (repo-gen object-name-gen))
  ([repo-name-gen]
   (mg/generator [:map
                  [:name [:string {:gen/gen repo-name-gen}]]])))

(def repos-gen
  (gen/let [repos-names (mg/generator [:set {:min 1 :max 5} object-name])]
    (apply gen/tuple
           (map #(repo-gen (gen/return %)) repos-names))))

(defn org-gen
  ([]
   (org-gen object-name-gen))
  ([org-name-gen]
   (gen/let [org-name org-name-gen]
     (mg/generator [:map
                    [:name [:string {:gen/gen (gen/return org-name)}]]
                    [:repos [:vector {:gen/gen repos-gen} :any]]]))))

(def orgs-gen
  (gen/let [org-names (mg/generator [:set {:min 1 :max 5} object-name])]
    (apply gen/tuple
           (map #(org-gen (gen/return %)) org-names))))

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

(defn list-org-repos [org-name]
  (request {:path (org-repos-path org-name)}))

(defn list-org-repos-request [org-name]
  (mock/request :get (org-repos-path org-name)))

(defspec list-org-repos-respects-response-schema
  10
  (prop/for-all
   [{:keys [handler ent-db org0]} (mock-gen/database {:repo [[3]]})]
   (m/validate list-org-repos-response-schema
               (handler (list-org-repos-request (:org/name org0))))))

(defspec list-org-repos-return-all-repos
  10
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

(defn create-org-repo [org body]
  (request {:path (org-repos-path org)
            :method :post
            :body body}))

(defn create-org-repo-request [org body]
  (-> (mock/request :post (org-repos-path org))
      (assoc :body body)))

(defspec create-org-repo-respects-response-schema
  10
  (prop/for-all
   [{:keys [handler org0]} (mock-gen/database {:org [[1]]})
    repo-name mock-gen/ref-name]
   (m/validate
    create-org-repo-response-schema
    (handler (create-org-repo-request (:org/name org0) {:name repo-name})))))

(defspec create-org-repo-requires-a-name
  10
  (prop/for-all
   [{:keys [handler org0]} (mock-gen/database {:org [[1]]})]
   (match? {:status 422}
           (handler (create-org-repo-request (:org/name org0) {})))))

(defspec create-org-repo-adds-repo-to-the-org
  10
  (prop/for-all
   [{:keys [handler database org0]} (mock-gen/database {:org [[1]]})
    repo-name mock-gen/ref-name]
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

(defn get-org-repo [org-name repo-name]
  (request {:path (org-repo-path org-name repo-name)}))

(defn get-org-repo-request [org-name repo-name]
  (mock/request :get (org-repo-path org-name repo-name)))

(defspec get-org-repo-respects-response-schema
  10
  (prop/for-all
   [{:keys [handler org0 repo0]} (mock-gen/database {:repo [[1]]})]
   (m/validate
    get-org-repo-response-schema
    (handler (get-org-repo-request (:org/name org0) (:repo/name repo0))))))

(defn update-org-repo [org-name repo-name body]
  (request {:path (org-repo-path org-name repo-name)
            :method :patch
            :body body}))

(defn update-org-repo-request [org-name repo-name body]
  (-> (mock/request :patch (org-repo-path org-name repo-name))
      (assoc :body body)))

(defspec update-org-repo-respects-response-schema
  10
  (prop/for-all
   [{:keys [handler org0 repo0]} (mock-gen/database {:repo [[1]]})]
   (m/validate
    get-org-repo-response-schema
    (handler (update-org-repo-request (:org/name org0) (:repo/name repo0) {:random-attr "random-value"})))))

(defspec update-org-repo-only-updates-set-fields
  10
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

(defn create-tree [org repo body]
  (request {:path (trees-path org repo)
            :method :post
            :body body}))

(defn get-tree [org repo tree-sha]
  (request {:path (tree-sha-path org repo tree-sha)}))

(defn get-tree-request [org repo tree-sha]
  (mock/request :get (tree-sha-path org repo tree-sha)))

(defspec create-tree-adds-tree-to-repo
  10
  (prop/for-all
   [{:keys [handler database org0 repo0]} (mock-gen/database {:repo [[1]]})
    tree mock-gen/github-tree]
   (let [{{:keys [sha]} :body} (handler (create-tree-request (:org/name org0) (:repo/name repo0) {:tree tree}))]
     (-> repo0 :repo/jgit (jgit/get-tree sha)))))

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
  10
  (prop/for-all
   [{:keys [handler org0 repo0 tree0]} (mock-gen/database {:tree [[1]]})]
   (m/validate get-tree-response-schema
               (handler (get-tree-request (:org/name org0) (:repo/name repo0) (:sha tree0))))))

(defn commits-path [org repo]
  (str "/repos/" org "/" repo "/git/commits"))

(defn commit-sha-path [org repo sha]
  (str (commits-path org repo) "/" sha))

(defn create-commit [org repo body]
  (request {:path (commits-path org repo)
            :method :post
            :body body}))

(defn create-commit-request [org repo body]
  (-> (mock/request :post (commits-path org repo))
      (assoc :body body)))

(defn get-commit [org repo sha]
  (request {:path (commit-sha-path org repo sha)}))

(defn get-commit-request [org repo sha]
  (mock/request :get (commit-sha-path org repo sha)))

(defspec create-commit-adds-commit-to-repo
  10
  (prop/for-all
   [{:keys [handler org0 repo0 tree0]} (mock-gen/database {:tree [[1]]})
    commit (mock-gen/commit)]
   (let [{{:keys [sha]} :body} (handler (create-commit-request (:org/name org0)
                                                               (:repo/name repo0)
                                                               (assoc commit :tree (:sha tree0))))]
     (-> repo0 :repo/jgit (jgit/get-commit sha)))))

(def get-commit-response-schema
  [:map
   [:status [:= 200]
    :body [:map
           [:sha :string]
           [:tree [:map
                   [:sha :string]]]
           [:message :string]]]])

(defspec get-commit-respects-response-schema
  10
  (prop/for-all
   [{:keys [handler org0 repo0 commit0]} (mock-gen/database {:commit [[1]]})]
   (m/validate get-commit-response-schema
               (handler (get-commit-request (:org/name org0) (:repo/name repo0) (:sha commit0))))))

(defn ref-path [org repo ref]
  (str "/repos/" org "/" repo "/git/ref/" ref))

(defn refs-path [org repo]
  (str "/repos/" org "/" repo "/git/refs"))

(defn refs-ref-path [org repo ref]
  (str (refs-path org repo) "/" ref))

(defn create-ref [org repo body]
  (request {:path (refs-path org repo)
            :method :post
            :body body}))

(defn create-ref-request [org repo body]
  (-> (mock/request :post (refs-path org repo))
      (assoc :body body)))

(defn get-ref [org repo ref]
  (request {:path (ref-path org repo ref)}))

(defn update-ref [org repo ref body]
  (request {:path (refs-ref-path org repo ref)
            :method :patch
            :body body}))

(defn update-ref-request [org repo ref body]
  (-> (mock/request :patch (refs-ref-path org repo ref))
      (assoc :body body)))

(defn delete-ref [org repo ref]
  (request {:path (refs-ref-path org repo ref)
            :method :delete}))

(defn delete-ref-request [org repo ref]
  (mock/request :delete (refs-ref-path org repo ref)))

(defspec create-ref-adds-ref-to-repo
  10
  (prop/for-all
   [{:keys [handler org0 repo0 commit0]} (mock-gen/database {:commit [[1]]})
    ref (gen/fmap #(str "refs/heads/" %) mock-gen/ref-name)]
   (handler (create-ref-request (:org/name org0) (:repo/name repo0) {:ref ref
                                                                          :sha (:sha commit0)}))
   (-> repo0 :repo/jgit (jgit/get-reference ref))))

(defspec update-ref-updates-the-ref
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    ref (gen/not-empty gen/string-alphanumeric)]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree [{:path "a" :mode "100644" :type "blob" :content "a"}]})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (create-ref org-name (:name repo) {:ref (str "refs/heads/" ref)
                                          :sha sha})
       (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:base_tree tree-sha :tree [{:path "a" :mode "100644" :type "blob" :content "a1"}]})
             {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                         :tree tree-sha
                                                                         :parents [sha]})]
         (update-ref org-name (:name repo) (str "heads/" ref) {:sha sha})
         (match? {:object {:sha sha}}
                 (:body (get-ref org-name (:name repo) (str "heads/" ref)))))))))

(defspec delete-ref-removes-ref-from-repo
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    tree mock-gen/github-tree
    ref (gen/not-empty gen/string-alphanumeric)]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (create-ref org-name (:name repo) {:ref (str "refs/heads/" ref)
                                          :sha sha})
       (delete-ref org-name (:name repo) (str "heads/" ref))
       (= 404
          (:status (get-ref org-name (:name repo) (str "heads/" ref))))))))

(defn branch-path [org repo branch]
  (str "/repos/" org "/" repo "/branches/" branch))

(defn get-branch [org repo branch]
  (request {:path (branch-path org repo branch)}))

(defspec get-branch-returns-branch-info
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    tree mock-gen/github-tree
    ref (gen/not-empty gen/string-alphanumeric)]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (create-ref org-name (:name repo) {:ref (str "refs/heads/" ref)
                                          :sha sha})
       (match? {:status 200
                :body {:name ref
                       :commit {:sha sha
                                :commit {:message "some message"
                                         :tree {:sha tree-sha}}}}}
               (get-branch org-name (:name repo) ref))))))

(defn contents-path [org repo path]
  (str "/repos/" org "/" repo "/contents/" path))

(defn get-content [org repo path ref]
  (request {:path (contents-path org repo path)
            :query-params {"ref" ref}}))

(def github-tree+file-gen
  (gen/let [tree mock-gen/github-tree
            file (gen/elements tree)]
    {:tree tree
     :file file}))

(defspec get-content-returns-content
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    {:keys [tree file]} github-tree+file-gen]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (create-ref org-name (:name repo) {:ref "refs/heads/main"
                                          :sha sha})
       (match? {:status 200
                :body {:type "file"
                       :path (:path file)
                       :content (base64/encode (:content file) "UTF-8")}}
               (get-content org-name (:name repo) (:path file) sha))))))

(defspec get-content-supports-refs
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    {:keys [tree file]} github-tree+file-gen]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (create-ref org-name (:name repo) {:ref "refs/heads/main"
                                          :sha sha})
       (match? {:status 200
                :body {:type "file"
                       :path (:path file)
                       :content (base64/encode (:content file) "UTF-8")}}
               (get-content org-name (:name repo) (:path file) "main"))))))

(defspec get-content-supports-default-branch
  10
  (prop/for-all
   [org-name object-name-gen
    repo (gen/fmap #(assoc % :default_branch "some-default-branch") (repo-gen))
    {:keys [tree file]} github-tree+file-gen]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (create-ref org-name (:name repo) {:ref "refs/heads/some-default-branch"
                                          :sha sha})
       (match? {:status 200
                :body {:type "file"
                       :path (:path file)
                       :content (base64/encode (:content file) "UTF-8")}}
               (request {:path (contents-path org-name (:name repo) (:path file))}))))))

