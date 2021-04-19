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
            [clojure.walk :as walk]
            [lambdaisland.regal.generator :as rg]
            [malli.core :as m]
            [malli.generator :as mg]
            [matcher-combinators.standalone :refer [match?]]
            [matcher-combinators.test]))

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

(defspec list-org-repos-respects-response-schema
  10
  (prop/for-all
   [org (org-gen)]
   (with-handler {:orgs [org]}
     (m/validate list-org-repos-response-schema
                 (list-org-repos (:name org))))))

(defspec list-org-repos-return-all-repos
  10
  (prop/for-all
   [orgs orgs-gen]
   (with-handler {:orgs orgs}
     (match? (set (mapcat #(->> % :repos (map (fn [{:keys [name]}]
                                                {:full_name (string/join "/" [(:name %) name])}))) orgs))
             (set (mapcat #(:body (list-org-repos (:name %))) orgs))))))

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

(defspec create-org-repo-respects-response-schema
  10
  (prop/for-all
   [org-name object-name-gen
    repo-name object-name-gen]
   (m/validate
    create-org-repo-response-schema
    (with-handler {:orgs [{:name org-name}]}
      (create-org-repo org-name {:name repo-name})))))

(defspec create-org-repo-requires-a-name
  10
  (prop/for-all
   [org-name object-name-gen]
   (match? {:status 422}
           (with-handler {:orgs [{:name org-name}]}
             (create-org-repo org-name {})))))

(defspec create-org-repo-adds-repo-to-the-org
  10
  (prop/for-all
   [[org repo-name] (gen/let [org (org-gen)
                              repo-name (gen/such-that #(not ((->> org :repos (map :name) set) %)) object-name-gen)]
                      [org repo-name])]
   (with-handler {:orgs [org]}
     (let [repos-before (set (:body (list-org-repos (:name org))))
           _ (create-org-repo (:name org) {:name repo-name})
           repos-after (set (:body (list-org-repos (:name org))))]
       (match? #{{:full_name (string/join "/" [(:name org) repo-name])}}
               (set/difference repos-after repos-before))))))

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

(defspec get-org-repo-respects-response-schema
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)]
   (m/validate
    get-org-repo-response-schema
    (with-handler {:orgs [{:name org-name
                           :repos [repo]}]}
      (get-org-repo org-name (:name repo))))))

(defn update-org-repo [org-name repo-name body]
  (request {:path (org-repo-path org-name repo-name)
            :method :patch
            :body body}))

(defspec update-org-repo-respects-response-schema
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)]
   (m/validate
    get-org-repo-response-schema
    (with-handler {:orgs [{:name org-name
                           :repos [repo]}]}
      (update-org-repo org-name (:name repo) {:random-attr "random-value"})))))

(defspec update-org-repo-only-updates-set-fields
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [repo-before (:body (get-org-repo org-name (:name repo)))
           _ (update-org-repo org-name (:name repo) {:visibility "private"})
           repo-after (:body (get-org-repo org-name (:name repo)))]
       (match? [nil {:visibility "private"} any?]
               (data/diff repo-before repo-after))))))

(defn flatten-obj [[obj-name node :as entry]]
  (if (string? node)
    entry
    (into
     {}
     (map (fn [[child-name child-node]]
            [(str obj-name "/" child-name) child-node])
          (val entry)))))

(defn tree->github-tree [tree]
  (->> (walk/postwalk
        (fn [node]
          (if (map? node)
            (into {} (map flatten-obj node))
            node))
        tree)
       (map (fn [[path content]]
              {:path path
               :mode "100644"
               :type "blob"
               :content content}))))

(def github-tree-gen
  (gen/fmap
   tree->github-tree
   (mg/generator [:schema {:registry {::file-content :string
                                      ::dir [:and
                                             [:map-of object-name [:ref ::node]]
                                             [:fn seq]]
                                      ::node [:or ::file-content ::dir]
                                      ::root ::dir}}
                  ::root])))

(defn trees-path [org repo]
  (str "/repos/" org "/" repo "/git/trees"))

(defn tree-sha-path [org repo tree-sha]
  (str (trees-path org repo) "/" tree-sha))

(defn create-tree [org repo body]
  (request {:path (trees-path org repo)
            :method :post
            :body body}))

(defn get-tree [org repo tree-sha]
  (request {:path (tree-sha-path org repo tree-sha)}))

(defspec create-tree-adds-tree-to-repo
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    tree github-tree-gen]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{:keys [sha]} :body} (create-tree org-name (:name repo) {:tree tree})]
       (= 200
          (:status (get-tree org-name (:name repo) sha)))))))

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
   [org-name object-name-gen
    repo (repo-gen)
    tree github-tree-gen]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{:keys [sha]} :body} (create-tree org-name (:name repo) {:tree tree})]
       (m/validate get-tree-response-schema
                   (get-tree org-name (:name repo) sha))))))

(defn commits-path [org repo]
  (str "/repos/" org "/" repo "/git/commits"))

(defn commit-sha-path [org repo sha]
  (str (commits-path org repo) "/" sha))

(defn create-commit [org repo body]
  (request {:path (commits-path org repo)
            :method :post
            :body body}))

(defn get-commit [org repo sha]
  (request {:path (commit-sha-path org repo sha)}))

(defspec create-commit-adds-commit-to-repo
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    tree github-tree-gen]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (= 200
          (:status (get-commit org-name (:name repo) sha)))))))

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
   [org-name object-name-gen
    repo (repo-gen)
    tree github-tree-gen]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (m/validate get-commit-response-schema
                   (get-commit org-name (:name repo) sha))))))

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

(defn get-ref [org repo ref]
  (request {:path (ref-path org repo ref)}))

(defn update-ref [org repo ref body]
  (request {:path (refs-ref-path org repo ref)
            :method :patch
            :body body}))

(defn delete-ref [org repo ref]
  (request {:path (refs-ref-path org repo ref)
            :method :delete}))

(defspec create-ref-adds-ref-to-repo
  10
  (prop/for-all
   [org-name object-name-gen
    repo (repo-gen)
    tree github-tree-gen
    ref (gen/not-empty gen/string-alphanumeric)]
   (with-handler {:orgs [{:name org-name
                          :repos [repo]}]}
     (let [{{tree-sha :sha} :body} (create-tree org-name (:name repo) {:tree tree})
           {{:keys [sha]} :body} (create-commit org-name (:name repo) {:message "some message"
                                                                       :tree tree-sha})]
       (create-ref org-name (:name repo) {:ref (str "refs/heads/" ref)
                                          :sha sha})
       (= 200
          (:status (get-ref org-name (:name repo) (str "heads/" ref))))))))

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
    tree github-tree-gen
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
    tree github-tree-gen
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
  (gen/let [tree github-tree-gen
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

