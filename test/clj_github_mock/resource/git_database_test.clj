(ns clj-github-mock.resource.git-database-test
  (:require [clojure.test :refer [deftest is]]
            [clj-github-mock.generators :as mock-gen]
            [matcher-combinators.test :refer [match?]]
            [ring.mock.request :as mock]
            [datascript.core :as d]))

(deftest get-tree-test
  (let [{{:keys [org0 repo0 tree0]} :ents handler :handler} (mock-gen/gen-ents {:tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})]
    (is (match? {:body {:sha "1b3303a1a927c3cf37df3deb03a5fd38df26c051"
                        :tree [{:path "some-file" :type "blob" :mode "100644" :sha "6b584e8ece562ebffc15d38808cd6b98fc3d97ea"}]}}
                (handler (mock/request :get (format "/repos/%s/%s/git/trees/%s" (:org/name org0) (:repo/name repo0) (:tree/sha tree0))))))))

(deftest post-tree-test
  (let [{{:keys [org0 repo0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:repo [[1]]})]
    (handler (-> (mock/request :post (format "/repos/%s/%s/git/trees" (:org/name org0) (:repo/name repo0)))
                 (assoc :body {:tree [{:path "some-file" :type "blob" :mode "100644" :content "content"}]})))
    (is (match? {:tree/sha "1b3303a1a927c3cf37df3deb03a5fd38df26c051"}
                (d/pull @conn '[*] [:tree/repo+sha [(:db/id repo0) "1b3303a1a927c3cf37df3deb03a5fd38df26c051"]])))))

(deftest get-commit-test
  (let [{{:keys [org0 repo0 commit0]} :ents handler :handler} (mock-gen/gen-ents {:commit [[1 {:spec-gen {:commit/message "message"}}]]})]
    (is (match? {:body {:sha (:commit/sha commit0)
                        :message "message"}}
                (handler (mock/request :get (format "/repos/%s/%s/git/commits/%s" (:org/name org0) (:repo/name repo0) (:commit/sha commit0))))))))

(deftest post-commit-test
  (let [{{:keys [org0 repo0 tree0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})
        {{:keys [sha]} :body} (handler (-> (mock/request :post (format "/repos/%s/%s/git/commits" (:org/name org0) (:repo/name repo0)))
                                           (assoc :body {:tree (:tree/sha tree0)
                                                         :message "message"})))]
    (is (match? {:commit/sha sha}
                (d/pull @conn '[*] [:commit/repo+sha [(:db/id repo0) sha]])))))

(deftest get-ref-test
  (let [{{:keys [org0 repo0 branch0]} :ents handler :handler} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/name "main"}}]]})]
    (is (match? {:body {:ref "refs/heads/main"
                        :object {:type :commit
                                 :sha (:commit/sha (:branch/commit branch0))}}}
                (handler (mock/request :get (format "/repos/%s/%s/git/ref/%s" (:org/name org0) (:repo/name repo0) "heads/main")))))))

(deftest post-ref-test
  (let [{{:keys [org0 repo0 commit0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:commit [[1]]})]
    (handler (-> (mock/request :post (format "/repos/%s/%s/git/refs" (:org/name org0) (:repo/name repo0)))
                 (assoc :body {:ref "refs/heads/main"
                               :sha (:commit/sha commit0)})))
    (is (match? {:branch/commit {:commit/sha (:commit/sha commit0)}}
                (d/pull @conn '[{:branch/commit [:commit/sha]}] [:branch/repo+name [(:db/id repo0) "main"]])))))

(deftest patch-ref-test
  (let [{{:keys [org0 repo0 commit0 tree0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/name "main"}}]]
                                                                                                   :tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})
        {{tree-sha :sha} :body} (handler (-> (mock/request :post (format "/repos/%s/%s/git/trees" (:org/name org0) (:repo/name repo0)))
                                             (assoc :body {:base_tree (:tree/sha tree0)
                                                           :tree [{:path "some-file" :type "blob" :mode "100644" :content "changed"}]})))
        {{:keys [sha]} :body} (handler (-> (mock/request :post (format "/repos/%s/%s/git/commits" (:org/name org0) (:repo/name repo0)))
                                           (assoc :body {:parents [(:commit/sha commit0)]
                                                         :tree tree-sha
                                                         :message "message"})))]
    (handler (-> (mock/request :patch (format "/repos/%s/%s/git/refs/%s" (:org/name org0) (:repo/name repo0) "heads/main"))
                 (assoc :body {:sha sha})))
    (is (match? {:branch/commit {:commit/sha  sha}}
                (d/pull @conn '[{:branch/commit [:commit/sha]}] [:branch/repo+name [(:db/id repo0) "main"]]))))
  (let [{{:keys [org0 repo0 wrong-commit]} :ents handler :handler} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/name "main"}
                                                                                                    :refs {:branch/commit :the-commit}}]]
                                                                                       :commit [[:the-commit]
                                                                                                [:wrong-commit]]})]
    (is (match? {:status 422}
                (handler (-> (mock/request :patch (format "/repos/%s/%s/git/refs/%s" (:org/name org0) (:repo/name repo0) "heads/main"))
                             (assoc :body {:sha (:commit/sha wrong-commit)}))))))
  (let [{{:keys [org0 repo0 wrong-commit]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/name "main"}
                                                                                                    :refs {:branch/commit :the-commit}}]]
                                                                                       :commit [[:the-commit]
                                                                                                [:wrong-commit]]})]
    (handler (-> (mock/request :patch (format "/repos/%s/%s/git/refs/%s" (:org/name org0) (:repo/name repo0) "heads/main"))
                 (assoc :body {:sha (:commit/sha wrong-commit)
                               :force true})))
    (is (match? {:branch/commit {:commit/sha (:commit/sha wrong-commit)}}
                (d/pull @conn '[{:branch/commit [:commit/sha]}] [:branch/repo+name [(:db/id repo0) "main"]])))))


(deftest delete-ref-test
  (let [{{:keys [org0 repo0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:branch [[1 {:spec-gen {:branch/name "main"}}]]})]
    (handler (mock/request :delete (format "/repos/%s/%s/git/refs/%s" (:org/name org0) (:repo/name repo0) "heads/main")))
    (is (nil? (d/entid @conn [:branch/repo+name [(:db/id repo0) "main"]])))))
