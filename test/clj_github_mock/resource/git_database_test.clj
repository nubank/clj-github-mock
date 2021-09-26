(ns clj-github-mock.resource.git-database-test
  (:require [clojure.test :refer [deftest is]]
            [clj-github-mock.generators :as mock-gen]
            [matcher-combinators.test :refer [match?]]
            [ring.mock.request :as mock]
            [datascript.core :as d]))

(deftest get-tree-test
  (let [{{:keys [owner0 repo0 tree0]} :ents handler :handler} (mock-gen/gen-ents {:tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})]
    (is (match? {:body {:sha "1b3303a1a927c3cf37df3deb03a5fd38df26c051"
                        :tree [{:path "some-file" :type "blob" :mode "100644" :sha "6b584e8ece562ebffc15d38808cd6b98fc3d97ea"}]}}
                (handler (mock/request :get (format "/repos/%s/%s/git/trees/%s" (:owner/name owner0) (:repo/name repo0) (:object/sha tree0))))))))

(deftest post-tree-test
  (let [{{:keys [owner0 repo0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:repo [[1]]})]
    (handler (-> (mock/request :post (format "/repos/%s/%s/git/trees" (:owner/name owner0) (:repo/name repo0)))
                 (assoc :body {:tree [{:path "some-file" :type "blob" :mode "100644" :content "content"}]})))
    (is (match? {:object/sha "1b3303a1a927c3cf37df3deb03a5fd38df26c051"}
                (d/pull @conn '[*] [:object/repo+type+sha [(:db/id repo0) "tree" "1b3303a1a927c3cf37df3deb03a5fd38df26c051"]])))))

(deftest get-commit-test
  (let [{{:keys [owner0 repo0 commit0]} :ents handler :handler} (mock-gen/gen-ents {:commit [[1 {:spec-gen {:commit/message "message"}}]]})]
    (is (match? {:body {:sha (:object/sha commit0)
                        :message "message"}}
                (handler (mock/request :get (format "/repos/%s/%s/git/commits/%s" (:owner/name owner0) (:repo/name repo0) (:object/sha commit0))))))))

(deftest post-commit-test
  (let [{{:keys [owner0 repo0 tree0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})
        {{:keys [sha]} :body} (handler (-> (mock/request :post (format "/repos/%s/%s/git/commits" (:owner/name owner0) (:repo/name repo0)))
                                           (assoc :body {:tree (:object/sha tree0)
                                                         :message "message"})))]
    (is (match? {:object/sha sha}
                (d/pull @conn '[*] [:object/repo+type+sha [(:db/id repo0) "commit" sha]])))))

(deftest get-ref-test
  (let [{{:keys [owner0 repo0 ref0]} :ents handler :handler} (mock-gen/gen-ents {:ref [[1 {:spec-gen {:ref/name "main"
                                                                                                      :ref/type :branch}}]]})]
    (is (match? {:body {:ref "refs/heads/main"
                        :object {:type :commit
                                 :sha (:object/sha (:ref/commit ref0))}}}
                (handler (mock/request :get (format "/repos/%s/%s/git/ref/%s" (:owner/name owner0) (:repo/name repo0) "heads/main")))))))

(deftest post-ref-test
  (let [{{:keys [owner0 repo0 commit0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:commit [[1]]})]
    (handler (-> (mock/request :post (format "/repos/%s/%s/git/refs" (:owner/name owner0) (:repo/name repo0)))
                 (assoc :body {:ref "refs/heads/main"
                               :sha (:object/sha commit0)})))
    (is (match? {:ref/commit {:object/sha (:object/sha commit0)}}
                (d/pull @conn '[{:ref/commit [:object/sha]}] [:ref/repo+type+name [(:db/id repo0) :branch "main"]])))))

(deftest patch-ref-test
  (let [{{:keys [owner0 repo0 commit0 tree0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:ref [[1 {:spec-gen {:ref/name "main"
                                                                                                                         :ref/type :branch}}]]
                                                                                                    :tree [[1 {:spec-gen {:tree/content [{:path "some-file" :type "blob" :mode "100644" :content "content"}]}}]]})
        {{tree-sha :sha} :body} (handler (-> (mock/request :post (format "/repos/%s/%s/git/trees" (:owner/name owner0) (:repo/name repo0)))
                                             (assoc :body {:base_tree (:object/sha tree0)
                                                           :tree [{:path "some-file" :type "blob" :mode "100644" :content "changed"}]})))
        {{:keys [sha]} :body} (handler (-> (mock/request :post (format "/repos/%s/%s/git/commits" (:owner/name owner0) (:repo/name repo0)))
                                           (assoc :body {:parents [(:object/sha commit0)]
                                                         :tree tree-sha
                                                         :message "message"})))]
    (handler (-> (mock/request :patch (format "/repos/%s/%s/git/refs/%s" (:owner/name owner0) (:repo/name repo0) "heads/main"))
                 (assoc :body {:sha sha})))
    (is (match? {:ref/commit {:object/sha  sha}}
                (d/pull @conn '[{:ref/commit [:object/sha]}] [:ref/repo+type+name [(:db/id repo0) :branch "main"]]))))
  (let [{{:keys [owner0 repo0 wrong-commit]} :ents handler :handler} (mock-gen/gen-ents {:ref [[1 {:spec-gen {:ref/name "main"
                                                                                                            :ref/type :branch}
                                                                                                 :refs {:ref/commit :the-commit}}]]
                                                                                       :commit [[:the-commit]
                                                                                                [:wrong-commit]]})]
    (is (match? {:status 422}
                (handler (-> (mock/request :patch (format "/repos/%s/%s/git/refs/%s" (:owner/name owner0) (:repo/name repo0) "heads/main"))
                             (assoc :body {:sha (:object/sha wrong-commit)}))))))
  (let [{{:keys [owner0 repo0 wrong-commit]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:ref [[1 {:spec-gen {:ref/name "main"
                                                                                                                            :ref/type :branch}
                                                                                                                 :refs {:ref/commit :the-commit}}]]
                                                                                                    :commit [[:the-commit]
                                                                                                             [:wrong-commit]]})]
    (handler (-> (mock/request :patch (format "/repos/%s/%s/git/refs/%s" (:owner/name owner0) (:repo/name repo0) "heads/main"))
                 (assoc :body {:sha (:object/sha wrong-commit)
                               :force true})))
    (is (match? {:ref/commit {:object/sha (:object/sha wrong-commit)}}
                (d/pull @conn '[{:ref/commit [:object/sha]}] [:ref/repo+type+name [(:db/id repo0) :branch "main"]])))))

(deftest delete-ref-test
  (let [{{:keys [owner0 repo0]} :ents conn :conn handler :handler} (mock-gen/gen-ents {:ref [[1 {:spec-gen {:ref/name "main"
                                                                                                            :ref/type :branch}}]]})]
    (handler (mock/request :delete (format "/repos/%s/%s/git/refs/%s" (:owner/name owner0) (:repo/name repo0) "heads/main")))
    (is (nil? (d/entid @conn [:ref/repo+type+name [(:db/id repo0) :branch "main"]])))))
