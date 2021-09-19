(ns clj-github-mock.resource.generators
  (:require [clojure.test.check.generators :as gen]
            [lambdaisland.regal.generator :as regal-gen]
            [malli.generator :as malli-gen]
            [medley.core :as m]
            [clojure.walk :as walk]))

(def blob
  "Generates a string that can be used as a blob content."
  gen/string-ascii)

(def object-name
  "Generates a name for objects like org names, repo names, branches, tags, etc."
  (regal-gen/gen [:+ :word]))

(defn unique-object-name
  "Creates a object-name generator that returns unique names."
  []
  (let [names (atom #{})]
    (gen/such-that
     #(let [result (@names %)]
        (swap! names conj %)
        (not result))
     object-name)))

(defn- flatten-map-tree-entry [[obj-name node]]
  (if (:type node)
    {obj-name node}
    (m/map-keys #(str obj-name "/" %) node)))

(defn- map-tree->github-tree [map-tree]
  (->> (walk/postwalk
        (fn [node]
          (if (and (map? node) (not (:type node)))
            (apply merge (map flatten-map-tree-entry node))
            node))
        map-tree)
       (mapv (fn [[path tree-object]]
               (assoc tree-object :path path)))))

(def ^:private modes-frequency {"100644" 95 "100755" 5})

(def ^:private github-tree-object
  (malli-gen/generator [:map
                        [:type [:= "blob"]]
                        [:mode (into [:enum {:gen/gen (gen/frequency (mapv #(vector (val %) (gen/return (key %))) modes-frequency))} (keys modes-frequency)])]
                        [:content [:string {:gen/gen blob}]]]))

(def github-tree
  "Generates a vector of github tree objects. A github tree object is a map
  representing a file with the following fields:

  - `:path`: a string with the path of the object, the path can be nested,
             in which case it will be separated by slashes
  - `:type`: this generator only creates object of the \"blob\" type
  - `:mode`: either `100644` for normal files and `100755` for executable files
             note: although the generator can return files with executable mode, the
             content of the file is not actualy something that can be executed
  - `:content`: a string with the content of the file"
  (gen/let [map-tree (gen/recursive-gen
                      #(gen/map object-name % {:min-elements 1})
                      github-tree-object)
            root-name (if (:type map-tree) object-name (gen/return nil))]
    (if root-name
      [(assoc map-tree :path root-name)]
      (map-tree->github-tree map-tree))))

(defn- update-gen [github-tree]
  (gen/let [item (gen/elements github-tree)
            new-content (gen/not-empty gen/string)]
    (assoc item :content new-content)))

(defn- delete-gen [github-tree]
  (gen/let [item (gen/elements github-tree)]
    (-> item
        (dissoc :content)
        (assoc :sha nil))))

; TODO support creating new files
(defn github-tree-changes
  "Creates a generator that given a github tree generates changes in that tree.
  The changes are themselves a github tree."
  [github-tree]
  (if (empty? github-tree)
    (gen/return github-tree)
    (gen/vector-distinct-by :path (gen/frequency [[9 (update-gen github-tree)] [1 (delete-gen github-tree)]]) {:min-elements 1})))

