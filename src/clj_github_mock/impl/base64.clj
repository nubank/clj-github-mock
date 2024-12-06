(ns clj-github-mock.impl.base64
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.util Base64 Base64$Decoder Base64$Encoder)))

(set! *warn-on-reflection* true)

(def ^:private ^Base64$Encoder base64-encoder (Base64/getEncoder))
(def ^:private ^Base64$Decoder base64-decoder (Base64/getDecoder))

(defn- line-wrap
  "Includes line breaks in the provided string `s` every `limit` characters.

  Used to mirror GitHub API's behavior that includes breaks in some
  base64-encoded strings."
  ^String [s limit]
  (->> s
       (partition-all limit)
       (map str/join)
       (str/join "\n")))

(defn- unwrap-lines
  "Strips line breaks from a base64-encoded string."
  ^String [s]
  (str/replace s "\n" ""))

(defn encode-bytes->str
  "Encodes the given byte array to its Base64 representation."
  ^String [^bytes bs]
  (let [data (.encode base64-encoder bs)]
    (line-wrap (String. data StandardCharsets/UTF_8) 60)))

(defn encode-str->str
  "Encodes the given String to its Base64 representation using UTF-8."
  ^String [^String s]
  (encode-bytes->str (.getBytes s StandardCharsets/UTF_8)))

(defn decode-str->bytes
  "Decodes the given Base64 String to a byte array."
  ^bytes [^String s]
  (let [bs (.getBytes (unwrap-lines s) StandardCharsets/UTF_8)]
    (.decode base64-decoder bs)))

(defn decode-str->str
  "Decodes the given Base64 String to a new String using UTF-8."
  ^String [^String s]
  (String. (decode-str->bytes s) StandardCharsets/UTF_8))
