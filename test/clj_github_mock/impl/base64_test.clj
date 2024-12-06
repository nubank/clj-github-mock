(ns clj-github-mock.impl.base64-test
  (:require [clj-github-mock.impl.base64 :as base64]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.util Arrays)))

(def test-cases
  [{:data ""
    :encoded ""}

   {:data    "Hello world"
    :encoded "SGVsbG8gd29ybGQ="}

   {:data "Eclipse Public License - v 2.0\n\n    THE ACCOMPANYING PROGRAM IS PROVIDED UNDER THE TERMS OF THIS ECLIPSE\n    PUBLIC LICENSE (\"AGREEMENT\"). ANY USE, REPRODUCTION OR DISTRIBUTION\n    OF THE PROGRAM CONSTITUTES RECIPIENT'S ACCEPTANCE OF THIS AGREEMENT.\n\n1. DEFINITIONS\n\n\"Contribution\" means:\n\n"
    :encoded "RWNsaXBzZSBQdWJsaWMgTGljZW5zZSAtIHYgMi4wCgogICAgVEhFIEFDQ09N\nUEFOWUlORyBQUk9HUkFNIElTIFBST1ZJREVEIFVOREVSIFRIRSBURVJNUyBP\nRiBUSElTIEVDTElQU0UKICAgIFBVQkxJQyBMSUNFTlNFICgiQUdSRUVNRU5U\nIikuIEFOWSBVU0UsIFJFUFJPRFVDVElPTiBPUiBESVNUUklCVVRJT04KICAg\nIE9GIFRIRSBQUk9HUkFNIENPTlNUSVRVVEVTIFJFQ0lQSUVOVCdTIEFDQ0VQ\nVEFOQ0UgT0YgVEhJUyBBR1JFRU1FTlQuCgoxLiBERUZJTklUSU9OUwoKIkNv\nbnRyaWJ1dGlvbiIgbWVhbnM6Cgo="}

   {:data (.readAllBytes (io/input-stream (io/resource "github-mark.png")))
    :encoded (slurp (io/resource "github-png-base64"))}])

(deftest base64-tests
  (doseq [{:keys [data encoded]} test-cases]
    (testing "encoding"
      (let [encoder (if (bytes? data)
                      base64/encode-bytes->str
                      base64/encode-str->str)]
        (is (= encoded (encoder data)))))

    (testing "decoding"
      (let [decoder (if (bytes? data)
                      base64/decode-str->bytes
                      base64/decode-str->str)
            checker (if (bytes? data)
                      ^[bytes bytes] Arrays/equals
                      =)]
        (is (checker data (decoder encoded)))))))

(defspec any-byte-array-roundtrips
  (prop/for-all [^bytes bs gen/bytes]
    (Arrays/equals bs (base64/decode-str->bytes (base64/encode-bytes->str bs)))))

(defspec any-string-roundtrips
  (prop/for-all [s gen/string]
    (= s (base64/decode-str->str (base64/encode-str->str s)))))
