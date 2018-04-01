(ns yawc.util-test
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [yawc.util :as util])
  (:import (java.io ByteArrayInputStream EOFException StringReader)
           (java.util Arrays)))

(deftest read-line-test
  (let [stream (StringReader. "Foo\nBar\nBaz\n")]
    (is (= (util/read-line stream) "Foo"))
    (is (= (util/read-line stream) "Bar"))
    (is (= (util/read-line stream) "Baz"))
    (is (thrown? EOFException (util/read-line stream)))))

(deftest read-bytes-test
  (testing "default read"
    (let [stream (ByteArrayInputStream. (byte-array (range 10)))]
      (is (= (.read stream (byte-array 0)) 0))
      (is (= (.read stream (byte-array 9)) 9))
      ;; does return with just 1 byte read
      (is (= (.read stream (byte-array 4)) 1))))

  (testing "read-bytes"
    (let [stream (ByteArrayInputStream. (byte-array (range 10)))]
      (is (Arrays/equals (util/read-bytes stream 0) (byte-array [])))
      (is (Arrays/equals (util/read-bytes stream 2) (byte-array [0 1])))
      (is (Arrays/equals (util/read-bytes stream 3) (byte-array [2 3 4])))
      (is (Arrays/equals (util/read-bytes stream 4) (byte-array [5 6 7 8])))
      ;; does not return with just 1 byte read
      (is (thrown? EOFException (util/read-bytes stream 4))))))

(deftest read-http-response-test
  (let [input (string/join "\r\n"
                           ["HTTP/1.1 101 Switching Protocols"
                            "Upgrade: websocket"
                            "Connection: Upgrade"
                            "Sec-WebSocket-Accept: xyz"
                            "" ""])
        stream (ByteArrayInputStream. (.getBytes input))
        response (util/read-http-response stream)
        headers (:headers response)]
    (is (= (:status response) "101"))
    (is (= (:reason response) "Switching Protocols"))
    (is (= (get headers "connection") "Upgrade"))
    (is (= (get headers "upgrade") "websocket"))))

(deftest get-random-base64-test
  (is (= (count (util/get-random-base64)) 24))
  (is (not= (util/get-random-base64)
            (util/get-random-base64))))

(deftest number->bits-test
  (is (= (util/number->bits 0 8) (repeat 8 false)))
  (is (= (util/number->bits 8 4) [true false false false]))
  (is (= (util/number->bits 9 4) [true false false true])))

(deftest bits->number-test
  (is (= (util/bits->number [true false false false]) 8))
  (is (= (util/bits->number [true false false false] 0 1) 1))
  (is (= (util/bits->number [true false false false] 0 2) 2))
  (is (= (util/bits->number [true false false false] 0 3) 4))
  (is (= (util/bits->number (util/number->bits (Math/pow 2 63) 64))
         (long (Math/pow 2 63)))))

(deftest payload-length->bits-test
  (is (= (util/payload-length->bits 1)
         (util/number->bits 1 7)))
  (is (= (util/payload-length->bits 67)
         (util/number->bits 67 7)))
  (is (= (util/payload-length->bits 127)
         (concat (util/number->bits 126 7)
                 (util/number->bits 127 16))))
  (is (= (util/payload-length->bits (Math/pow 2 15))
         (concat (util/number->bits 126 7)
                 (util/number->bits (Math/pow 2 15) 16))))
  (is (= (util/payload-length->bits (Math/pow 2 63))
         (concat (util/number->bits 127 7)
                 (util/number->bits (Math/pow 2 63) 64))))
  (is (thrown? Exception (util/payload-length->bits (Math/pow 2 64)))))

(deftest bytes->utf8-test
  (is (= (util/bytes->utf8 (byte-array [])) ""))
  (is (= (util/bytes->utf8 (.getBytes "Hello World!")) "Hello World!"))
  (is (thrown? Exception (util/bytes->utf8 (byte-array [255])))))

(deftest parse-close-payload-test
  (is (= (util/parse-close-payload []) {:status-code nil, :message ""}))
  (is (= (util/parse-close-payload (.toByteArray (biginteger 1002)))
         {:status-code 1002 :message ""}))
  (is (= (util/parse-close-payload (concat (.toByteArray (biginteger 1000))
                                           (.getBytes "Hello")))
         {:status-code 1000 :message "Hello"}))
  (is (thrown? Exception (util/parse-close-payload [255 255]))))

(deftest merge-frames-test
  (let [frame (util/merge-frames {:opcode 0 :payload (byte-array [5 6])}
                                 [{:opcode 0 :payload (byte-array [3 4])}
                                  {:opcode 1 :payload (byte-array [1 2])}])]
    (is (Arrays/equals (:payload frame) (byte-array [1 2 3 4 5 6])))
    (is (= (:opcode frame) 1))
    (is (= (:length frame) 6))))

(deftest masking-key-bytes-test
  (is (= (count (util/masking-key-bytes)) 4))
  (is (not= (util/masking-key-bytes)
            (util/masking-key-bytes))))

(deftest mask-payload-test
  (let [payload (byte-array 100)
        key-bytes (util/masking-key-bytes)]
    (is (Arrays/equals (util/mask-payload payload key-bytes)
                       (byte-array (take 100 (cycle key-bytes)))))))
