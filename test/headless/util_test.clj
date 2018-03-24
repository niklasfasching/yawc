(ns headless.util-test
  (:require [headless.util :as util]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io])
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
