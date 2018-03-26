(ns headless.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [headless.core :as core]
            [headless.util :as util]
            [clojure.string :as string])
  (:import (java.lang Runtime)
           (java.io ByteArrayInputStream ByteArrayOutputStream File)
           (java.net Socket)
           (java.util Arrays)))

(deftest connect-test
  (let [host "localhost"
        port 9001
        path "/path"
        response (string/join "\r\n"
                              ["HTTP/1.1 101 Switching Protocols"
                               "Upgrade: websocket"
                               "Connection: Upgrade"
                               "Sec-WebSocket-Accept: foobar"
                               "" ""])
        in (ByteArrayInputStream. (.getBytes response))
        out (ByteArrayOutputStream.)
        client {:in in :out out :result (promise)}
        result (core/connect client {:host host :port port :path path})
        request (String. (.toByteArray out))]
    (is (= (:status result) "101"))
    (is (= (:reason result) "Switching Protocols"))
    (is (= (:content result) ""))
    (is (= (:headers result) {"upgrade" "websocket"
                              "connection" "Upgrade"
                              "sec-websocket-accept" "foobar"}))
    (is (=(re-find #"Host:.*" request) "Host: localhost:9001"))
    (is (=(re-find #"GET.*" request) "GET /path HTTP/1.1"))))

(deftest emit-test
  ;; Example from RFC 6455 - 5.7
  (testing "A single-frame unmasked text message containing 'Hello'"
    (let [payload (.getBytes "Hello")
          out (ByteArrayOutputStream.)
          client {:out out :result (promise)}]
      (core/emit client {:fin 1 :opcode 1 :payload payload :mask 0})
      (is (Arrays/equals (.toByteArray out)
                         (byte-array [0x81 0x05 0x48 0x65 0x6c 0x6c 0x6f])))))

  ;; Example from RFC 6455 - 5.7
  (testing "Unmasked Ping request with message containing 'Hello'"
    (let [payload (.getBytes "Hello")
          out (ByteArrayOutputStream.)
          client {:out out :result (promise)}]
      (core/emit client {:fin 1 :opcode 9 :payload payload :mask 0})
      (is (Arrays/equals (.toByteArray out)
                         (byte-array [0x89 0x05 0x48 0x65 0x6c 0x6c 0x6f])))))

  (testing "Throw when emitting on closed client"
    (let [payload (.getBytes "Hello")
          out (ByteArrayOutputStream.)
          client {:out out :result (promise)}
          frame {:fin 1 :opcode 9 :payload payload :mask 0}]
      (deliver (:result client) {:status-code 1000 :message ""})
      (is (thrown-with-msg? Exception #"closed .*:status-code 1000"
                            (core/emit client frame))))))

(deftest receive-test
  (let [payload (.getBytes "Hello")
        client (doto {:out (ByteArrayOutputStream.) :result (promise)}
                 (core/emit {:fin 1 :opcode 1 :payload payload :mask 0}))
        in (ByteArrayInputStream. (.toByteArray (:out client)))
        frame (core/receive (assoc client :in in))]
    (is (= (:fin frame) 1))
    (is (= (:rsv frame) 0))
    (is (= (:length frame) 5))
    (is (= (String. (:payload frame)) "Hello"))))

(deftest close-test
  (let [client {:out (ByteArrayOutputStream.) :result (promise)}]
    (core/close client 1000 "")
    (is (realized? (:result client)))
    (is (= @(:result client) {:status-code 1000 :message ""}))))
