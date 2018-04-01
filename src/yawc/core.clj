(ns yawc.core
  "Minimal webocket client implementation according to RFC 6455."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [yawc.util :as util])
  (:import (clojure.lang ExceptionInfo)
           (java.net Socket)))

(defn connect
  "Connect to websocket server and return input and output streams.
  A websocket connection in opened by performing a HTTP handshake. If the
  handshake succeeds, a HTTP response with status 101 is send back.
  See RFC 6455 - 4.2.1 for more information on the opening handshake from
  the client."
  [{:keys [in out] :as client} {:keys [host port path] :as options}]
  (let [lines [(str "GET " path " HTTP/1.1")
               (str "Sec-WebSocket-Key: " (util/get-random-base64))
               (str "Host: " host ":" port)
               "Upgrade: websocket"
               "Connection: Upgrade"
               "Sec-WebSocket-Version: 13" "" ""]
        http-request (string/join "\r\n" lines)]
    (.write out (.getBytes http-request))
    (.flush out)
    (let [response (util/read-http-response in)]
      (if (= (:status response) "101")
        response
        (throw (ex-info "Could not connect to server" response))))))

(defn emit
  "Emit websocket `frame` on `out` stream of `client`.
  The frame should contain [fin opcode payload]. The keys [rsv mask masking-key]
  are optional and default to {:rsv 0 :mask 1}"
  [{:keys [out] :as client} {:keys [fin opcode payload rsv mask] :as frame}]
  (if (realized? (:result client))
    (throw (Exception. (str "Client has closed with " @(:result client))))
    (let [rsv (or rsv 0)
          mask (or mask 1)
          masking-key-bytes (if (= mask 1) (util/masking-key-bytes) nil)
          payload (if masking-key-bytes
                    (util/mask-payload payload masking-key-bytes)
                    payload)
          header-bits (concat (util/number->bits fin 1)
                              (util/number->bits rsv 3)
                              (util/number->bits opcode 4)
                              (util/number->bits mask 1)
                              (util/payload-length->bits (count payload)))
          header-bytes (map util/bits->number (partition 8 header-bits))]
      (.write out (byte-array header-bytes))
      (if masking-key-bytes (.write out (byte-array masking-key-bytes)))
      (.write out (byte-array payload))
      (.flush out))))

(defn receive
  "Receive a websocket frame from the `in` stream of `client`.
  The frame contains the keys [fin rsv opcode length payload]."
  [{:keys [in] :as client}]
  (let [bits (concat (util/number->bits (.read in) 8)
                     (util/number->bits (.read in) 8))
        fin (util/bits->number bits 0 1)
        rsv (util/bits->number bits 1 4)
        opcode (util/bits->number bits 4 8)
        mask (util/bits->number bits 8 9)
        length (let [length (util/bits->number bits 9 16)]
                 (cond (< length 126) length
                       (= length 126) (BigInteger. 1 (util/read-bytes in 2))
                       (= length 127) (BigInteger. 1 (util/read-bytes in 8))
                       :else (util/error "Invalid payload length" nil 1002)))
        payload (util/read-bytes in length)]
    {:fin fin :rsv rsv :opcode opcode :length length :payload payload}))

(defn close
  "Close connection of `client` with `status-code` and `message` as payload.
  The client should but is not required to wait for the server to close the
  connection. Waiting complicates things so we are bad and close immediately."
  [client status-code message]
  (let [payload (concat (and status-code (.toByteArray (biginteger status-code)))
                        (.getBytes message))]
    (emit client {:fin 1 :opcode 8 :payload (byte-array payload)})
    (.close (:out client)) ; closing the out stream closes socket & in stream
    (deliver (:result client) {:status-code status-code :message message})))

(defn validate-frame
  "Validate `frame` according to websocket spec.
  Validates rsv, masking, continuation, fragmentation & length."
  [frames {:keys [fin rsv mask opcode length] :as frame}]
  (cond
    (and (not (contains? #{0 8 9 10} opcode))
         (not-empty frames)) (util/error "Wrong continuation opcode" frame 1002)
    (= mask 1) (util/error "Frame from server must not be masked" frame 1002)
    (not= rsv 0) (util/error "RSV must be 000" frame 1002)
    (and (contains? #{8 9 10} opcode)
         (> length 125)) (util/error "ControlFrame too long" frame 1002)
    (and (contains? #{8 9 10} opcode)
         (not (= fin 1))) (util/error "ControlFrame fragmented" frame 1002)))

(defn handle-frame
  "React to `frame` by emitting reply to `client` and notifying `cb`.
  cb is called with message type, payload & client as arguments.
  Message type is one of #(:text :binary :close :ping :pong).
  Validates opcode of `frame` to correspond to one of the above message types."
  [client {:keys [opcode payload] :as frame} cb]
  (condp = opcode
    1 (cb :text (util/bytes->utf8 payload) client)
    2 (cb :binary payload client)
    8 (let [parsed-payload (util/parse-close-payload payload)]
        (close client (:status-code parsed-payload) (:message parsed-payload))
        (cb :close parsed-payload client)
        (reduced nil))
    9 (do
        (emit client {:fin 1 :opcode 10 :payload payload})
        (cb :ping payload client))
    10 (cb :pong payload client)
    (util/error "Invalid opcode" frame 1002)))

(defn receive-loop
  "Receive frames for `client` in a loop until connection is closed.
  Calls `cb` for each (valid) received frame."
  [client cb]
  (try
    (loop [frames nil]
      (let [{:keys [fin opcode] :as frame} (receive client)]
        (validate-frame frames frame)
        (cond
          (and (= opcode 0)
               (= fin 1)) (let [merged-frame (util/merge-frames frame frames)]
                            (handle-frame client merged-frame cb)
                            (recur nil))
          (= fin 1) (when-not (reduced? (handle-frame client frame cb))
                      (recur frames))
          :else (recur (cons frame frames)))))
    (catch ExceptionInfo e
      (let [status-code (:status-code (ex-data e))]
        (close client status-code "")
        (cb :close {:status-code status-code :message ""} client)))))

(defn open
  "Returns a websocket client connected to `host`:`port`/`path`.
  Whenever a message is received `cb` is called with (cb type payload client).
  - type is one of :text :binary :ping :pong :close.
  - payload is
    - utf8 string for :text
    - binary for :ping, :pong & :binary
    - {status-code message} for :close"
  [{:keys [host port cb] :as options}]
  (let [socket (Socket. host port)
        in (io/input-stream socket)
        out (io/output-stream socket)
        client {:socket socket :in in :out out :result (promise)}
        cb (fn [type value client] (do (cb type value client) nil))]
    (connect client options)
    (future (receive-loop client cb))
    client))
