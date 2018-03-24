(ns headless.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [headless.util :as util])
  (:import (java.net Socket)))

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
