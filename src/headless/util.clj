(ns headless.util
  (:refer-clojure :exclude [read-line])
  (:require [clojure.string :as string])
  (:import (java.util Base64 Random)
           (java.io EOFException)))

(def base64-encoder (Base64/getEncoder))
(def newline-byte (int \newline))
(def random (Random.))

(defn error [message frame status-code]
  (throw (ex-info message {:frame frame :status-code status-code})))

(defn read-line
  "Read a line from `stream`.
  Java provides BufferedReader for this use case, but we cannot use it as we
  want to work on the raw stream (bytes) after the HTTP handshake. A
  BufferedReader would read ahead and buffer (d'uh) and thus prevent us from
  reading the bytes from the stream ourselves."
  [in]
  (loop [bytes []]
    (let [byte (.read in)]
      (cond
        (= byte -1) (throw (EOFException. "EOF while trying to read line."))
        (= byte newline-byte) (String. (byte-array bytes))
        :else (recur (conj bytes byte))))))

(defn read-bytes
  "Read exactly `n` bytes from `stream` (= readFully from DataInputStream).
  The default (.read stream byte-array) method does not necessarily fill
  the whole byte array passed to it. As this is oftentimes exactly what we want,
  this function wraps the default read to loop until we got everything we need.
  As the default read blocks until at least one byte has been read, the loop is
  going to be easy on our poor processor."
  [stream n]
  (if (> n 0)
    (loop [bs (byte-array n)
           offset 0]
      (let [nread (.read stream bs offset (- n offset))
            offset (+ offset nread)]
        (cond (= nread -1) (throw (EOFException. "EOF while reading bytes."))
              (= offset n) bs
              :else (recur bs offset))))
    (byte-array 0)))

(defn read-http-response
  "Read a HTTP response from `stream` into {:status :reason :headers :content}.
  All values apart from :headers are strings. Headers are a map of header to
  header value. Header names are normalized to all lower-case."
  [stream]
  (let [status-line (read-line stream)
        [match status reason] (re-find #"HTTP[^ ]+ (\d+) (.*)" status-line)]
    (if (string/blank? match)
      (throw (Exception. (str "Expected HTTP response but got: " status-line))))
    (let [headers (loop [headers {}]
                    (let [line (read-line stream)
                          [key value] (string/split line #" ?: ?")]
                      (if (string/blank? line)
                        headers
                        (recur (assoc headers (string/lower-case key)
                                      (string/trim value))))))
          content (->> (get headers "content-length" "0")
                       (Integer/parseInt)
                       (read-bytes stream)
                       (String.))]
      {:status status :reason reason :headers headers :content content})))

(defn get-random-base64
  "Returns a randomly generated 16 byte value encoded as base64.
  See RFC 6455 - 11.3.1 for more information."
  []
  (let [seed (byte-array 16)]
    (.nextBytes random seed)
    (->> seed (.encode base64-encoder) (String.))))
