(ns yawc.util
  (:refer-clojure :exclude [read-line])
  (:require [clojure.string :as string])
  (:import (java.io ByteArrayOutputStream EOFException)
           (java.nio ByteBuffer)
           (java.nio.charset MalformedInputException StandardCharsets)
           (java.util Base64 Random)))

(def base64-encoder (Base64/getEncoder))
(def newline-byte (int \newline))
(def ulong-max (Math/pow 2 64))
(def ushort-max (Math/pow 2 16))
(def utf8-decoder (.newDecoder (StandardCharsets/UTF_8)))
(def random (Random.))
(def valid-status-codes
  (into #{nil 1000 1001 1002 1003 1007 1008 1009 1010 1011} (range 3000 5000)))

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

(defn number->bits
  "Returns `value` represented as `n` bits, most significant bit first.
  `value` must be representable as long."
  [value n]
  (let [number (long value)]
    (mapv #(bit-test number %) (reverse (range n)))))

(defn bits->number
  "Returns number from binary representation `bits`, most significant bit first."
  ([bits start end]
   (bits->number (subvec (vec bits) start end)))
  ([bits]
   (reduce-kv (fn [byte i bit] (if bit (bit-set byte i) byte))
              0 (vec (reverse bits)))))

(defn payload-length->bits
  "Returns `length` encoded as websocket frame length bits.
  See RFC 6455 - 5.2 on Payload length for more information."
  [length]
  (cond (< length 126) (number->bits length 7)
        (< length ushort-max) (concat (number->bits 126 7)
                                      (number->bits length 16))
        (< length ulong-max) (concat (number->bits 127 7)
                                     (number->bits length 64))
        :else (error "Payload too long" nil 1002)))

(defn bytes->utf8
  "Convert bytes `bs` to utf8 string. Throws exception on malformed utf8.
  Casting bytes to string using (String. bs) replaces malformed bytes/characters
  with some default rather than throwing an exception. To satisfy the websocket
  spec receiving malformed utf8 must not be accepted, so this is required."
  [bs]
  (try
    (->> (ByteBuffer/wrap bs) (.decode utf8-decoder) .toString)
    (catch MalformedInputException e
      (error (str "Malformed UTF8: " (String. bs)) nil 1007))))

(defn parse-close-payload
  "Parses `payload` bytes and returns close {status-code message}.
  The status code is validated according to RFC 6455 - 7.4.1, 7.4.2."
  [payload]
  (let [[status-code-bs message-bs] (split-at 2 payload)
        status-code (if (not-empty status-code-bs)
                      (biginteger (byte-array status-code-bs)))
        message (bytes->utf8 (byte-array message-bs))]
    (if-not (contains? valid-status-codes status-code)
      (error (str "Invalid status-code " status-code) nil 1002))
    {:status-code status-code :message message}))

(defn merge-frames
  "Merge `frame` and `frames` into one frame.
  Returns the first frame of `frames` extended with the merged payload and
  payload length."
  [frame frames]
  (let [frames (reverse (cons frame frames))
        payload (let [out (ByteArrayOutputStream.)]
                  (doseq [frame frames] (.write out (:payload frame)))
                  (.toByteArray out))]
    (assoc (first frames) :payload payload :length (alength payload))))

(defn masking-key-bytes
  "Returns 4 random bytes for use as masking key.
  See RFC 6455 - 5.3 for more information."
  []
  (let [seed (byte-array 4)]
    (.nextBytes random seed)
    seed))

(defn mask-payload
  "Return `payload` masked with masking `key-bytes`.
  `key-bytes` are generated via `masking-key-bytes` and xored with the `payload`
  bytes. As the payload can get quite big, this function has been optimized and
  is not as pretty as it could be. Profiling led to an evolution of:
  1. (map bit-xor payload (cycle key-bytes)) ; 4mb -> ~6s (executed with doall)
  2. (mapv bit-xor payload (cycle key-bytes)) ; 4mb -> ~2s
  3. current version ; 4mb -> ~1s
  This could probably be optimized much further or even inlined into emit (as
  writing a byte array to a stream is already looping over each byte)
  but YAGNI."
  [payload key-bytes]
  (amap ^bytes payload i _ (byte (bit-xor ^byte (aget ^bytes payload i)
                                          (aget ^bytes key-bytes (mod i 4))))))
