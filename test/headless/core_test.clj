(ns headless.core-test
  (:require [clojure.test :refer :all]
            [headless.core :as core])
  (:import (java.lang Runtime)
           (java.io File)))

;; NOTE: the autobahn-server has to be started manually before running the tests

(deftest connect-test
  (let [host "localhost"
        port 9001
        path "/"
        socket (Socket. host port)
        in (io/input-stream socket)
        out (io/output-stream socket)
        client {:socket socket :in in :out out}
        result (core/connect client {:host host :port port :path path})]
    (is (= (:status result) "101"))))
