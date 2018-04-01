(ns yawc.integration-test
  "Run the autobahn-testsuite client integration tests.
  The integration tests do not use clojure.test as they depend on the
  autobahn-testsuite docker container and have to be run inside docker-compose,
  i.e. should not be executed with the rest of the tests. Also they take forever
  to run."
  (:refer-clojure :exclude [await])
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]
            [yawc.core :as core])
  (:import javax.script.ScriptEngineManager))

(def nashorn (.getEngineByName (ScriptEngineManager.) "nashorn"))

(defn parse-json
  "Parse string `json` into some java data structure clojure understands.
  There are definitely better suited libraries for this but this is definitely
  more fun. Also performance is not a concern."
  [json]
  (->> (string/escape json {\newline ""})
       (format "Java.asJSONCompatible(JSON.parse('%s'));")
       (.eval nashorn)))

(defn await [promised timeout message]
  (let [result (deref promised timeout :timed-out)]
    (if (= result :timed-out)
      (throw (Exception. (format "Timeout (%dms) awaiting %s" timeout message)))
      result)))

(defn get-case-count [{:keys [host port]}]
  (let [case-count (promise)
        path "/getCaseCount"]
    (core/open
     {:host host
      :port port
      :path path
      :cb (fn [type payload client]
            (when (= type :text)
              (deliver case-count payload)))})
    (Integer/parseInt (await case-count 1000 path))))

(defn run-test-case [i {:keys [host port agent]}]
  (let [result (promise)
        path (format "/runCase?case=%s&agent=%s" i agent)]
    (core/open
     {:host host
      :port port
      :path path
      :cb (fn [type payload client]
            (case type
              :text (core/emit client {:fin 1 :opcode 1
                                       :payload (.getBytes payload)})
              :binary (core/emit client {:fin 1 :opcode 2 :payload payload})
              :close (deliver result i)
              nil))})
    (await result 30000 path)))

(defn update-report [{:keys [host port agent]}]
  (let [path (format "/updateReports?agent=%s" agent)
        result (promise)]
    (core/open {:host host
                :port port
                :path path
                :cb (fn [type payload client]
                      (when (= type :close)
                        (deliver result :updated)))})
    (await result 10000 path)))

(defn get-report [{:keys [agent] :as options}]
  (let [keys ["behavior" "behaviorClose" "duration"]
        comparator (fn [a b]
                     (let [[a1] (string/split (get a "id") #"\.")
                           [b1] (string/split (get b "id") #"\.")]
                       (< (Integer/parseInt a1) (Integer/parseInt b1))))
        report-index-json (slurp "./autobahn/reports/clients/index.json")
        report-index (parse-json report-index-json)
        report (or (get report-index agent)
                   (throw (Exception. (format "No report for %s" options))))
        report (mapv (fn [[k v]] (assoc (select-keys v keys) "id" k)) report)
        sorted-report (sort comparator report)]
    sorted-report))

(defn run-test-cases [options]
  (let [case-count (get-case-count options)]
    (doseq [i (range 1 (+ case-count 1))]
      (run-test-case i options))
    (update-report options)
    (let [report (get-report options)]
      (pprint/print-table report)
      (if (some (fn [case] (= (get case "behavior") "FAILED")) report)
        (System/exit 1)
        (System/exit 0)))))

(defn -main [host]
  (let [options {:host host :port 9001 :agent "yawc"}]
    (run-test-cases options)))
