(defproject headless "0.0.1"
  :description "bare-bones websocket client"
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot headless.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
