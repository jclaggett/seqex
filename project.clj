(defproject sc "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.5.0-beta1"]
                 [criterium "0.3.1"]]

  ;; Describe a performance specific test profile
  :test-selectors {:default (complement :perf), :perf :perf}

  ;; Use this to allow YourKit to connect:
  ;; :jvm-opts ["-agentpath:yjp/libyjpagent.so"])

