(defproject n01se/seqex "2.0.2-SNAPSHOT"
  :description "Sequence Expressions."
  :url "http://github.com/jclaggett/seqex"
  :license {:name         "Eclipse Public License - v 1.0"
            :url          "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments     "same as Clojure"}
  :min-lein-version "2.0.0"

  :dependencies [[criterium "0.3.1"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [clojure-complete "0.2.3"]]


  ;; Describe a performance specific test profile
  :test-selectors {:default (complement :perf), :perf :perf}

  :profiles {:1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}}
  :aliases {"1.8" ["with-profile" "1.8"]
            "1.7" ["with-profile" "1.7"]
            "1.6" ["with-profile" "1.6"]
            "1.5" ["with-profile" "1.5"]}
  ;; Use this to allow YourKit to connect:
  ;; :jvm-opts ["-agentpath:yjp/libyjpagent.so"]
  )
