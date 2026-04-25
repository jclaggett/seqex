(defproject london.rahul/panini "0.3.1"
  :description "Define Clojure macro syntax with clojure.spec.alpha."
  :license {:name         "Eclipse Public License - v 1.0"
            :url          "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments     "Includes work derived from seqex and new Panini code."}
  :min-lein-version "2.0.0"
  :java-version "25"
  :properties {"maven.compiler.source" "25"
               "maven.compiler.target" "25"
               "java.version" "25"}
  :pom-plugins [[org.apache.maven.plugins/maven-compiler-plugin "3.13.0"
                 {:configuration {:source "25"
                                  :target "25"}}]]

  :dependencies [[org.clojure/clojure "1.12.4"]])
