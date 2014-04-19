(defproject com.climate/prng "0.1.0-SNAPSHOT"
  :description "Pseudorandom number generators in Clojure"
  :url "http://github.com/TheClimateCorporation/prng"
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :min-lein-version "2.0.0"
  :pedantic? :warn
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.apache.commons/commons-lang3 "3.3.2"]
                 [org.bouncycastle/bcprov-jdk15on "1.50"]
                 [de.kotka/lazymap "3.1.1"]]
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :profiles {:dev {:plugins [[lein-marginalia "0.7.1"]]
                   :dependencies [[criterium "0.4.2"]]
                   :resource-paths ["resources" "test/resources"]
                   :jvm-opts []}}
  :test-selectors {:default #(not-any? % [:benchmark])
                   :benchmark :benchmark
                   :all (constantly true)})
