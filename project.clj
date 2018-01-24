(defproject linux-commands "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [nio "1.0.3"]
                 [clojure-term-colors "0.1.0-SNAPSHOT"]
                 [org.clojure/core.async "0.4.474"]]
  :main ^:skip-aot linux-commands.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
