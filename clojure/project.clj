(defproject biotool "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-biosequence "0.5.2"]
                 [org.clojure/tools.cli "0.3.5"]
                 [com.taoensso/timbre "4.8.0"]]
  :main ^:skip-aot biotool.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
