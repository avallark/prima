(defproject prima "0.1.0-SNAPSHOT"
  :description "Prima app prints prime number multiplication tables"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main prima.core
  :profiles {:uberjar {:aot :all}})
