(defproject temperature-rest "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://localhost/temperature"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.3.1"]
                 [ring/ring-defaults "0.1.2"]
                 [ring/ring-json "0.3.1"]
                 [cheshire "5.4.0"]
                 [clj-time "0.9.0"]
                 [iota "1.1.2"]]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler temperature-rest.core.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})
