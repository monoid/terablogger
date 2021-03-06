(defproject terablogger "0.1.0-SNAPSHOT"
  :description "Static blog engine inspired by nanoblogger."
  :url "https://github.com/monoid/terablogger"
  :license {:name "GPL"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.4"]
                 [de.ubercode.clostache/clostache "1.3.1"]
                 [markdown-clj "0.9.28"]
                 [net.sf.textile4j/textile4j "1.2"]]
  :main terablogger.core)
