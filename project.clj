(defproject swank-clojure "1.5.0-SNAPSHOT"
  :description "Swank server connecting Clojure to Emacs SLIME"
  :url "http://github.com/technomancy/swank-clojure"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [clj-stacktrace "0.2.4"]
                 [org.clojure/tools.namespace "0.1.0"]
                 [cdt "1.2.6.2"]]
  :dev-dependencies [[lein-multi "1.0.0"]]
  :multi-deps {"1.3" [[org.clojure/clojure "1.3.0"]
                      [clj-stacktrace "0.2.4"]
                      [cdt "1.2.6.2"]]}
  :warn-on-reflection true
  :shell-wrapper {:main swank.swank})
