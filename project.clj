(defproject swank-clojure "1.3.4-SNAPSHOT"
  :description "Swank server connecting Clojure to Emacs SLIME"
  :url "http://github.com/technomancy/swank-clojure"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [clj-stacktrace "0.2.4-SNAPSHOT"]]
  :dev-dependencies [[lein-multi "1.0.0"]]
  :multi-deps {"1.3" [[org.clojure/clojure "1.3.0"]]}
  :shell-wrapper {:main swank.swank})
