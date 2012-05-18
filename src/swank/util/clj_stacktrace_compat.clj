(ns swank.util.clj-stacktrace-compat
  "This is an ugly hack to support older version of clj-stacktrace
  that are often pulled in by other libs as a dep."
  (:require [clj-stacktrace.repl :as repl])
  (:require [clj-stacktrace.utils :as utils]))

(if (ns-resolve 'clj-stacktrace.repl 'pst-elem-str)
  (def pst-elem-str (ns-resolve 'clj-stacktrace.repl 'pst-elem-str))
  (let [colored (ns-resolve 'clj-stacktrace.repl 'colored)]
    (defn pst-elem-str
      [color? parsed-elem print-width]
      (colored color? (clj-stacktrace.repl/elem-color parsed-elem)
               (str (utils/rjust print-width
                                 (clj-stacktrace.repl/source-str parsed-elem))
                    " " (clj-stacktrace.repl/method-str parsed-elem))))))

(def find-source-width (ns-resolve 'clj-stacktrace.repl 'find-source-width))
