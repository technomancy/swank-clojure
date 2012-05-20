(ns swank.dev
  (:use (swank util)))

;;; TODO determine if this is used anywhere in 3rd party code.
;;; Swank-clojure does NOT use it.
(defmacro with-swank-io [& body]
  `(binding [*out* @(:writer-redir (first @swank.core.server/connections))]
     ~@body))
