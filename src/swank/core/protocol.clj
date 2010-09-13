(ns swank.core.protocol
  (:use (swank util)
        (swank.util io))
  (:require swank.rpc))

;; Read forms
(def #^{:private true}
     *namespace-re* #"(^\(:emacs-rex \([a-zA-Z][a-zA-Z0-9]+):")

(defn- fix-namespace
  "Changes the namespace of a function call from pkg:fn to ns/fn. If
   no pkg exists, then nothing is done."
  ([text] (.replaceAll (re-matcher *namespace-re* text) "$1/")))

(defn- fix-cursor-marker
  "Changes the cursor marker"
  ([text] (.replace text "swank::%cursor-marker%" ":cursor-marker")))

(defn write-swank-message
  "Given a `writer' (java.io.Writer) and a `message' (typically an
   sexp), encode the message according to the swank protocol and
   write the message into the writer."
  ([#^java.io.Writer writer message]
     (swank.rpc/encode-message writer message)) 
  {:tag String})

(def read-fail-exception (Exception. "Error reading swank message"))

(defn read-swank-message
  "Given a `reader' (java.io.Reader), read the message as a clojure
   form (typically a sexp). This method will block until a message is
   completely transfered.

   See also `write-swank-message'."
  ([#^java.io.Reader reader]
     (swank.rpc/decode-message reader)))
