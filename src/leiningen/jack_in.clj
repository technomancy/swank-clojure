(ns leiningen.jack-in
  (:use [leiningen.compile :only [eval-in-project]]
        [leiningen.swank :only [swank]])
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.util.jar JarFile)
           (java.security MessageDigest)))

(defn- get-manifest [file]
  (let [attrs (-> file JarFile. .getManifest .getMainAttributes)]
    (zipmap (map str (keys attrs)) (vals attrs))))

(defn- get-payloads [file]
  (.split (get (get-manifest file) "Swank-Elisp-Payload") " "))

(defn elisp-payload-files []
  ["swank/payload/slime.el" "swank/payload/slime-repl.el"]
  #_(apply concat ["swank/payload/slime.el" "swank/payload/slime-repl.el"]
           (->> (scan-paths (System/getProperty "sun.boot.class.path")
                            (System/getProperty "java.ext.dirs")
                            (System/getProperty "java.class.path"))
                (filter #(jar-file? (.getName (:file %))))
                (get-payloads))))

(defn hex-digest [file]
  (format "%x" (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA1")
                                       (-> file io/resource slurp .getBytes)))))

(defn loader [resource]
  (let [feature (second (re-find #".*/(.*?).el$" resource))
        checksum (subs (hex-digest resource) 0 8)
        basename (format "%s/.emacs.d/swank/%s-%s"
                         (System/getProperty "user.home")
                         feature checksum)
        elisp (str basename ".el")
        bytecode (str basename ".elc")
        elisp-file (io/file elisp)]
    (.mkdirs (.getParentFile elisp-file))
    (with-open [r (.openStream (io/resource resource))]
      (io/copy r elisp-file))
    (with-open [w (io/writer elisp-file :append true)]
      (.write w (format "\n(provide '%s-%s)\n" feature checksum)))
    (format "(when (not (featurep '%s-%s))
               (if (file-readable-p \"%s\")
                 (load-file \"%s\")
               (byte-compile-file \"%s\" t)))"
            feature checksum bytecode bytecode elisp)))

(defn payload-loaders []
  (for [file (elisp-payload-files)]
    (loader file)))

(defn jack-in
  "Jack in to a Clojure SLIME session from Emacs.

This task is intended to be launched from Emacs using M-x clojure-jack-in,
which is part of the clojure-mode library."
  [project port]
  (println ";;; Bootstrapping bundled version of SLIME; please wait...\n\n")
  (println (string/join "\n" (payload-loaders)))
  (println "(sleep-for 0.1)") ; TODO: remove
  (println "(run-hooks 'slime-load-hook) ; on port" port)
  (swank project port "localhost" ":message" "\";;; proceed to jack in\""))
