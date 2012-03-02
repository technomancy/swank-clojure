(ns swank.util.sys
  (:import (java.io BufferedReader InputStreamReader)))

(defn get-pid
  "Returns the PID of the JVM. This is largely a hack and may or may
   not be accurate depending on the JVM in which clojure is running
   off of."
  ([]
     (or (first (.. java.lang.management.ManagementFactory
                    (getRuntimeMXBean) (getName) (split "@")))
         (System/getProperty "pid")))
  {:tag String})

(defn #^java.lang.Process cmd [p]
  (.. Runtime getRuntime (exec (str p))))

(defn cmdout [^Process o]
  (let [r (BufferedReader.
           (InputStreamReader. (.getInputStream o)))]
    (line-seq r)))

;; would prefer (= (System/getenv "OSTYPE") "cygwin")
;; but clojure's java not in cygwin env
(defn is-cygwin? []
  (not= nil (try (cmdout (cmd "cygpath c:\\")) (catch Exception e))))

(defn universal-path [path]
  (if (is-cygwin?)
    (first (cmdout (cmd (str "cygpath " path))))
    path))

(defn preferred-user-home-path []
  (or (System/getenv "HOME")
      (System/getProperty "user.home")))

(defn user-home-path []
   (universal-path (preferred-user-home-path)))
