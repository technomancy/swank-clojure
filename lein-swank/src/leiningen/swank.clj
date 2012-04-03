(ns leiningen.swank
  "Launch swank server for Emacs to connect."
  (:require [clojure.java.io :as io]))

(defn opts-list [project-opts port host cli-opts]
  (apply concat (merge {:repl-out-root true :block true
                        :host "localhost" :port 4005}
                       project-opts
                       (apply hash-map (map read-string cli-opts))
                       (if host {:host host})
                       (if port {:port (Integer. port)}))))

(defn swank-form [project port host cli-opts]
  ;; bootclasspath workaround: http://dev.clojure.org/jira/browse/CLJ-673
  (when (:eval-in-leiningen project)
    (require '[clojure walk template stacktrace]))
  `(do
     (when-let [repl-init# '~(:repl-init project)]
       (require repl-init#))
     (swank.swank/start-server ~@(opts-list (:swank-options project)
                                            port host cli-opts))))

(def ^{:private true} jvm-opts
  "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n")

(defn- add-cdt-jvm-opts [project]
  (if (seq (filter #(re-find #"jdwp" %)
                   (:jvm-opts project)))
    project
    (update-in project [:jvm-opts] conj jvm-opts)))

(defn add-cdt-project-args
  "CDT requires the JDK's tools.jar and sa-jdi.jar. Add them to the classpath."
  [project]
  (if (:swank-cdt project true)
    (let [libdir (io/file (System/getProperty "java.home") ".." "lib")
          extra-cp (for [j ["tools.jar" "sa-jdi.jar"]
                         :when (.exists (io/file libdir j))]
                     (.getAbsolutePath (io/file libdir j)))]
      (-> project
          (update-in [:extra-classpath-dirs] concat extra-cp)
          add-cdt-jvm-opts))
    project))

(defn eval-in-project
  "Support eval-in-project in both Leiningen 1.x and 2.x."
  [project form init]
  (let [[eip two?] (or (try (require 'leiningen.core.eval)
                            [(resolve 'leiningen.core.eval/eval-in-project)
                             true]
                            (catch java.io.FileNotFoundException _))
                       (try (require 'leiningen.compile)
                            [(resolve 'leiningen.compile/eval-in-project)]
                            (catch java.io.FileNotFoundException _)))]
    (if two?
      (eip project form init)
      (eip project form nil nil init))))

(defn add-swank-dep [project]
  (if (some #(= 'swank-clojure (first %)) (:dependencies project))
    project
    (update-in project [:dependencies] conj ['swank-clojure "1.4.2"])))

(defn swank
  "Launch swank server for Emacs to connect. Optionally takes PORT and HOST."
  ([project port host & opts]
     ;; TODO: only add the dependency if it's not already present
     (eval-in-project (-> project
                          (add-cdt-project-args)
                          (add-swank-dep))
                      (swank-form project port host opts)
                      '(require 'swank.swank)))
  ([project port] (swank project port nil))
  ([project] (swank project nil)))
