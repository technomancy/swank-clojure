(ns swank.swank
  (:use [swank core util]
        [swank.core connection server]
        [swank.util.concurrent thread]
        [swank.util.net sockets]
        [swank.commands.basic :only [get-thread-list]]
        [clojure.main :only [repl]])
  (:require [swank.commands]
            [swank.commands basic indent completion
             contrib inspector])
  (:import [java.lang System Thread]
           [java.io File])
  (:gen-class))

(def current-server (atom nil))

(defn ignore-protocol-version [version]
  (reset! protocol-version version))

(defn- connection-serve [conn]
  (let [#^Thread control
        (dothread-swank
          (thread-set-name "Swank Control Thread")
          (try
           (control-loop conn)
           (catch Exception e
             (when-not @shutting-down?
               (.println System/err "exception in control loop")
               (.printStackTrace e))
             nil))
          (close-socket! (conn :socket)))
        read
        (dothread-swank
          (thread-set-name "Swank Read Loop Thread")
          (try
           (read-loop conn control)
           (catch Exception e
             ;; This could be put somewhere better
             (when-not @shutting-down?
               (.println System/err "exception in read loop")
               (.printStackTrace e)
               (.interrupt control)
               (dosync (alter connections (partial remove #{conn})))))))]
    (dosync
     (ref-set (conn :control-thread) control)
     (ref-set (conn :read-thread) read))))

(defn load-cdt-with-dynamic-classloader []
    ;; cdt requires a dynamic classloader for tools.jar add-classpath
    ;;  lein swank doesn't seem to provide one.  Loading the backend
    ;;  like this works around that problem.
    (.start (Thread. #(do (use 'swank.core.cdt-backends)
                          (eval '(cdt-backend-init))))))

(defn start-server
  "Start the server and write the listen port number to
   PORT-FILE. This is the entry point for Emacs."
  [& opts]
  (if @current-server
    (println System/err "Swank server already running")
    (do
      (reset! shutting-down? false)
      (let [opts (apply hash-map opts)]
        (reset! color-support? (:colors? opts false))
        (reset! exit-on-quit? (:exit-on-quit opts true))
        (when (:load-cdt-on-startup opts)
          (load-cdt-with-dynamic-classloader))
        (reset! current-server
                (setup-server (get opts :port 0)
                              simple-announce
                              connection-serve
                              opts))
        (when (:block opts)
          (doseq [#^Thread t (get-thread-list)]
            (.join t)))))))

(defn stop-server
  "Stop the currently running server, shutdown its threads, and release the port."
  []
  (if @current-server
    (do
      (reset! shutting-down? true)
      (doseq [c @connections]
        (doseq [t [:control-thread :read-thread :repl-thread]]
          (when-let [^Thread thread @(c t)]
            (.interrupt thread))))
      (close-server-socket! @current-server)
      (dosync (ref-set connections []))
      (reset! current-server nil))
    (println System/err "Swank server not running")))

(defn start-repl
  "Start the server wrapped in a repl. Use this to embed swank in your code."
  ([port & opts]
     (let [stop (atom false)
           port (if (string? port) (Integer/parseInt port) (int port))
           opts (assoc (apply hash-map opts) :port port)]
       (repl :read (fn [rprompt rexit]
                     (if @stop rexit
                         (do (reset! stop true)
                             `(start-server ~@(apply concat opts)))))
             :need-prompt (constantly false))))
  ([] (start-repl (or (System/getenv "PORT") 4005))))

(defn -main [port & opts]
  (apply start-server
         (for [a (concat [":port" port] opts)]
           (cond (re-find #"^\d+$" a) (Integer/parseInt a)
                 (re-find #"^:\w+$" a) (keyword (subs a 1))
                 :else a))))
