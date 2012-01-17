(ns swank.core.server
  (:use (swank util core)
        (swank.util sys io)
        (swank.util.concurrent thread)
        (swank.util.net sockets)
        (swank.core connection protocol))
  (:import (java.io File FileReader BufferedReader PrintWriter
                    InputStreamReader OutputStreamWriter)
           (java.net Socket)))

;; The swank.core.server is the layer above swank.util.net.sockets
;;  - Manages the socket server
;;  - Accepts and authenticates incoming connections
;;  - Creates swank.core.connections
;;  - Spins up new threads

(defonce connections (ref []))

(def slime-secret-path (str (user-home-path) File/separator ".slime-secret"))

(defn- slime-secret
  "Returns the first line from the slime-secret file, path found in
   slime-secret-path (default: .slime-secret in the user's home
   directory).

   See also: `accept-authenticated-connection'"
  ([] (failing-gracefully
        (let [slime-secret-file (File. (str (user-home-path) File/separator ".slime-secret"))]
          (when (and (.isFile slime-secret-file) (.canRead slime-secret-file))
            (with-open [secret (BufferedReader. (FileReader. slime-secret-file))]
              (.readLine secret)))))))

(defn- make-output-redirection
  ([conn]
     (call-on-flush-stream
      #(with-connection conn
         (send-to-emacs `(:write-string ~%)))))
  {:tag java.io.StringWriter})

;; rename to authenticate-socket, takes in a connection
(defn- accept-authenticated-connection
  "Accepts and returns new connection if it is from an authenticated
   machine. Otherwise, return nil.

   Authentication depends on the contents of a slime-secret file on
   both the server (swank) and the client (emacs slime). If no
   slime-secret file is provided on the server side, all connections
   are accepted.

   See also: `slime-secret'"
  ([#^Socket socket opts]
     (returning [conn (make-connection socket (or (:encoding opts)
                                                  (System/getProperty
                                                   "swank.encoding"
                                                   "utf-8-unix")))]
                (when (:repl-out-root opts)
                  (alter-var-root #'*out* (constantly
                                           (PrintWriter.
                                            (make-output-redirection conn))))
                  (alter-var-root #'*err* (constantly
                                           (PrintWriter.
                                            (make-output-redirection conn)))))
       (if-let [secret (slime-secret)]
         (when-not (= (read-from-connection conn) secret)
           (close-socket! socket))
         conn))))

(defn- socket-serve [connection-serve socket opts]
  (with-connection (accept-authenticated-connection socket opts)
    (let [out-redir (java.io.PrintWriter. (make-output-redirection
                                           *current-connection*))]
      (binding [*out* out-redir
                *err* out-redir]
        (dosync (ref-set (*current-connection* :writer-redir) *out*))
        (dosync (alter connections conj *current-connection*))
        (connection-serve *current-connection*)))))

;; Setup frontent
(defn start-swank-socket-server!
  "Starts and returns the socket server as a swank host. Takes an
   optional set of options as a map:

    :announce - an fn that will be called and provided with the
    listening port of the newly established server. Default: none."
  ([server connection-serve] (start-swank-socket-server! connection-serve {}))
  ([server connection-serve options]
     (start-server-socket! server connection-serve)
     (when-let [announce (options :announce)]
       (announce options))
     server))

(defn setup-server
  "The port it started on will be called as an argument to (announce-fn
   port). A connection will then be created and (connection-serve conn)
   will then be called."
  [port announce-fn connection-serve opts]
  (start-swank-socket-server!
   (make-server-socket {:port    port
                        :host    (opts :host "localhost")
                        :backlog (opts :backlog 0)})
   #(socket-serve connection-serve % opts)
   (merge {:announce announce-fn} opts)))

;; Announcement functions
(defn simple-announce [{:keys [message host port] :as opts}]
  (println (or message (format "Connection opened on %s port %s." host port))))

