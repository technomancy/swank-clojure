(ns swank.core
  (:refer-clojure :exclude [next])
  (:use (swank util commands)
        (swank.util hooks)
        (swank.util.concurrent thread)
        (swank.core connection hooks threadmap debugger-backends))
  (:require (swank.util.concurrent [mbox :as mb])
            (clj-stacktrace core repl))
  (:use [swank.util.clj-stacktrace-compat
         :only [pst-elem-str find-source-width]])
  (:import (java.io BufferedReader)))

;; Protocol version
(defonce protocol-version (atom "20100404"))

;; Emacs packages
(def #^{:dynamic true} *current-package*)

;; current emacs eval id
(def #^{:dynamic true} *pending-continuations* '())

(def color-support? (atom false))

(def exit-on-quit? (atom true))

(def sldb-stepping-p nil)
(def sldb-initial-frames 10)
(def #^{:dynamic true} #^{:doc "The current level of recursive debugging."}
  *sldb-level* 0)
(def #^{:dynamic true} #^{:doc "The current restarts."}
  *sldb-restarts* 0)

(def #^{:doc "Include swank-clojure thread in stack trace for debugger."}
     debug-swank-clojure false)

(defonce active-threads (ref ()))

(defn maybe-ns [package]
  (cond
   (symbol? package) (or (find-ns package) (maybe-ns 'user))
   (string? package) (maybe-ns (symbol package))
   (keyword? package) (maybe-ns (name package))
   (instance? clojure.lang.Namespace package) package
   :else (maybe-ns 'user)))

(defmacro with-emacs-package [& body]
  `(binding [*ns* (maybe-ns *current-package*)]
     ~@body))

(defmacro with-package-tracking [& body]
  `(let [last-ns# *ns*]
     (try
      ~@body
      (finally
       (when-not (= last-ns# *ns*)
         (send-to-emacs `(:new-package ~(str (ns-name *ns*))
                                       ~(str (ns-name *ns*)))))))))

(defmacro dothread-swank [& body]
  `(dothread-keeping-clj [*current-connection*]
     ~@body))

;; Exceptions for debugging
(defonce debug-quit-exception (Exception. "Debug quit"))
(defonce debug-continue-exception (Exception. "Debug continue"))
(defonce debug-abort-exception (Exception. "Debug abort"))
(defonce debug-invalid-restart-exception (Exception. "Invalid restart"))

(def #^{:dynamic true} #^Throwable *current-exception* nil)

;; Local environment
(def #^{:dynamic true} *current-env* nil)

(let [&env :unavailable]
  (defmacro local-bindings
    "Produces a map of the names of local bindings to their values."
    []
    (if-not (= &env :unavailable)
      (let [symbols (keys &env)]
        (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))))

;; Handle Evaluation
(defn send-to-emacs
    "Sends a message (msg) to emacs."
    ([msg]
       (mb/send @(*current-connection* :control-thread) msg)))

(defn send-repl-results-to-emacs [val]
  (send-to-emacs `(:write-string ~(str (pr-str val) "\n") :repl-result)))

(defn with-env-locals
  "Evals a form with given locals. The locals should be a map of symbols to
values."
  [form]
  (if (seq *current-env*)
    `(let ~(vec (mapcat #(list % `(*current-env* '~%)) (keys *current-env*)))
       ~form)
    form))

(defn eval-in-emacs-package [form]
  (with-emacs-package
   (eval form)))


(defn eval-from-control
  "Blocks for a mbox message from the control thread and executes it
   when received. The mbox message is expected to be a slime-fn."
  ([] (let [form (mb/receive (current-thread))]
        (apply (ns-resolve *ns* (first form)) (rest form)))))

(defn eval-loop
  "A loop which continuosly reads actions from the control thread and
   evaluates them (will block if no mbox message is available)."
  ([] (continuously (eval-from-control))))

(defn exception-causes [#^Throwable t]
  (lazy-seq
    (cons t (when-let [cause (.getCause t)]
              (exception-causes cause)))))

(defn- debug-quit-exception? [t]
  (some #(identical? debug-quit-exception %) (exception-causes t)))

(defn debug-continue-exception? [t]
  (some #(identical? debug-continue-exception %) (exception-causes t)))

(defn- debug-abort-exception? [t]
  (some #(identical? debug-abort-exception %) (exception-causes t)))

(defn- debug-invalid-restart-exception? [t]
  (some #(identical? debug-invalid-restart-exception %) (exception-causes t)))

(defn exception-str [width elem]
  (pst-elem-str
   @color-support? (clj-stacktrace.core/parse-trace-elem elem) width))

(defmethod exception-stacktrace :default [t]
  (let [width (find-source-width
               (clj-stacktrace.core/parse-exception t))]
    (map #(list %1 %2 '(:restartable nil))
         (iterate inc 0)
         (map #(exception-str width %) (.getStackTrace #^Throwable t)))))

(defmethod debugger-condition-for-emacs :default []
  (list (or (.getMessage #^Throwable *current-exception*) "No message.")
        (str "  [Thrown " (class *current-exception*) "]")
        nil))

(defn make-restart [kw name description f]
  [kw [name description f]])

(defn add-restart-if [condition restarts kw name description f]
  (if condition
    (conj restarts (make-restart kw name description f))
    restarts))

(declare sldb-debug)
(defn cause-restart-for [#^Throwable thrown depth]
  (make-restart
   (keyword (str "cause" depth))
   (str "CAUSE" depth)
   (str "Invoke debugger on cause "
        (apply str (take depth (repeat " ")))
        (.getMessage thrown)
        " [Thrown " (class thrown) "]")
   (partial sldb-debug nil thrown *pending-continuations*)))

(defn add-cause-restarts [restarts #^Throwable thrown]
  (loop [restarts restarts
         cause (.getCause thrown)
         level 1]
    (if cause
      (recur
       (conj restarts (cause-restart-for cause level))
       (.getCause cause)
       (inc level))
      restarts)))

(defmethod calculate-restarts :default [#^Throwable thrown]
  (let [restarts [(make-restart :quit "QUIT" "Quit to the SLIME top level"
                                (fn [] (throw debug-quit-exception)))]
        restarts (add-restart-if
                  (pos? *sldb-level*)
                  restarts
                  :abort "ABORT" (str "ABORT to SLIME level " (dec *sldb-level*))
                  (fn [] (throw debug-abort-exception)))
        restarts (add-restart-if
                  (and (.getMessage thrown)
                       (.contains (.getMessage thrown) "BREAK"))
                  restarts
                  :continue "CONTINUE" (str "Continue from breakpoint")
                  (fn [] (throw debug-continue-exception)))
        restarts (add-cause-restarts restarts thrown)]
    (into (array-map) restarts)))

(defn format-restarts-for-emacs []
  (doall (map #(list (first (second %)) (second (second %))) *sldb-restarts*)))

(defmethod build-backtrace :default [start end]
  (doall (take (- end start) (drop start (exception-stacktrace *current-exception*)))))

(defn build-debugger-info-for-emacs [start end]
  (list (debugger-condition-for-emacs)
        (format-restarts-for-emacs)
        (build-backtrace start end)
        *pending-continuations*))

(defn sldb-loop
  "A loop that is intented to take over an eval thread when a debug is
   encountered (an continue to perform the same thing). It will
   continue until a *debug-quit* exception is encountered."
  [level]
  (try
   (send-to-emacs
    (list* :debug (current-thread) level
           (build-debugger-info-for-emacs 0 sldb-initial-frames)))
   ([] (continuously
        (do
          (send-to-emacs `(:debug-activate ~(current-thread) ~level nil))
          (eval-from-control))))
   (catch Throwable t
     (send-to-emacs
      `(:debug-return ~(current-thread) ~*sldb-level* ~sldb-stepping-p))
     (if-not (handled-exception? t)
       (throw t)))))

(defn invoke-debugger
  [locals #^Throwable thrown id]
  (binding [*current-env* locals
            *current-exception* thrown
            *sldb-restarts* (calculate-restarts thrown)
            *sldb-level* (inc *sldb-level*)]
    (sldb-loop *sldb-level*)))

(defn sldb-debug [locals thrown id]
  (try
   (invoke-debugger locals thrown id)
   (catch Throwable t
     (when (and (pos? *sldb-level*)
                (not (debug-abort-exception? t)))
       (throw t)))))

(defmacro break
  []
  `(invoke-debugger (local-bindings) (Exception. "BREAK:") *pending-continuations*))

(defn doall-seq [coll]
  (if (seq? coll)
    (doall coll)
    coll))

(defn eval-for-emacs [form buffer-package id]
  (try
   (binding [*current-package* buffer-package
             *pending-continuations* (cons id *pending-continuations*)]
     (if-let [f (slime-fn (first form))]
       (let [form (cons f (rest form))
             result (doall-seq (eval-in-emacs-package form))]
         (run-hook pre-reply-hook)
         (send-to-emacs `(:return ~(thread-name (current-thread))
                                  (:ok ~result) ~id)))
       ;; swank function not defined, abort
       (send-to-emacs `(:return ~(thread-name (current-thread)) (:abort) ~id))))
   (catch Throwable t
     ;; Thread/interrupted clears this thread's interrupted status; if
     ;; Thread.stop was called on us it may be set and will cause an
     ;; InterruptedException in one of the send-to-emacs calls below
     (Thread/interrupted)

     ;; (.printStackTrace t #^java.io.PrintWriter *err*)

     (cond
      (debug-quit-exception? t)
      (do
        (send-to-emacs `(:return ~(thread-name (current-thread)) (:abort) ~id))
        (if-not (zero? *sldb-level*)
          (throw t)))

      (debug-abort-exception? t)
      (do
        (send-to-emacs `(:return ~(thread-name (current-thread)) (:abort) ~id))
        (if-not (zero? *sldb-level*)
          (throw debug-abort-exception)))

      (debug-continue-exception? t)
      (do
        (send-to-emacs `(:return ~(thread-name (current-thread)) (:abort) ~id))
        (throw t))
      ;;
      (debug-invalid-restart-exception? t)
      (send-to-emacs `(:return ~(thread-name (current-thread))
                               (:ok "Restart index out of bounds") ~id))
      ;;
      (debugger-exception? t)
      (throw t)

      :else
      (do
        (set! *e t)
        (try
          (sldb-debug
           nil
           (if debug-swank-clojure t (or (.getCause t) t))
           id)
          ;; reply with abort
          (finally (send-to-emacs
                    `(:return ~(thread-name (current-thread)) (:abort) ~id)))))))))

(defn- add-active-thread [thread]
  (dosync
   (commute active-threads conj thread)))

(defn- remove-active-thread [thread]
  (dosync
   (commute active-threads (fn [threads] (remove #(= % thread) threads)))))

(def swank-worker-thread-name "Swank Worker Thread")
(defonce swank-worker-thread-group
  (ThreadGroup. swank-worker-thread-name))

(defn spawn-worker-thread
  "Spawn an thread that blocks for a single command from the control
   thread, executes it, then terminates."
  ([conn]
     ;; binding is a signal to dbe's not to allow bp's on this thread
     ;;  because it will hang swank
     (binding [*new-thread-group* swank-worker-thread-group]
         (dothread-swank
          (try
            (add-active-thread (current-thread))
            (thread-set-name swank-worker-thread-name)
            (eval-from-control)
            (finally
             (remove-active-thread (current-thread))))))))

(defn spawn-repl-thread
  "Spawn an thread that sets itself as the current
   connection's :repl-thread and then enters an eval-loop"
  ([conn]
     (dothread-swank
      (thread-set-name "Swank REPL Thread")
      (with-connection conn
        (eval-loop)))))

(defn find-or-spawn-repl-thread
  "Returns the current connection's repl-thread or create a new one if
   the existing one does not exist."
  ([conn]
     ;; TODO - check if an existing repl-agent is still active & doesn't have errors
     (dosync
      (or (when-let [conn-repl-thread @(conn :repl-thread)]
            (when (.isAlive #^Thread conn-repl-thread)
              conn-repl-thread))
          (ref-set (conn :repl-thread)
                   (spawn-repl-thread conn))))))

(defn thread-for-evaluation
  "Given an id and connection, find or create the appropiate agent."
  ([id conn]
     (cond
      (= id true) (spawn-worker-thread conn)
      (= id :repl-thread) (find-or-spawn-repl-thread conn)
      :else (find-thread id))))


;;; slime proto :emacs-return support and the swank commands
;;; that depend on it: :eval :read-from-minibuffer :y-or-n-p :read-string

(defonce emacs-return-promises (atom {}))

(defn- create-emacs-return-promise [thread tag]
  (let [p (promise)]
    (swap! emacs-return-promises
           (fn [promise-map]
             (assoc promise-map [thread tag] p)))
    p))

(defn- clear-emacs-return-promise [thread tag]
  (swap! emacs-return-promises
         (fn [promise-map] (dissoc promise-map [thread tag]))))

(defn- deliver-emacs-return-promise [thread tag val]
  (let [p (@emacs-return-promises [thread tag])]
    (if p
      (deliver p val))))

(defn- send-slime-command-to-emacs-and-wait [slime-command & args]
  (assert (#{:eval :read-from-minibuffer
             :y-or-n-p :read-string}
           slime-command))
  (let [thread (thread-name (current-thread))
        tag (str (java.util.UUID/randomUUID))
        p (create-emacs-return-promise thread tag)]
    (send-to-emacs `(~slime-command ~thread ~tag ~@args))
    (let [retval @p]
      (clear-emacs-return-promise thread tag)
      retval)))

(defn eval-in-emacs
  "Sends an elisp `formstring` to slime for evaluation and blocks
  until that the result of the eval is available.

  Unlike `eval-in-emacs-async`, this function unwraps the slime-proto
  return value and cleans up the promise used .

  NOTE: you must (setq slime-enable-evaluate-in-emacs t) on the Emacs
  side before calling this function."

  [formstring]

  (let [retval (send-slime-command-to-emacs-and-wait :eval formstring)]
    (case (first retval)
      :ok (second retval)
      :abort (throw (Exception. "Emacs eval abort")))))

(defn eval-in-emacs-async
  "Sends an elisp `formstring` to slime for evaluation and immediately
  returns a promise that the result of the eval will be delivered to
  eventually.

  The value delivered to the promise is either (:ok retval)
  or (:abort) if there was any error.

  The `thread` argument should be the thread name or id. The `tag`
  argument is an arbitrary string identifier used to address the
  return value from emacs to the correct promise. UUID's are a good
  option.

  Callers of this function are responsible for calling
  (clear-emacs-return-promise thread tag) after they have retrieved
  the return value from the promise.

  NOTE: you must (setq slime-enable-evaluate-in-emacs t) on the Emacs
  side before calling this function."

  [formstring thread tag]
  (let [p (create-emacs-return-promise thread tag)]
    (send-to-emacs `(:eval ~thread ~tag ~formstring))
    p))

(defn read-line-from-emacs []
  (send-slime-command-to-emacs-and-wait :read-string))

(defn read-from-emacs-minibuffer [prompt & [initial-value]]
  (send-slime-command-to-emacs-and-wait :read-from-minibuffer prompt initial-value))

(defmacro with-read-line-support
  "Rebind *in* to a proxy that dispatches .readLine to Emacs,
   so `(read-line)` will work within slime sessions.

   Note, .read / (read), etc will not work."
  [& body]
  `(binding [*in* (proxy [BufferedReader] [*in*]
                    (readLine []
                      (swank.core/read-line-from-emacs)))]
     ~@body))

;; Handle control

(defn read-loop
  "A loop that reads from the socket (will block when no message
   available) and dispatches the message to the control thread."
  ([conn control]
     (with-connection conn
       (continuously (mb/send control (read-from-connection conn))))))

(defn dispatch-event
  "Dispatches/executes an event in the control thread's mailbox queue."
  ([ev conn]
     (let [[action & args] ev]
       (cond
         (= action :emacs-rex)
         (let [[form-string package thread id] args
               thread (thread-for-evaluation thread conn)]
           (mb/send thread `(eval-for-emacs ~form-string ~package ~id)))

         ;; handle events from the debugger backend
         (= action :dbe-rex)
         (let [[form-string thread] args
               thread (thread-for-evaluation thread conn)]
           (mb/send thread (read-string form-string)))

         (= action :return)
         (let [[thread & ret] args]
           (binding [*print-level* nil, *print-length* nil]
             (write-to-connection conn `(:return ~@ret))))

         (one-of? action
                  :presentation-start :presentation-end
                  :new-package :new-features :ed :percent-apply
                  :indentation-update
                  :eval :eval-no-wait :background-message :inspect
                  :read-from-minibuffer :y-or-n-p)
         (binding [*print-level* nil, *print-length* nil]
           (write-to-connection conn ev))

         (= action :write-string)
         (write-to-connection conn ev)

         (= action :read-string)
         (write-to-connection conn ev)

         (one-of? action :emacs-return :emacs-return-string)
         (apply deliver-emacs-return-promise args)  ; args = [thread tag val]

         (one-of? action
                  :debug :debug-condition :debug-activate :debug-return)
         (let [[thread & args] args]
           (write-to-connection conn `(~action ~(thread-map-id thread) ~@args)))

         (= action :emacs-interrupt)
         (let [[thread & args] args]
           (handle-interrupt thread conn args))
         :else
         nil))))

;; Main loop definitions
(defn control-loop
  "A loop that reads from the mbox queue and runs dispatch-event on
   it (will block if no mbox control message is available). This is
   intended to only be run on the control thread."
  ([conn]
     (binding [*1 nil, *2 nil, *3 nil, *e nil]
       (with-connection conn
         (continuously (dispatch-event (mb/receive (current-thread)) conn))))))

;;; default implementations of some core multimethods
(defmethod eval-string-in-frame :default [expr n]
  (if (and (zero? n) *current-env*)
    (with-bindings *current-env*
      (eval expr))))

(defmethod swank-eval :default [form]
  (eval (with-env-locals form)))

(defmethod get-stack-trace :default [n]
  (nth (.getStackTrace #^Throwable *current-exception*) n))

(defmethod handled-exception? :default [t]
  (debug-continue-exception? t))

(defmethod debugger-exception? :default [t]
  false)

(defmethod handle-interrupt :default [thread conn args]
  (dosync
   (cond
     (and (true? thread) (seq @active-threads))
     (.stop #^Thread (first @active-threads))

     (= thread :repl-thread) (.stop #^Thread @(conn :repl-thread)))))
