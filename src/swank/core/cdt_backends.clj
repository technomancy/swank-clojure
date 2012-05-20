(ns swank.core.cdt-backends
  (:refer-clojure :exclude [next])
  (:require [cdt.ui :as cdt]
            [cdt.reval]
            [swank.core.cdt-utils :as cutils]
            [swank.core :as core]
            [swank.util.concurrent.thread :as st]
            [clj-stacktrace repl core])
  (:use swank.core.debugger-backends
        [swank.commands :only [defslimefn]])
  (:import java.util.concurrent.TimeUnit))

(defmethod swank-eval :cdt [form]
  (cdt/safe-reval (:thread @*debugger-env*)
                  (:frame @*debugger-env*) form true identity))

(defn- get-full-stack-trace []
  (.getStackTrace (cutils/get-thread #_(.getName @control-thread)
                                     (.name (:thread @*debugger-env*)))))

(defmethod get-stack-trace :cdt [n]
  (swap! *debugger-env* assoc :frame n)
  (reset! last-viewed-source
          (select-keys @*debugger-env* [:thread :frame]))
  (nth (get-full-stack-trace) n))

;; (defmethod exception-stacktrace :cdt [_]
;;   (map #(list %1 %2 '(:restartable nil))
;;        (iterate inc 0)
;;        (map str (get-full-stack-trace))))

(defmethod exception-stacktrace :cdt [_]
  (let [width 25   ;; @@ TODO: hard-coded for now as below does not work:
        #_(clj-stacktrace.repl/find-source-width
           (clj-stacktrace.core/parse-exception t))]
    (map #(list %1 %2 '(:restartable nil))
         (iterate inc 0)
         (map #(core/exception-str width %) (get-full-stack-trace)))))

(defmethod debugger-condition-for-emacs :cdt []
  (:env @*debugger-env*))

(defmethod calculate-restarts :cdt [_]
  (let [quit-exception (cutils/get-quit-exception)
        restarts
        [(core/make-restart :quit "QUIT"
                            "Quit to the SLIME top level"
                            #(throw cutils/debug-cdt-continue-exception))]
        restarts (core/add-restart-if
                  (pos? core/*sldb-level*)
                  restarts
                  :abort "ABORT" (str "ABORT to SLIME level "
                                      (dec core/*sldb-level*))
                  (fn [] (throw core/debug-abort-exception)))]
    (into (array-map) restarts)))

(defmethod build-backtrace :cdt [start end]
  (doall (take (- end start)
               (drop start (exception-stacktrace nil)))))

(defmethod eval-string-in-frame :cdt [string n]
  (swap! *debugger-env* assoc :frame n)
  (cdt/safe-reval (:thread @*debugger-env*)
                  (:frame @*debugger-env*)
                  (read-string string) true identity))

(defmethod eval-last-frame :cdt [form-string]
  (cdt/safe-reval
   (:thread @last-viewed-source)
   (:frame @last-viewed-source)
   (read-string form-string) true identity))

(defmacro reval [form]
  `(cdt/safe-reval
    (:thread @last-viewed-source)
    (:frame @last-viewed-source)
    '~form true read-string))

(defn- reset-last-viewed-source []
  (reset! last-viewed-source (atom nil)))

(defmacro make-cdt-method [name func]
  `(defmethod ~name :cdt []
     (reset-last-viewed-source)
     (~(ns-resolve (the-ns 'cdt.ui) func)
      (:thread @*debugger-env*))
     true))

(make-cdt-method step step)
(make-cdt-method next step-over)
(make-cdt-method finish finish)
(make-cdt-method continue continue-thread)

(defonce cdt-started-promise (promise))

(defn wait-till-cdt-started []
  (try
    (.get (future (and @cdt-started-promise (cdt/event-handler-started?)))
          5000 TimeUnit/MILLISECONDS)
    (catch Exception e
      (throw (IllegalStateException.
              (str "CDT failed to start.  Check for errors on stdout"))))))

(defmethod line-bp :cdt [file line]
  (wait-till-cdt-started)
  (cdt/line-bp file line
               (cutils/get-non-system-threads)
               (cutils/get-system-thread-groups) true))

(defmacro set-bp [sym]
  `(do
     (wait-till-cdt-started)
     (cdt/set-bp-sym '~sym [(cutils/get-non-system-threads)
                            (cutils/get-system-thread-groups) true])))

(defmethod debugger-exception? :cdt [t]
  (or (cutils/debug-cdt-continue-exception? t)
      (cutils/debug-finish-exception? t)
      (cutils/debug-next-exception? t)
      (cutils/debug-step-exception? t)))

(defmethod handled-exception? :cdt [t]
  (cond
   (core/debug-continue-exception? t)
   true
   (cutils/debug-step-exception? t)
   (step)
   (cutils/debug-next-exception? t)
   (next)
   (cutils/debug-cdt-continue-exception? t)
   (continue)
   (cutils/debug-finish-exception? t)
   (finish)))

(defn- gen-debugger-env [env]
  (atom {:env (:env env)
         :thread (cdt/get-thread-from-id (:thread env))
         :frame 0}))

(defn get-frame-locals [env]
  (try
    (let [thread (cdt/get-thread-from-id (:thread env))
          frame-num 0
          ;foo (doall (cdt.reval/gen-locals-and-closures thread frame-num))
          local-names (cdt.reval/local-names thread frame-num)
          locals (into {}
                       (doall
                        (map
                         (fn [nm]
                           [nm
                            (cdt.reval/fixup-string-reference-impl
                             (cdt.reval/reval-ret-str thread frame-num
                                                      nm true))
                            ])
                         local-names)))]

      ;; (println "**: " foo "\n")
      locals)
    (catch Throwable t
      (.printStackTrace t #^java.io.PrintWriter *err*)
      (println "CDT failed to get frame locals:" t))))

(defslimefn sldb-cdt-debug [env]
  (binding [*debugger-env* (gen-debugger-env env)]
    (core/sldb-debug (get-frame-locals env) nil core/*pending-continuations*)))

(defslimefn sldb-line-bp [file line]
  (line-bp file line))

(defslimefn sldb-step [_]
  (throw cutils/debug-step-exception))

(defslimefn sldb-next [_]
  (throw cutils/debug-next-exception))

(defslimefn sldb-out [_]
  (throw cutils/debug-finish-exception))

(defn set-catch [class]
  (wait-till-cdt-started)
  (cdt/set-catch class :all
                 (cutils/get-non-system-threads)
                 (cutils/get-system-thread-groups) true))

(defn display-msg [msg]
  (doseq [f [cutils/display-background-msg println]]
    (f msg)))

(defmethod handle-interrupt :cdt [_ _ _]
  (.deleteEventRequests
   (.eventRequestManager (cdt/vm))
   (.breakpointRequests (.eventRequestManager (cdt/vm))))
  (.deleteEventRequests
   (.eventRequestManager (cdt/vm))
   (.exceptionRequests (.eventRequestManager (cdt/vm))))
  (cdt/continue-vm)
  (reset! cdt/catch-list {})
  (reset! cdt/bp-list {})
  (reset-last-viewed-source)
  (display-msg "Clearing CDT event requests and continuing."))

(defn cdt-backend-init [release]
  (try
    (cdt/cdt-attach-pid)
    (cdt/create-thread-start-request)
    (reset! dispatch-val :cdt)

    ;; classloader exceptions often cause deadlocks and are almost
    ;; never interesting so filter them out
    (cdt/set-catch-exclusion-filter-strings
     "java.net.URLClassLoader*" "java.lang.ClassLoader*" "*ClassLoader.java")
    (cdt/set-handler cdt/exception-handler cutils/default-handler)
    (cdt/set-handler cdt/breakpoint-handler cutils/default-handler)
    (cdt/set-handler cdt/step-handler cutils/default-handler)
    (cdt/set-display-msg cutils/display-background-msg)
    (cutils/set-control-thread)
    (cutils/set-system-thread-groups)
    (cutils/init-emacs-helper-functions)
    ;; this invocation of handle-interrupt is only needed to force the loading
    ;;  of the classes required by force-continue because inadvertently
    ;;  catching an exception which happens to be in the classloader can cause a
    ;;  deadlock

    (handle-interrupt :cdt nil nil)
    (deliver cdt-started-promise true)
    (display-msg (str "Swank CDT release " release " started" ))
    (catch Exception e
      (println "CDT " release "startup failed: " e))))
