(ns swank.core.cdt-utils
  (:refer-clojure :exclude [next])
  (:require [cdt.ui :as cdt]
            [swank.util.concurrent.mbox :as mb]
            [swank.core :as core])
  (:use swank.core.debugger-backends))

(defn- match-name [thread-name]
  #(re-find (re-pattern (str "^" thread-name "$")) (.getName %)))

(defn- get-all-threads []
  (map key (Thread/getAllStackTraces)))

(defn get-thread [thread-name]
  (first (filter
          (match-name thread-name)
          (get-all-threads))))

(def control-thread (atom nil))

(defn set-control-thread []
  (reset! control-thread
          (get-thread "Swank Control Thread")))

(defn get-control-thread []
  (when-not @control-thread
    (Thread/sleep 100)
    (set-control-thread))
  @control-thread)

(def system-thread-group-names #{#"JDI main" #"JDI \[\d*\]" #"system"
                                 (re-pattern core/swank-worker-thread-name)})
(def system-thread-groups (atom []))
(defn- system-thread-group? [g]
  (some #(re-find % (.name g)) system-thread-group-names))

(defn set-system-thread-groups []
  (reset! system-thread-groups
          (filter system-thread-group?
                  (cdt/all-thread-groups))))

(defn get-system-thread-groups [] @system-thread-groups)

(def system-thread-names
     #{#"^CDT Event Handler$" #"^Swank Control Thread$" #"^Read Loop Thread$"
       #"^Socket Server \[\d*\]$"})

(defn system-thread? [t]
  (some #(re-find % (.name t)) system-thread-names))

(defn get-system-threads []
  (filter system-thread? (cdt/list-threads)))

(defn get-non-system-threads []
  (remove system-thread? (cdt/list-threads)))

(def bp-text (str "From here you can: "
                  "e/eval, v/show source, s/step, x/next, o/exit func"))

(def exception-text "From here you can: e/eval, v/show source")

(defn- gen-env-list [e text]
  (let [[_ s1 _ s2]
        (re-find #"(.*)(@.* )(in thread.*)" (str e))]
    (list (str "CDT " s1 " " s2) text
          '((:show-frame-source 0)))))

(defn- get-env [e]
  (condp = (second (re-find #"^(.*)Event@" (str e)))
      "Breakpoint"
    (gen-env-list e bp-text)
    "Step"
    (gen-env-list e bp-text)
    "Exception"
    (gen-env-list e exception-text)))

(defn- event-data [e]
  {:thread (.uniqueID (cdt/get-thread-from-event e))
   :env (get-env e)})

(defonce exception-events (atom #{}))

(defn- send-to-control-thread [e]
  (mb/send (get-control-thread)
           ;; pr-str would be better here instead of str, but can
           ;;  lead to blocking the event handler thread
           `(:dbe-rex ~(str `(swank.core.cdt-backends/sldb-cdt-debug
                                 ~(event-data e))) true)))

(defn default-handler [e]
  (if-not (cdt/exception-event? e)
    (send-to-control-thread e)
    (if (@exception-events (.exception e))
      (cdt/continue-thread (cdt/get-thread-from-event e))
      (do
        (swap! exception-events conj (.exception e))
        (send-to-control-thread e)))))

(defn display-background-msg [s]
  (mb/send (get-control-thread)
           `(:eval-no-wait "slime-message" ("%s" ~s))))

(defmacro make-debugger-exception [exception-name]
  (let [full-str-name (str "debug-" exception-name "-exception")
        name-sym (symbol full-str-name)
        func-sym (symbol (str full-str-name "?"))]
    `(do
       (defonce ~name-sym (Exception. (str "Debug " ~(str exception-name))))
       (defn ~func-sym [t#]
         (some #(identical? ~name-sym  %)
               (core/exception-causes t#))))))

(make-debugger-exception step)
(make-debugger-exception finish)
(make-debugger-exception next)
(make-debugger-exception cdt-continue)

(defn- exception? []
  (.startsWith (first (:env @*debugger-env*)) "CDT Exception"))

(defn get-quit-exception []
  (if (exception?)
    core/debug-abort-exception
    debug-cdt-continue-exception))
