(ns swank.core.debugger-backends
  (:refer-clojure :exclude [next]))

(def #^{:dynamic true} *debugger-env* (atom nil))
(def last-viewed-source (atom nil))

(defn get-debugger-backend [& args]
  (when @*debugger-env* :cdt))

(def dispatch-val (atom :default))

(defn dbe-dispatch [& args]
  @dispatch-val)

(defmacro def-default-backend-multimethods [methods]
  `(do
     ~@(for [m methods]
        `(defmulti ~m get-debugger-backend))))

(def-default-backend-multimethods
  [exception-stacktrace debugger-condition-for-emacs calculate-restarts
   build-backtrace eval-string-in-frame step get-stack-trace
   next finish continue swank-eval handled-exception? debugger-exception?])

(defmulti line-bp dbe-dispatch)
(defmulti eval-last-frame dbe-dispatch)

(defmulti handle-interrupt
  (fn [thread _ _] thread))
