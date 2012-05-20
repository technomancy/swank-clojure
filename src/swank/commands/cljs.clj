(ns ^{:doc "Support for sending evaluation of forms into a ClojureScript repl."}
  swank.commands.cljs
  (:use [swank.core :only (with-emacs-package)]
        [swank.commands :onlny (defslimefn)])
  (:require [cljs.repl :as repl]
            [cljs.compiler :as comp])
  )

(def cljs-targets "A mapping of registered repl environments which can be used as targets." (atom {}))

(defn register-repl
  "Register a new REPL environment for interactive-eval-with-target to dispatch to."
  [key env]
  (swap! cljs-targets assoc key env))

(defn eval-in-cljs
  "Evaluate the given string in the provided ClojureScript repl environment."
  [env form-string]
  (binding [comp/*cljs-ns* comp/*cljs-ns*]
            (let [form (read-string form-string),
                  ;; Note: the following is lifted from cljs.repl.browser; FIXME: we
                  ;; should add support there to do this without a repl thread.
                  context {:context :statement
                           :locals {}
                           :ns (@comp/namespaces comp/*cljs-ns*)}]

              (repl/evaluate-form env context "<swank-cljs-repl>" form)
              )))

(defslimefn ^{:doc "Evaluate a Clojure form in a ClojureScript environment."}
  interactive-eval-with-target [target form-string]
  (let [env (get @cljs-targets target)]
    (if env
      (eval-in-cljs env form-string)
      (throw (Exception.
              (format "Emacs eval abort; eval target '%s' not found" target)))
      )))


;; Notes:
;;
;; You will need an Emacs customization that overrides
;; slime-interactive-eval tocall (swank:interactive-eval-with-target) instead of
;; (swank:interactive-eval), such as is provided in clojure-mode.el.
;;
;; Also, before you can eval to a target, you will need your VM to have a repl
;; instance registered via 'register-repl' (e.g. browser repl).
