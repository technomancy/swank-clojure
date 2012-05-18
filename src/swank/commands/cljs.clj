(ns ^{:doc "Support for sending evaluation of forms into a ClojureScript repl."}
  swank.cljsrepl
  (:use [swank.core :only (with-emacs-package)]
        [swank.commands :onlny (defslimefn)])
  (:require [cljs.repl :as repl]
            [cljs.repl.browser :as browser]
            [cljs.compiler :as comp])
  )

(def cljs-env "Global cljs environment for a repl." (atom nil))

;; FIXME: this sucks, I wish I didn't need a thread. Need to expose better
;; access to cljs repl over there.
(defn- make-env
  "Create a new ClojureScript browser repl and run it in a background thread."
  []
  (let [env (browser/repl-env), ;; FIXME: we should allow customization
        thread (Thread. (fn [] (repl/repl env)))]
    (.start thread)
    env))

(defn eval-in-cljs [form-string]
  ;; Create a repl as global on demand, if not already started.
  ;; FIXME: not sure if we want to do this automatically in the future.
  (when-not @cljs-env (swap! cljs-env (fn [_] (make-env))))

  (let [form (read-string form-string),
        ;; Note: the following is lifted from cljs.repl.browser; FIXME: we
        ;; should add support there to do this without a repl thread.
        context {:context :statement
                 :locals {}
                 :ns (@comp/namespaces comp/*cljs-ns*)}]

    #_(.println System/out form)

    ;; FIXME: we need to report error when no browser is connected, send to
    ;; thread, etc.
    (repl/evaluate-form @cljs-env context "<swank-cljs-repl>" form)
    ))

(defslimefn ^{:doc"Evaluate a Clojure form in a global ClojureScript environment."}
  interactive-eval-with-target [target form-string]
  (condp = target
    :cljs-repl (eval-in-cljs form-string)
    ;; Note: eventually add support for other repls.
    (:abort (throw (Exception. (format "Emacs eval abort - Invalid eval target '%s'" target)))))
  )


;;------------------------------------------------------------------------------
;; Required emacs configuration:
;;
;; You will have to insert this advice in your .emacs in order to instruct
;; slime to use an alternative target for evaluation.
;;
;;    (defadvice slime-interactive-eval (around slime-current-eval-target activate)
;;      (if (string-match "\\.cljs$" (buffer-file-name))
;;          (slime-eval-with-transcript `(swank:interactive-eval-with-target :cljs-repl ,string))
;;        ad-do-it))
