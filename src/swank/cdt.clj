(ns swank.cdt
  ;; convenience namespace to give users easy access to main functions
  (:refer-clojure :exclude [next])
  (:require [cdt.ui :as cdt]
            [swank.core.cdt-backends :as cbackends]))

(def swank-cdt-release "1.5.0a")

(cdt/expose cbackends/set-catch cbackends/set-bp cbackends/reval
            cdt/delete-catch cdt/delete-bp cdt/delete-all-breakpoints
            cdt/print-bps cdt/bg)

(cbackends/cdt-backend-init swank-cdt-release)
