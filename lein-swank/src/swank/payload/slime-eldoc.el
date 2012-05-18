(require 'eldoc)
(defun clojure-slime-eldoc-message ()
  (when (and (featurep 'slime)
             (slime-background-activities-enabled-p))
    (slime-echo-arglist) ; async, return nil for now
    nil))

(defun clojure-localize-documentation-function ()
  (set (make-local-variable 'eldoc-documentation-function)
       'clojure-slime-eldoc-message))

(add-hook 'slime-mode-hook 'clojure-localize-documentation-function)
