;;; slime-compile-presave.el --- Refuse to save non-compiling Slime buffers

;; Copyright © 2011 Phil Hagelberg
;;
;; Authors: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://github.com/technomancy/swank-clojure
;; Version: 1.0.0
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;;; Code:

(defvar slime-compile-presave? nil
  "Refuse to save slime-enabled buffers if they don't compile.")

;;;###autoload
(defun slime-compile-presave-toggle ()
  (interactive)
  (message "slime-compile-presave %s."
           (if (setq slime-compile-presave? (not slime-compile-presave?))
               "enabled" "disabled")))

;;;###autoload
(defun slime-compile-presave-enable ()
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook (defun slime-compile-presave ()
                                (when slime-compile-presave?
                                  (slime-eval `(swank:eval-and-grab-output
                                                ,(buffer-substring-no-properties
                                                  (point-min) (point-max))))))))

;;;###autoload
(add-hook 'slime-mode-hook 'slime-compile-presave-enable)

(provide 'slime-compile-presave)
;;; slime-compile-presave.el ends here
