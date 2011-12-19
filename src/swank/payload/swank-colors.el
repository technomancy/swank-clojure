(defadvice sldb-insert-frame (around colorize-clj-trace (frame &optional face))
  (progn
    (ad-set-arg 0 (list (sldb-frame.number frame)
                        (ansi-color-apply (sldb-frame.string frame))
                        (sldb-frame.plist frame)))
    ad-do-it
    (save-excursion
      (forward-line -1)
      (skip-chars-forward "0-9 :")
      (let ((beg-line (point)))
        (end-of-line)
        (remove-text-properties beg-line (point) '(face nil))))))

(ad-activate #'sldb-insert-frame)
