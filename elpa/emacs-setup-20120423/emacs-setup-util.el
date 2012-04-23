(defun emacs-setup-thing-exists (thing)
  (and (boundp thing)
       (not (eq (eval thing) nil))))

(provide 'emacs-setup-util)