(defconst emacs-start-time (current-time))

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-config.org"))

(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Initialization done. (%.3fs)" elapsed))
