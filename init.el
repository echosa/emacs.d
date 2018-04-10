(defconst emacs-start-time (current-time))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-config.org"))

(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Initialization done. (%.3fs)" elapsed))
