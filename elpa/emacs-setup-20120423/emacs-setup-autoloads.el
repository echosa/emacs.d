;;; emacs-setup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (resume save-current-configuration wipe restore-window-configuration
;;;;;;  current-window-configuration-printable) "revive" "revive.el"
;;;;;;  (20373 31543))
;;; Generated autoloads from revive.el

(autoload 'current-window-configuration-printable "revive" "\
Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of revive:all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start)).

\(fn)" nil nil)

(autoload 'restore-window-configuration "revive" "\
Restore the window configuration.
Configuration CONFIG should be created by
current-window-configuration-printable.

\(fn CONFIG)" nil nil)

(autoload 'wipe "revive" "\
Wipe Emacs.

\(fn)" t nil)

(autoload 'save-current-configuration "revive" "\
Save current window/buffer configuration into configuration file.

\(fn &optional NUM)" t nil)

(autoload 'resume "revive" "\
Resume window/buffer configuration.
Configuration should be saved by save-current-configuration.

\(fn &optional NUM)" t nil)

;;;***

;;;### (autoloads nil nil ("emacs-setup-keys.el" "emacs-setup-layout.el"
;;;;;;  "emacs-setup-pkg.el" "emacs-setup-require.el" "emacs-setup-util.el"
;;;;;;  "emacs-setup.el") (20373 31543 396902))

;;;***

(provide 'emacs-setup-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emacs-setup-autoloads.el ends here
