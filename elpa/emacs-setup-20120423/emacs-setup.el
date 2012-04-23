;;; emacs-setup.el --- Package for maintaining your emacs configuration. Clean up your .emacs!

;;; Commentary:

;; emacs-setup is an emacs package that is meant to help make maintaining your
;; emacs setup easier. Through the use of M-x customize, the following can be
;; setup through emacs-setup:

;; Directories to be added to the load path.
;; Packages to require, and any setup elisp code.
;; Keybindings.
;; Emacs window/frame session setups (through the use of revive.el, included)
;; and more!

;; Usage:

;; In your .emacs, load emacs-setup:

;; (load-file "~/path/to/emacs-setup/emacs-setup.el")

;; then run (emacs-setup-base), which takes two optional arguments:
;;   FRAME - passed to after-make-frame-hook (typically nil)
;;   THE-CUSTOM-FILE - if you store your file which is used by customize in a
;;                     place other than default (e.g. I keep mine in Dropbox)
;;                     you must specifiy that location here.

;; (emacs-setup-base)
;;  - or - 
;; (emacs-setup-base nil "~/path/to/custom-file.el")

;; finally, make a call to

;; (emacs-setup)

;; Once loaded, you can use M-x customize-group emacs-setup to setup your
;; environment.

;; emacs-setup is broken down into several parts, which can each be customized
;; individually:

;; emacs-setup - This is the main part of emacs-setup. You can set your base
;;               directory (your .emacs.d or equivalent), directories to ignore
;;               when recursively adding to load path, and various list of
;;               s-expressions (base, pre, post, etc.) The s-expression lists
;;               can be used to setup things that would normally be in your
;;               .emacs, but are not customizable options. For instance,
;;               (set-frame-font), (set-background-color), (transient-mark-mode),
;;               etc. I'm not going to try an support every option of emacs.
;;               Instead, simply add these configuration lines (one sexp per line)
;;               to the appropriate sexp group, depending on when they need to run.
;;               When emacs-setup-base is run, the last thing it does is run all
;;               the s-expressions in emacs-setup-base-sexp. When emacs-setup is
;;               run, it runs in this order:
;;               - emacs-setup-pre-sexp
;;               - require pacakges via emacs-setup-require
;;               - emacs-setup-pre-layout-sexp
;;               - setup layout - via emacs-setup-layout
;;               - emacs-setup-post-sexp
;;               - bind keys in emacs-setup-keys

;; emacs-setup-keys - This part of emacs-setup allows you to have your keybindings
;;                    all in one place via customize. You can manually add and
;;                    remove keybindings, or you can use the functions
;;                    emacs-setup-bind-key,
;;                    emacs-seutp-unbind-key-by-key, or
;;                    emacs-setup-unbind-key-by-functions
;;                    to interactively bind or unbind keys, which are saved to
;;                    customize for you.
                   
;; emacs-setup-layout - This part fo emacs-setup allows you to manage multiple
;;                      screen layouts or sessions. Basic usage is to get your
;;                      emacs frame(s)/window(s) like you want them, then run
;;                      M-x emacs-setup-save-frame-configuration
;;                      This will save all frame(s)/window(s) to a unique
;;                      layout file recognized by your screen resolution and
;;                      emacs frame size. This allows you have multiple
;;                      layouts which will automatically load on different
;;                      computers with different screen sizes.
;;                      You can also set a default frame size, height, width,
;;                      fullscreen, etc. all via customize.
;;                      It is also possible to save/restore named layouts.
                     
;; emacs-setup-require - This is ths part of emacs-setup where you can tell it
;;                       which packages to load, and give setup s-expressions.
;;                       You can customize the load-path and env-path, whether or
;;                       not to loade elpa and where your package.el is (if not
;;                       using emacs 24). Customizing the variable
;;                       emacs-setup-require-list
;;                       is where you can add which packages should be load, in
;;                       the order you supply them, as well as any configuration
;;                       for each package after it is loaded.
;;                       When emacs-setup is run, if any pacakges fail to load, a
;;                       buffer called *invalid-packages* will be displayed telling
;;                       you which failed.

;; emacs-setup is written and maintained by Brian Zwahr <echosa@gmail.com>

;;; Code:

;;; **************
;;; CUSTOMIZATIONS
;;; **************

(defgroup emacs-setup nil
  "Easy emacs setup."
  :group 'environment)

(defcustom emacs-setup-elisp-base-dir "~/.emacs.d"
  "Base directory where you keep your .el files to be loaded."
  :group 'emacs-setup
  :type '(file :must-match t))

(defcustom emacs-setup-elisp-ignore-dirs nil
  "Sub-directories of emacs-setup-base-elisp-dir to ignore when loading 
(i.e. .svn, etc.)."
  :group 'emacs-setup
  :type '(repeat :tag "Sub-directory name: " (string)))

(defcustom emacs-setup-base-sexp nil
  "List of function names to run during base setup."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

(defcustom emacs-setup-pre-sexp nil
  "List of function names to call before setup is run."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

(defcustom emacs-setup-pre-layout-sexp nil
  "List of function names to call during setup before any layout stuff is run."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

(defcustom emacs-setup-post-sexp nil
  "List of function names to call after setup has loaded."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

;;; *********
;;; FUNCTIONS
;;; *********
(defun emacs-setup-base (&optional frame the-custom-file)
  "Performs initial setup. The frame argument is there for 
after-make-frame-hook."
  (interactive)
  (let ((dir (file-name-directory 
              (find-lisp-object-file-name 'emacs-setup-base 'function))))
    (add-to-list 'load-path dir)
    (require 'emacs-setup-require)
    (require 'emacs-setup-layout)
    (require 'emacs-setup-keys))
  (when the-custom-file
    (setq custom-file the-custom-file))
  (if custom-file
      (load custom-file)
    (error "No custom file set."))
  ;; This must come first for requires to work from here on out!
  (emacs-setup-load-recursive-el-directories
   emacs-setup-elisp-base-dir
   emacs-setup-elisp-ignore-dirs)
  (dolist (dir emacs-setup-load-path-list)
    (add-to-list 'load-path dir))
  (let ((env-path (getenv "PATH")))
    (dolist (dir emacs-setup-env-path-list)
      (setq env-path (concat dir ":" env-path)))
    (setenv "PATH" env-path))
  (emacs-setup-call-base-sexp))

(defun emacs-setup ()
  (interactive)
  (emacs-setup-call-pre-sexp)
  (emacs-setup-require-packages)
  (emacs-setup-call-pre-layout-sexp)
  (emacs-setup-layout)
  (emacs-setup-call-post-sexp)
  (emacs-setup-bind-keys)
  (message "Setup complete. Emacs is ready to go!"))

(defun emacs-setup-call-base-sexp ()
  (dolist (sexp emacs-setup-base-sexp)
    (eval sexp)))

(defun emacs-setup-call-pre-sexp ()
  (dolist (sexp emacs-setup-pre-sexp)
    (eval sexp)))

(defun emacs-setup-call-pre-layout-sexp ()
  (dolist (sexp emacs-setup-pre-layout-sexp)
    (eval sexp)))

(defun emacs-setup-call-post-sexp ()
  (dolist (sexp emacs-setup-post-sexp)
    (eval sexp)))

(provide 'emacs-setup)

;;; emacs-setup.el ends here
