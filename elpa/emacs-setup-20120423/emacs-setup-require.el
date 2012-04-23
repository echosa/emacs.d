(require 'emacs-setup-util)

;;; **************
;;; CUSTOMIZATIONS
;;; **************

(defgroup emacs-setup-require nil
  "Emacs setup layout customizations."
  :group 'emacs-setup)

(defcustom emacs-setup-load-path-list nil
  "This is a list of directory paths to add to the emacs load-path."
  :group 'emacs-setup-require
  :type '(repeat :tag "Directory: " (directory)))

(defcustom emacs-setup-env-path-list nil
  "This is a list of directories to add to the emacs env PATH."
  :group 'emacs-setup-require
  :type '(repeat :tag "Directory: " (directory)))

(defcustom emacs-setup-require-list nil
  "Holds the names of all packages to be required, along with an optional list 
of s-expressions after the require statement is called."
  :group 'emacs-setup-require
  :type '(alist :key-type (string :tag "Package Name: ") 
                :value-type 
                (repeat :tag "Configuration Lines: " (sexp))))

(defcustom emacs-setup-load-elpa nil
  "If t, load elpa from emacs-setup-elpa-package-file."
  :group 'emacs-setup-require
  :type 'boolean)

(defcustom emacs-setup-elpa-package-file "~/.emacs.d/elpa/package.el"
  "This points to the ELPA package.el, if used."
  :group 'emacs-setup-require
  :type 'file)

;;; *********
;;; VARIABLES
;;; *********

(defvar emacs-setup-ring nil
  "Ring for emacs-setup.")

;;; *********
;;; FUNCTIONS
;;; *********

(defun emacs-setup-load-recursive-el-directories (base-dir ignore-dirs)
  (let ((el-dirs-list (list base-dir))
        (current-directory-list (directory-files-and-attributes base-dir t))
        (ignore-dirs (append ignore-dirs (list "." ".."))))
    (while current-directory-list
      (cond
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        (unless (member
                 (file-name-nondirectory (car (car current-directory-list)))
                 ignore-dirs)
          (setq el-dirs-list
                (cons (car (car current-directory-list)) el-dirs-list))
          (setq el-dirs-list
                (append
                 (emacs-setup-load-recursive-el-directories
                  (car (car current-directory-list)) ignore-dirs)
                 el-dirs-list)))))
      (setq current-directory-list (cdr current-directory-list)))
    (dolist (el-dir el-dirs-list)
      (let ((dir-to-load (file-name-as-directory el-dir)))
        (unless (member dir-to-load load-path)
          (setq load-path (cons dir-to-load load-path)))))
    el-dirs-list))

(defun emacs-setup-load-package-el ()
  "Returns the appropriate package.el."
  (when (and emacs-setup-load-elpa
             (not (string= "" emacs-setup-elpa-package-file))
             (file-readable-p emacs-setup-elpa-package-file))
      (load (expand-file-name emacs-setup-elpa-package-file)))
  (fboundp 'package-initialize))

(defun emacs-setup-require-packages ()
  "Loads the packages in emacs-setup-require-list, a list of cons cells with 
the car being a string of the name of the packages and an optional cdr that is 
any functions that need to run to accompany the package. Also loads elpa if
user has that option set."
  (interactive)
  (let ((package-names ""))
    (condition-case e
        (progn
          ;; elpa
          (when (emacs-setup-load-package-el)
            (package-initialize))
          ;; required packages
          (when (emacs-setup-thing-exists 'emacs-setup-require-list)
            (let (invalid-packages)
              (dolist (package emacs-setup-require-list)
                (condition-case nil
                    (unless (featurep (intern (car package)))
                      (require (intern (car package)))
                      (when (cdr package)
                        (mapc 'eval (cdr package))))
                  (error
                   (setq invalid-packages
                         (push (car package) invalid-packages)))))
              (when invalid-packages
                (get-buffer-create "*invalid-packages*")
                (switch-to-buffer "*invalid-packages*")
                (dolist (package-name invalid-packages)
                  (setq package-names
                        (concat package-names package-name ", ")))
                (setq package-names (substring package-names 0 -2))
                (insert
                 (concat "These packages were not loaded: "
                         package-names "\n"))))))
      (error
       (message "There was an error loading packages: %s" package-names)
       (message "%s" (error-message-string e))))))

(defun emacs-setup-add-feature ()
  "Adds an entry to emacs-setup-require-list."
  (interactive)
  (let ((feature (read-string "Require: "))
        config
        sexp)
    (condition-case nil
        (while (setq sexp (read-from-minibuffer "s-expression: " nil nil t))
          (add-to-list 'config sexp))
      ;; we catch error to signify no s-expression was entered
      (error
       (set-variable
        'emacs-setup-require-list
        (add-to-list 'emacs-setup-require-list (cons feature config) t))
       (customize-save-variable 'emacs-setup-require-list
                                emacs-setup-require-list)
       (message "Added feature %s with configuration: %s" feature config)))))

(defun emacs-setup-remove-feature ()
  (interactive)
  (let (features)
    (dolist (feature emacs-setup-require-list)
      (add-to-list 'features (car feature)))
    (let* ((feature (completing-read "Feature: " features nil t)))
      (setq features
            (delete (cons feature
                          (cdr (assoc feature emacs-setup-require-list)))
                    emacs-setup-require-list))
      (set-variable 'emacs-setup-require-list features)
      (customize-save-variable
       'emacs-setup-require-list
       emacs-setup-require-list)
      (message "Removed feature: %s" feature))))

(provide 'emacs-setup-require)
