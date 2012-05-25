;;; yasnippet-tests.el --- some yasnippet tests

;; Copyright (C) 2012  Jo�o T�vora

;; Author: Jo�o T�vora <joaot@siscog.pt>
;; Keywords: emulations, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test basic snippet mechanics and the loading system 

;;; Code:

(require 'yasnippet)
(require 'ert)
(require 'ert-x)



;;; Snippet mechanics

(ert-deftest field-navigation ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${1:brother} from another ${2:mother}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother"))
    
    (should (looking-at "brother"))
    (ert-simulate-command '(yas/next-field-or-maybe-expand))
    (should (looking-at "mother"))
    (ert-simulate-command '(yas/prev-field))
    (should (looking-at "brother"))))

(ert-deftest simple-mirror ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${1:brother} from another $1")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another brother"))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "bla from another bla"))))

(ert-deftest mirror-with-transformation ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${1:brother} from another ${1:$(upcase yas/text)}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another BROTHER"))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "bla from another BLA"))))

(ert-deftest nested-placeholders-kill-superfield ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "brother from ${2:another ${3:mother}}!")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother!"))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from bla!"))))

(ert-deftest nested-placeholders-use-subfield ()
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "brother from ${2:another ${3:mother}}!")
    (ert-simulate-command '(yas/next-field-or-maybe-expand))
    (ert-simulate-command `(yas/mock-insert "bla"))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another bla!"))))

;; (ert-deftest in-snippet-undo ()
;;   (with-temp-buffer
;;     (yas/minor-mode 1)
;;     (yas/expand-snippet "brother from ${2:another ${3:mother}}!")
;;     (ert-simulate-command '(yas/next-field-or-maybe-expand))
;;     (ert-simulate-command `(yas/mock-insert "bla"))
;;     (ert-simulate-command '(undo))
;;     (should (string= (buffer-substring-no-properties (point-min) (point-max))
;;                      "brother from another mother!"))))


;;; Misc tests
;;; 

(ert-deftest protection-overlay-no-cheating ()
  "Protection overlays at the very end of the buffer, are dealt by cheatingly inserting a newline!

TODO: correct this bug!"
  :expected-result :failed
  (with-temp-buffer
    (yas/minor-mode 1)
    (yas/expand-snippet "${2:brother} from another ${1:mother}")
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                     "brother from another mother") ;; no newline should be here!
            )))

;;; Loading
;;;
(defmacro with-some-interesting-snippet-dirs (&rest body)
  `(yas/saving-variables
    (with-snippet-dirs
     '((".emacs.d/snippets"
        ("c-mode"
         (".yas-parents" . "cc-mode")
         ("printf" . "printf($1);"))
        ("emacs-lisp-mode" ("ert-deftest" . "(ert-deftest ${1:name} () $0)"))
        ("lisp-interaction-mode" (".yas-parents" . "emacs-lisp-mode")))
       ("library/snippets"
        ("c-mode" (".yas-parents" . "c++-mode"))
        ("cc-mode" ("def" . "# define"))
        ("emacs-lisp-mode" ("dolist" . "(dolist)"))
        ("lisp-interaction-mode" ("sc" . "brother from another mother"))))
     ,@body)))

(ert-deftest basic-jit-loading ()
  "Test basic loading and expansion of snippets"
  (with-some-interesting-snippet-dirs
   (yas/reload-all)
   (yas/basic-jit-loading-1)))

(ert-deftest basic-jit-loading-with-compiled-snippets ()
  "Test basic loading and expansion of snippets"
  (with-some-interesting-snippet-dirs
   (yas/reload-all)
   (yas/recompile-all)
   (flet ((yas/load-directory-2
           (&rest dummies)
           (ert-fail "yas/load-directory-2 shouldn't be called when snippets have been compiled")))
     (yas/reload-all)
     (yas/basic-jit-loading-1))))

(defun yas/basic-jit-loading-1 (&optional compile)
  (with-temp-buffer
    (should (= 4 (hash-table-count yas/scheduled-jit-loads)))
    (should (= 0 (hash-table-count yas/tables)))
    (lisp-interaction-mode)
    (yas/minor-mode 1)
    (should (= 2 (hash-table-count yas/scheduled-jit-loads)))
    (should (= 2 (hash-table-count yas/tables)))
    (should (= 1 (hash-table-count (yas/table-uuidhash (gethash 'lisp-interaction-mode yas/tables)))))
    (should (= 2 (hash-table-count (yas/table-uuidhash (gethash 'emacs-lisp-mode yas/tables)))))
    (yas/should-expand '(("sc" . "brother from another mother")
                         ("dolist" . "(dolist)")
                         ("ert-deftest" . "(ert-deftest name () )")))
    (c-mode)
    (yas/minor-mode 1)
    (should (= 0 (hash-table-count yas/scheduled-jit-loads)))
    (should (= 4 (hash-table-count yas/tables)))
    (should (= 1 (hash-table-count (yas/table-uuidhash (gethash 'c-mode yas/tables)))))
    (should (= 1 (hash-table-count (yas/table-uuidhash (gethash 'cc-mode yas/tables)))))
    (yas/should-expand '(("printf" . "printf();")
                         ("def" . "# define")))
    (yas/should-not-expand '("sc" "dolist" "ert-deftest"))))

;;; Helpers
;;;

(defun yas/should-expand (keys-and-expansions)
  (dolist (key-and-expansion keys-and-expansions)
    (yas/exit-all-snippets)
    (erase-buffer)
    (insert (car key-and-expansion))
    (let ((yas/fallback-behavior nil))
      (ert-simulate-command '(yas/expand)))
    (should (string= (buffer-substring-no-properties (point-min) (point-max))
                       (cdr key-and-expansion))))
  (yas/exit-all-snippets))

(defun yas/should-not-expand (keys)
  (dolist (key keys)
    (yas/exit-all-snippets)
    (erase-buffer)
    (insert key)
    (let ((yas/fallback-behavior nil))
      (ert-simulate-command '(yas/expand)))
    (should (string= (buffer-substring-no-properties (point-min) (point-max)) key))))

(defun yas/mock-insert (string)
  (interactive)
  (do ((i 0 (1+ i)))
      ((= i (length string)))
    (insert (aref string i))))

(defun yas/make-file-or-dirs (ass)
  (let ((file-or-dir-name (car ass))
        (content (cdr ass)))
    (cond ((listp content)
           (make-directory file-or-dir-name 'parents)
           (let ((default-directory (concat default-directory "/" file-or-dir-name)))
             (mapc #'yas/make-file-or-dirs content)))
          ((stringp content)
           (with-current-buffer (find-file file-or-dir-name)
             (insert content)
             (save-buffer)
             (kill-buffer (current-buffer))))
          (t
           (message "[yas] oops don't know this content")))))


(defun yas/variables ()
  (let ((syms))
    (mapatoms #'(lambda (sym)
                  (if (and (string-match "^yas/[^/]" (symbol-name sym))
                           (boundp sym))
                      (push sym syms))))
    syms))


(defmacro yas/saving-variables (&rest body)
  `(let ,(mapcar #'(lambda (sym)
                     `(,sym ,sym))
                 (yas/variables))
     ,@body))

(defmacro with-snippet-dirs (dirs &rest body)
  `(let ((default-directory (make-temp-file "yasnippet-fixture" t)))
     (unwind-protect
         (progn       
           (setq yas/snippet-dirs ',(mapcar #'car (cadr dirs)))
           (mapc #'yas/make-file-or-dirs ,dirs)
           ,@body)
       (when (>= emacs-major-version 23)
         (delete-directory default-directory 'recursive)))))

;;; Older emacsen
;;;
(unless (fboundp 'special-mode)
  (define-minor-mode special-mode "Just a placeholder for something isn't in emacs 22"))

;;; btw to test this in emacs22 mac osx:
;;; curl -L -O https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert.el
;;; curl -L -O https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert-x.el
;;; /usr/bin/emacs -nw -Q -L . -l yasnippet-tests.el --batch -e ert

(provide 'yasnippet-tests)
;;; yasnippet-tests.el ends here
