;;; company-semantic.el --- a company-mode back-end using CEDET Semantic
;;
;; Copyright (C) 2009-2010 Nikolaj Schumacher
;;
;; This file is part of company 0.5.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)
(or (require 'semantic-analyze nil t)
    (require 'semantic/analyze))
(eval-when-compile (require 'cl))

(defcustom company-semantic-metadata-function 'company-semantic-summary-and-doc
  "*The function turning a semantic tag into doc information."
  :group 'company
  :type 'function)

(defvar company-semantic-modes '(c-mode c++-mode jde-mode java-mode))

(defvar company-semantic--current-tags nil
  "Tags for the current context")

(defun company-semantic-doc-or-summary (tag)
  (or (semantic-documentation-for-tag tag)
      (and (require 'semantic-idle nil t)
           (require 'semantic/idle nil t)
           (funcall semantic-idle-summary-function tag nil t))))

(defun company-semantic-summary-and-doc (tag)
  (let ((doc (semantic-documentation-for-tag tag))
        (summary (funcall semantic-idle-summary-function tag nil t)))
    (and (stringp doc)
         (string-match "\n*\\(.*\\)$" doc)
         (setq doc (match-string 1 doc)))
    (concat (funcall semantic-idle-summary-function tag nil t)
            (when doc
                  (if (< (+ (length doc) (length summary) 4) (window-width))
                      " -- "
                    "\n"))
            doc)))

(defun company-semantic-doc-buffer (tag)
  (let ((doc (semantic-documentation-for-tag tag)))
    (when doc
      (with-current-buffer (company-doc-buffer)
        (insert (funcall semantic-idle-summary-function tag nil t)
                "\n"
                doc)
        (current-buffer)))))

(defsubst company-semantic-completions (prefix)
  (ignore-errors
    (let ((completion-ignore-case nil)
          (context (semantic-analyze-current-context)))
      (setq company-semantic--current-tags
            (semantic-analyze-possible-completions context))
      (all-completions prefix company-semantic--current-tags))))

(defun company-semantic-completions-raw (prefix)
  (setq company-semantic--current-tags nil)
  (dolist (tag (semantic-analyze-find-tags-by-prefix prefix))
    (unless (eq (semantic-tag-class tag) 'include)
      (push tag company-semantic--current-tags)))
  (delete "" (mapcar 'semantic-tag-name company-semantic--current-tags)))

(defun company-semantic--pre-prefix-length (prefix-length)
  "Sum up the length of all chained symbols before POS.
Symbols are chained by \".\" or \"->\"."
  (save-excursion
    (let ((pos (point)))
      (goto-char (- (point) prefix-length))
      (while (looking-back "->\\|\\.")
        (goto-char (match-beginning 0))
        (skip-syntax-backward "w_"))
      (- pos (point)))))

(defun company-semantic--grab ()
  "Grab the semantic prefix, but return everything before -> or . as length."
  (let ((symbol (company-grab-symbol)))
    (when symbol
      (cons symbol (company-semantic--pre-prefix-length (length symbol))))))

;;;###autoload
(defun company-semantic (command &optional arg &rest ignored)
  "A `company-mode' completion back-end using CEDET Semantic."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-semantic))
    ('prefix (and (memq major-mode company-semantic-modes)
                  (semantic-active-p)
                  (not (company-in-string-or-comment))
                  (or (company-semantic--grab) 'stop)))
    ('candidates (if (and (equal arg "")
                          (not (looking-back "->\\|\\.")))
                     (company-semantic-completions-raw arg)
                   (company-semantic-completions arg)))
    ('meta (funcall company-semantic-metadata-function
                    (assoc arg company-semantic--current-tags)))
    ('doc-buffer (company-semantic-doc-buffer
                  (assoc arg company-semantic--current-tags)))
    ;; because "" is an empty context and doesn't return local variables
    ('no-cache (equal arg ""))
    ('location (let ((tag (assoc arg company-semantic--current-tags)))
                 (when (buffer-live-p (semantic-tag-buffer tag))
                   (cons (semantic-tag-buffer tag)
                         (semantic-tag-start tag)))))))

(provide 'company-semantic)
;;; company-semantic.el ends here
