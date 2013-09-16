(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

;; *************
;; moving around
;; *************
(defun other-window-backwards ()
  "Move to the previous window."
  (interactive)
  (other-window -1))

(defun hs-goto-line (line)
  "Goes to the given line, expanding the code if its hidden by hide-show."
  (interactive "nGoto Line: ")
  (goto-line line)
  (when (and (featurep 'hideshow) (get-char-property (point) 'hs))
    (hs-show-block)
    (goto-line line)))

(defun delete-to-end-of-buffer (add-to-kill-ring-p)
  "Deletes from point to end of buffer.
If prefix argument is given, kill the region, adding it to the kill ring."
  (interactive "P")
  (if add-to-kill-ring-p
      (kill-region (point) (point-max))
    (delete-region (point) (point-max))))

;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))
(define-key ctl-x-4-map "t" 'toggle-window-split)

;; *********
;; ansi-term
;; *********
(defun my-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste)
  (let ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
        (base00  "#657b83")
        (base0   "#839496")
        (base1   "#93a1a1")
        (base2   "#eee8d5")
        (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
    (setq ansi-term-color-vector 
          (vconcat `(unspecified ,base02 ,red ,green ,yellow ,blue ,magenta 
                                 ,cyan ,base2)))))

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))

;; ***********
;; compilation
;; ***********
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (unless (fboundp 'ansi-color-apply-on-region)
    (require 'ansi-color))
  (when (fboundp 'ansi-color-apply-on-region)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (toggle-read-only))

;; **********
;; re-buidler
;; **********
(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
    (message "Not in a re-builder buffer!")))

;; *************
;; sudo commands
;; *************
(defun sudo-shell-command (command &optional output-buffer error-buffer)
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (shell-command
   (concat "echo " (read-passwd "Password? ") " | sudo -S " command)
   output-buffer
   error-buffer))

(defun make-elisp-header ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((use-semicolons (looking-at ";"))
          (count (- (save-excursion (end-of-line) (point)) (point))))
      (open-line 1)
      (if use-semicolons
          (insert ";; " (make-string (- count 3) ?\*))
        (insert (make-string count ?\*)))
      (forward-line 2)
      (beginning-of-line)
      (open-line 1)
      (if use-semicolons
          (insert ";; " (make-string (- count 3) ?\*))
        (insert (make-string count ?\*))))))

;; *******
;; flymake
;; *******
;; (defun flymake-create-temp-intemp (file-name prefix)
;;   "Return file name in temporary directory for checking
;;    FILE-NAME. This is a replacement for
;;    `flymake-create-temp-inplace'. The difference is that it gives
;;    a file name in `temporary-file-directory' instead of the same
;;    directory as FILE-NAME.

;;    For the use of PREFIX see that function.

;;    Note that not making the temporary file in another directory
;;    \(like here) will not if the file you are checking depends on
;;    relative paths to other files \(for the type of checks flymake
;;    makes)."
;;   (unless (stringp file-name)
;;     (error "Invalid file-name"))
;;   (or prefix
;;       (setq prefix "flymake"))
;;   (let* ((name (concat
;;                 (file-name-nondirectory
;;                  (file-name-sans-extension file-name))
;;                 "_" prefix))
;;          (ext  (concat "." (file-name-extension file-name)))
;;          (temp-name (make-temp-file name nil ext))
;;          )
;;     (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
;;     temp-name))

;; (defun flymake-php-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-intemp))
;; 	 (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "php" (list "-f" local-file "-l"))))

;; ***
;; js2
;; ***
(defun my-js2-mode-hook ()
  (setf forward-sexp-function nil))

;; ****
;; php+
;; ****
(defun my-php+-mode-hook ()
  (make-local-variable (quote whitespace-style))
  (setf whitespace-style (quote (face lines-tail tab-mark)))
  (whitespace-mode t)
  (subword-mode 1))

;; *********
;; java-mode
;; *********
(defun my-java-mode-hook ()
  (make-local-variable (quote whitespace-style))
  (setf whitespace-style (quote (face lines-tail tab-mark)))
  (whitespace-mode t)
  (subword-mode 1)
  (setq ac-sources (append '(ac-source-semantic) ac-sources))
  (auto-complete-mode t)
  (when (fboundp 'semantic-decoration-mode)
    (semantic-decoration-mode 1))
  (linum-mode)
  (linum-relative-toggle))

;; *****
;; morse
;; *****
(defun fix-morse (string)
  (with-temp-buffer
    (insert (replace-regexp-in-string
             "_" "-" (replace-regexp-in-string
                      " " "" (replace-regexp-in-string
                              "   " "/" string))))
    (unmorse-region (point-min) (point-max))
    (buffer-string)))

(defun unfix-morse (string)
  (replace-regexp-in-string
   "-" "_" (replace-regexp-in-string
            "/" " " (mapconcat (lambda (x) (make-string 1 x))
                               (with-temp-buffer 
                                 (insert string)
                                 (morse-region (point-min) (point-max))
                                 (buffer-string))
                               " "))))

;; *****
;; CEDET
;; *****
;; (defadvice cogre-delete (after cogre-refresh-after-delete)
;;   (cogre-refresh))
;; (ad-activate 'cogre-delete)

;; **************
;; Tip of the Day
;; **************
(require 'cl)
(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n========================\n\n"
               (with-temp-buffer
                 (describe-function-1 command)
                 (buffer-string))
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

;; *************************
;; Paredit in the minibuffer
;; *************************
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (and (eq this-command 'eval-expression)
           (fboundp 'paredit-mode))
      (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(provide 'my-funcs)

