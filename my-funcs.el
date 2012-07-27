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

(defun delete-to-end-of-buffer ()
  "Deletes from point to end of buffer."
  (interactive)
  (delete-region (point) (point-max)))

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
    (when (looking-at ";")
      (let ((count (- (save-excursion (end-of-line) (point)) (point))))
        (open-line 1)
        (insert ";; " (make-string (- count 3) ?\*))
        (forward-line 2)
        (beginning-of-line)
        (open-line 1)
        (insert ";; " (make-string (- count 3) ?\*))))))

;; *******
;; flymake
;; *******
(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking
   FILE-NAME. This is a replacement for
   `flymake-create-temp-inplace'. The difference is that it gives
   a file name in `temporary-file-directory' instead of the same
   directory as FILE-NAME.

   For the use of PREFIX see that function.

   Note that not making the temporary file in another directory
   \(like here) will not if the file you are checking depends on
   relative paths to other files \(for the type of checks flymake
   makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-php-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local-file "-l"))))

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
  (subword-mode 1)
  (linum-mode 1)
  (flymake-mode))

(provide 'my-funcs)

