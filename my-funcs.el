;; ***********
;; setup funcs
;; ***********
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
  (when (and (featurep 'hideshow)
             (get-char-property (point) 'hs))
    (hs-show-block)
    (goto-line line)))

;; *********
;; ansi-term
;; *********
(defun my-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun send-control-c ()
 (interactive)
 (term-send-raw-string ""))

(defun send-control-x ()
 (interactive)
 (term-send-raw-string ""))

(defun send-control-z ()
 (interactive)
 (term-send-raw-string ""))

(defun my-term-hook ()
 (message "Running term hook.")
 (define-key term-raw-map "\C-y" 'my-term-paste)
 (define-key term-raw-map (kbd "C-c M-x") 'smex)
 (define-key term-raw-map (kbd "C-c M-:") 'eval-expression)
 (define-key term-raw-map (kbd "C-c C-c") 'send-control-c)
 (define-key term-raw-map (kbd "C-c C-x") 'send-control-x)
 (define-key term-raw-map (kbd "C-c C-z") 'send-control-z)
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
(add-hook 'term-mode-hook 'my-term-hook)

;; ***********
;; compilation
;; ***********
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(when (fboundp (quote colorize-compilation-buffer))
  (add-hook (quote compilation-filter-hook) (quote colorize-compilation-buffer)))

(provide 'my-funcs)
