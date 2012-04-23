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

(provide 'my-funcs)
