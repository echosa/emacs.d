;; ***************************
;; hooks for required packages
;; ***************************
(defun my-term-mode-hook ()
  (define-key term-raw-map "\C-y" 'my-term-paste))

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

;; ***********
;; setup funcs
;; ***********
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

;; *************
;; moving around
;; *************
(defun scroll-up-without-moving-point ()
  (interactive)
  (scroll-up 1))

(defun scroll-down-without-moving-point ()
  (interactive)
  (scroll-down 1))

(defun other-window-backwards ()
  "Move to the previous window."
  (interactive)
  (other-window -1))

(defun goto-window-with-buffer (name &optional start end)
  (let ((start (if start start 0))
        (end (if end end (length name))))
    (dolist (window (window-list))
      (let ((bufname (buffer-name (window-buffer window))))
        (when (<= end (length bufname))
          (let ((bufcheck (substring bufname start end)))
            (when (equal bufcheck name)
              (select-window window)
              (return t))))))))

(defun hs-goto-line (line)
  "Goes to the given line, expanding the code if its hidden by hide-show."
  (interactive "nGoto Line: ")
  (goto-line line)
  (when (and (featurep 'hideshow)
             (get-char-property (point) 'hs))
    (hs-show-block)
    (goto-line line)))

;; *******
;; editing
;; *******
(defun open-new-line (arg)
  "Move to the next line and then opens a line. See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun delete-this-line ()
  "This function deletes the current line and its newline,
  without moving the cursor as much as possible."
  (interactive)
  (let ((col (current-column)))
    (beginning-of-line)
    (kill-line)
    (kill-line)
    (end-of-line)
    (when (< col (current-column))
      (backward-char (- (current-column) col)))))

(defun duplicate-current-line ()
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun delete-whitespace-starting-here ()
  (interactive)
  (let ((count 0))
    (save-excursion
      (while (equal " " (char-to-string (char-after)))
        (setq count (+ count 1))
        (forward-char)))
    (delete-char count)))

(defun for-each-dired-marked-file (fn)
  "Do stuff for each marked file, only works in dired window"
  (if (eq major-mode 'dired-mode)
      (let ((filenames (dired-get-marked-files)))
        (mapcar fn filenames))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))

(defun dired-delete-trailing-whitespace ()
  (let ((open-buffers (buffer-list)))
    (for-each-dired-marked-file
     (lambda (filename)
       (find-file filename)
       (delete-trailing-whitespace)
       (save-buffer)
       (unless (member (current-buffer) open-buffers)
         (kill-buffer))))))

(defun php-project-delete-trailing-whitespace (&optional project)
  "Removes all trailing whitespace in a project's files."
  (interactive)
  (unless project
    (setq project 
          (if (php-project-buffer-project)
              (php-project-buffer-project)
            (php-project-ask-for-project))))
  (when (member project php-project-list)
    (let ((dir (php-project-directory project)))
      (find-name-dired (php-project-directory project) "*.php")
      (let ((dired-buffer (current-buffer)))
        (dired-toggle-marks)
        (dired-delete-trailing-whitespace)
        (kill-buffer dired-buffer)))))

;; ************
;; buffer stuff 
;; ************
(defun switch-to-scratch-buffer ()
  (interactive)
  (let ((bufname "*scratch*"))
    (unless (goto-window-with-buffer bufname)
      (switch-to-buffer bufname))))

(defun my-ibuffer-load-hook ()
  "Hook for when ibuffer is loaded."
  (define-ibuffer-filter unsaved-file-buffers
    "Only show unsaved buffers backed by a real file."
    (:description "unsaved file buffers")
    (and (buffer-local-value 'buffer-file-name buf)
         (buffer-modified-p buf)))
  (define-key ibuffer-mode-map (kbd "/ *") 'ibuffer-filter-by-unsaved-file-buffers))

;; ************
;; window stuff
;; ************
;; from emacswiki WindowNavigation
(defun get-column-window-list (&optional side)
  (let ((column (if (eq side 'right) (caddr (window-pixel-edges))
                    (car (window-pixel-edges))))
        (current-window (selected-window))
        column-window-list)
    (mapc '(lambda (window)
                  (when (eq (car (window-pixel-edges window)) column)
                           (add-to-list 'column-window-list window)))
            (window-list))
    (setq column-window-list 
            (sort column-window-list
                  '(lambda (a b) 
                        (< (cadr (window-pixel-edges a))
                                 (cadr (window-pixel-edges b))))))
    (while (not (eq (car column-window-list) current-window))
      (setq column-window-list (rotate-list column-window-list 1)))
    column-window-list))

;; from emacswiki WindowNavigation
(defun rotate-list (list count)
  (cond
   ((eq count 0) list)
   ((not list) list)
   (t
    (rotate-list (nconc  (cdr list) (list (car list)) '()) (1- count)))))

(defun close-other-column-windows ()
  (interactive)
  (dolist (window (get-column-window-list))
    (unless (eq window (selected-window))
      (delete-window window))))

;; *********************************
;; increase/decrease number at point
;; *********************************
(require 'thingatpt)
(put 'decimal 'end-op
     (lambda ()
       (re-search-forward "[0-9-]*" nil t)))
(put 'decimal 'beginning-op
     (lambda ()
       (if (re-search-backward "[^0-9-]" nil t)
           (forward-char))))
(defun change-number-at-point (func)
  (let* ((bounds (bounds-of-thing-at-point 'decimal))
         (number (buffer-substring (car bounds) (cdr bounds)))
         (point (point)))
    (goto-char (car bounds))
    (delete-char (length number))
    (insert (number-to-string (funcall func (string-to-number number))))
    (goto-char point)))

(defun inc-number-at-point ()
  (interactive)
  (change-number-at-point (lambda (number) (+ number 1))))

(defun dec-number-at-point ()
  (interactive)
  (change-number-at-point (lambda (number) (- number 1))))

;; ***
;; w3m
;; ***
(defun my-open-url (url &optional args)
  (if (goto-window-with-buffer "*w3m*")
      (w3m-browse-url url t)
    (condition-case nil
        (browse-url-default-browser url args)
      (error
       (browse-url-default-macosx-browser url t)))))

;; ****
;; term
;; ****
(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

;; ********
;; hideshow
;; ********
;; Add the following to your .emacs and uncomment it in order to get a + symbol
;; in the fringe and a yellow marker indicating the number of hidden lines at
;; the end of the line for hidden regions:
(when (and (display-graphic-p) (fringe-bitmap-p 'hs-marker))
  (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0]))

(defcustom hs-fringe-face 'hs-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)

(defface hs-fringe-face
  '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(defcustom hs-face 'hs-face
  "*Specify the face to to use for the hidden region indicator"
  :type 'face
  :group 'hideshow)

(defface hs-face
  '((t (:background "#ff8" :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
           )
      (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string)
      )))

(setq hs-set-up-overlay 'display-code-line-counts)

;; ****
;; misc
;; ****
(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

(provide 'my-funcs)
