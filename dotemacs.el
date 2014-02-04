;; global settings
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'uniquify)

(require 'ido)
(ido-mode)
(icomplete-mode 99)
(ido-vertical-mode 1)

(evil-mode 1)
;; (key-chord-mode 1)
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;; (require 'evil-leader)
;; (define-key evil-ex-map "b " 'ido-switch-buffer)
;; (define-key evil-ex-map "e " 'ido-find-file)
;; (evil-leader/set-leader "<SPC>")
;; (evil-leader/set-key "x" 'execute-extended-command)
;; (evil-leader/set-key ":" 'eval-expression)
;; (evil-leader/set-key "k" 'ido-kill-buffer)
;; (evil-leader/set-key "p" 'projectile-commander)
;; (global-evil-leader-mode)

(projectile-global-mode)

(add-hook 'clojure-mode-hook 'linum-mode)

(require 'pbcopy)
(turn-on-pbcopy)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

(require 'ecukes)

(defun other-window-backwards ()
  "Move to the previous window."
  (interactive)
  (other-window -1))

(defun delete-to-end-of-buffer (add-to-kill-ring-p)
  "Deletes from point to end of buffer.
If prefix argument is given, kill the region, adding it to the kill ring."
  (interactive "P")
  (funcall
   (if add-to-kill-ring-p 'kill-region 'delete-region)
   (point) (point-max)))

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

(defun my-java-mode-hook ()
  (make-local-variable (quote whitespace-style))
  (setf whitespace-style (quote (face lines-tail tab-mark)))
  (whitespace-mode t)
  (subword-mode 1))

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

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)
(global-set-key "\C-xO" 'other-window-backwards)
(global-set-key "\C-x9" 'delete-other-windows-vertically)
(global-set-key "\M-D" 'delete-to-end-of-buffer)
(global-set-key "\M-@" 'er/expand-region)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-c\C-j" 'ace-jump-mode)
(global-set-key "\C-cm" 'mc/mark-more-like-this-extended)
(global-set-key "\C-cv" 'eval-buffer)
(define-key ctl-x-4-map "t" 'toggle-window-split)

(defun my-php-mode-hook ()
  (make-local-variable (quote whitespace-style))
  (setf whitespace-style (quote (face lines-tail tab-mark)))
  (whitespace-mode t)
  (subword-mode 1))

(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar nil))
(tool-bar-mode -1)
(global-font-lock-mode t)
(transient-mark-mode t)
(show-paren-mode t)
(column-number-mode t)
(temp-buffer-resize-mode 0)
(add-hook 'before-save-hook 'time-stamp)
(menu-bar-mode -1)
(when window-system (load-theme 'solarized-dark t))
(wrap-region-global-mode)
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
(setq web-mode-autocompletes-flag t)
(put 'dired-find-alternate-file 'disabled nil)

(when (string= system-name "Saffron.local")
  (setenv "PATH" (mapconcat 'concat
                            (append '("/usr/local/pear/bin"
                                      "/usr/local/bin"
                                      "/Users/echosa/.cask/bin")
                                    (list (getenv "PATH")))
                            ":"))
  (set-face-attribute 'default nil :family "Consolas" :height 120)
  (when (window-system)
    (ns-set-resource nil "ApplePressAndHoldEnabled" "NO"))
  (load-file "~/quicklisp/slime-helper.el")
  (setq slime-net-coding-system 'utf-8-unix)
  (setq inferior-lisp-program "sbcl"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.tmp/" nil))))
 '(auto-save-list-file-prefix "~/.emacs.tmp/.saves-")
 '(backup-directory-alist (quote (("" . "~/.emacs.tmp"))))
 '(blink-cursor-mode t)
 '(c-basic-offset 4)
 '(case-fold-search t)
 '(case-replace t)
 '(display-buffer-reuse-frames t)
 '(display-time-24hr-format nil)
 '(display-time-day-and-date t)
 '(evil-default-cursor (quote (t "white")))
 '(evil-emacs-state-modes (quote (archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-commit-mode magit-diff-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-branch-manager-mode magit-stash-mode magit-status-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode cider-repl-mode)))
 '(erc-nick "echosa")
 '(erc-user-full-name "Echosa")
 '(fill-column 80)
 '(ido-completion-buffer "*Ido Completions*")
 '(ido-completion-buffer-all-completions t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(linum-format (lambda (line) (propertize (format (concat "%" (number-to-string (length (number-to-string (line-number-at-pos (point-max))))) "d ") line) (quote face) (quote linum))))
 '(message-kill-buffer-on-exit t)
 '(message-log-max 5000)
 '(org-agenda-compact-blocks nil)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-all-dates nil)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame) (gnus . gnus-other-frame) (file . find-file))))
 '(org-log-done (quote note))
 '(org-log-into-drawer t)
 '(org-timeline-show-empty-dates nil)
 '(org-todo-keywords (quote ((sequence "TODO(t)" "IN PROGRESS(i!)" "|" "DONE(d@/!)") (sequence "ON HOLD(h!)" "|" "CANCELLED(c@/!)"))))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(reb-mode-hook (quote ((lambda nil (define-key reb-mode-map "\245" (quote reb-query-replace-this-regxp))))))
 '(scroll-conservatively 10000)
 '(server-port nil)
 '(tab-width 4)
 '(term-scroll-show-maximum-output nil)
 '(term-scroll-to-bottom-on-output t)
 '(tramp-default-method "ssh")
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vimgolf-key "2ccc64006e5b916b14d5fde44bd3dca4")
 '(winner-mode t nil (winner))
 '(woman-use-own-frame nil t))

