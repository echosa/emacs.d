
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(put 'dired-find-alternate-file 'disabled nil)

(setq load-prefer-newer t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(icomplete-mode)
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(ido-vertical-mode 1)

(evil-mode 1)
(setq evil-default-cursor '(t))

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)

(define-key evil-ex-map "b " 'ido-switch-buffer)
(define-key evil-ex-map "e " 'ido-find-file)

(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "x" 'execute-extended-command)
(evil-leader/set-key ":" 'eval-expression)
(evil-leader/set-key "k" 'ido-kill-buffer)
(evil-leader/set-key "p" 'projectile-commander)
(evil-leader/set-key "d" 'dired)
(evil-leader/set-key "e" 'er/expand-region)
(evil-leader/set-key "m" 'mc/mark-more-like-this-extended)
(evil-leader/set-key "s" 'string-inflection-toggle)
(global-evil-leader-mode)

(setq evil-emacs-state-modes
      '(archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-rebase-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-commit-mode magit-revision-mode magit-diff-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-branch-manager-mode magit-stash-mode magit-status-mode magit-wazzup-mode magit-refs-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode cider-repl-mode emacsagist-mode elfeed-show-mode elfeed-search-mode notmuch-tree term-mode))

(add-hook 'geben-mode-hook 'evil-emacs-state)

(global-linum-mode)

(setq linum-format (lambda
                     (line)
                     (propertize
                      (format (concat "%"
                                      (number-to-string
                                       (length
                                        (number-to-string
                                         (line-number-at-pos
                                          (point-max)))))
                                      "d ")
                              line)
                      'face
                      'linum)))

(winner-mode 1)

(require 'pbcopy)
(turn-on-pbcopy)

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(require 'magit)

(projectile-global-mode)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

(defun my-emacs-lisp-mode-hook ()
  (eldoc-mode)
  (linum-mode))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

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
(setq reb-mode-hook
      '((lambda nil
          (define-key reb-mode-map "\245" 'reb-query-replace-this-regxp))))

(when window-system
  (load-theme 'solarized-dark t))

(setq auto-save-file-name-transforms '((".*" "~/.emacs.tmp/" nil)))
(setq auto-save-list-file-prefix "~/.emacs.tmp/.saves-")
(setq backup-directory-alist '(("" . "~/.emacs.tmp")))

(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar nil))

(tool-bar-mode -1)

(menu-bar-mode -1)

(setq inhibit-startup-screen t)

(transient-mark-mode t)

(global-font-lock-mode t)

(column-number-mode t)

(show-paren-mode t)

(setq blink-cursor-mode t)

(setq indicate-empty-lines t)
