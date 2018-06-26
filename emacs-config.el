(defun echosa-export-config ()
  (when (string= (buffer-name (current-buffer)) "emacs-config.org")
    (let ((org-file "~/.emacs.d/emacs-config.org")
          (elisp-file "~/.emacs.d/emacs-config.el")
          (readme-file "~/.emacs.d/README.org"))
      (org-babel-tangle-file org-file elisp-file "emacs-lisp")
      (copy-file org-file readme-file t)
      (message "Config export complete!"))))
(add-hook 'after-save-hook 'echosa-export-config)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(temp-buffer-resize-mode 0)
(add-hook 'before-save-hook 'time-stamp)
(setq fill-column 80)
(setq scroll-conservatively 101)
(setq case-fold-search t)
(setq case-replace t)
(setq display-buffer-reuse-frames t)
(setq display-time-24hr-format nil)
(setq display-time-day-and-date t)
(setq large-file-warning-threshold nil)
(setq truncate-partial-width-windows nil)

(put 'dired-find-alternate-file 'disabled nil)

(setq load-prefer-newer t)

(when (memq window-system '(mac ns x))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/bin") exec-path)))

(use-package exec-path-from-shell
  :disabled t
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(desktop-save-mode 1)

(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package icomplete
  :config
  (icomplete-mode))
(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t))

(use-package evil
  :disabled
  :ensure t
  :after (key-chord)
  :config
  (setq evil-default-cursor '(t))
  (evil-mode 1)
  (define-key evil-ex-map "b " 'ido-switch-buffer)
  (define-key evil-ex-map "e " 'ido-find-file)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-emacs-state-map "jk" 'evil-normal-state))

(use-package key-chord
  :disabled
  :ensure t
  :config
  (key-chord-mode 1))

(use-package evil-leader
  :disabled
  :ensure t
  :after (evil)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "x" 'execute-extended-command)
  (evil-leader/set-key ":" 'eval-expression)
  (evil-leader/set-key "k" 'ido-kill-buffer)
  (evil-leader/set-key "p" 'projectile-commander)
  (evil-leader/set-key "d" 'dired)
  (evil-leader/set-key "e" 'er/expand-region)
  (evil-leader/set-key "m" 'mc/mark-more-like-this-extended)
  (evil-leader/set-key "s" 'string-inflection-toggle)
  (evil-leader/set-key "r" 'xref-find-definitions)
  (evil-leader/set-key "?" 'xref-find-references)
  (global-evil-leader-mode))

(use-package evil-surround
  :disabled
  :ensure t
  :config
  (global-evil-surround-mode 1))

(setq evil-emacs-state-modes
      '(archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-rebase-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-commit-mode magit-revision-mode magit-diff-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-branch-manager-mode magit-stash-mode magit-status-mode magit-wazzup-mode magit-refs-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode cider-repl-mode emacsagist-mode elfeed-show-mode elfeed-search-mode notmuch-tree term-mode xref--xref-buffer-mode))

(use-package winner
  :defer 5
  :config
  (winner-mode 1))

(use-package pbcopy
  :ensure t
  :defer t
  :config
  (turn-on-pbcopy))

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package magit
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :if window-system
  :config
  (global-git-gutter-mode))

(use-package git-gutter
  :ensure t
  :if (not window-system)
  :config
  (global-git-gutter-mode 1))

(use-package projectile
  :ensure t
  :defer 5
  :config
  (projectile-global-mode))

(use-package company
  :ensure t
  :bind (("C-<tab>" . company-complete))
  :config
  (global-company-mode))

(use-package ag
  :ensure t)

(use-package paredit
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode clojure-mode) . paredit-mode))

(use-package php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook 'flymake-mode)
  (add-hook 'php-mode-hook 'php-enable-symfony2-coding-style))

(use-package company-php
  :ensure t)

(use-package ac-php
  :ensure t
  :after (php-mode company-php)
  :init
  (bind-key "C-c ]" 'ac-php-find-symbol-at-point php-mode-map)
  (bind-key "C-c [" 'ac-php-location-stack-back php-mode-map)
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (require 'company-php)
               (company-mode t)
               (ac-php-core-eldoc-setup)
               (make-local-variable 'company-backends)
               (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package php-cs-fixer
  :ensure t
  :config
  (require 'cl)
  (add-hook 'before-save-hook 'php-cs-fixer-before-save))


(use-package geben
  :ensure t
  :defer t)

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json\\'")

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
        ("\\.twig\\'" . web-mode)))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package cider
  :ensure t)

(defun my-org-mode-hook ()
  (auto-fill-mode))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(use-package emms
  :ensure t
  :defer t
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (define-emms-simple-player mplayer-no-video '(file url)
    (concat "\\`\\(http[s]?\\|mms\\)://\\|"
            (apply #'emms-player-simple-regexp
                   emms-player-base-format-list))
    "mplayer" "-slave" "-quiet" "-really-quiet" "-vo" "null")
  (define-emms-simple-player mplayer-playlist-no-video '(streamlist)
    "\\`http[s]?://"
    "mplayer" "-slave" "-quiet" "-really-quiet" "-playlist" "-vo" "null"))

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

(let ((my-theme "solarized-dark"))
  (cond
   ((string= my-theme "solarized-dark")
    (use-package solarized-theme
      :if window-system
      :ensure t
      :config
      (load-theme 'solarized-dark t)))
   ((string= my-theme "tango-dark")
    (load-theme 'tango-dark t))))

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.tmp/" nil)))
(setq auto-save-list-file-prefix "~/.emacs.d/.tmp/.saves-")
(setq backup-directory-alist '(("" . "~/.emacs.d/.tmp")))

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

(global-hl-line-mode 1)

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

(use-package highlight-indent-guides
  :disabled t
  :ensure t
  :defer t
  :hook ((prog-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
