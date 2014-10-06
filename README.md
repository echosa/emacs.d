<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Packages</a>
<ul>
<li><a href="#sec-1-1">1.1. Cask</a></li>
<li><a href="#sec-1-2">1.2. Pallet</a></li>
<li><a href="#sec-1-3">1.3. Package.el</a></li>
<li><a href="#sec-1-4">1.4. Paradox</a></li>
</ul>
</li>
<li><a href="#sec-2">2. Uniquify</a></li>
<li><a href="#sec-3">3. Ido</a></li>
<li><a href="#sec-4">4. Evil</a></li>
<li><a href="#sec-5">5. Line Numbers</a></li>
<li><a href="#sec-6">6. Winner-mode</a></li>
<li><a href="#sec-7">7. pbcopy</a></li>
<li><a href="#sec-8">8. Programming</a>
<ul>
<li><a href="#sec-8-1">8.1. General</a></li>
<li><a href="#sec-8-2">8.2. Git</a></li>
<li><a href="#sec-8-3">8.3. Projectile</a></li>
<li><a href="#sec-8-4">8.4. Paredit</a></li>
<li><a href="#sec-8-5">8.5. Emacs Lisp</a></li>
<li><a href="#sec-8-6">8.6. Javascript</a></li>
<li><a href="#sec-8-7">8.7. PHP</a></li>
</ul>
</li>
<li><a href="#sec-9">9. Compilation</a></li>
<li><a href="#sec-10">10. Org-mode</a></li>
<li><a href="#sec-11">11. IRC</a></li>
<li><a href="#sec-12">12. WWW</a></li>
<li><a href="#sec-13">13. Email</a></li>
<li><a href="#sec-14">14. Terminals</a></li>
<li><a href="#sec-15">15. Man Pages</a></li>
<li><a href="#sec-16">16. Miscellaneous Functions</a></li>
<li><a href="#sec-17">17. Regular Expressions</a></li>
<li><a href="#sec-18">18. Key Bindings</a></li>
<li><a href="#sec-19">19. Theme</a></li>
<li><a href="#sec-20">20. Backup and Auto Save</a></li>
<li><a href="#sec-21">21. General Setup</a></li>
<li><a href="#sec-22">22. Local Setup</a></li>
</ul>
</div>
</div>


# Packages

## Cask

Cask is a dependency manager for Emacs, similar to npm for Node.

[Cask website](http://cask.github.io)

First, we require and initialize Cask.

    (require 'cask "~/.cask/cask.el")
    (cask-initialize)

## Pallet

Pallet keeps the Cask file up-to-date when installing packages through
\`M-x list-packages\`.

[Pallet website](https://github.com/rdallasgray/pallet)

    (require 'pallet)

## Package.el

Also, we need to set up the package repositories for Emacs' own package
manager.

    (setq package-archives
          '(("gnu" . "http://elpa.gnu.org/packages/")
            ("marmalade" . "http://marmalade-repo.org/packages/")
            ("melpa" . "http://melpa.milkbox.net/packages/")))

## Paradox

Finally, paradox is a thing.

    (setq paradox-github-token t)

# Uniquify

If I have two buffers open with two files that have the same name, (e.g. two 
different README files from two different projects), Emacs will, by default, 
name the buffers \`README\` and \`README<1>\`. This is useless. Therefore, I turn on 
uniquify and use it to name buffers wtih the same file name based on their 
parent directories: \`README<projdir1>\` and \`README<projectdir2>\`.

    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

# Ido

Ido makes minibuffer completions much nicer, easier, and faster.

    (require 'ido)
    (ido-mode)
    (setq ido-completion-buffer "*Ido Completions*")
    (setq ido-completion-buffer-all-completions t)
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (icomplete-mode 99)
    (ido-vertical-mode 1)

# Evil

Call me heathen if you wish, but I prefer Vim navigation keys.

[Evil website](https://gitorious.org/evil/pages/Home)

    (evil-mode 1)
    (setq evil-default-cursor '(t))

Using \`key-chord-mode\`, I have the vim equivalent of \`imap jk <Esc>\`, which 
allows me to to \`jk\` instead of \`Esc\` to get out of insert mode.

    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)

I want Ido buffer switching and file finding when using Vim's \`:b\` and \`:e\`.

    (define-key evil-ex-map "b " 'ido-switch-buffer)
    (define-key evil-ex-map "e " 'ido-find-file)

I also want Vim's \`vs\` command for surrounding text with quotes, braces, etc.

    (require 'surround)
    (global-surround-mode 1)

To make things even easier, I set up a "leader key" of \`Space\`, so that I can
type \`Space <letter>\` to run a command. For instance, \`Space x\` instead of
\`M-x\` to execute commands.

    (require 'evil-leader)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "x" 'execute-extended-command)
    (evil-leader/set-key ":" 'eval-expression)
    (evil-leader/set-key "k" 'ido-kill-buffer)
    (evil-leader/set-key "p" 'projectile-commander)
    (evil-leader/set-key "e" 'mu4e)
    (evil-leader/set-key "j" 'ace-jump-mode)
    (evil-leader/set-key "J" 'ace-window)
    (global-evil-leader-mode)

Finally, there are some modes that I want to always be in Emacs mode instead
of Evil.

    (setq evil-emacs-state-modes
          '(archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-commit-mode magit-diff-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-branch-manager-mode magit-stash-mode magit-status-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode cider-repl-mode emacsagist-mode elfeed-show-mode elfeed-search-mode notmuch-tree term-mode))

# Line Numbers

I like line numbers. They help quite a bit with moving around. Here
I turn on linum-mode globally.

    (global-linum-mode)

Then I change the way line numbers are
displayed to be right-justified.

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

# Winner-mode

Winner-mode makes it really easy to handle window changes in
Emacs. `C-c left-arrow` goes back to the previous window
configuration (undo), and `C-c right-arrow` goes forward
(redo). This is especially helpful for when a popop window ruins
your layout. Simply `C-c left-arrow` to get back to where you were.

    (winner-mode 1)

# pbcopy

Clipboard sharing. Copy in Emacs, paste in OS X, and vice versa.

[pbcopy source](https://github.com/jkp/pbcopy.el)

    (require 'pbcopy)
    (turn-on-pbcopy)

# Programming

## General

Here are some general, all-purpose, language-agnostic default settings.

    (setq c-basic-offset 4)
    (setq tab-width 4)

## Git

Magit is awesome.

    (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
    (setq magit-server-window-for-commit 'pop-to-buffer)
    (setq magit-use-overlays nil)

## Projectile

Projectile is, quite simply and objectively, the shit. There's no other way to
put it.

[Projectile on Github](https://github.com/bbatsov/projectile)

    (projectile-global-mode)

## Paredit

If you write any form of Lisp and don't use paredit, change that. 

[Paredit website](http://mumble.net/~campbell/emacs/paredit.el)

[Emacs Rocks episode on paredit](http://emacsrocks.com/e14.html)

    (require 'paredit)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)

## Emacs Lisp

    (defun my-emacs-lisp-mode-hook ()
      (eldoc-mode)
      (linum-mode)
      (lexbind-mode))
    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
    (add-hook 'clojure-mode-hook 'linum-mode)

## Javascript

Prefer js2-mode to javascript-mode.

    (autoload 'js2-mode "js2" nil t)
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

## PHP

    (defun my-php-mode-hook ()
      (make-local-variable (quote whitespace-style))
      (setf whitespace-style (quote (face lines-tail tab-mark)))
      (whitespace-mode t)
      (linum-mode 1)
      (subword-mode 1)
      (php-enable-symfony2-coding-style))
    (add-hook 'php-mode-hook 'my-php-mode-hook)

# Compilation

\`M-x compile\` has some issues with ansi color codes. This fixes
it. Found at [StackOverflow](http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer). 

    (require 'ansi-color)
    (defun colorize-compilation-buffer ()
      (toggle-read-only)
      (ansi-color-apply-on-region (point-min) (point-max))
      (toggle-read-only))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

# Org-mode

    (setq org-agenda-compact-blocks nil)
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-show-all-dates nil)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-window-setup 'current-window)
    (setq org-link-frame-setup
          '((vm . vm-visit-folder-other-frame)
            (gnus . gnus-other-frame)
            (file . find-file)))
    (setq org-log-done 'note)
    (setq org-log-into-drawer t)
    (setq org-timeline-show-empty-dates nil)
    (setq org-todo-keywords
          '((sequence "TODO(t)" "IN PROGRESS(i!)" "|" "DONE(d@/!)")
            (sequence "ON HOLD(h!)" "|" "CANCELLED(c@/!)")))

# IRC

    (setq erc-nick "echosa")
    (setq erc-user-full-name "Echosa")

# WWW

    (setq browse-url-browser-function 'eww-browse-url)

# Email

    (setq send-mail-function 'smtpmail-send-it)

# Terminals

    (setq term-scroll-show-maximum-output nil)
    (setq term-scroll-to-bottom-on-output t)

# Man Pages

    (setq woman-use-own-frame nil)

# Miscellaneous Functions

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

# Regular Expressions

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

# Key Bindings

    (global-set-key "\M-n" 'scroll-up-line)
    (global-set-key "\M-p" 'scroll-down-line)
    (global-set-key "\C-x9" 'delete-other-windows-vertically)
    (global-set-key "\M-@" 'er/expand-region)
    (global-set-key "\C-cm" 'mc/mark-more-like-this-extended)

# Theme

Solarized Dark is where it's at.

    (when window-system (load-theme 'solarized-dark t))

Trust all themes.

    (setq custom-safe-themes t)

# Backup and Auto Save

    (setq auto-save-file-name-transforms '((".*" "~/.emacs.tmp/" nil)))
    (setq auto-save-list-file-prefix "~/.emacs.tmp/.saves-")
    (setq backup-directory-alist '(("" . "~/.emacs.tmp")))

# General Setup

    (when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar nil))
    (tool-bar-mode -1)
    (global-font-lock-mode t)
    (transient-mark-mode t)
    (show-paren-mode t)
    (column-number-mode t)
    (temp-buffer-resize-mode 0)
    (add-hook 'before-save-hook 'time-stamp)
    (menu-bar-mode -1)
    (wrap-region-global-mode)
    (add-hook 'sgml-mode-hook 'zencoding-mode)
    (add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
    (setq web-mode-autocompletes-flag t)
    (put 'dired-find-alternate-file 'disabled nil)
    (setq default-directory (concat (getenv "HOME") "/"))
    (setq blink-cursor-mode t)
    (setq visible-bell t)
    (setq fill-column 80)
    (setq inhibit-startup-screen t)
    (setq scroll-conservatively 101)
    (setq case-fold-search t)
    (setq case-replace t)
    (setq display-buffer-reuse-frames t)
    (setq display-time-24hr-format nil)
    (setq display-time-day-and-date t)
    (setq indent-tabs-mode nil)
    (setq indicate-empty-lines t)
    (setq large-file-warning-threshold nil)
    (setq truncate-partial-width-windows nil)

# Local Setup

    (pcase system-name
      ("Saffron.local" (progn
                         (setq exec-path
                               '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin13.1.0"))
                         (setenv "PATH" (mapconcat 'concat
                                                   (append '("/usr/local/pear/bin"
                                                             "/usr/local/bin"
                                                             "/Users/echosa/.cask/bin")
                                                           (list (getenv "PATH")))
                                                   ":"))
                         (set-face-attribute 'default nil :family "Consolas" :height 120)
                         (when (window-system)
                           (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")))))
