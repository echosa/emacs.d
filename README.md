<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Introduction</a></li>
<li><a href="#sec-2">2. Installation</a>
<ul>
<li><a href="#sec-2-1">2.1. Get from GitHub</a></li>
<li><a href="#sec-2-2">2.2. Start Emacs</a></li>
<li><a href="#sec-2-3">2.3. Install packages</a></li>
<li><a href="#sec-2-4">2.4. Restart Emacs</a></li>
<li><a href="#sec-2-5">2.5. Additional Useful Package Information</a></li>
</ul>
</li>
<li><a href="#sec-3">3. Packages</a>
<ul>
<li><a href="#sec-3-1">3.1. Repositories</a></li>
</ul>
</li>
<li><a href="#sec-4">4. General Setup</a></li>
<li><a href="#sec-5">5. Uniquify</a></li>
<li><a href="#sec-6">6. Ido</a></li>
<li><a href="#sec-7">7. Evil</a></li>
<li><a href="#sec-8">8. Line Numbers</a></li>
<li><a href="#sec-9">9. Winner-mode</a></li>
<li><a href="#sec-10">10. pbcopy</a></li>
<li><a href="#sec-11">11. Minibuffer</a></li>
<li><a href="#sec-12">12. Programming</a>
<ul>
<li><a href="#sec-12-1">12.1. General</a></li>
<li><a href="#sec-12-2">12.2. Git</a></li>
<li><a href="#sec-12-3">12.3. Projectile</a></li>
<li><a href="#sec-12-4">12.4. Paredit</a></li>
<li><a href="#sec-12-5">12.5. Emacs Lisp</a></li>
</ul>
</li>
<li><a href="#sec-13">13. Miscellaneous Functions</a></li>
<li><a href="#sec-14">14. Regular Expressions</a></li>
<li><a href="#sec-15">15. Theme</a></li>
<li><a href="#sec-16">16. Backup and Auto Save</a></li>
<li><a href="#sec-17">17. Prettyify Emacs</a>
<ul>
<li><a href="#sec-17-1">17.1. Things I don't want to see</a>
<ul>
<li><a href="#sec-17-1-1">17.1.1. Scroll bars</a></li>
<li><a href="#sec-17-1-2">17.1.2. Tool bar</a></li>
<li><a href="#sec-17-1-3">17.1.3. Menu bar</a></li>
<li><a href="#sec-17-1-4">17.1.4. Splash screen</a></li>
</ul>
</li>
<li><a href="#sec-17-2">17.2. Things I do want to see</a>
<ul>
<li><a href="#sec-17-2-1">17.2.1. Highlight current region/selection</a></li>
<li><a href="#sec-17-2-2">17.2.2. Syntax highlighting</a></li>
<li><a href="#sec-17-2-3">17.2.3. Column number</a></li>
<li><a href="#sec-17-2-4">17.2.4. Show matching parenthesis</a></li>
<li><a href="#sec-17-2-5">17.2.5. Blinking cursor</a></li>
<li><a href="#sec-17-2-6">17.2.6. Show empty lines</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


# Introduction<a id="sec-1" name="sec-1"></a>

Inspired by [Sacha Chua](http://sachachua.com/blog/2012/06/literate-programming-emacs-configuration-file/), I have moved my Emacs configuration into an
organized and descriptive [org-mode](http://orgmode.org) file. What you are reading now
is, in fact, my Emacs configuration file.

Well, sort of.

With org-mode, I can export the information to a number of
formats. That means you could be reading this directly from the .org
file itself, from a markdown file (like the [README](https://github.com/echosa/emacs.d/blob/master/README.md) on my Emacs
configuration GitHub repo), or from an HTML file somewhere on the
web.

How this works is based around a part of org-mode called
org-babel. org-babel allows org-mode to execute code that is
embedded into a .org file. If you look at the [actual .emacs file](https://github.com/echosa/emacs.d/blob/master/init.el)
that my Emacs loads, you'll see that all it does is load the .org
file containing my configuration (the one you're reading now) and
parse it through org-babel to execute only the blocks of elisp that
make up the actual configuration, while ignoring the extra
documetation and narrative, like this introdution section.

If you're wondering about performance, org-babel doesn't do this
parse every time I open Emacs. Instead, it sees that I'm trying to
load \`emacs-config.org\` and checks for the existence of
\`emacs-config.el\`. If it doesn't find that file, or finds an out of
date version, only then does it parse the .org file to create a new
.el file. This means there's a bit of a slow startup the first time
after the org-mode file is changes, but after that there's no
noticable change in performance (at least on my machine).

Anyway, what follows is my actual Emacs configuration, embedded into
a descriptive narrative.

# Installation<a id="sec-2" name="sec-2"></a>

## Get from GitHub<a id="sec-2-1" name="sec-2-1"></a>

First, you need to get the config from GitHub. I recommend actually cloning
instead of just downloading a zip file, because a cloned repo will be 
easier to update.

First, delete, move, or rename your existing \`~/.emacs.d/\` directory and your
existing \`~/.emacs\` file. Next, clone the repository into your home directory:

\`$ git clone git@github.com:echosa/emacs.d.git ~/.emacs.d\`

## Start Emacs<a id="sec-2-2" name="sec-2-2"></a>

Start Emacs and ignore any errors for now.

## Install packages<a id="sec-2-3" name="sec-2-3"></a>

Next, the packages need to be installed. Once Emacs is started, run
\`package-install-selected-packages\`. This will install all the
packages defined in \`package-selected-packages\` in the init
file. More info on that below.

## Restart Emacs<a id="sec-2-4" name="sec-2-4"></a>

Now that packages are installed, restart Emacs and you should be
all setup!

## Additional Useful Package Information<a id="sec-2-5" name="sec-2-5"></a>

Newer versions of Emacs include a variable called
\`package-selected-packages\` which can automatically track the
packages you install \`M-x list-packages\` and \`M-x package
install\`. So, you can just keep using the package.el functionality
as usual without any habit changes, and Emacs will remember what
you need. Then, if you ever need to reinstall Emacs fresh, you can
run \`package-install-selected-packages\`, and Emacs will reinstall
all your required and desired packages. It's pretty nice, to be
honest, and has replaced the Cask/Pallet combo I used to use for
this very purpose.

# Packages<a id="sec-3" name="sec-3"></a>

External and third-party packages are great. They make adding new things to
Emacs much nicer and less complicated.

## Repositories<a id="sec-3-1" name="sec-3-1"></a>

We need to set up the package repositories for Emacs' own package manager.

    (setq package-archives
          '(("gnu" . "http://elpa.gnu.org/packages/")
            ("marmalade" . "http://marmalade-repo.org/packages/")
            ("melpa" . "https://melpa.org/packages/")
            ))

# General Setup<a id="sec-4" name="sec-4"></a>

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
    (global-hl-line-mode 1)

Allow \`a\` to be used in dired to reuse the buffer instead of creating new buffers for every
directory.

    (put 'dired-find-alternate-file 'disabled nil)

Don't load outdated complied files.

    (setq load-prefer-newer t)

Make sure Emacs can find and run commands on the PATH.

    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))

# Uniquify<a id="sec-5" name="sec-5"></a>

If I have two buffers open with two files that have the same name, (e.g. two 
different README files from two different projects), Emacs will, by default, 
name the buffers \`README\` and \`README<1>\`. This is useless. Therefore, I turn on 
uniquify and use it to name buffers wtih the same file name based on their 
parent directories: \`README<projdir1>\` and \`README<projectdir2>\`.

    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

# Ido<a id="sec-6" name="sec-6"></a>

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

# Evil<a id="sec-7" name="sec-7"></a>

Call me heathen if you wish, but I prefer Vim navigation keys.

[Evil website](https://gitorious.org/evil/pages/Home)

    (evil-mode 1)
    (setq evil-default-cursor '(t))

Using \`key-chord-mode\`, I have the vim equivalent of \`imap jk <Esc>\`, which 
allows me to use \`jk\` instead of \`Esc\` to get out of insert mode.

    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-motion-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-emacs-state-map "jk" 'evil-normal-state)

I want Ido buffer switching and file finding when using Vim's \`:b\` and \`:e\`.

    (define-key evil-ex-map "b " 'ido-switch-buffer)
    (define-key evil-ex-map "e " 'ido-find-file)

To make things even easier, I set up a "leader key" of \`Space\`, so that I can
type \`Space <letter>\` to run a command. For instance, \`Space x\` instead of
\`M-x\` to execute commands.

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

Finally, there are some modes that I want to always be in Emacs mode instead
of Evil.

Major modes:

    (setq evil-emacs-state-modes
          '(archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-rebase-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-commit-mode magit-revision-mode magit-diff-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-branch-manager-mode magit-stash-mode magit-status-mode magit-wazzup-mode magit-refs-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode cider-repl-mode emacsagist-mode elfeed-show-mode elfeed-search-mode notmuch-tree term-mode))

Minor modes:

    (add-hook 'geben-mode-hook 'evil-emacs-state)

# Line Numbers<a id="sec-8" name="sec-8"></a>

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

# Winner-mode<a id="sec-9" name="sec-9"></a>

Winner-mode makes it really easy to handle window changes in
Emacs. `C-c left-arrow` goes back to the previous window
configuration (undo), and `C-c right-arrow` goes forward
(redo). This is especially helpful for when a popop window ruins
your layout. Simply `C-c left-arrow` to get back to where you were.

    (winner-mode 1)

# pbcopy<a id="sec-10" name="sec-10"></a>

Clipboard sharing. Copy in Emacs, paste in OS X, and vice versa.

[pbcopy source](https://github.com/jkp/pbcopy.el)

    (require 'pbcopy)
    (turn-on-pbcopy)

# Minibuffer<a id="sec-11" name="sec-11"></a>

This little snippet adds eldoc support to the minibuffer. Requires Emacs 24.4.
[Found on EndlessParenthesis.com.](http://endlessparentheses.com/sweet-new-features-in-24-4.html)

    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

# Programming<a id="sec-12" name="sec-12"></a>

## General<a id="sec-12-1" name="sec-12-1"></a>

Indent with 4 spaces, not a tabstop.

    (setq c-basic-offset 4)
    (setq tab-width 4)
    (setq indent-tabs-mode nil)

## Git<a id="sec-12-2" name="sec-12-2"></a>

Magit is awesome.

    (require 'magit)

## Projectile<a id="sec-12-3" name="sec-12-3"></a>

Projectile is, quite simply and objectively, the shit. There's no other way to
put it.

[Projectile on Github](https://github.com/bbatsov/projectile)

    (projectile-global-mode)

## Paredit<a id="sec-12-4" name="sec-12-4"></a>

If you write any form of Lisp and don't use paredit, change that. 

[Paredit website](http://mumble.net/~campbell/emacs/paredit.el)

[Emacs Rocks episode on paredit](http://emacsrocks.com/e14.html)

    (require 'paredit)
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)

## Emacs Lisp<a id="sec-12-5" name="sec-12-5"></a>

    (defun my-emacs-lisp-mode-hook ()
      (eldoc-mode)
      (linum-mode))
    (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

# Miscellaneous Functions<a id="sec-13" name="sec-13"></a>

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

# Regular Expressions<a id="sec-14" name="sec-14"></a>

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

# Theme<a id="sec-15" name="sec-15"></a>

    (when window-system
      (load-theme 'solarized-dark t))

# Backup and Auto Save<a id="sec-16" name="sec-16"></a>

    (setq auto-save-file-name-transforms '((".*" "~/.emacs.tmp/" nil)))
    (setq auto-save-list-file-prefix "~/.emacs.tmp/.saves-")
    (setq backup-directory-alist '(("" . "~/.emacs.tmp")))

# Prettyify Emacs<a id="sec-17" name="sec-17"></a>

## Things I don't want to see<a id="sec-17-1" name="sec-17-1"></a>

### Scroll bars<a id="sec-17-1-1" name="sec-17-1-1"></a>

    (when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar nil))

### Tool bar<a id="sec-17-1-2" name="sec-17-1-2"></a>

    (tool-bar-mode -1)

### Menu bar<a id="sec-17-1-3" name="sec-17-1-3"></a>

    (menu-bar-mode -1)

### Splash screen<a id="sec-17-1-4" name="sec-17-1-4"></a>

It's unnecessary, really.

    (setq inhibit-startup-screen t)

## Things I do want to see<a id="sec-17-2" name="sec-17-2"></a>

### Highlight current region/selection<a id="sec-17-2-1" name="sec-17-2-1"></a>

    (transient-mark-mode t)

### Syntax highlighting<a id="sec-17-2-2" name="sec-17-2-2"></a>

    (global-font-lock-mode t)

### Column number<a id="sec-17-2-3" name="sec-17-2-3"></a>

    (column-number-mode t)

### Show matching parenthesis<a id="sec-17-2-4" name="sec-17-2-4"></a>

    (show-paren-mode t)

### Blinking cursor<a id="sec-17-2-5" name="sec-17-2-5"></a>

    (setq blink-cursor-mode t)

### Show empty lines<a id="sec-17-2-6" name="sec-17-2-6"></a>

    (setq indicate-empty-lines t)
