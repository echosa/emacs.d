(load-file "~/.emacs.d/global.el")
;; saffron specific
(setenv "PATH" (mapconcat 'concat
                          (append '("/usr/local/pear/bin"
                                    "/usr/local/bin"
                                    "/Users/echosa/.cask/bin")
                                  (list (getenv "PATH")))
                          ":"))
(set-face-attribute 'default nil :family "Consolas" :height 120)
(when (and (fboundp 'ns-set-resource)
	   (eq window-system 'ns))
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO"))
(server-start)
(load-file "~/quicklisp/slime-helper.el")
(setq slime-net-coding-system 'utf-8-unix)
(setq inferior-lisp-program "sbcl")

(add-to-list 'load-path "~/Dropbox/github/phpplus-mode")
(require 'php+-mode)
(autoload 'php+-mode "php+-mode" nil t)

;; ****
;; php+
;; ****
(defun my-php+-mode-hook ()
  (make-local-variable (quote whitespace-style))
  (setf whitespace-style (quote (face lines-tail tab-mark)))
  (whitespace-mode t)
  (subword-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources (quote ((:source "~/.authinfo.gpg" :host t :protocol nil))))
 '(compilation-error-regexp-alist nil)
 '(exec-path (quote ("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Users/echosa/.carton/bin")))
 '(ispell-program-name "/usr/local/bin/aspell")
 '(ns-alternate-modifier (quote meta))
 '(ns-antialias-text t)
 '(ns-command-modifier (quote super))
 '(ns-control-modifier (quote control))
 '(ns-function-modifier (quote none))
 '(ns-pop-up-frames nil)
 '(ns-use-qd-smoothing nil)
 '(php+-flymake-enable t)
 '(php+-flymake-tests (quote (lint phpcs phpmd)))
 '(php+-mode-css-compile-on-save t)
 '(php+-mode-delete-trailing-whitespace t)
 '(php+-mode-hook (quote (my-php+-mode-hook)))
 '(php+-mode-js-compile-on-save t)
 '(php+-mode-php-compile-on-save nil)
 '(php+-mode-show-project-in-modeline t)
 '(php+-mode-show-trailing-whitespace t)
 '(php-auto-fill t)
 '(php-blank-line-at-end-of-class t)
 '(php-doc-default-author (quote ("Brian Zwahr" . "echosa@gmail.com")))
 '(php-doc-default-php-version "5.4")
 '(php-file-patterns (quote ("\\.php[s34]?\\'" "\\.inc\\'")))
 '(php-format-align-array-double-arrows nil)
 '(php-format-break-all-method-call-arguments nil)
 '(php-format-break-all-method-chain-links t)
 '(php-hide-show-hide-doc-blocks t)
 '(php-hide-show-ignore-extensions (quote (".phtml")))
 '(php-parse-send-to-front (quote (("__construct" nil method public) ("init" nil method public) ("setUp" nil method public) ("tearDown" nil property public))))
 '(php-tag-arguments (quote ("--PHP-kinds=+cf" "--regex-PHP='/abstract class ([^ ]*)//c/'" "--regex-PHP='/interface ([^ ]*)//c/'" "--regex-PHP='/(public |static |abstract |protected |private )+function ([^ (]*)//f/'")))
 '(php-tag-shell-command "/opt/local/bin/ctags")
 '(php-tags-relative t)
 '(php-test-ask-save nil)
 '(php-test-compile-tests (quote (lint phpcs phpmd)))
 '(php-test-file-extensions (quote ("php" "inc" "phtml")))
 '(php-test-show-command nil)
 '(phpcs-shell-command "/usr/local/pear/bin/phpcs")
 '(phpcs-standard "PSR2")
 '(phpmd-rulesets (quote (codesize design naming unusedcode)))
 '(phpmd-shell-command "/usr/local/pear/bin/phpmd")
 '(phpunit-shell-command "/usr/local/pear/bin/phpunit")
 '(send-mail-function (quote sendmail-send-it))
 '(tag-shell-command "/usr/local/bin/ctags")
 '(trash-directory "~/.Trash")
 '(zf-use-hyphens-in-viewscript-urls t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(semantic-decoration-on-fileless-includes ((t (:background "#005000")))))

