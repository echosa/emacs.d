
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(geben-path-mappings
   (quote
    (("~/dev/chess.com" "/www/sites/chess.com/current"))))
 '(package-selected-packages
   (quote
    (solarized-theme browse-at-remote geben php-mode exec-path-from-shell ag lua-mode cider flx-ido ido-vertical-mode evil evil-leader key-chord magit paredit pbcopy projectile))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
