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
