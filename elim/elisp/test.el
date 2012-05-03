;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without warranty of any kind.

(setq eproc (elim-start))
(setq uname (user-login-name))
(elim-add-account eproc (concat uname "-elim@irc.freenode.net") "prpl-irc" nil)
(elim-connect     eproc (concat uname "-elim@irc.freenode.net"))
(elim-disconnect  eproc (concat uname "-elim@irc.freenode.net"))
(elim-join-chat   eproc (concat uname "-elim@irc.freenode.net") "#emacs" 
                  '("channel" "#emacs"))

(elim-join-chat   eproc (concat uname "-elim@irc.freenode.net") "#elim" 
                  '("channel" "#elim"))

;; you'll need to grab conversation UIDs from *Messages*
;; accounts can be identified by either uid or name+protocol
(elim-process-send eproc
  (elim-daemon-call 'message nil
    `(alist nil
       (string ((name . "account-name"))
               ,(concat uname "-elim@irc.freenode.net"))
       (string ((name . "im-protocol" )) "prpl-irc" )
       (int    ((name . "conv-uid"    )) "134985920")
       (string ((name . "text"        )) "wootles"  ))))

(elim-process-send eproc
  (elim-daemon-call 'message nil
    `(alist nil
       (string ((name . "account-name")) 
               ,(concat uname "-elim@irc.freenode.net"))
       (int    ((name . "conv-uid"   )) "135065544")
       (string ((name . "text"       )) 
               ,(read-string "IM> " "" nil "elim-test-string" t)))) )

(elim-process-send eproc
  (elim-daemon-call 'enumerations nil `(alist nil)))

(elim-process-send eproc
  (elim-daemon-call 'enumerations nil 
   `(alist nil (string ((name . "enum-type")) ":connection-error"))) )

(elim-process-send eproc
  (elim-daemon-call 'chat-params nil 
   '(alist nil (string ((name . "im-protocol")) "prpl-irc"))))

(elim-process-send eproc
  (elim-daemon-call 'list-accounts nil nil))

(elim-process-send eproc
  (elim-daemon-call 'list-protocols nil nil))

(elim-process-send eproc
  (elim-daemon-call 'account-options nil
    '(alist nil (string ((name . "im-protocol")) "prpl-jabber"))))

(elim-process-send eproc
  (elim-daemon-call 'account-options nil
    '(alist nil 
            (string ((name . "im-protocol")) "prpl-jabber") 
            (int    ((name . "account-uid")) "134679776"  )) ) )

(elim-process-send eproc
  (elim-daemon-call 'set-account-options nil
    '(alist nil 
            (string ((name . "account-alias")) "fledermaus" )
            (string ((name . "im-protocol"  )) "prpl-jabber") 
            (int    ((name . "account-uid"  )) "134679776"  )) ) )

(elim-fetch-process-data eproc :response-by-id)
(length (elim-fetch-process-data (get-process "*elim*") :accounts))
(setq eproc-blist (elim-fetch-process-data (get-process "*elim*") :blist))
(elim-fetch-process-data (get-process "*elim*") :protocols     )
(elim-fetch-process-data eproc :initialised   )
(assq 'elim-blist-remove-node (elim-fetch-process-data eproc :client-ops))
(elim-fetch-process-data eproc :callbacks     )

(let ((garak-elim-process eproc) (garak-account-uid 134909176))
  (garak-cmd-join "#emacs -")
  )

(assq :proto (cdr (elim-account-data eproc "vivek-elim@irc.freenode.net")))

(message "room-params: %S" (elim-chat-parameters eproc "prpl-irc"))

(process-plist eproc)

(progn
  (mapcar 
   (lambda (B) (when B (kill-buffer B)))
   (list (get-buffer "*elim*")
         (get-buffer "*elim-proto*")))
  (setq eproc nil))

;; !!
(setq eproc (elim-start ))


(elim-get-call-handler-by-name eproc 'elim-blist-update-node)

(elim-parse-proto-args 
 '(alist nil
         (int   ((name . "status")) "0")
         (alist ((name . "value" )) 
                (string ((name . "a-string")) "sashdksaj")
                (bool   ((name . "true"    )) "1"        )
                (bool   ((name . "false"   )) "0"        )
                (float  ((name . "π"       )) "3.1415"   )
                (int    ((name . "mfeat"             )
                         (type . ":message-flags"    )) "16438")
                (int    ((name . "ctype"             )
                         (type . ":conversation-type")) "3"))))


(setq eproc (get-process "*elim*"))
(delete-process eproc)
(elim-load-enum-values (get-process "*elim*"))


(elim-atom-to-proto t)

(elim-process-send
 (get-process "*elim*")
 (elim-daemon-call 'debug-mode nil
                   (elim-simple-list-to-proto-alist '("debug" t))))
