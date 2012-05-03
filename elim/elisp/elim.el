;; Copyright © 2009-2011 Vivek Dasmohapatra

;; email : vivek@etla.org
;; irc   : fledermaus on freenode, oftc
;; jabber: fledermaus@jabber.earth.li

;; This file is part of elim.

;; elim is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; elim is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with elim.  If not, see <http://www.gnu.org/licenses/>.
(require 'cl        )
(require 'browse-url)
(require 'wid-edit  )
(require 'warnings  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables & global state functions:
(defgroup elim nil
  "Multi-protocol IM (Instant Messaging) library"
  :prefix "elim-"
  :group  'external)

(defcustom elim-executable
  (expand-file-name
   (concat
    (file-name-directory
     (or load-file-name buffer-file-name)) "../elim-client"))
  "Location of the elim binary, or one that talks the same protocol"
  :group 'elim
  :type  '(file))

(defcustom elim-sh-file "/bin/sh"
  "Shell to use to run `elim-executable'.
Must support Bourne shell style redirection."
  :group 'elim
  :type  '(file))

(defcustom elim-directory (expand-file-name "~/.emacs.d/elim")
  "elim client (libpurple) working directory.
Buddy lists etc will be stored here"
  :group 'elim
  :type  '(directory))

(defvar elim-enum-alist      nil
  "An alist of (:key (:label0 . value0) (:label1 . value1)) entries describing
known libpurple enumerations.")
(defvar elim-enum-flag-types nil
  "A list of symbols which are keys in `elim-enum-alist' that are ORable
flags rather than simple enumerations.")
(defvar elim-initialising    nil
  "True when elim is still starting up.")

(defvar elim-call-handler-alist
  '(;; account ops
    (elim-account-notify-added  ) ;; noop
    (elim-account-status-changed)
    (elim-account-request-add   ) ;; noop
    (elim-account-request-auth  ) ;; done
    ;; blist (buddy list) ops
    (elim-blist-create-node     ) ;; done
    (elim-blist-remove-node     ) ;; done
    (elim-blist-update-node     ) ;; done
    ;; connection ops
    (elim-connection-state      )
    (elim-connection-progress   )
    (elim-disconnect-reason     )
    ;; network status
    (elim-network-up            )
    (elim-network-down          )
    ;; requests
    (elim-request-fields        ) ;; done
    (elim-request-input         . elim-request-item)
    (elim-request-file          . elim-request-item)
    (elim-request-directory     . elim-request-item)
    (elim-request-action        ) ;; done
    ;; file transfer
    (elim-file-transfer-status  )
    (elim-file-transfer-percent )
    ;; conversation
    (elim-conv-create           )
    (elim-conv-destroy          )
    (elim-conv-write-chat       )
    (elim-conv-write-im         )
    (elim-conv-write-sys        )
    (elim-typing-update         )
    ;; chat
    (elim-chat-add-users        )
    (elim-chat-remove-users     )
    (elim-chat-rename-user      )
    ;; roomlist
    (elim-roomlist-show         )
    (elim-roomlist-create       )
    (elim-roomlist-add          )
    (elim-roomlist-set-field    )
    (elim-roomlist-in-progress  )
    (elim-roomlist-destroy      ))
  "Alist of function call handlers. The car of a given element is the
elim protocol function symbol. The cdr is the handler function, or nil
if the symbol to look for is the same as that of the protocol function.")

(defvar elim-resp-handler-alist
  '((add-account    . elim-response-filter-account-data)
    (add-buddy      . nil)
    (connect        . nil)
    (debug          . nil)
    (enumerations   . elim-enumerations-response)
    (disconnect     . nil)
    (init           . elim-init-response)
    (list-protocols . elim-list-protocols-response)
    (list-accounts  . elim-list-accounts-response )
    (message        . nil)
    (remove-account . elim-remove-account-response))
  "Alist of function response handlers. The car of a given element is the
elim protocol function symbol. The cdr is the handler function, or nil
if we do not intend to wait for the response (this is the usual case).")

(defvar elim-call-id          0 "State variable for generating call ids.")
(defvar elim-proto-buffer   nil "Buffer which the elim process filter uses.")
(defvar elim-process        nil "The elim daemon process.")
(defvar elim-conversations  nil "Alist of conversations keyed by uid.")
(defvar elim-accounts       nil "Alist of IM accounts keyed by UID.")

(defun elim-call-id ()
  (number-to-string (setq elim-call-id (1+ elim-call-id))))

(defun elim-command ()
  (format "%s 2>> %s/elim.$$.stderr" elim-executable elim-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol parsing and formatting:
(defun elim-unpack-enum (etype value)
  (let ((enum-vals (cdr (assq etype elim-enum-alist))) (rval nil))
    (if (memq etype elim-enum-flag-types)
        (mapc
         (lambda (v)
           (when (/= (logand (cdr v) value) 0)
             (setq rval (cons (car v) rval)))) enum-vals)
      (setq rval (or (car (rassq value enum-vals)) value)))
    rval))

(defun elim-pack-enum (etype value)
  (let ((enum-vals (cdr (assq etype elim-enum-alist))) (rval 0))
    (if (memq etype elim-enum-flag-types)
        (mapc
         (lambda (v)
           (setq rval (logior rval (cdr (assq v enum-vals)))))
         (if (listp value) value (list value)))
      (setq rval (or (cdr (assq value enum-vals)) value)))
    rval))

(defun elim-string-to-number (attr thing)
  (let (etype)
    (if (setq etype (intern-soft (cdr (assq 'type attr))))
        (elim-unpack-enum etype (string-to-number thing))
      (string-to-number thing))))

(defun elim-parse-proto-args (arg)
  "Take an elim protocol argument s-expression ARG and convert it into
a straightforward elisp s-expression."
  (let ( (name   nil)
         (parsed nil)
         (type  (car  arg))
         (attr  (cadr arg))
         (value (cddr arg)) )
    (when attr (setq name (cdr (assq 'name attr))))
    (setq parsed
          (cond ((not arg) nil)
                ((eq type 'string) (identity                   (car value)))
                ((eq type 'int   ) (elim-string-to-number attr (car value)))
                ((eq type 'float ) (string-to-number           (car value)))
                ((eq type 'bool  ) (/= 0 (string-to-number    (car value))))
                ((eq type 'data  ) (base64-decode-string       (car value)))
                ((eq type 'list  ) (mapcar  'elim-parse-proto-args   value))
                ((eq type 'alist ) (mapcar  'elim-parse-proto-args   value))
                (t                 (error  "Bad elim arg type : %S"   type))))
    (if name (cons name parsed) parsed)))

(defun elim-entity-to-string (match)
  (format "%c" (string-to-int (match-string 1 match))))

(defun elim-number-to-proto (number &optional attr)
  "Take a NUMBER and return an elim protocol sexp representing it"
  (when (not attr) (setq attr 'nil))
  (let ((val (number-to-string number)))
    (if (integerp number)
        (list 'int attr val)
      (if (string-match "^\\(-?[0-9]+\\)\\(?:.0\\)$" val)
          (list 'int attr (match-string 1 val))
        (list 'float attr val))) ))

(defun elim-data-to-proto (x &optional n)
  (list 'data (if n (list (cons 'name n)) nil)
        (base64-encode-string (string-as-unibyte x)) ))

(defun elim-binp (x)
  (and (stringp x) (string-match "\0" x)))

(defun elim-unprop (s) (set-text-properties 0 (length s) nil s) s)
(defun elim-atom-to-proto (x &optional n)
  "Return an elim protocol sexp representing X (a number, string or t or nil)."
  (let ((attr (if n (list (cons 'name n)) 'nil)))
    (cond ((elim-binp x) (elim-data-to-proto x n))
          ((stringp   x) (list 'string attr (elim-unprop x)))
          ((numberp   x) (elim-number-to-proto       x attr))
          ((booleanp  x) (list  'bool  attr  (if x "1" "0")))
          ((symbolp   x) (list 'string attr (symbol-name x))) )))

(defun elim-atom-to-item (k v)
  "Take a number, t, nil or string V and prepare an elim protocol alist
item envelope with name K."
  (elim-atom-to-proto v k))

(defun elim-sexp-to-item (k v)
  "Take a sexp V already in elim protocol form and wrap it in an elim protocol
alist item envelope with name K."
  (let ((item (copy-sequence v)))
    (nconc (list (car item) (list (cons 'name k))) (cddr item))))

(defun elim-simple-alist-to-proto-alist (alist)
  (let (rval)
    (mapc
     (lambda (cell)
       (setq rval (cons (elim-atom-to-item (car cell) (cdr cell)) rval))) alist)
    (nconc (list 'alist nil) (nreverse rval)) ))

(defun elim-simple-list-to-proto-alist (arg-list)
  "Take ARG-LIST of the form (string-key value string-key value ...)
and return an s-expression suitable for use as the argument in an elim
protocol function call or rsponse.\n
The values can be numbers, t, nil or strings."
  (let (k v rval)
    (while arg-list
      (setq k        (format "%s" (car arg-list))
            v        (cadr arg-list)
            arg-list (cddr arg-list)
            rval     (cons (elim-atom-to-item k v) rval)))
    (nconc (list 'alist nil) (nreverse rval)) ))

(defun elim-daemon-call (name id args)
  "Take a call NAME (symbol), and ID (string or nil, usually nil) and
a protocol arglist ARGS (eg as produced by `elim-simple-list-to-proto-alist'),
and return an s-expression suitable for making a call to an elim daemon."
  (let ( (cid (or id (elim-call-id))) )
    (if args
        (list 'function-call nil (list name (list (cons 'id cid))) args)
      (list 'function-call nil (list name (list (cons 'id cid)))) )))

(defun elim-daemon-response (name id status value)
  (let (named-value type payload)
    (setq type        (car  value)
          payload     (cddr value)
          named-value (nconc (list type (list (cons 'name "value"))) payload))
    ;;(message "PAYLOAD:\n%S" payload)
    (list 'function-response nil (list name (list (cons 'id id)))
          (list 'alist nil
                (list 'int (list (cons 'name "status"))
                      (number-to-string status))
                named-value)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun elim-debug (&rest args)
  (let ((buffer (get-buffer "*elim-debug*")))
    (when (not buffer)
      (buffer-disable-undo (setq buffer (get-buffer-create "*elim-debug*"))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (insert (apply 'format args) "\n\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon i/o loop
(defun elim-handle-sexp (proc sexp)
  ;;(when (eq (caar (cddr sexp)) 'elim-blist-remove-node)
  ;;  (elim-debug "RECEIVED: %S" sexp))
  (when (listp sexp)
    (let ((type (car   sexp))
          (name (caar (cddr sexp)))
          (attr (car  (cdar (cddr sexp))))
          (args (cadr (cddr sexp))) )
      (setq args (elim-parse-proto-args args))
      (elim-debug "received: %S.%S %S" type name args)
      (cond
       ((eq type 'function-call    ) (elim-handle-call proc name attr args))
       ((eq type 'function-response) (elim-handle-resp proc name attr args))
       (t (elim-debug "unknown protocol envelope: %S" type))) )) )

;; if we implement setting up response handlers for individual call instances
;; via elim-callback-alist, this is where we will fetch said handlers back:
(defun elim-set-resp-handler-by-id (proc id handler)
  (elim-update-process-data proc :callbacks id handler))

(defun elim-get-resp-handler-by-id (proc id)
  (let ((store (elim-fetch-process-data proc :callbacks)) slot)
    (when (setq slot (assoc id store))
      (setq store (assq-delete-all (car slot) store))
      (elim-store-process-data proc :callbacks store))
    (cdr slot)))

(defun elim-get-resp-handler-by-name (proc name)
  (cdr (assq name elim-resp-handler-alist)))

(defun elim-get-call-handler-by-name (proc name)
  (let ((slot (assq name elim-call-handler-alist)))
    (or (cdr slot) (car slot)) ))

(defun elim-handle-resp (proc name attr args)
  (let ( (id      (cdr (assq 'id attr)))
         (handler  nil) )
    (setq handler (or (elim-get-resp-handler-by-id   proc id  )
                      (elim-get-resp-handler-by-name proc name)))
    ;; if told to use a handler but it hasn't been defined/loaded/implemented:
    (when (or (not handler) (and handler (not (functionp handler))))
      (setq handler 'elim-default-response-handler))
    ;; finally, we must have a valid handler symbol to proceed:
    (when handler (funcall handler proc name id attr args)) ))

(defun elim-handle-call (proc name attr args)
  (let ( (handler (elim-get-call-handler-by-name proc name))
         (id      (cdr (assq 'id attr))) )
    (if (functionp handler)
        (funcall handler proc name id 0 args)
      (elim-debug "elim-handle-call %s -> client" name)
      (elim-call-client-handler proc name id 0 args)) ))

(defun elim-process-sentinel (process message)
  (let ((status (process-status process)))
    (when (memq status '(exit signal closed failed))
      (elim-call-client-handler process 'elim-exit "-none-" 0 status)
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process)) )) ))

(defun elim-input-filter (process data)
  (let ((pt nil) (sexp nil) (read-error nil) (sexp-list nil))
    (with-current-buffer (process-buffer process)
      (setq pt    (point))
      (goto-char  (point-max))
      (insert      data)
      (goto-char   pt)
      (condition-case read-error
          (progn
            (while (setq sexp (read (current-buffer)))
              (setq sexp-list (cons sexp sexp-list))
              (delete-region pt (point))))
        (error
         (elim-debug "input filter: -- no more sexps remaining --")
         (goto-char pt)))
      (when sexp-list
        (mapc
         (lambda (S) (elim-handle-sexp process S))
         (nreverse sexp-list))) )))

(defun elim-process-send (process sexp-value &optional callback)
  (let ((print-level  nil)
        (print-length nil)
        (sexp-string  nil)
        (attr         nil)
        (call-id      nil))
    (setq sexp-string (prin1-to-string sexp-value))
    (when callback
      (setq attr    (car (cdar (cddr sexp-value)))
            call-id (cdr (assq 'id attr)))
      (elim-set-resp-handler-by-id process call-id callback))
    (process-send-string process sexp-string)
    (elim-debug "sent: %s" sexp-string)
    (accept-process-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fetching and storing per-daemon data:
(defun elim-store-process-data (proc key value)
  (let ((elim-data (process-get proc :elim-data)))
    (setcdr (or (assq key elim-data)
                (car (setq elim-data (cons (cons key nil) elim-data)))) value)
    (process-put proc :elim-data elim-data)))

(defun elim-fetch-process-data (proc key)
  (cdr (assq key (process-get proc :elim-data))))

(defun elim-init-step (proc)
  (elim-store-process-data proc :initialised
                           (1+ (elim-fetch-process-data proc :initialised))))

(defun elim-update-process-data (proc type key value)
  (let ((store (elim-fetch-process-data proc type)) slot)
    (if (setq   slot (assoc key store))
        (setcdr slot value)
      (setq store (cons (cons key value) store))
      (elim-store-process-data proc type store))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon response handlers:
(defun elim-unwrap-resp-args (status raw-args)
  (let (value)
    (when (not status) (setq status (elim-avalue "status" raw-args)))
    (cond ((equal status  0) (elim-avalue "value"   raw-args))
          ((integerp status) (elim-avalue "message" raw-args))
          (t raw-args)) ))

(defun elim-default-fail-handler (proc name id status message)
  (let ((handler (elim-client-handler proc 'error)))
    (if (functionp handler)
        (funcall handler proc name id status message)
      (warn "%s<%s> failed (%S)" name id status message)) ))

(defun elim-client-handler (proc name)
  (cdr (assq name (elim-fetch-process-data proc :client-ops))))

(defun elim-store-response-by-id (proc name id attr args)
  (let ((store (elim-fetch-process-data proc :response-by-id)))
    (setq store (cons (list id name attr args) store))
    (elim-store-process-data proc :response-by-id store)))

(defun elim-fetch-response-by-id (proc id)
  (let ((store (elim-fetch-process-data proc :response-by-id)) slot)
    (when (setq slot (assoc id store))
      (setq store (assq-delete-all (car slot) store))
      (elim-store-process-data proc :response-by-id store))
    slot))

(defun elim-call-client-handler (proc name id status args)
  (let ( (handler (elim-client-handler proc name)) )
    (elim-debug "(elim-call-client-handler %S %S %s ...)" name id status)
    (if (functionp handler)
        (funcall handler proc name id status args)
      (when (and (not (zerop status))
                 (setq handler (elim-client-handler proc 'error)))
        (when (functionp handler)
          (funcall handler proc name id status args)) )) ))

(defun elim-default-response-handler (proc name id attr args)
  (let ((status (elim-avalue "status" args)) call-arg)
    (setq call-arg (elim-unwrap-resp-args status args))
    (if (equal status 0) ;;
        nil 
      (elim-default-fail-handler proc name id status call-arg))
    (elim-call-client-handler proc name id status call-arg) ))

(defun elim-list-protocols-response (proc name id attr args)
  (let ((status (elim-avalue "status" args)) call-arg)
    (setq call-arg (elim-unwrap-resp-args status args))
    (if (not (equal status 0))
        (elim-default-fail-handler proc name id attr args)
      (elim-store-process-data proc :protocols call-arg) )
    (elim-call-client-handler proc name id status call-arg)
    (when elim-initialising (elim-init-step proc)) ))

(defun elim-enum-symbol (name)
  (intern (concat ":" (replace-regexp-in-string "_" "-" (downcase name)))))

(defun elim-enumerations-response (proc name id attr args)
  (when (equal (elim-avalue "status" args) 0)
    (let ((enum-alist (elim-avalue "value" args)) key entry)
      (mapc
       (lambda (E)
         (setq key   (intern (car E))
               entry (mapcar
                      (lambda (F)
                        (cons (elim-enum-symbol (car F)) (cdr F))) (cdr E)))
         (setcdr (or (assq key elim-enum-alist)
                     (car (setq elim-enum-alist
                                (cons (cons key nil) elim-enum-alist)))) entry))
       enum-alist))
    (mapc (lambda (E)
            (when (string-match "-flags$" (symbol-name (car E)))
              (add-to-list 'elim-enum-flag-types (car E)))) elim-enum-alist))
  (when elim-initialising (elim-init-step proc)) )

(defun elim-response-filter-account-data (proc name id attr args)
  (let (uid proto aname store adata (val (elim-avalue "value" args)) slot)
    (when (setq uid (elim-avalue "account-uid" val))
      (setq aname (elim-avalue "account-name" val)
            proto (elim-avalue "im-protocol"  val)
            adata (list (cons :name  aname)
                        (cons :proto proto)))
      (elim-update-process-data proc :accounts uid adata)
      (elim-call-client-handler proc name id 0 val)
      (or adata uid)) ))

(defun elim-remove-account-response (proc name id attr args)
  (let (uid store slot value)
    (when (equal (elim-avalue "status" args) 0)
      (setq value (elim-avalue "value" args)
            store (elim-fetch-process-data proc :accounts)
            uid   (elim-avalue "account-uid" value)
            slot  (assoc uid store))
      (when slot
        (setq store (delq slot store))
        (elim-store-process-data proc :accounts store))
    (elim-call-client-handler proc name id 0 value)) ))

(defun elim-list-accounts-response (proc name id attr args)
  (when (equal (elim-avalue "status" args) 0)
    (let (acl uid data name proto conn (accts (elim-avalue "value" args)))
      (mapc
       (lambda (acct)
         (setq data  (cdr   acct)
               uid   (elim-avalue "account-uid"  data)
               name  (elim-avalue "account-name" data)
               proto (elim-avalue "im-protocol"  data)
               acl   (cons (cons uid (list (cons :name  name )
                                           (cons :proto proto))) acl))) accts)
      (elim-store-process-data proc :accounts acl)))
  (when elim-initialising (elim-init-step proc)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon-to-client call handlers:

(defun elim-blist-update-node (proc name id status args)
  (let ((bnode-uid (elim-avalue "bnode-uid" args)))
    (elim-update-process-data proc :blist bnode-uid args))
  (elim-call-client-handler proc name id status args))

(defun elim-blist-create-node (proc name id status args)
  "Create node calls are unreliable, since they are made before
the node is inserted into the buddy list, so this stubroutine ensures
that this information does not propagate further, either into elim or
into any clients."
  nil)

(defun elim-blist-remove-node (proc name id status args)
  (let ((store     (elim-fetch-process-data proc :blist))
        (bnode-uid (elim-avalue "bnode-uid" args))
        (bnode      nil))
    (when (setq bnode (assoc bnode-uid store))
      (setq store (assq-delete-all (car bnode) store))
      (elim-store-process-data proc :blist store)) )
  ;; remember the data store for this buddy will be _gone_ now
  ;; so the client must rely on args for all buddy related data:
  (elim-call-client-handler proc name id status args))

(defun elim-account-info-cache (proc name id status args type)
  (when (memq type '(:account-status :account-connection))
    (let ((account-uid (elim-avalue "account-uid" args)))
      (elim-update-process-data proc type account-uid args))
    (elim-call-client-handler proc name id status args)))

(defun elim-account-status-changed (proc name id status args)
  (elim-account-info-cache proc name id status args :account-status))

(defun elim-connection-state (proc name id status args)
  (elim-account-info-cache proc name id status args :account-connection))

(defun elim-file-transfer-status (proc name id status args);
  (elim-update-process-data proc :xfers (elim-avalue "xfer-uid" args) args)
  (elim-call-client-handler proc name id status args))

;; elim-file-transfer-percent  - let the default handler fall through to client

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon to client request handlers (requests are calls that require a
;; structured response and UI interaction with the user)
(defvar elim-form-ui-data nil)
(defvar elim-form-ui-args nil)

(defun elim-form-proto-values (widgets)
  (let (value slot alist-cache iname ivalue)
    (mapc
     (lambda (W)
       (setq iname  (widget-get W 'ident)
             ivalue (widget-value W))
       (when iname
         (if (consp iname)
             (if (setq slot (assoc (car iname) alist-cache))
                 (setcdr slot (cons (elim-atom-to-item (cdr iname) ivalue)
                                    (cdr slot)))
               (setq alist-cache
                     (cons (cons (car iname)
                                 (list (elim-atom-to-item (cdr iname) ivalue)))
                           alist-cache)) )
           (setq value (cons (elim-atom-to-item iname ivalue) value))) ))
     widgets)
    (mapc
     (lambda (A)
       (setq value
             (cons
              (nconc (list 'alist (list (cons 'name (car A)))) (cdr A)) value)))
     alist-cache)
    value))

(defun elim-form-ui-ok-cb (&optional parent child event &rest stuff)
  (when elim-form-ui-args
    (let (value iname ivalue proc name id args response)
      (setq proc (cadr (memq :process elim-form-ui-args))
            name (cadr (memq :name    elim-form-ui-args))
            id   (cadr (memq :call-id elim-form-ui-args)))
      (cond
       ((eq 'elim-request-fields name)
        (setq value    (elim-form-proto-values elim-form-ui-data)
              response (elim-daemon-response name id 0
                                             (nconc (list 'alist nil) value))))
       (t
        (setq value    (car (elim-form-proto-values elim-form-ui-data))
              response (elim-daemon-response name id 0 value))))
      (elim-process-send proc response))
    (kill-buffer nil) ))

(defun elim-form-ui-nok-cb (&optional parent child event &rest stuff)
  (when elim-form-ui-args
    (let (proc name id response)
      (setq proc (cadr (memq :process elim-form-ui-args))
            name (cadr (memq :name    elim-form-ui-args))
            id   (cadr (memq :call-id elim-form-ui-args)))
      (setq response (elim-daemon-response name id -1 (list 'alist nil)))
      (elim-process-send proc response))
    (kill-buffer nil) ))

(defun elim-form-ui-handle-event (&optional parent child event &rest stuff)
  (widget-put parent :value (widget-value parent)))

(defun elim-form-widget-create (type ident &rest args)
  "Create an emacs widget, and record it in `elim-form-ui-data'."
  (let ((widget nil))
    (setq widget            (apply 'widget-create type args)
          elim-form-ui-data (cons widget elim-form-ui-data))
    (widget-put widget 'ident ident)
    widget))

(defun elim-request-field-string (id data)
  (let (r-type w-type)
    (setq r-type (elim-avalue "r-type" data)
          w-type (cond ((eq r-type 'elim-request-file     ) 'file     )
                       ((eq r-type 'elim-request-directory) 'directory)
                       (t                                   'string   )))
    (elim-form-widget-create w-type
                             id
                             :notify 'elim-form-ui-handle-event
                             :tag    (or (elim-avalue "label" data) id)
                             :size   20
                             :secret (and (elim-avalue "masked" data) ?*)
                             (or (elim-avalue "value"   data)
                                 (elim-avalue "default" data) "")) ))

(defun elim-request-field-choice (id data)
  (let ((idx 0) (item nil))
    (apply 'elim-form-widget-create
           'choice
           id
           :notify 'elim-form-ui-handle-event
           :tag    (or (elim-avalue "label"   data) id)
           :value  (or (elim-avalue "value"   data)
                       (elim-avalue "default" data)  0)
           (mapcar
            (lambda (o)
              (setq item (list 'const :tag o idx) idx (1+ idx)) item)
            (elim-avalue "choices" data))) ))

(defun elim-request-field-toggle (id data)
  (let ((idx 0) (item nil))
    (elim-form-widget-create 'boolean
                             id
                             :notify 'elim-form-ui-handle-event
                             :tag    (or (elim-avalue "label" data) id)
                             (or (elim-avalue "value"   data)
                                 (elim-avalue "default" data) nil)) ))

(defun elim-request-field-number (id data)
  (elim-form-widget-create 'integer
                           id
                           :notify 'elim-form-ui-handle-event
                           :tag    (or (elim-avalue "label" data) id)
                           (or (elim-avalue "value"   data)
                               (elim-avalue "default" data) 0)))

(defun elim-request-field (field)
  (let ((id   (car field))
        (data (cdr field)) type)
    (setq type (elim-avalue "type" data))
    (cond ((eq type :string ) (elim-request-field-string id data))
          ((eq type :boolean) (elim-request-field-toggle id data))
          ((eq type :integer) (elim-request-field-number id data))
          ((eq type :int    ) (elim-request-field-number id data))
          ((eq type :choice ) (elim-request-field-choice id data))
          (t (widget-insert (format "Unsupported type: %S %s" id type))))
    (widget-insert "\n")))

(defun elim-request-field-group (group)
  (let ((label  (car group))
        (fields (cdr group)))
    (widget-insert label ":\n")
    (mapc 'elim-request-field fields)
    (widget-insert "====================================================\n")))

(defun elim-init-ui-buffer ()
  (kill-all-local-variables)
  (let ((inhibit-read-only t)) (erase-buffer))
  (remove-overlays)
  (make-local-variable 'elim-form-ui-data)
  (make-local-variable 'elim-form-ui-args))

(defun elim-request-item (proc name id status args)
  (let (field-buf buf-name ok-label nok-label prompt secret default)
    (setq prompt    (concat (or (elim-avalue "title"        args)
                                (elim-avalue "primary"      args)
                                (elim-avalue "who"          args)
                                (elim-avalue "account-name" args) "Input:"))
          default   (elim-avalue "default" args)
          secret    (elim-avalue "secret"  args)
          buf-name  (concat "* " prompt " *")
          field-buf (generate-new-buffer buf-name))
    (cond ((eq name 'elim-request-file)
           (if (elim-avalue "savep" args)
               (setq prompt (concat prompt "\nSave File: "))
             (setq prompt (concat prompt "\nChoose File: "))) ))
    (with-current-buffer field-buf
      (elim-init-ui-buffer)
      (setq elim-form-ui-args (list :process proc
                                    :name    name
                                    :call-id id  ))
      (widget-insert (concat (or (elim-avalue "primary" args)
                                 (elim-avalue "title"   args)) "\n"
                             (elim-avalue "secondary" args) "\n"))
      ;; actually create the widget:
      (cond ((memq name '(elim-request-input
                          elim-request-file
                          elim-request-directory))
             (elim-request-field-string "value"
                                        (list (cons "r-type"  name   )
                                              (cons "default" default)
                                              (cons "label"   prompt )
                                              (cons "masked"  secret ))))
            (t (error "request type %S not implemented" name)))
      (setq ok-label  (or (elim-avalue "ok-label"  args) "Ok"    )
            nok-label (or (elim-avalue "nok-label" args) "Cancel"))
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" nok-label)
                               :notify 'elim-form-ui-nok-cb)
      (widget-insert " ")
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" ok-label)
                               :notify 'elim-form-ui-ok-cb)
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))
    (display-buffer field-buf)))

(defun elim-request-fields (proc name id status args)
  (let (fields field-buf buf-name ok-label nok-label)
    (setq fields    (elim-avalue "fields" args)
          buf-name  (concat "* " (or (elim-avalue "title"        args)
                                     (elim-avalue "primary"      args)
                                     (elim-avalue "who"          args)
                                     (elim-avalue "account-name" args)
                                     "Information Required:"         ) " *")
          field-buf (generate-new-buffer buf-name))
    (with-current-buffer field-buf
      (elim-init-ui-buffer)
      (setq elim-form-ui-args (list :process proc
                                    :name    name
                                    :call-id id  ))
      (widget-insert (concat (or (elim-avalue "primary" args)
                                 (elim-avalue "title"   args))
                             "\n" (elim-avalue "secondary" args)))
      ;; actually create the widget:
      (mapc 'elim-request-field-group fields)
      (setq ok-label  (elim-avalue "ok-label"  args)
            nok-label (elim-avalue "nok-label" args))
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" nok-label)
                               :notify 'elim-form-ui-nok-cb)
      (widget-insert " ")
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" ok-label)
                               :notify 'elim-form-ui-ok-cb)
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))
    (display-buffer field-buf)))

(defun elim-request-action (proc name id status args)
  (let (action-buf buf-name actions default)
    (setq actions    (elim-avalue "actions" args)
          buf-name   (concat "* " (or (elim-avalue "title"   args)
                                      (elim-avalue "primary" args)
                                      (elim-avalue "who"     args)
                                      "Choose an Action"         ) " *")
          action-buf (generate-new-buffer buf-name)
          default    (cdr (nth (or (elim-avalue "default" args) 0) actions)))
    (with-current-buffer action-buf
      (elim-init-ui-buffer)
      (setq elim-form-ui-args (list :process proc
                                    :name    name
                                    :call-id id  ))
      (widget-insert (concat (or (elim-avalue "primary" args)
                                 (elim-avalue "title"   args))
                             "\n" (elim-avalue "secondary" args) "\n"))
      (apply 'elim-form-widget-create
             'choice
             "value"
             :format "%t: %[%v%]\n"
             :notify 'elim-form-ui-handle-event
             :tag    "Action"
             :value  default
             (mapcar
              (lambda (A)
                (list 'choice-item :format "%[%t%]" :tag (car A) (cdr A)))
              actions))
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" "Cancel")
                               :notify 'elim-form-ui-nok-cb)
      (widget-insert " ")
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" "Ok")
                               :notify 'elim-form-ui-ok-cb)
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))
    (display-buffer action-buf)))

(defun elim-account-request-auth (proc name id status args)
  (let (auth-buf buf-name question)
    (setq question  (concat "Authorise: "
                            (or (elim-avalue "message" args)
                                (elim-avalue "user"    args)
                                (elim-avalue "id"      args)
                                (elim-avalue "alias"   args)))
          buf-name  (format "* %s *" question)
          auth-buf  (generate-new-buffer buf-name))
    (with-current-buffer auth-buf
      (elim-init-ui-buffer)
      (setq elim-form-ui-args (list :process proc
                                    :name    name
                                    :call-id id  ))
      (elim-form-widget-create 'boolean
                               "value"
                               :notify 'elim-form-ui-handle-event
                               :tag     question
                               nil)
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" "Cancel")
                               :notify 'elim-form-ui-nok-cb)
      (widget-insert " ")
      (elim-form-widget-create 'push-button
                               nil
                               :format (format "[%%[%s%%]]" "Ok")
                               :notify 'elim-form-ui-ok-cb)
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))
    (display-buffer auth-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; daemon calls not intended for direct client use:
(defun elim-init (process &optional ui-string user-dir)
  "Initialises the elim daemon. Not normally called by the user,
as it relies on initialisation done by `elim-start'."
  (let ((init-call  nil)
        (dummy      nil)
        (proto-call nil)
        (uiid     (or ui-string "elim"))
        (dir      (or user-dir  elim-directory))
        (arglist    nil))
    (setq arglist    (list         "dot-dir" dir "ui-id" uiid)
          arglist    (elim-simple-list-to-proto-alist arglist)
          proto-call (elim-daemon-call  'init   nil   arglist))
    (elim-process-send process proto-call) ))

(defun elim-load-enum-values (process)
  (elim-process-send process
    (elim-daemon-call 'enumerations nil '(alist nil)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-daemon functions intended for the user, and general utilities
(defun elim-protocol-alist (process)
  "Fetches the cached alist of im protocol ids and names supported by PROCESS."
  (elim-fetch-process-data process :protocols))

(defun elim-account-alist (process)
  (elim-fetch-process-data process :accounts))

(defun elim-protocol-supported (process protocol)
  "Returns nil if PROTOCOL is not in the cache of supported im protocols,
or a cons of \(\"protocol-id\" . \"protocol-name\") if it is."
  (or (assoc  protocol                 (elim-protocol-alist process))
      (assoc (concat "prpl-" protocol) (elim-protocol-alist process))))

(defun elim-avalue (key alist) (cdr (assoc key alist)))

(defun elim-assoc  (key alist &optional test test-cons)
  "Look up KEY in ALIST using TEST as a perdicate. If TEST-CONS is set,
the predicate receives the whole cons cell as its argument, not just the key."
  (let ((slot nil) (pred (or test 'equal)))
    (mapc
     (lambda (C)
       (when (and (not slot)
                  (ignore-errors (funcall pred key (if test-cons C (car C)))))
         (setq slot C))) alist) slot))

(defun elim-get-buddies (process &optional filters item)
  "Returns a list of blist-node uids (or other buddy list node ITEM),
optionally filtering the list according to FILTERS.
FILTERS should be nil or an alist of the form:\n
  '((\"bnode-type\"  . :buddy-node)
    (\"account-uid\" . 136142592  ))\n
Where each key is a key from a blist-node entry (a string), and each value is 
a value to compare the item from each blist node with.
string values from the blist-node are compared with string-match,
symbols are compared with eq, and other values are compared with equal.
\(The type of the value in the bnode controls this, not the type in the filter).
\nIf ITEM is set, it should be a key from the blist node.\n
The return value is either a list of buddy uids whose data matches the FILTERS
\(a FILTERS of nil matches everything) or, if ITEM is set, the values from the 
corresponding key in each matching buddy node.\n
Examples:\n
  (elim-get-buddies proc '((\"bnode-type\"  . :buddy-node)) \"bnode-name\")
  (elim-get-buddies proc '((\"account-uid\" . 136142592)))"
  (delete 'ignore
          (mapcar
           (lambda (b)
             (let ((id (car b)) (data (cdr b)) (f filters) key val filter test)
               (while (and id f)
                 (setq key    (caar f)
                       filter (cdar f)
                       f      (cdr  f)
                       val    (elim-avalue key data)
                       test   (cond ((stringp val) 'string-match)
                                    ((symbolp val) 'eq          )
                                    (t             'equal       ))
                       id     (when (funcall test filter val) id)))
               (if id (if item (elim-avalue item data) id) 'ignore)))
           (elim-fetch-process-data process :blist)) ))

(defun elim-buddy-data (process buddy &optional account)
  "Given an ACCOUNT (an account name string or account uid number), and
BUDDY (name or uid) return its data in the form (uid (:key . value) ...).
ACCOUNT need not be supplied if BUDDY is a uid"
  (let (adata auid blist bdata s)
    (setq blist (elim-fetch-process-data process :blist))
    (cond
     ((numberp buddy) (assoc buddy blist))
     ((stringp buddy)
      (setq adata (elim-account-data process account)
            auid  (car adata))
      (elim-assoc buddy blist
                  (lambda (K C)
                    (setq s (cdr C))
                    (and (eql   auid (elim-avalue "account-uid" s))
                         (equal K    (elim-avalue "bnode-name"  s)))) t) )) ))

(defun elim-account-info (process account type)
  "Given an ACCOUNT (an account uid number or name string), return the last
status or connection update for it (the arg passed to
`elim-account-status-changed' or `elim-connection-state' respectively)
TYPE should be :account-status or :account-connection"
  (when (memq type '(:account-status :account-connection))
    (let ((accounts (elim-fetch-process-data process type)))
      (cond ((numberp account) (elim-assoc account accounts '=))
            ((stringp account)
             (elim-assoc account accounts
                         (lambda (K C)
                           (equal K (cdr (assoc "account-name" (cdr C))))) t))
            (t (elim-debug "account %S not found" account) nil)) )))

(defun elim-account-status (process account)
  (elim-account-info process account :account-status))

(defun elim-account-connection (process account)
  (elim-account-info process account :account-connection))

(defun elim-account-data (process account)
  "Given an ACCOUNT (an account name string or account uid number), return
its data in the form (uid (:key . value) ...). :key items should include
:name and :proto, but others may also be present."
  (let ((accounts (elim-fetch-process-data process :accounts)))
    (cond ((numberp account) (elim-assoc account accounts '=))
          ((stringp account)
           (elim-assoc account accounts
                       (lambda (K C)
                         (equal K (cdr (assq :name (cdr C))))) t))
           (t (elim-debug "account %S not found" account)     nil)) ))

(defun elim-account-proto-items (process account)
  (let ((adata (elim-account-data process account)) (arglist nil))
    (when adata
      (list
       (elim-atom-to-item "account-uid"  (car adata))
       (elim-atom-to-item "account-name" (cdr (assq :name  (cdr adata))))
       (elim-atom-to-item "im-protocol"  (cdr (assq :proto (cdr adata))))) )))

(defun elim-buddy-list (process)
  (elim-fetch-process-data process :blist))

(defun elim-buddy-children (process node)
  (let (uid child children seen cuid parent)
    (if (consp node)
        (setq parent node uid (car parent))
      (setq uid node parent (elim-buddy process uid)))
    (setq cuid (elim-avalue "bnode-child" parent) seen (list uid))
    (mapc (lambda (child)
            (when (and (eql (elim-avalue "bnode-parent" child) uid)
                       (setq cuid  (car child))
                       (not (member cuid seen)))
              (setq seen     (cons cuid seen)
                    children (cons child children)) ))
          (elim-fetch-process-data process :blist))
    children))

(defun elim-buddy (process uid)
  (elim-avalue uid (elim-fetch-process-data process :blist)))

(defun elim-action-click (e &optional p)
  (interactive "e")
  (if e (setq p (cadr (event-start e))))
  (let ((type (get-text-property p :action)))
    (cond ((eq type :url) (browse-url (get-text-property p :url)))
          (t nil)) ))

(defvar elim-action-button-map (make-sparse-keymap))
(define-key elim-action-button-map (kbd "RET"      ) 'elim-action-click)
(define-key elim-action-button-map (kbd "<mouse-2>") 'elim-action-click)

(defun elim-add-face (object face &optional start end)
  (let (fprop a b)
    (or start (setq start 0))
    (or end   (setq end (if (bufferp object)
                            (1+ (buffer-size object))
                          (length object))))
    (setq b start a start)
    (while (< b end)
      (setq fprop (get-text-property a 'face object)
            fprop (if (and fprop (listp fprop)) (cons face fprop)
                    (if fprop (list face fprop) face))
            b     (next-single-char-property-change a 'face object end))
      (add-text-properties a b (list 'face fprop) object)
      (setq a b)) object))

(defun elim-buttonised-url (str)
  (let ((url  (match-string 1 str))
        (link (match-string 2 str)))
    (propertize link
                :action     :url
                :url        url
                'fontified  t
                'mouse-face 'bold
                'local-map  elim-action-button-map)))

(defun elim-italicised-text (str)
  (elim-add-face (match-string 1 str) 'italic))

(defun elim-emboldened-text (str)
  (elim-add-face (match-string 1 str) 'bold))


(defun elim-interpret-markup (text)
  (let ((case-fold-search t))
    (setq text (replace-regexp-in-string
                "<a.+?href=\"\\(.*?\\)\".*?>\\(\\(?:.\\|\n\\)*?\\)</a>"
                'elim-buttonised-url text)
          text (replace-regexp-in-string "<I>\\(\\(?:.\\|\n\\)+?\\)</I>"
                                         'elim-italicised-text text)
          text (replace-regexp-in-string "<B>\\(\\(?:.\\|\n\\)+?\\)</B>"
                                         'elim-emboldened-text text)
          text (replace-regexp-in-string "<BR\\s-*/>" "\n" text))
    text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elim daemon calls:

(defun elim-start (&optional ui-string user-dir client-ops)
  "Starts up an elim daemon with the standard location and command, and
initialises it (setq up the elim user directory, loads the list of
supported protocols asynchronously). Returns the elim object (a process)
which is the first parameter for most calls intended for client libraries
to use. \n
UI-STRING is a parameter (a string) which libpurple uses to identify
the libpurple instance internally. It will be initialised to something
suitable if you do not supply it (or set it to nil).\n
USER-DIR is the directory in which more-or-less permanent user data
\(buddy lists, account details and so forth) will be stored. It will also
be initialised to the value of `elim-directory' if you do not supply it."
  (let ( (buf (generate-new-buffer "*elim*"))
         (process-connection-type  nil)
         (elim-initialising          t)
         (shell-file-name elim-sh-file)
         (elim                     nil) )
    ;; set up the process, io buffer, input filters etc:
    (make-directory (or user-dir elim-directory) t)
    (setq elim (start-process-shell-command
                (buffer-name buf) buf (elim-command)))
    (elim-store-process-data elim :client-ops      client-ops)
    (elim-store-process-data elim :initialised     0)
    (set-process-filter      elim 'elim-input-filter)
    (set-process-sentinel    elim 'elim-process-sentinel)
    ;; make the calls necessary to initialise the daemon state
    ;; and prepare for handling IM related calls:
    (elim-load-enum-values   elim) ;; Enumeration definitions
    (while (and (eq (process-status elim) 'run)
                (< (elim-fetch-process-data elim :initialised) 1))
      (accept-process-output))
    (elim-init elim ui-string user-dir)
    (elim-update-protocol-list elim) ;; IM protocols supported
    (elim-update-account-list  elim) ;; User's accounts
    ;; wait for the above three calls to finish:
    (while (and (eq (process-status elim) 'run)
                (<  (elim-fetch-process-data elim :initialised) 3))
      (accept-process-output))
    elim))

(defun elim-get-prefs (process &optional callback)
  "Fetch the preference list for the IM daemon."
  (elim-process-send process (elim-daemon-call 'get-prefs nil nil) callback))

(defun elim-set-prefs (process prefs-alist &optional callback)
  (let (arglist)
    (setq arglist
          (elim-sexp-to-item "prefs"
                             (elim-simple-alist-to-proto-alist prefs-alist))
          arglist (nconc (list 'alist nil) (list arglist)))
    (elim-process-send process
                       (elim-daemon-call 'set-prefs nil arglist) callback) ))

(defun elim-update-account-list (process)
  "Update (asynchronously) the IM account list cache."
  (elim-process-send process (elim-daemon-call 'list-accounts nil nil)))

(defun elim-update-protocol-list (process)
  "Updates (asynchronously) the alist of im protocol id's and names supported
by PROCESS. Not normally useful to the user."
  (elim-process-send process (elim-daemon-call 'list-protocols nil nil)))

(defun elim-add-account (process account protocol password &optional options)
  "Given an elim PROCESS, add the account defined by
ACCOUNT (string, eg \"foo@irc.freenode.net\")
PROTOCOL (eg \"prpl-irc\")
PASSWORD (a string or nil)
and an optional list of strings:
OPTIONS (\"key\" \"value\" ...)."
  (if (elim-protocol-supported process protocol)
      (let (arglist optitem)
        (setq optitem (when options (elim-simple-list-to-proto-alist options))
              optitem (when optitem (elim-sexp-to-item "options" optitem))
              arglist (list "account-name" account
                            "im-protocol"  protocol
                            "password"     password)
              arglist (elim-simple-list-to-proto-alist arglist))
        (when options (setq arglist (nconc arglist (list optitem))))
        (elim-process-send process (elim-daemon-call 'add-account nil arglist))
        t)
    (error "Unsupported IM protocol: %s" protocol)))

(defun elim-remove-buddy (process account buddy)
  (let (bdata arglist bname dummy buid)
    (setq bdata   (elim-buddy-data process buddy account)
          buid    (car bdata)
          account (or account (elim-avalue "account-uid" (cdr bdata)))
          arglist (elim-account-proto-items process account)
          arglist (nconc (list 'alist nil
                               (elim-atom-to-item "bnode-uid" buid))
                         arglist))
    (elim-process-send process
                       (elim-daemon-call 'remove-buddy nil arglist)) ))

(defun elim-send-file (process account buddy &optional file callback)
  (let (bdata arglist)
    (setq bdata   (elim-buddy-data process buddy account)
          buddy   (or (elim-avalue "bnode-name" bdata) buddy)
          account (or account (elim-avalue "account-uid" (cdr bdata)))
          arglist (elim-account-proto-items process account)
          arglist (nconc (list 'alist nil
                               (elim-atom-to-item "recipient" buddy)
                               (elim-atom-to-item "filename"  file ))
                         arglist))
    (elim-process-send process
                       (elim-daemon-call 'send-file nil arglist)) ))

(defun elim-add-buddy (process account buddy &optional group)
  "Given an elim PROCESS an ACCOUNT name or uid and a BUDDY (im screen name),
add that user to your buddy list"
  (let ((arglist (elim-account-proto-items process account)))
    (elim-debug "ELIM-ADD-BUDDY( PROC %S %S %S )" account buddy group)
    (if arglist
        (progn
          (setq group   (or group "Buddies")
                arglist (nconc (list 'alist nil
                                     (elim-atom-to-item "bnode-name" buddy)
                                     (elim-atom-to-item "group"      group))
                               arglist))
          (elim-process-send process (elim-daemon-call 'add-buddy nil arglist)))
      (error "No such account: %s" account)) ))

(defun elim-leave-conversation (process conv-uid)
  (let (arglist)
    (when (numberp conv-uid)
      (setq arglist (list 'alist nil (elim-atom-to-item "conv-uid" conv-uid)))
      (elim-process-send process
                         (elim-daemon-call 'end-conversation nil arglist)) )))

(defun elim-account-op (process account operation)
  "Perform an OPERATION (symbol) on ACCOUNT (name or uid) which needs no other
parameters. Used by `elim-disconnect', `elim-connect' and similar functions."
  (let ((arglist (elim-account-proto-items process account)))
    (when arglist
      (setq arglist (nconc (list 'alist nil) arglist))
      (elim-process-send process (elim-daemon-call operation nil arglist)) )))

(defun elim-disconnect (process account)
  "Disconnect from ACCOUNT (a uid (number) or string (account name))."
  (elim-account-op process account 'disconnect))

(defun elim-connect (process account)
  "Connect to ACCOUNT (a uid (number) or string (account name))."
  (elim-account-op process account 'connect))

(defun elim-register (process account)
  "Register a new ACCOUNT (a uid (number) or string (account name))."
  (elim-account-op process account 'register))

(defun elim-unregister (process account)
  "Unregister (delete from server) ACCOUNT (a uid or string (account name))."
  (display-warning
   'elim
   (format "I'm sorry %s, I can't do that.\n
libpurple unregister support is too flaky right now." user-login-name)
   :warning) )

(defun elim-remove-account (process account)
  "Remove ACCOUNT (a uid or string (account name)) from elim's account list."
  (elim-account-op process account 'remove-account))

(defun elim-do-cmd  (process account conversation cmd)
  "Execute a CONVERSATION (name or uid) command CMD (a string, excluding the
/ prefix) as implemented by libpurple, on ACCOUNT (name or uid)."
  (let (acct-args arglist conv-arg text-arg)
    (setq acct-args (elim-account-proto-items process account)
          conv-arg  (cond ((numberp conversation)
                           (elim-atom-to-item "conv-uid"  conversation))
                          ((stringp conversation)
                           (elim-atom-to-item "conv-name" conversation)))
          text-arg  (elim-atom-to-item "command" cmd)
          arglist   (nconc (list 'alist nil conv-arg text-arg) acct-args))
    (if acct-args
        (elim-process-send process
                           (elim-daemon-call 'command nil arglist))
      (error "No such account: %S" account)) ))

(defun elim-message (process account conversation text)
  "Send a TEXT to ACCOUNT (name or uid) CONVERSATION (uid or name)
via elim PROCESS. If CONVERSATION is a name then a new conversation
may be started if none by that name exists."
  (let (acct-args arglist conv-arg text-arg)
    (setq acct-args (elim-account-proto-items  process account)
          conv-arg  (cond ((numberp conversation)
                           (elim-atom-to-item "conv-uid"  conversation))
                          ((stringp conversation)
                           (elim-atom-to-item "conv-name" conversation)))
          text-arg  (elim-atom-to-item "text" text)
          arglist   (nconc (list 'alist nil conv-arg text-arg) acct-args))
    (if acct-args
        (elim-process-send process
                           (elim-daemon-call 'message nil arglist))
        (error "No such account: %S" account)) ))

(defun elim-alias-bnode (process buddy alias &optional account)
  (message "(elim-alias-bnode %S %S %S %S)" 
           process buddy alias account)
  (let (acct-args bdata arglist buid)
    (when account (setq acct-args (elim-account-proto-items process account)))
    (setq bdata   (elim-buddy-data process buddy account)
          buid    (car bdata)
          arglist (nconc (list 'alist nil
                               (elim-atom-to-item "bnode-uid" buid)
                               (elim-atom-to-item "alias"    alias))
                         acct-args))
    (elim-process-send process
                       (elim-daemon-call 'alias-bnode nil arglist)) ))

(defun elim-set-account-icon (process account &optional icon)
  (let (acct-args icon-file arglist)
    (setq acct-args (elim-account-proto-items process account)
          icon-file (elim-atom-to-item "icon-file" icon)
          arglist   (nconc (list 'alist nil icon-file) acct-args))
    (elim-process-send process (elim-daemon-call 'set-icon nil arglist)) ))

(defun elim-toggle-user-blocked (process buddy &optional account)
  (let (acct-data buddy-data auid buid call arglist)
    (setq acct-data  (elim-account-data process account)
          auid       (car acct-data)
          buddy-data (elim-buddy-data process buddy auid)
          auid       (elim-avalue "account-uid" (cdr buddy-data))
          buid       (car buddy-data)
          arglist    (cond (buid (list "bnode-uid"   buid))
                           (auid (list "account-uid" auid "user-name" buddy)))
          arglist    (elim-simple-list-to-proto-alist arglist)
          call       (elim-daemon-call 'buddy-privacy nil arglist))
    (elim-process-send process call) ))

(defun elim-buddy-info (process account buddy)
  (let (acct-data buddy-data auid buid call arglist)
    (setq acct-data  (elim-account-data process account))
    (when acct-data
      (setq auid       (car acct-data)
            buddy-data (elim-buddy-data process buddy auid))
      (when buddy-data
        (setq buid    (car buddy-data)
              arglist (elim-simple-list-to-proto-alist (list "bnode-uid" buid))
              call (elim-daemon-call 'buddy-info nil arglist))
        (elim-process-send process call))) ))

(defun elim-image (process id &optional cb)
  (let ((arglist (elim-simple-list-to-proto-alist (list "image-id" id))))
    (elim-process-send process (elim-daemon-call 'image nil arglist) cb) ))

(defun elim-join-chat-parse-chat-parameter (thing)
  (let (name)
    (when (setq name (cond ((numberp thing) "bnode-uid" )
                           ((stringp thing) "chat-alias")))
      (elim-atom-to-item name thing)) ))

(defun elim-join-chat (process account chat &optional options)
  "Join CHAT (a name or buddy uid) on ACCOUNT (uid or name) via elim PROCESS,
if ACCOUNT is a name, then OPTIONS (a flat list of \"key\" \"value\" pairs)
must also be supplied."
  (let (chat-arg optitems acct-args arglist)
    (when options
      (setq optitems  (elim-simple-list-to-proto-alist  options )
            optitems  (elim-sexp-to-item "chat-options" optitems)
            optitems  (list optitems)))
    (setq acct-args (elim-account-proto-items process account )
          chat-arg  (elim-join-chat-parse-chat-parameter  chat)
          arglist   (nconc (list 'alist nil chat-arg) acct-args optitems))
    (elim-process-send process
                       (elim-daemon-call 'join-chat nil arglist)) ))

(defun elim-list-chats (process account)
  (let ((account-data (elim-account-data process account)) arglist)
    (if account-data
        (setq account (car account-data))
      (error "No such account: %S" account))
    (setq arglist (list 'alist nil (elim-atom-to-item "account-uid" account)))
    (elim-process-send process (elim-daemon-call 'list-chats nil arglist))))

(defun elim-account-options (process account &optional callback)
  (let ((account-data (elim-account-data process account)) arglist)
    (if account-data
        (setq account (car account-data))
      (error "No such account: %S" account))
    (setq arglist (list 'alist nil (elim-atom-to-item "account-uid" account)))
    (elim-process-send
     process (elim-daemon-call 'account-options nil arglist) callback)))

(defun elim-buddy-menu (process buddy &optional callback)
  (let ((buddy-data (elim-buddy-data process buddy)) arglist sexp)
    (setq arglist (elim-simple-list-to-proto-alist
                   (list "bnode-uid" (elim-avalue "bnode-uid" buddy-data)))
          sexp    (elim-daemon-call 'buddy-menu nil arglist))
    (elim-process-send process sexp callback) ))

(defun elim-account-menu (process account &optional callback)
  (let ((acct-data (elim-account-data process account)) arglist sexp)
    (elim-debug "elim-account-menu: %S" acct-data)
    (setq arglist (elim-simple-list-to-proto-alist
                   (list "account-uid" (car acct-data)))
          sexp    (elim-daemon-call 'account-menu nil arglist))
    (elim-process-send process sexp callback) ))

(defvar elim-standard-status-types
  '(("available"      . :available  )
    ("away"           . :away       )
    ("do-not-disturb" . :unavailable)
    ("unavailable"    . :unavailable)
    ("invisible"      . :invisible  )
    ("offline"        . :offline    )))

(defun elim-standard-status-type (id)
  (let (type)
    (when (setq type (elim-avalue id elim-standard-status-types))
      (setq type (elim-pack-enum :status-primitive type)))
    type))

(defun elim-set-status (process status-id &optional message type)
  (let ((args (list "status-id" status-id)) stype)
    (when message (setq args (cons "status-message" (cons message args))))
    (when (setq stype (elim-standard-status-type status-id))
      (setq type stype))
    (when type    (setq args (cons "status-type"    (cons type    args))))
    (setq args (elim-simple-list-to-proto-alist args))
    (elim-process-send process (elim-daemon-call 'status nil args)) ))

(defun elim-chat-parameters (process protocol)
  "Given an elim PROCESS and a PROTOCOL id, return an alist giving details
on the parameters required to join a chat/room/channel on that PROTOCOL."
  (when (elim-protocol-supported process protocol)
    (let ((id       (elim-call-id))
          (response nil)
          (args     nil)
          (arglist  (list 'alist nil
                          (elim-atom-to-item "im-protocol" protocol))))
      (elim-set-resp-handler-by-id process id 'elim-store-response-by-id)
      (elim-process-send process
                         (elim-daemon-call 'chat-params id arglist))
      (while (not (setq response (elim-fetch-response-by-id process id)))
        (accept-process-output))
      (setq args (nth 3 response))
      (if (equal (elim-avalue "status" args) 0)
        (elim-avalue "parameters" (elim-avalue "value" args)) nil) )))

(provide 'elim)
