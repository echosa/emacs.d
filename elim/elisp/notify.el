;; Copyright © 2010 Vivek Dasmohapatra 

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
(require 'dbus-util)

(defconst notify-service   "org.freedesktop.Notifications"
  "The dbus service providing user notifications.")
(defconst notify-interface "org.freedesktop.Notifications"
  "The dbus interface providing user notifications.")
(defconst notify-path
  (replace-regexp-in-string "\\.\\|^" "/" notify-service)
  "The path at which we expect to find `notify-interface'")

(defvar notify-interface-spec nil)
(defvar notify-action-invoked nil)
(defvar notify-notice-closed  nil)
(defvar notify-notifications  nil)

(defun notify-action (id action)
  (let ((handler (assq id notify-notifications)) callback)
    (when handler
      (if (setq callback (cdr handler)) (funcall callback action))
      ;;(message "deleting handler for %S after invocation" id)
      (setq notify-notifications (assq-delete-all id notify-notifications)) )))

(defun notify-closed (id &rest ignored)
  (let ((handler (assq id notify-notifications)) callback)
    (when handler
      ;;(message "deleting handler for %S after close" id)
      (setq notify-notifications (assq-delete-all id notify-notifications)) )))

(defun notify-init ()
  "Idempotently initialise the `notify-service' interface and handlers.
Returns t if the interface was found, nil otherwise."
  (or notify-interface-spec
      (setq notify-interface-spec
            (dbus-util-introspect-service notify-service)))
  (or notify-action-invoked
      (setq notify-action-invoked
            (dbus-register-signal :session
                                  notify-service
                                  notify-path
                                  notify-interface
                                  "ActionInvoked"
                                  'notify-action)))
  (or notify-notice-closed
      (setq notify-notice-closed
            (dbus-register-signal :session
                                  notify-service
                                  notify-path
                                  notify-interface
                                  "NotificationClosed"
                                  'notify-closed)))
  (if notify-interface-spec t nil))

(defun notify-exit ()
  "Idempotently uninitialise the `notify-service' interface and unregister any
related handlers."
  (mapc (lambda (obj) (and obj (dbus-unregister-object obj))
          (list notify-action-invoked notify-notice-closed)))
  (setq notify-interface-spec nil
        notify-action-invoked nil
        notify-notice-closed  nil))

(defun notify-show-message (&rest args)
  "Display a notification via the `notify-service' dbus interface,
typically provided by notification-daemon.
The arguments are passed as a list of keys and values, with :symbols as
keys. The keys are mapped to the Notify method's arguments by stripping
the leading : and replacing \"-\" with \"_\".

::action-handler specifies the function to be called if you pass an action
or actions to the Notify method and one is invoked. This handler will be
called with one argument: The key from the action list which corresponds
to the button pressed.

Example:

  (notify-show-message :id       0
                       ::action-handler (lambda (a) (message \"action: %S\" a))
                       :body     \"Time to Die.\"
                       :icon     \"/home/vivek/src/elim/icons/garak.png\"
                       :actions  '(\"some-action\" \"Action Button Label\")
                       :hints    nil
                       :timeout  30000
                       :app-name \"(elim . garak)\" 
                       :summary  \"wake up!\")

Since the arguments are named, you need not specify them in any order.
You do not need to specify optional arguments either."
  (when (notify-init)
    (let ((action-handler (cadr (memq ::action-handler args))) return-handler)
      (setq return-handler
            `(lambda (id)
               (setq notify-notifications
                     (cons (cons id ',action-handler) notify-notifications))))
      (dbus-util-call-method :session
                             notify-service nil notify-interface "notify"
                             return-handler args))))

;; (notify-show-message :id       0
;;                      ::action-handler 
;;                      (lambda (&rest args) (message "AH%S" args))
;;                      :body     "Time to Die."
;;                      :icon     "/home/vivek/src/elim/icons/garak.png"
;;                      :actions  '("foo" "bar")
;;                      :hints    nil
;;                      :timeout  30000
;;                      :app-name "elim" 
;;                      :summary  "wake up!")

(provide 'notify)
