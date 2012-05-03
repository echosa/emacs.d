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
(require 'dbus)
(require 'xml)
(require 'assoc)

(defvar dbus-util-service-cache nil
  "Alist keyed on (\"service\" .  \"path\") of dbus service specifications")

(defun dbus-util-strip-xml (xml)
  (replace-regexp-in-string ">[ \t\r\n]+<" "><" xml))

(defun dbus-util-service-to-path (service)
  "Construct a likely path based on a service name"
  (replace-regexp-in-string "\\.\\|^" "/" service))

(defun dbus-util-introspect-service (service &optional path)
  "Retrieve a service definition from dbus"
  (or path (setq path (dbus-util-service-to-path service)))
  (let ((key (cons service path)) srv)
    (if (setq srv (cdr (assoc key dbus-util-service-cache)))
        srv
      (setq srv (with-temp-buffer
                  (insert (dbus-util-strip-xml
                           (dbus-introspect :session service path)))
                  (xml-parse-region (point-min) (point-max))) 
            srv (car srv))
      (aput 'dbus-util-service-cache key srv) srv)))

(defun dbus-util-find-interface (service interface &optional path)
  ;; (message "(dbus-util-introspect-service %S %S)" service path)
  (let (found)
    (mapc (lambda (i)
            ;; (message "checking %S" i)
            (when (equal (xml-get-attribute i 'name) interface) (setq found i)))
          (xml-get-children
           (dbus-util-introspect-service service path) 'interface))
    found))

(defun dbus-util-find-method (service interface method &optional path)
  (let (sig if)
    (setq if (dbus-util-find-interface service interface path))
    ;;(message "(dbus-util-find-interface %S %S %S)" service interface path)
    (mapc (lambda (m)
            (when (equal (xml-get-attribute m 'name) method)
              (setq sig m)))
          (xml-get-children if 'method))
    sig))

(defun dbus-util-argspec-to-cons (arg &optional name)
  (when (equal (xml-get-attribute-or-nil arg 'direction) "in")
    (cons (or (xml-get-attribute-or-nil arg 'name) name)
          (xml-get-attribute arg 'type))))

(defun dbus-util-method-args-in (method)
  (let ((idx 0) arg)
    (delq nil
          (mapcar 
           (lambda (node)
             (if (setq arg (dbus-util-argspec-to-cons node (format "%d" idx)))
                 (setq idx (1+ idx))) arg)
           (xml-get-children method 'arg))) ))

(defun dbus-util-coerce-value (value type)
  "Coerce an elisp VALUE to an appropriate form for passing to dbus methods
expecting an argument matching TYPE (a dbus type signature string).
Only simple cases are handled for now.\n
Returns a data structure that dbus.el should be able to use for TYPE."
  (let ((sig0 (aref type 0))
        (sig1 (substring type 1)))
    (if (stringp value) (setq value (encode-coding-string value 'utf-8)))
    (case sig0
      (?s (list :string value))
      (?u (list :uint32 value))
      (?i (list :int32  value))
      (?a (list
           (if (not value)
               (apply 'list :array :signature sig1 nil)
             (cons :array
                   (apply 'append
                          (mapcar
                           (lambda (x) (dbus-util-coerce-value x sig1))
                           value))) )))
      ('otherwise (list value))) ))

(defun dbus-util-coerce-args (spec args)
  "Take a dbus method SPEC and coerce (:name val ...) arglist ARGS into an
appropriate form for a dbus call to it."
  (let (name value arg-assoc)
    (while args
      (setq name      (substring (symbol-name (car args)) 1)
            name      (replace-regexp-in-string "-" "_" name)
            value     (cadr args)
            arg-assoc (cons (cons name value) arg-assoc)
            args      (cddr args)))
    ;; we want earlier duplicate args to override later copies, makes
    ;; our lives easier since prepending to slists is easy, appending hard:
    (setq arg-assoc (nreverse arg-assoc))
    (apply 'append
     (mapcar
      (lambda (s)
        (dbus-util-coerce-value (cdr (assoc (car s) arg-assoc)) (cdr s)))
      spec))))

(defun dbus-util-uglify-char (thing) (upcase (substring thing 1)))
(defun dbus-util-uglify-name (name)
  (setq name (replace-regexp-in-string "-\\(.\\)" 'dbus-util-uglify-char name))
  (aset name 0 (upcase (aref name 0))) name)

(defun dbus-util-deuglify-char (thing)
  (concat (match-string 1 thing) "-"
          (if (> (- (match-end 2) (match-beginning 2)) 1)
              (match-string 2 thing)
            (downcase (match-string 2 thing))) ))

(defun dbus-util-deuglify-name (name)
  (let ((case-fold-search nil)
        (munge (lambda (x)
                 (concat (match-string 1 x) "-"
                         (downcase (match-string 2 x))))))
    (setq name (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]+\\)" 
                                         'dbus-util-deuglify-char name t)
          name (replace-regexp-in-string "\\(.\\)\\([A-Z][a-z]\\)"
                                         munge name t))
    (aset name 0 (downcase (aref name 0)))
    name))

(defun dbus-util-call-method (bus service path interface name handler
                              &optional args)
  "Call a dbus method specified by BUS (:session or :system),
SERVICE, PATH, INTERFACE and NAME (all strings), calling HANDLER (a function)
with the return value(s) when available.

NAME can be an elisp-style-name rather than an UnReadableDBusStyleName.

ARGS should be an even-sized list of :name appropriate-type-value pairs.

ARGS will be mapped to the appropriate dbus method argument list based
on the dbus method signature automatically, and need not be specified
in the same order as the dbs signature.

If duplicate :name entries are present, only the first is used."
  (let (method-spec arg-spec dbus-args)
    (setq name        (dbus-util-uglify-name name)
          path        (or path (dbus-util-service-to-path service))
          method-spec (dbus-util-find-method service interface name path)
          arg-spec    (dbus-util-method-args-in method-spec)
          dbus-args   (dbus-util-coerce-args arg-spec args))
    ;;(message "(dbus-util-find-method %S %S %S %S)"
    ;;         service interface name path)
    ;;(message "%s·%s·%s(%S#%S→%S)"
    ;;         service interface name args method-spec dbus-args)
    (apply 'dbus-call-method-asynchronously
           bus service path interface name handler dbus-args)))

(provide 'dbus-util)
