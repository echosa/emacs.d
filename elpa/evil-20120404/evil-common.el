;;;; Common functions and utilities

(require 'evil-vars)
(require 'rect)

(condition-case nil
    (require 'windmove)
  (error
   (message "evil: Could not load `windmove', \
window commands not available.")
   nil))

(when (and (require 'undo-tree nil t)
           (fboundp 'global-undo-tree-mode))
  (global-undo-tree-mode 1))

;;; Compatibility with different Emacs versions

(defmacro evil-called-interactively-p ()
  "Wrapper for `called-interactively-p'.
In older versions of Emacs, `called-interactively-p' takes
no arguments. In Emacs 23.2 and newer, it takes one argument."
  (if (version< emacs-version "23.2")
      '(called-interactively-p)
    '(called-interactively-p 'any)))

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Returns t iff region and mark are active."
    (and transient-mark-mode mark-active)))

;; Emacs <23 does not know `characterp'
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

;; `make-char-table' requires this property in Emacs 22
(unless (get 'display-table 'char-table-extra-slots)
  (put 'display-table 'char-table-extra-slots 0))

;; macro helper
(eval-and-compile
  (defun evil-unquote (exp)
    "Return EXP unquoted."
    (while (eq (car-safe exp) 'quote)
      (setq exp (cadr exp)))
    exp))

(defun evil-delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK. If APPEND is
non-nil, the entry is appended to the hook. If LOCAL is non-nil,
the buffer-local value of HOOK is modified."
  (if (and (not (booleanp condition)) (eval condition))
      (eval form)
    (let* ((name (or name (format "evil-delay-form-in-%s" hook)))
           (fun (make-symbol name))
           (condition (or condition t)))
      (fset fun `(lambda (&rest args)
                   (when ,condition
                     (remove-hook ',hook #',fun ',local)
                     ,form)))
      (put fun 'permanent-local-hook t)
      (add-hook hook fun append local))))
(put 'evil-delay 'lisp-indent-function 2)

;;; List functions

(defun evil-add-to-alist (list-var key val &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list."
  (let ((tail (symbol-value list-var)))
    (while (and tail (not (equal (car-safe (car-safe tail)) key)))
      (setq tail (cdr tail)))
    (if tail
        (setcar tail (cons key val))
      (set list-var (append (symbol-value list-var)
                            (list (cons key val)))))
    (if elements
        (apply #'evil-add-to-alist list-var elements)
      (symbol-value list-var))))

;; custom version of `delete-if'
(defun evil-filter-list (predicate list &optional pointer)
  "Delete by side-effect all items satisfying PREDICATE in LIST.
Stop when reaching POINTER. If the first item satisfies PREDICATE,
there is no way to remove it by side-effect; therefore, write
\(setq foo (evil-delete-if 'predicate foo)) to be sure of
changing the value of `foo'."
  (let ((tail list) elt head)
    (while (and tail (not (eq tail pointer)))
      (setq elt (car tail))
      (cond
       ((funcall predicate elt)
        (setq tail (cdr tail))
        (if head
            (setcdr head tail)
          (setq list tail)))
       (t
        (setq head tail
              tail (cdr tail)))))
    list))

(defun evil-member-if (predicate list &optional pointer)
  "Find the first item satisfying PREDICATE in LIST.
Stop when reaching POINTER, which should point at a link
in the list."
  (let (elt)
    (catch 'done
      (while (and (consp list) (not (eq list pointer)))
        (setq elt (car list))
        (if (funcall predicate elt)
            (throw 'done elt)
          (setq list (cdr list)))))))

(defun evil-member-recursive-if (predicate tree)
  "Find the first item satisfying PREDICATE in TREE."
  (cond
   ((funcall predicate tree)
    tree)
   ((listp tree)
    (catch 'done
      (dolist (elt tree)
        (when (setq elt (evil-member-recursive-if predicate elt))
          (throw 'done elt)))))))

(defun evil-concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates.
Elements are compared with `eq'."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (add-to-list 'result elt nil #'eq)))
    (nreverse result)))

(defun evil-concat-alists (&rest sequences)
  "Concatenate association lists, removing duplicates.
An alist is a list of cons cells (KEY . VALUE) where each key
may occur only once. Later values overwrite earlier values."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (setq result (assq-delete-all (car-safe elt) result))
        (push elt result)))
    (nreverse result)))

(defun evil-concat-plists (&rest sequences)
  "Concatenate property lists, removing duplicates.
A property list is a list (:KEYWORD1 VALUE1 :KEYWORD2 VALUE2...)
where each keyword may occur only once. Later values overwrite
earlier values."
  (let (result)
    (dolist (sequence sequences result)
      (while sequence
        (setq result
              (plist-put result (pop sequence) (pop sequence)))))))

(defun evil-concat-keymap-alists (&rest sequences)
  "Concatenate keymap association lists, removing duplicates.
A keymap alist is a list of cons cells (VAR . MAP) where each keymap
may occur only once, but where the variables may be repeated
\(e.g., (VAR . MAP1) (VAR . MAP2) is allowed). The order matters,
with the highest priority keymaps being listed first."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (unless (rassq (cdr-safe elt) result)
          (push elt result))))
    (nreverse result)))

(defun evil-plist-delete (prop plist)
  "Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (evil-plist-delete :prop foo)) to be sure of
changing the value of `foo'."
  (let ((tail plist) elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

(defun evil-get-property (alist key &optional prop)
  "Return property PROP for KEY in ALIST.
ALIST is an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list.
If PROP is nil, return all properties for KEY.
If KEY is t, return an association list of keys
and their PROP values."
  (cond
   ((null prop)
    (cdr (assq key alist)))
   ((eq key t)
    (let (result val)
      (dolist (entry alist result)
        (setq key (car entry)
              val (cdr entry))
        (when (plist-member val prop)
          (setq val (plist-get val prop))
          (push (cons key val) result)))))
   (t
    (plist-get (cdr (assq key alist)) prop))))

(defun evil-put-property (alist-var key prop val &rest properties)
  "Set PROP to VAL for KEY in ALIST-VAR.
ALIST-VAR points to an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list storing PROP and VAL."
  (set alist-var
       (let* ((alist (symbol-value alist-var))
              (plist (cdr (assq key alist))))
         (setq plist (plist-put plist prop val))
         (when properties
           (setq plist (evil-concat-plists plist properties)
                 val (car (last properties))))
         (setq alist (assq-delete-all key alist))
         (push (cons key plist) alist)))
  val)

(defun evil-state-property (state prop &optional value)
  "Return the value of property PROP for STATE.
PROP is a keyword as used by `evil-define-state'.
STATE is the state's symbolic name.
If VALUE is non-nil and the value is a variable,
return the value of that variable."
  (let ((val (evil-get-property evil-state-properties state prop)))
    (if (and value (symbolp val) (boundp val))
        (symbol-value val)
      val)))

(defmacro evil-swap (this that &rest vars)
  "Swap the values of variables THIS and THAT.
If three or more arguments are given, the values are rotated.
E.g., (evil-swap A B C) sets A to B, B to C, and C to A."
  `(progn
     (setq ,this (prog1 ,that
                   (setq ,that ,this)))
     ,@(when vars
         `((evil-swap ,that ,@vars)))))

(defmacro evil-sort (min max &rest vars)
  "Place the smallest value in MIN and the largest in MAX.
If three or more arguments are given, place the smallest
value in the first argument and the largest in the last,
sorting in between."
  (let ((sorted (make-symbol "sortvar")))
    `(let ((,sorted (sort (list ,min ,max ,@vars) '<)))
       (setq ,min (pop ,sorted)
             ,max (pop ,sorted)
             ,@(apply #'append
                      (mapcar #'(lambda (var)
                                  (list var `(pop ,sorted)))
                              vars))))))

;;; Command properties

(defmacro evil-define-command (command &rest body)
  "Define a command COMMAND.

\(fn COMMAND (ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((interactive '(interactive))
        arg args doc doc-form key keys)
    ;; collect arguments
    (when (listp (car-safe body))
      (setq args (pop body)))
    ;; collect docstring
    (when (> (length body) 1)
      (if (eq (car-safe (car-safe body)) 'format)
          (setq doc-form (pop body))
        (when (stringp (car-safe body))
          (setq doc (pop body)))))
    ;; collect keywords
    (setq keys (plist-put keys :repeat t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (unless nil ; TODO: add keyword check
        (setq keys (plist-put keys key arg))))
    ;; collect `interactive' form
    (when (and body (consp (car body))
               (eq (car (car body)) 'interactive))
      (let* ((iform (pop body))
             (result (apply #'evil-interactive-form (cdr iform)))
             (form (car result))
             (attrs (cdr result)))
        (setq interactive `(interactive ,form)
              keys (evil-concat-plists keys attrs))))
    `(progn
       ;; the compiler does not recognize `defun' inside `let'
       ,(when (and command body)
          `(defun ,command ,args
             ,@(when doc `(,doc))
             ,interactive
             ,@body))
       ,(when (and command doc-form)
          `(put ',command 'function-documentation ,doc-form))
       ;; set command properties for symbol or lambda function
       (let ((func ',(if (and (null command) body)
                         `(lambda ,args
                            ,interactive
                            ,@body)
                       command)))
         (apply #'evil-set-command-properties func ',keys)
         func))))

;; If no Evil properties are defined for the command, several parts of
;; Evil apply certain default rules; e.g., the repeat system decides
;; whether the command is repeatable by monitoring buffer changes.
(defun evil-has-command-property-p (command property)
  "Whether COMMAND has Evil PROPERTY.
See also `evil-has-command-properties-p'."
  (plist-member (evil-get-command-properties command) property))

(defun evil-has-command-properties-p (command)
  "Whether Evil properties are defined for COMMAND.
See also `evil-has-command-property-p'."
  (and (evil-get-command-properties command) t))

(defun evil-get-command-property (command property &optional default)
  "Return the value of Evil PROPERTY of COMMAND.
If the command does not have the property, return DEFAULT.
See also `evil-get-command-properties'."
  (if (evil-has-command-property-p command property)
      (evil-get-property evil-command-properties command property)
    default))

(defun evil-get-command-properties (command)
  "Return all Evil properties of COMMAND.
See also `evil-get-command-property'."
  (evil-get-property evil-command-properties command))

(defun evil-set-command-property (command property value)
  "Set PROPERTY to VALUE for COMMAND.
To set multiple properties at once, see
`evil-set-command-properties' and `evil-add-command-properties'."
  (evil-put-property 'evil-command-properties command property value))
(defalias 'evil-put-command-property 'evil-set-command-property)

(defun evil-add-command-properties (command &rest properties)
  "Add PROPERTIES to COMMAND.
PROPERTIES should be a property list.
To replace all properties at once, use `evil-set-command-properties'."
  (apply #'evil-put-property
         'evil-command-properties command properties))

(defun evil-set-command-properties (command &rest properties)
  "Replace all of COMMAND's properties with PROPERTIES.
PROPERTIES should be a property list.
This erases all previous properties; to only add properties,
use `evil-set-command-property'."
  (setq evil-command-properties
        (assq-delete-all command evil-command-properties))
  (when properties
    (apply #'evil-add-command-properties command properties)))

(defun evil-remove-command-properties (command &rest properties)
  "Remove PROPERTIES from COMMAND.
PROPERTIES should be a list of properties (:PROP1 :PROP2 ...).
If PROPERTIES is the empty list, all properties are removed."
  (let (plist)
    (when properties
      (setq plist (evil-get-command-properties command))
      (dolist (property properties)
        (setq plist (evil-plist-delete property plist))))
    (apply #'evil-set-command-properties command plist)))

(defun evil-yank-handler (&optional motion)
  "Return the yank handler for MOTION.
MOTION defaults to the current motion."
  (setq motion (or motion evil-this-motion))
  (evil-get-command-property motion :yank-handler))

(defun evil-declare-motion (command)
  "Declare COMMAND to be a movement function.
This ensures that it behaves correctly in Visual state."
  (evil-add-command-properties command :keep-visual t :repeat 'motion))

(defun evil-declare-repeat (command)
  "Declare COMMAND to be repeatable."
  (evil-add-command-properties command :repeat t))

(defun evil-declare-not-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat nil))

(defun evil-declare-ignore-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat 'ignore))

(defun evil-declare-change-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes."
  (evil-add-command-properties command :repeat 'change))

(defun evil-declare-abort-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat 'abort))

(defun evil-delimited-arguments (string &optional num)
  "Parse STRING as a sequence of delimited arguments.
Returns a list of NUM strings, or as many arguments as
the string contains. The first non-blank character is
taken to be the delimiter. If some arguments are missing
from STRING, the resulting list is padded with nil values.
Two delimiters following directly after each other gives
an empty string."
  (save-match-data
    (let ((string (or string ""))
          (count (or num -1)) (idx 0)
          argument delim match result)
      (when (string-match "^[[:space:]]*\\([^[:space:]]\\)" string)
        (setq delim (match-string 1 string)
              argument (format "%s\\(\\(?:[\\].\\|[^%s]\\)*\\)"
                               (regexp-quote delim)
                               delim))
        (while (and (/= count 0) (string-match argument string idx))
          (setq match (match-string 1 string)
                idx (match-end 1)
                count (1- count))
          (when (= count 0)
            (unless (save-match-data
                      (string-match
                       (format "%s[[:space:]]*$" delim) string idx))
              (setq match (substring string (match-beginning 1)))))
          (unless (and (zerop (length match))
                       (zerop (length (substring string idx))))
            (push match result))))
      (when (and num (< (length result) num))
        (dotimes (i (- num (length result)))
          (push nil result)))
      (nreverse result))))

(defun evil-concat-charsets (&rest sets)
  "Concatenate character sets.
A character set is the part between [ and ] in a regular expression.
If any character set is complemented, the result is also complemented."
  (let ((bracket "") (complement "") (hyphen "") result)
    (save-match-data
      (dolist (set sets)
        (when (string-match "^\\^" set)
          (setq set (substring set 1)
                complement "^"))
        (when (string-match "^]" set)
          (setq set (substring set 1)
                bracket "]"))
        (when (string-match "^-" set)
          (setq set (substring set 1)
                hyphen "-"))
        (setq result (concat result set)))
      (format "%s%s%s%s" complement bracket hyphen result))))

;;; Key sequences

(defun evil-keypress-parser (&optional input)
  "Read from keyboard or INPUT and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument.
Both COUNT and CMD may be nil."
  (let ((input (listify-key-sequence input))
        (inhibit-quit t)
        char cmd count digit event seq)
    (while (progn
             (setq event (or (pop input) (read-event)))
             (when (eq event ?\e)
               (when (sit-for evil-esc-delay t)
                 (setq event 'escape)))
             (setq char (or (when (characterp event) event)
                            (when (symbolp event)
                              (get event 'ascii-character))))
             ;; this trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (if (or (characterp char) (integerp char))
                 (setq digit (- (logand char ?\177) ?0))
               (setq digit nil))
             (if (keymapp cmd)
                 (setq seq (append seq (list event)))
               (setq seq (list event)))
             (setq cmd (key-binding (vconcat seq) t))
             (cond
              ;; if CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; numeric prefix argument
              ((or (eq cmd #'digit-argument)
                   (and (eq (length seq) 1)
                        (not (keymapp cmd))
                        count
                        (memq digit '(0 1 2 3 4 5 6 7 8 9))))
               ;; store digits in a string, which is easily converted
               ;; to a number afterwards
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               t)
              ;; catch middle digits like "da2w"
              ((and (not cmd)
                    (> (length seq) 1)
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq seq (nbutlast seq 1))
               (setq cmd (key-binding (vconcat seq)))
               t)
              ((eq cmd 'negative-argument)
               (unless count
                 (setq count "-"))))))
    ;; determine COUNT
    (when (stringp count)
      (if (string= count "-")
          (setq count nil)
        (setq count (string-to-number count))))
    ;; return command description
    (list cmd count)))

(defun evil-read-key (&optional prompt)
  "Read a key from the keyboard.
Translates it according to the input method."
  (let ((old-global-map (current-global-map))
        (new-global-map (make-sparse-keymap))
        (overriding-terminal-local-map (make-sparse-keymap))
        overriding-local-map char)
    (unwind-protect
        (progn
          (define-key new-global-map [menu-bar]
            (lookup-key global-map [menu-bar]))
          (define-key new-global-map [tool-bar]
            (lookup-key global-map [tool-bar]))
          (add-to-list 'new-global-map
                       (make-char-table 'display-table
                                        'self-insert-command) t)
          (use-global-map new-global-map)
          (setq char (aref (read-key-sequence prompt nil t) 0))
          (if (memq char '(?\C-q ?\C-v))
              (read-quoted-char)
            (unless (eq char ?\e)
              (or (cdr (assq char '((?\r . ?\n))))
                  char))))
      (use-global-map old-global-map))))

(defun evil-read-motion (&optional motion count type modifier)
  "Read a MOTION, motion COUNT and motion TYPE from the keyboard.
The type may be overridden with MODIFIER, which may be a type
or a Visual selection as defined by `evil-define-visual-selection'.
Return a list (MOTION COUNT [TYPE])."
  (let ((modifiers '((evil-visual-char . char)
                     (evil-visual-line . line)
                     (evil-visual-block . block)))
        command prefix)
    (unless motion
      (while (progn
               (setq command (evil-keypress-parser)
                     motion (pop command)
                     prefix (pop command))
               (when prefix
                 (if count
                     (setq count (string-to-number
                                  (concat (number-to-string count)
                                          (number-to-string prefix))))
                   (setq count prefix)))
               ;; if the command is a type modifier, read more
               (when (rassq motion evil-visual-alist)
                 (setq modifier
                       (or modifier
                           (car (rassq motion evil-visual-alist))))))))
    (when modifier
      (setq type (or type (evil-type motion 'exclusive)))
      (cond
       ((eq modifier 'char)
        ;; TODO: this behavior could be less hard-coded
        (if (eq type 'exclusive)
            (setq type 'inclusive)
          (setq type 'exclusive)))
       (t
        (setq type modifier))))
    (list motion count type)))

(defun evil-mouse-events-p (keys)
  "Returns non-nil iff KEYS contains a mouse event."
  (catch 'done
    (dotimes (i (length keys))
      (when (or (and (fboundp 'mouse-event-p)
                     (mouse-event-p (aref keys i)))
                (mouse-movement-p (aref keys i)))
        (throw 'done t)))
    nil))

(defun evil-extract-count (keys)
  "Splits the key-sequence KEYS into prefix-argument and the rest.
Returns the list (PREFIX CMD SEQ REST), where PREFIX is the
prefix count, CMD the command to be executed, SEQ the subsequence
calling CMD, and REST is all remaining events in the
key-sequence. PREFIX and REST may be nil if they do not exist.
If a command is bound to some keyboard macro, it is expanded
recursively."
  (catch 'done
    (let* ((len (length keys))
           (beg 0)
           (end 1)
           (found-prefix nil))
      (while (and (<= end len))
        (let ((cmd (key-binding (substring keys beg end))))
          (cond
           ((memq cmd '(undefined nil))
            (error "No command bound to %s" (substring keys beg end)))
           ((arrayp cmd) ; keyboard macro, replace command with macro
            (setq keys (vconcat (substring keys 0 beg)
                                cmd
                                (substring keys end))
                  end (1+ beg)
                  len (length keys)))
           ((functionp cmd)
            (if (or (memq cmd '(digit-argument negative-argument))
                    (and found-prefix
                         (evil-get-command-property
                          cmd :digit-argument-redirection)))
                ;; skip those commands
                (setq found-prefix t ; found at least one prefix argument
                      beg end
                      end (1+ end))
              ;; a real command, finish
              (throw 'done
                     (list (unless (zerop beg)
                             (string-to-number
                              (concat (substring keys 0 beg))))
                           cmd
                           (substring keys beg end)
                           (when (< end len)
                             (substring keys end))))))
           (t ; append a further event
            (setq end (1+ end))))))
      (error "Key sequence contains no complete binding"))))

(defmacro evil-redirect-digit-argument (map keys target)
  "Bind a wrapper function calling TARGET or `digit-argument'.
MAP is a keymap for binding KEYS to the wrapper for TARGET.
The wrapper only calls `digit-argument' if a prefix-argument
has already been started; otherwise TARGET is called."
  (let* ((target (eval target))
         (wrapper (intern (format "evil-digit-argument-or-%s"
                                  target))))
    `(progn
       (define-key ,map ,keys ',wrapper)
       (evil-define-command ,wrapper ()
         :digit-argument-redirection ,target
         :keep-visual t
         :repeat nil
         (interactive)
         (cond
          (current-prefix-arg
           (setq this-command #'digit-argument)
           (call-interactively #'digit-argument))
          (t
           (setq this-command #',target)
           (call-interactively #',target)))))))

(defun evil-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

;;; Display

(defun evil-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above."
  (unless (and (listp specs)
               (null (cdr-safe (last specs))))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (condition-case nil
          (funcall spec)
        (error nil)))
     ((stringp spec)
      (evil-set-cursor-color spec))
     (t
      (setq cursor-type spec)))))

(defun evil-set-cursor-color (color)
  "Set the cursor color to COLOR."
  (unless (equal (frame-parameter nil 'cursor-color) color)
    ;; `set-cursor-color' forces a redisplay, so only
    ;; call it when the color actually changes
    (set-cursor-color color)))

(defun evil-refresh-cursor (&optional state buffer)
  "Refresh the cursor for STATE in BUFFER.
STATE defaults to the current state.
BUFFER defaults to the current buffer."
  (when (and (boundp 'evil-local-mode) evil-local-mode)
    (let* ((state (or state evil-state 'normal))
           (default (or evil-default-cursor t))
           (cursor (evil-state-property state :cursor t))
           (color (or (and (stringp cursor) cursor)
                      (and (listp cursor)
                           (evil-member-if #'stringp cursor)))))
      (with-current-buffer (or buffer (current-buffer))
        ;; if both STATE and `evil-default-cursor'
        ;; specify a color, don't set it twice
        (when (and color (listp default))
          (setq default (evil-filter-list #'stringp default)))
        (evil-set-cursor default)
        (evil-set-cursor cursor)))))
(put 'evil-refresh-cursor 'permanent-local-hook t)

(defmacro evil-save-cursor (&rest body)
  "Save the current cursor; execute BODY; restore the cursor."
  (declare (indent defun)
           (debug t))
  `(let ((cursor cursor-type)
         (color (frame-parameter (selected-frame) 'cursor-color))
         (inhibit-quit t))
     (unwind-protect
         (progn ,@body)
       (evil-set-cursor cursor)
       (evil-set-cursor color))))

(defun evil-echo (string &rest args)
  "Display an unlogged message in the echo area.
That is, the message is not logged in the *Messages* buffer.
\(To log the message, just use `message'.)"
  (unless evil-no-display
    (let (message-log-max)
      (apply #'message string args))))

(defun evil-echo-area-save ()
  "Save the current echo area in `evil-echo-area-message'."
  (setq evil-echo-area-message (current-message)))

(defun evil-echo-area-restore ()
  "Restore the echo area from `evil-echo-area-message'.
Does not restore if `evil-write-echo-area' is non-nil."
  (unless evil-write-echo-area
    (if evil-echo-area-message
        (message "%s" evil-echo-area-message)
      (message nil)))
  (setq evil-echo-area-message nil
        evil-write-echo-area nil))

;; toggleable version of `with-temp-message'
(defmacro evil-save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         evil-echo-area-message
         evil-write-echo-area)
     (unwind-protect
         (progn
           (evil-echo-area-save)
           ,@body)
       (evil-echo-area-restore))))

(defmacro evil-without-display (&rest body)
  "Execute BODY without Evil displays.
Inhibits echo area messages, mode line updates and cursor changes."
  (declare (indent defun)
           (debug t))
  `(let ((evil-no-display t))
     ,@body))

(defun evil-num-visible-lines ()
  "Returns the number of currently visible lines."
  (- (window-height) 1))

(defun evil-max-scroll-up ()
  "Returns the maximal number of lines that can be scrolled up."
  (1- (line-number-at-pos (window-start))))

(defun evil-max-scroll-down ()
  "Returns the maximal number of lines that can be scrolled down."
  (if (pos-visible-in-window-p (window-end))
      0
    (1+ (- (line-number-at-pos (point-max))
           (line-number-at-pos (window-end))))))

;;; Movement

(defun evil-normalize-position (pos)
  "Return POS if it does not exceed the buffer boundaries.
If POS is less than `point-min', return `point-min'.
Is POS is more than `point-max', return `point-max'.
If POS is a marker, return its position."
  (cond
   ((not (number-or-marker-p pos))
    pos)
   ((< pos (point-min))
    (point-min))
   ((> pos (point-max))
    (point-max))
   ((markerp pos)
    (marker-position pos))
   (t
    pos)))

(defmacro evil-save-goal-column (&rest body)
  "Restores the goal column after execution of BODY.
See also `evil-save-column'."
  (declare (indent defun)
           (debug t))
  `(let ((goal-column goal-column)
         (temporary-goal-column temporary-goal-column))
     ,@body))

(defmacro evil-save-column (&rest body)
  "Restores the column after execution of BODY.
See also `evil-save-goal-column'."
  (declare (indent defun)
           (debug t))
  `(let ((col (current-column)))
     (evil-save-goal-column
       ,@body
       (move-to-column col))))

(defun evil-narrow (beg end)
  "Restrict the buffer to BEG and END.
BEG or END may be nil, specifying a one-sided restriction including
`point-min' or `point-max'. See also `evil-with-restriction.'"
  (setq beg (or (evil-normalize-position beg) (point-min)))
  (setq end (or (evil-normalize-position end) (point-max)))
  (narrow-to-region beg end))

(defmacro evil-with-restriction (beg end &rest body)
  "Execute BODY with the buffer narrowed to BEG and END.
BEG or END may be nil as passed to `evil-narrow'; this creates
a one-sided restriction."
  (declare (indent 2)
           (debug t))
  `(save-restriction
     (evil-narrow ,beg ,end)
     ,@body))

(defmacro evil-narrow-to-field (&rest body)
  "Narrow to the current field."
  (declare (indent defun)
           (debug t))
  `(evil-with-restriction (field-beginning) (field-end)
     ,@body))

(defun evil-move-beginning-of-line (&optional arg)
  "Move to the beginning of the line as displayed.
Like `move-beginning-of-line', but retains the goal column."
  (evil-save-goal-column
    (move-beginning-of-line arg)
    (beginning-of-line)))

(defun evil-move-end-of-line (&optional arg)
  "Move to the end of the line as displayed.
Like `move-end-of-line', but retains the goal column."
  (evil-save-goal-column
    (move-end-of-line arg)
    (end-of-line)))

(defun evil-adjust-cursor (&optional force)
  "Move point one character back if at the end of a non-empty line.
If at the end of the buffer, move point to the previous line.
This behavior is contingent on the variable `evil-move-cursor-back';
use the FORCE parameter to override it."
  (cond
   ((and (eobp) (bolp))
    (evil-with-restriction
        (field-beginning nil nil (line-beginning-position -1)) nil
      (forward-line -1)
      (back-to-indentation)
      (setq temporary-goal-column (current-column))))
   ((and (eolp)
         (not (bolp))
         (= (point)
            (save-excursion
              (evil-move-end-of-line)
              (point))))
    (evil-move-cursor-back force))))

(defun evil-move-cursor-back (&optional force)
  "Move point one character back within the current line.
Contingent on the variable `evil-move-cursor-back' or the FORCE
argument. Honors field boundaries, i.e., constrains the movement
to the current field as recognized by `line-beginning-position'."
  (when (or evil-move-cursor-back force)
    (unless (or (= (point) (line-beginning-position))
                (and visual-line-mode
                     (= (point) (save-excursion
                                  (beginning-of-visual-line)
                                  (point)))))
      (backward-char))))

(defun evil-line-position (line &optional column)
  "Return the position of LINE.
If COLUMN is specified, return its position on the line.
A negative number means the end of the line."
  (save-excursion
    (when (fboundp 'evil-goto-line)
      (evil-goto-line line))
    (if (numberp column)
        (if (< column 0)
            (beginning-of-line 2)
          (move-to-column column))
      (beginning-of-line))
    (point)))

(defun evil-column (&optional pos)
  "Return the horizontal position of POS.
POS defaults to point."
  (save-excursion
    (when pos
      (goto-char pos))
    (current-column)))

(defun evil-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right if DIR
is non-nil) and returns point."
  (interactive "p")
  (move-to-column column force)
  (unless force
    (when (or (not dir) (and (numberp dir) (< dir 1)))
      (when (> (current-column) column)
        (evil-move-cursor-back))))
  (point))

(defmacro evil-loop (spec &rest body)
  "Loop with countdown variable.
Evaluate BODY with VAR counting down from COUNT to 0.
COUNT can be negative, in which case VAR counts up instead.
The return value is the value of VAR when the loop
terminates, which is 0 if the loop completes successfully.
RESULT specifies a variable for storing this value.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug dolist))
  (let* ((i (make-symbol "loopvar"))
         (var (pop spec))
         (count (pop spec))
         (result (pop spec)))
    (setq var (or (unless (eq var result) var) i)
          result (or result var))
    `(let ((,var ,count))
       (setq ,result ,var)
       (while (/= ,var 0)
         ,@body
         (if (> ,var 0)
             (setq ,var (1- ,var))
           (setq ,var (1+ ,var)))
         (setq ,result ,var))
       ,var)))

(defmacro evil-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with VAR bound to 1 or -1,
depending on the sign of COUNT. RESULT, if specified, holds
the number of unsuccessful iterations, which is 0 if the loop
completes successfully. This is also the return value.

Each iteration must move point; if point does not change,
the loop immediately quits. See also `evil-loop'.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form &optional symbolp) body)))
  (let* ((var (or (pop spec) (make-symbol "unitvar")))
         (countval (or (pop spec) 0))
         (result (pop spec))
         (i (make-symbol "loopvar"))
         (count (make-symbol "countvar"))
         (done (make-symbol "donevar"))
         (orig (make-symbol "origvar")))
    `(let* ((,count ,countval)
            (,var (if (< ,count 0) -1 1)))
       (catch ',done
         (evil-loop (,i ,count ,result)
           (let ((,orig (point)))
             ,@body
             (when (= (point) ,orig)
               (throw ',done ,i))))))))

(defmacro evil-signal-without-movement (&rest body)
  "Catches errors provided point moves within this scope."
  (declare (indent defun)
           (debug t))
  `(let ((p (point)))
     (condition-case err
         (progn ,@body)
       (error
        (when (= p (point))
          (signal (car err) (cdr err)))))))

(defun evil-goto-min (&rest positions)
  "Go to the smallest position in POSITIONS.
Non-numerical elements are ignored.
See also `evil-goto-max'."
  (when (setq positions (evil-filter-list
                         #'(lambda (elt)
                             (not (number-or-marker-p elt)))
                         positions))
    (goto-char (apply #'min positions))))

(defun evil-goto-max (&rest positions)
  "Go to the largest position in POSITIONS.
Non-numerical elements are ignored.
See also `evil-goto-min'."
  (when (setq positions (evil-filter-list
                         #'(lambda (elt)
                             (not (number-or-marker-p elt)))
                         positions))
    (goto-char (apply #'max positions))))

;; The purpose of this function is the provide line motions which
;; preserve the column. This is how `previous-line' and `next-line'
;; work, but unfortunately the behaviour is hard-coded: if and only if
;; the last command was `previous-line' or `next-line', the column is
;; preserved. Furthermore, in contrast to Vim, when we cannot go
;; further, those motions move point to the beginning resp. the end of
;; the line (we never want point to leave its column). The code here
;; comes from simple.el, and I hope it will work in future.
(defun evil-line-move (count &optional noerror)
  "A wrapper for line motions which conserves the column.
Signals an error at buffer boundaries unless NOERROR is non-nil."
  (cond
   (noerror
    (condition-case nil
        (evil-line-move count)
      (error nil)))
   (t
    (evil-signal-without-movement
      (setq this-command (if (>= count 0)
                             #'next-line
                           #'previous-line))
      (let ((opoint (point)))
        (condition-case err
            (with-no-warnings
              (funcall this-command (abs count)))
          ((beginning-of-buffer end-of-buffer)
           (let ((col (or goal-column
                          (if (consp temporary-goal-column)
                              (car temporary-goal-column)
                            temporary-goal-column))))
             (if line-move-visual
                 (vertical-motion (cons col 0))
               (line-move-finish col opoint (< count 0)))
             ;; Maybe we should just `ding'?
             (signal (car err) (cdr err))))))))))

(defun evil-move-chars (chars count)
  "Move point to the end or beginning of a sequence of CHARS.
CHARS is a character set as inside [...] in a regular expression."
  (let ((regexp (format "[%s]" chars)))
    (evil-motion-loop (var count)
      (cond
       ((< var 0)
        (re-search-backward regexp nil t)
        (skip-chars-backward chars))
       (t
        (re-search-forward regexp nil t)
        (skip-chars-forward chars))))))

;; this function is slightly adapted from paragraphs.el
(defun evil-move-sentence (count)
  "Move by sentence."
  (let ((count (or count 1))
        (opoint (point))
        (sentence-end (sentence-end))
        pos par-beg par-end)
    (evil-motion-loop (var count)
      (cond
       ;; backward
       ((< var 0)
        (setq pos (point)
              par-beg (save-excursion
                        (and (zerop (evil-move-paragraph -1))
                             (point))))
        (if (and (re-search-backward sentence-end par-beg t)
                 (or (< (match-end 0) pos)
                     (re-search-backward sentence-end par-beg t)))
            (goto-char (match-end 0))
          (goto-char (or par-beg pos))))
       ;; forward
       (t
        (setq par-end (save-excursion
                        (and (zerop (evil-move-paragraph 1))
                             (point))))
        (if (re-search-forward sentence-end par-end t)
            (skip-chars-backward " \t\n")
          (goto-char (or par-end (point)))))))))

(defun evil-move-paragraph (count)
  "Move by paragraph."
  (let ((count (or count 1))
        npoint opoint)
    (evil-motion-loop (var count)
      (setq opoint (point))
      (cond
       ((< var 0)
        (forward-paragraph -1)
        (setq npoint (point))
        (skip-chars-forward " \t\n")
        (when (and (>= (point) opoint) (< npoint opoint))
          (goto-char npoint)
          (forward-paragraph -1)
          (skip-chars-forward " \t\n")
          (when (and (>= (point) opoint) (< npoint opoint))
            (goto-char opoint))))
       (t
        (forward-paragraph 1)
        (setq npoint (point))
        (skip-chars-backward " \t\n")
        (when (<= (point) opoint)
          (goto-char npoint)
          (forward-paragraph 1)
          (skip-chars-backward " \t\n")
          (when (<= (point) opoint)
            (goto-char opoint))))))))

(defun evil-in-regexp-p (regexp &optional pos)
  "Whether POS is inside a match for REGEXP.
POS defaults to the current position of point."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (if (re-search-forward regexp nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max)))
      (when (re-search-backward regexp nil t)
        (and (> pos (match-beginning 0))
             (< pos (match-end 0)))))))

(defun evil-in-comment-p (&optional pos)
  "Whether POS is inside a comment.
POS defaults to the current position of point."
  (let ((parse #'(lambda (p)
                   (let ((c (char-after p)))
                     (or (and c (eq (char-syntax c) ?<))
                         (memq (get-text-property p 'face)
                               '(font-lock-comment-face
                                 font-lock-comment-delimiter-face))
                         (nth 4 (parse-partial-sexp
                                 (save-excursion
                                   (beginning-of-defun)
                                   (point)) p)))))))
    (save-excursion
      (goto-char (or pos (point)))
      (and (or (funcall parse (point))
               ;; `parse-partial-sexp's notion of comments
               ;; doesn't span lines
               (progn
                 (back-to-indentation)
                 (unless (eolp)
                   (forward-char)
                   (funcall parse (point))))) t))))

(defun evil-in-string-p (&optional pos)
  "Whether POS is inside a string.
POS defaults to the current position of point."
  (save-excursion
    (goto-char (or pos (point)))
    (and (nth 3 (parse-partial-sexp
                 (save-excursion (beginning-of-defun) (point))
                 (point))) t)))

(defun evil-find-beginning (predicate &optional pos limit)
  "Find the beginning of a series of characters satisfying PREDICATE.
POS is the starting point and defaults to the current position.
Stops at LIMIT, which defaults to the beginning of the buffer."
  (setq pos (or pos (point))
        limit (or limit (buffer-end -1)))
  (while (let ((prev (1- pos)))
           (when (and (>= prev limit)
                      (funcall predicate prev))
             (setq pos prev))))
  pos)

(defun evil-find-end (predicate &optional pos limit)
  "Find the end of a series of characters satisfying PREDICATE.
POS is the starting point and defaults to the current position.
Stops at LIMIT, which defaults to the end of the buffer."
  (setq pos (or pos (point))
        limit (or limit (buffer-end 1)))
  (while (let ((next (1+ pos)))
           (when (and (<= next limit)
                      (funcall predicate next))
             (setq pos next))))
  pos)

(defun evil-comment-beginning (&optional pos)
  "Return beginning of comment containing POS.
POS defaults to the current position of point."
  (let ((pos (or pos (point))))
    (when (evil-in-comment-p pos)
      (evil-find-beginning #'evil-in-comment-p pos))))

(defun evil-comment-end (&optional pos)
  "Return end of comment containing POS.
POS defaults to the current position of point."
  (let ((pos (or pos (point))))
    (when (evil-in-comment-p pos)
      (evil-find-end #'evil-in-comment-p pos))))

(defun evil-string-beginning (&optional pos)
  "Return beginning of string containing POS.
POS defaults to the current position of point."
  (let ((pos (or pos (point))))
    (when (evil-in-string-p pos)
      (evil-normalize-position
       (1- (evil-find-beginning #'evil-in-string-p pos))))))

(defun evil-string-end (&optional pos)
  "Return end of string containing POS.
POS defaults to the current position of point."
  (let ((pos (or pos (point))))
    (when (evil-in-string-p pos)
      (evil-normalize-position
       (1+ (evil-find-end #'evil-in-string-p pos))))))

(defmacro evil-narrow-to-comment (&rest body)
  "Narrow to the current comment or docstring, if any."
  (declare (indent defun)
           (debug t))
  `(save-restriction
     (cond
      ((evil-in-comment-p)
       (narrow-to-region (evil-comment-beginning) (evil-comment-end)))
      ((evil-in-string-p)
       (narrow-to-region (evil-string-beginning) (evil-string-end))))
     ,@body))

(defmacro evil-with-or-without-comment (&rest body)
  "Try BODY narrowed to the current comment; then try BODY unnarrowed.
If BODY returns non-nil inside the current comment, return that.
Otherwise, execute BODY again, but without the restriction."
  (declare (indent defun)
           (debug t))
  `(or (when (or (evil-in-comment-p) (evil-in-string-p))
         (evil-narrow-to-comment ,@body))
       (progn ,@body)))

(defun evil-insert-newline-above ()
  "Inserts a new line above point and places point in that line
with regard to indentation."
  (evil-narrow-to-field
    (evil-move-beginning-of-line)
    (insert "\n")
    (forward-line -1)
    (back-to-indentation)))

(defun evil-insert-newline-below ()
  "Inserts a new line below point and places point in that line
with regard to indentation."
  (evil-narrow-to-field
    (evil-move-end-of-line)
    (insert "\n")
    (back-to-indentation)))

;;; Markers

(defun evil-global-marker-p (char)
  "Whether CHAR denotes a global marker."
  (or (and (>= char ?A) (<= char ?Z))
      (assq char (default-value 'evil-markers-alist))))

(defun evil-set-marker (char &optional pos advance)
  "Set the marker denoted by CHAR to position POS.
POS defaults to the current position of point.
If ADVANCE is t, the marker advances when inserting text at it;
otherwise, it stays behind."
  (interactive (list (read-char)))
  (let ((marker (evil-get-marker char t)) alist)
    (unless (markerp marker)
      (cond
       ((and marker (symbolp marker) (boundp marker))
        (set marker (or (symbol-value marker) (make-marker)))
        (setq marker (symbol-value marker)))
       ((functionp marker)
        (error "Cannot set special marker `%c'" char))
       ((evil-global-marker-p char)
        (setq alist (default-value 'evil-markers-alist)
              marker (make-marker))
        (evil-add-to-alist 'alist char marker)
        (setq-default evil-markers-alist alist))
       (t
        (setq marker (make-marker))
        (evil-add-to-alist 'evil-markers-alist char marker))))
    (add-hook 'kill-buffer-hook #'evil-swap-out-markers nil t)
    (set-marker-insertion-type marker advance)
    (set-marker marker (or pos (point)))))

(defun evil-get-marker (char &optional raw)
  "Return the marker denoted by CHAR.
This is either a marker object as returned by `make-marker',
a number, a cons cell (FILE . POS) with FILE being a string
and POS a number, or nil. If RAW is non-nil, then the
return value may also be a variable, a movement function,
or a marker object pointing nowhere."
  (let ((marker (if (evil-global-marker-p char)
                    (cdr-safe (assq char (default-value
                                           'evil-markers-alist)))
                  (cdr-safe (assq char evil-markers-alist)))))
    (save-excursion
      (if raw
          marker
        (when (and (symbolp marker) (boundp marker))
          (setq marker (symbol-value marker)))
        (when (functionp marker)
          (funcall marker)
          (setq marker (point)))
        (when (markerp marker)
          (if (eq (marker-buffer marker) (current-buffer))
              (setq marker (marker-position marker))
            (setq marker (and (marker-buffer marker) marker))))
        (when (or (numberp marker)
                  (markerp marker)
                  (and (consp marker)
                       (stringp (car marker))
                       (numberp (cdr marker))))
          marker)))))

(defun evil-swap-out-markers ()
  "Turn markers into file references when the buffer is killed."
  (and buffer-file-name
       (dolist (entry evil-markers-alist)
         (and (markerp (cdr entry))
              (eq (marker-buffer (cdr entry)) (current-buffer))
              (setcdr entry (cons buffer-file-name
                                  (marker-position (cdr entry))))))))
(put 'evil-swap-out-markers 'permanent-local-hook t)

(defun evil-jump-hook (&optional command)
  "Set jump point if COMMAND has a non-nil :jump property."
  (setq command (or command this-command))
  (when (evil-get-command-property command :jump)
    (evil-set-jump)))

(defun evil-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (unless (region-active-p)
    (evil-save-echo-area
      (mapc #'(lambda (marker)
                (set-marker marker nil))
            evil-jump-list)
      (setq evil-jump-list nil)
      (push-mark pos))))

(defun evil-get-register (register &optional noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil."
  (when (characterp register)
    (or (cond
         ((eq register ?\")
          (current-kill 0))
         ((and (<= ?0 register) (<= register ?9))
          (current-kill (- register ?0) t))
         ((eq register ?*)
          (let ((x-select-enable-primary t))
            (current-kill 0)))
         ((eq register ?+)
          (let ((x-select-enable-clipboard t))
            (current-kill 0)))
         (t
          (get-register register)))
        (unless noerror
          (error "Register `%c' is empty" register)))))

(defun evil-set-register (register text)
  "Set the contents of register REGISTER to TEXT."
  (cond
   ((eq register ?\")
    (kill-new text))
   ((and (<= ?0 register) (<= register ?9))
    (if (null kill-ring)
        (kill-new text)
      (let ((kill-ring-yank-pointer kill-ring-yank-pointer)
            interprogram-paste-function
            interprogram-cut-function)
        (current-kill (- register ?0))
        (setcar kill-ring-yank-pointer text))))
   ((eq register ?*)
    (let ((x-select-enable-primary t))
      (kill-new text)))
   ((eq register ?+)
    (let ((x-select-enable-clipboard t))
      (kill-new text)))
   (t
    (set-register register text))))

(defun evil-register-list ()
  "Returns an alist of all registers"
  (sort (append (mapcar #'(lambda (reg)
                            (cons reg (evil-get-register reg)))
                        '(?\" ?* ?+ ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
                register-alist)
        #'(lambda (reg1 reg2) (< (car reg1) (car reg2)))))

;;; Region

;; `set-mark' does too much at once
(defun evil-move-mark (pos)
  "Set buffer's mark to POS.
If POS is nil, delete the mark."
  (when pos
    (setq pos (evil-normalize-position pos)))
  (set-marker (mark-marker) pos))

(defun evil-save-transient-mark-mode ()
  "Save Transient Mark mode and make the new setup buffer-local.
The variables to save are listed in `evil-transient-vars'.
Their values are stored in `evil-transient-vals'.
See also `evil-restore-transient-mark-mode'."
  (dolist (var evil-transient-vars)
    (when (and (boundp var)
               (not (assq var evil-transient-vals)))
      (push (list var (symbol-value var)
                  (and (assq var (buffer-local-variables)) t))
            evil-transient-vals)
      (make-variable-buffer-local var)
      (put var 'permanent-local t))))

(defun evil-restore-transient-mark-mode ()
  "Restore Transient Mark mode from `evil-transient-vals'.
See also `evil-save-transient-mark-mode'."
  (let (entry local var val)
    (while (setq entry (pop evil-transient-vals))
      (setq var (pop entry)
            val (pop entry)
            local (pop entry))
      (unless local
        (kill-local-variable var))
      (unless (equal (symbol-value var) val)
        (if (fboundp var)
            (funcall var (if var 1 -1))
          (setq var val))))))

(defun evil-save-mark ()
  "Save the current mark, including whether it is transient.
See also `evil-restore-mark'."
  (unless evil-visual-previous-mark
    (setq evil-visual-previous-mark (mark t))
    (evil-save-transient-mark-mode)))

(defun evil-restore-mark ()
  "Restore the mark, including whether it was transient.
See also `evil-save-mark'."
  (when evil-visual-previous-mark
    (evil-restore-transient-mark-mode)
    (evil-move-mark evil-visual-previous-mark)
    (setq evil-visual-previous-mark nil)))

;; In theory, an active region implies Transient Mark mode, and
;; disabling Transient Mark mode implies deactivating the region.
;; In practice, Emacs never clears `mark-active' except in Transient
;; Mark mode, so we define our own toggle functions to make things
;; more predictable.
(defun evil-transient-mark (&optional arg)
  "Toggle Transient Mark mode.
Ensure that the region is properly deactivated.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if transient-mark-mode -1 1)))
  (cond
   ((< arg 1)
    (evil-active-region -1)
    ;; Transient Mark mode cannot be disabled
    ;; while CUA mode is enabled
    (when (fboundp 'cua-mode)
      (cua-mode -1))
    (when transient-mark-mode
      (transient-mark-mode -1)))
   (t
    (unless transient-mark-mode
      (evil-active-region -1)
      (transient-mark-mode 1)))))

(defun evil-active-region (&optional arg)
  "Toggle active region.
Ensure that Transient Mark mode is properly enabled.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if (region-active-p) -1 1)))
  (cond
   ((and (< arg 1))
    (when (or transient-mark-mode mark-active)
      (setq mark-active nil
            deactivate-mark nil)
      (when (boundp 'cua--explicit-region-start)
        (setq cua--explicit-region-start nil))
      (run-hooks 'deactivate-mark-hook)))
   (t
    (evil-transient-mark 1)
    (when deactivate-mark
      (setq deactivate-mark nil))
    (unless (mark t)
      (evil-move-mark (point)))
    (unless (region-active-p)
      (set-mark (mark t)))
    (when (boundp 'cua--explicit-region-start)
      (setq cua--explicit-region-start t)))))

(defmacro evil-with-transient-mark-mode (&rest body)
  "Execute BODY with Transient Mark mode.
Then restore Transient Mark mode to its previous setting."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         evil-transient-vals)
     (unwind-protect
         (progn
           (evil-save-transient-mark-mode)
           (evil-transient-mark 1)
           ,@body)
       (evil-restore-transient-mark-mode))))

(defmacro evil-with-active-region (beg end &rest body)
  "Execute BODY with an active region from BEG to END."
  (declare (indent 2)
           (debug t))
  `(let ((beg ,beg) (end ,end)
         evil-transient-vals)
     (evil-with-transient-mark-mode
       (save-excursion
         (evil-active-region 1)
         (evil-move-mark beg)
         (goto-char end)
         ,@body))))

(defun evil-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defun evil-apply-on-block (func beg end &rest args)
  "Call FUNC for each line of Visual Block selection.
The selection may be specified explicitly with BEG and END.
FUNC must take at least two arguments, the beginning and end of
each line. Extra arguments to FUNC may be passed via ARGS."
  (let (beg-marker end-marker left right eob)
    (save-excursion
      (evil-sort beg end)
      ;; calculate columns
      (setq left  (evil-column beg)
            right (evil-column end))
      ;; ensure LEFT < RIGHT
      (when (> left right)
        (evil-sort left right)
        (setq beg (save-excursion
                    (goto-char beg)
                    (move-to-column left)
                    (point))
              end (save-excursion
                    (goto-char end)
                    (move-to-column right)
                    (point))))
      (goto-char beg)
      (setq beg-marker (move-marker (make-marker) beg)
            end-marker (move-marker (make-marker) end))
      (set-marker-insertion-type beg-marker nil)
      (set-marker-insertion-type end-marker t)
      ;; apply FUNC on each line
      (while (progn
               (apply func
                      (save-excursion
                        (move-to-column left t)
                        (point))
                      (save-excursion
                        (move-to-column right t)
                        (point))
                      args)
               (forward-line 1)
               (and (prog1 (not eob)
                      (setq eob (eobp)))
                    (<= (point) end-marker))))
      (set-marker beg-marker nil)
      (set-marker end-marker nil))))

;;; Paste

(defun evil-yank-characters (beg end &optional register yank-handler)
  "Saves the characters defined by the region BEG and END in the kill-ring."
  (let ((text (filter-buffer-substring beg end)))
    (when yank-handler
      (setq text (propertize text 'yank-handler (list yank-handler))))
    (when register
      (evil-set-register register text))
    (kill-new text)))

(defun evil-yank-lines (beg end &optional register yank-handler)
  "Saves the lines in the region BEG and END into the kill-ring."
  (let* ((text (filter-buffer-substring beg end))
         (yank-handler (list (or yank-handler
                                 #'evil-yank-line-handler))))
    ;; Ensure the text ends with a newline. This is required
    ;; if the deleted lines were the last lines in the buffer.
    (when (or (zerop (length text))
              (/= (aref text (1- (length text))) ?\n))
      (setq text (concat text "\n")))
    (setq text (propertize text 'yank-handler yank-handler))
    (when register
      (evil-set-register register text))
    (kill-new text)))

(defun evil-yank-rectangle (beg end &optional register yank-handler)
  "Stores the rectangle defined by region BEG and END into the kill-ring."
  (let ((lines (list nil)))
    (apply-on-rectangle #'extract-rectangle-line beg end lines)
    ;; We remove spaces from the beginning and the end of the next.
    ;; Spaces are inserted explicitly in the yank-handler in order to
    ;; NOT insert lines full of spaces.
    (setq lines (nreverse (cdr lines)))
    ;; `text' is used as default insert text when pasting this rectangle
    ;; in another program, e.g., using the X clipboard.
    (let* ((yank-handler (list (or yank-handler
                                   #'evil-yank-block-handler)
                               lines
                               nil
                               #'evil-delete-yanked-rectangle))
           (text (propertize (mapconcat #'identity lines "\n")
                             'yank-handler yank-handler)))
      (when register
        (evil-set-register register text))
      (kill-new text))))

(defun evil-yank-line-handler (text)
  "Inserts the current text linewise."
  (let ((text (apply #'concat (make-list (or evil-paste-count 1) text)))
        (opoint (point)))
    (remove-list-of-text-properties
     0 (length text) yank-excluded-properties text)
    (cond
     ((eq this-command #'evil-paste-before)
      (evil-move-beginning-of-line)
      (evil-move-mark (point))
      (insert text)
      (setq evil-last-paste
            (list #'evil-paste-before
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (evil-set-marker ?\[ (mark))
      (evil-set-marker ?\] (1- (point)))
      (evil-exchange-point-and-mark)
      (back-to-indentation))
     ((eq this-command #'evil-paste-after)
      (evil-move-end-of-line)
      (evil-move-mark (point))
      (insert "\n")
      (insert text)
      (evil-set-marker ?\[ (1+ (mark)))
      (evil-set-marker ?\] (1- (point)))
      (delete-char -1) ; delete the last newline
      (setq evil-last-paste
            (list #'evil-paste-after
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (evil-move-mark (1+ (mark t)))
      (evil-exchange-point-and-mark)
      (back-to-indentation))
     (t
      (insert text)))))

(defun evil-yank-block-handler (lines)
  "Inserts the current text as block."
  (let ((count (or evil-paste-count 1))
        (col (if (eq this-command #'evil-paste-after)
                 (1+ (current-column))
               (current-column)))
        (current-line (line-number-at-pos (point)))
        (opoint (point))
        epoint)
    (dolist (line lines)
      ;; concat multiple copies according to count
      (setq line (apply #'concat (make-list count line)))
      ;; strip whitespaces at beginning and end
      (string-match "^ *\\(.*?\\) *$" line)
      (let ((text (match-string 1 line))
            (begextra (match-beginning 1))
            (endextra (- (match-end 0) (match-end 1))))
        ;; maybe we have to insert a new line at eob
        (while (< (line-number-at-pos (point))
                  current-line)
          (goto-char (point-max))
          (insert "\n"))
        (setq current-line (1+ current-line))
        ;; insert text unless we insert an empty line behind eol
        (unless (and (< (evil-column (line-end-position)) col)
                     (zerop (length text)))
          ;; if we paste behind eol, it may be sufficient to insert tabs
          (if (< (evil-column (line-end-position)) col)
              (move-to-column (+ col begextra) t)
            (move-to-column col t)
            (insert (make-string begextra ? )))
          (remove-list-of-text-properties 0 (length text)
                                          yank-excluded-properties text)
          (insert text)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string endextra ? )))
          (setq epoint (point)))
        (forward-line 1)))
    (setq evil-last-paste
          (list this-command
                evil-paste-count
                opoint
                (length lines)                   ; number of rows
                (* count (length (car lines))))) ; number of colums
    (evil-set-marker ?\[ opoint)
    (evil-set-marker ?\] (1- epoint))
    (goto-char opoint)
    (when (and (eq this-command #'evil-paste-after)
               (not (eolp)))
      (forward-char))))

(defun evil-delete-yanked-rectangle (nrows ncols)
  "Special function to delete the block yanked by a previous paste command."
  (let ((opoint (point))
        (col (if (eq last-command #'evil-paste-after)
                 (1+ (current-column))
               (current-column))))
    (dotimes (i nrows)
      (delete-region (save-excursion
                       (move-to-column col)
                       (point))
                     (save-excursion
                       (move-to-column (+ col ncols))
                       (point)))
      (unless (eobp) (forward-line)))
    (goto-char opoint)))

;; TODO: if undoing is disabled in the current buffer, paste-pop won't
;; work. Although this is probably not a big problem, because usually
;; buffers where `evil-paste-pop' may be useful have undoing enabled.
;; A solution would be to temporarily enable undo when pasting and
;; store the undo information in a special variable that does not
;; interfere with `buffer-undo-list'.
(defun evil-paste-pop (count)
  "Replace the just-yanked stretch of killed text with a different stretch.
This command is allowed only immediatly after a `yank',
`evil-paste-before', `evil-paste-after' or `evil-paste-pop'.
This command uses the same paste command as before, i.e., when
used after `evil-paste-after' the new text is also yanked using
`evil-paste-after', used with the same paste-count argument.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (memq last-command
                '(evil-paste-after
                  evil-paste-before))
    (error "Previous command was not an evil-paste: %s" last-command))
  (unless evil-last-paste
    (error "Previous paste command used a register"))
  (evil-undo-pop)
  (goto-char (nth 2 evil-last-paste))
  (current-kill count)
  (setq this-command (nth 0 evil-last-paste))
  (funcall (nth 0 evil-last-paste) (nth 1 evil-last-paste)))

(defun evil-paste-pop-next (count)
  "Same as `evil-paste-pop' but with negative argument."
  (interactive "p")
  (evil-paste-pop (- count)))

;;; Interactive forms

(defun evil-match-interactive-code (interactive &optional pos)
  "Match an interactive code at position POS in string INTERACTIVE.
Returns the first matching entry in `evil-interactive-alist', or nil."
  (let ((length (length interactive))
        (pos (or pos 0)))
    (catch 'done
      (dolist (entry evil-interactive-alist)
        (let* ((string (car entry))
               (end (+ (length string) pos)))
          (when (and (<= end length)
                     (string= string
                              (substring interactive pos end)))
            (throw 'done entry)))))))

(defun evil-concatenate-interactive-forms (&rest forms)
  "Concatenate interactive list expressions FORMS.
Returns a single expression where successive expressions
are joined, if possible."
  (let (result)
    (when forms
      (while (cdr forms)
        (cond
         ((null (car forms))
          (pop forms))
         ((and (eq (car (car forms)) 'list)
               (eq (car (cadr forms)) 'list))
          (setq forms (cons (append (car forms)
                                    (cdr (cadr forms)))
                            (cdr (cdr forms)))))
         (t
          (push (pop forms) result))))
      (when (car forms)
        (push (pop forms) result))
      (setq result (nreverse result))
      (cond
       ((null result))
       ((null (cdr result))
        (car result))
       (t
        `(append ,@result))))))

(defun evil-interactive-string (string)
  "Evaluate the interactive string STRING.
The string may contain extended interactive syntax.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `evil-define-command'."
  (let ((length (length string))
        (pos 0)
        code expr forms match plist prompt properties)
    (while (< pos length)
      (if (eq (aref string pos) ?\n)
          (setq pos (1+ pos))
        (setq match (evil-match-interactive-code string pos))
        (if (null match)
            (error "Unknown interactive code: `%s'"
                   (substring string pos))
          (setq code (car match)
                expr (car (cdr match))
                plist (cdr (cdr match))
                pos (+ pos (length code)))
          (when (functionp expr)
            (setq prompt
                  (substring string pos
                             (or (string-match "\n" string pos)
                                 length))
                  pos (+ pos (length prompt))
                  expr `(funcall ,expr ,prompt)))
          (setq forms (append forms (list expr))
                properties (append properties plist)))))
    (cons `(append ,@forms) properties)))

(defun evil-interactive-form (&rest args)
  "Evaluate interactive forms ARGS.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `evil-define-command'."
  (let (forms properties)
    (dolist (arg args)
      (if (not (stringp arg))
          (setq forms (append forms (list arg)))
        (setq arg (evil-interactive-string arg)
              forms (append forms (cdr (car arg)))
              properties (append properties (cdr arg)))))
    (cons (apply #'evil-concatenate-interactive-forms forms)
          properties)))

;;; Types

(defun evil-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (let (type)
    (cond
     ((overlayp object)
      (setq type (overlay-get object :type)))
     ((evil-range-p object)
      (setq type (nth 2 object)))
     ((listp object)
      (setq type (plist-get object :type)))
     ((commandp object)
      (setq type (evil-get-command-property object :type)))
     ((symbolp object)
      (setq type (get object 'type))))
    (setq type (or type default))
    (and (evil-type-p type) type)))

(defun evil-set-type (object type)
  "Set the type of OBJECT to TYPE.
For example, (evil-set-type 'next-line 'line)
will make `line' the type of the `next-line' command."
  (cond
   ((overlayp object)
    (overlay-put object :type type))
   ((evil-range-p object)
    (evil-set-range-type object type))
   ((listp object)
    (plist-put object :type type))
   ((commandp object)
    (evil-set-command-property object :type type))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun evil-type-property (type prop)
  "Return property PROP for TYPE."
  (evil-get-property evil-type-properties type prop))

(defun evil-type-p (sym)
  "Whether SYM is the name of a type."
  (assq sym evil-type-properties))

(defun evil-expand (beg end type &rest properties)
  "Expand BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'evil-transform
         ;; don't expand if already expanded
         (unless (plist-get properties :expanded) :expand)
         beg end type properties))

(defun evil-contract (beg end type &rest properties)
  "Contract BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'evil-transform :contract beg end type properties))

(defun evil-normalize (beg end type &rest properties)
  "Normalize BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'evil-transform :normalize beg end type properties))

(defun evil-transform
  (transform beg end type &rest properties)
  "Apply TRANSFORM on BEG and END with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list. If TRANSFORM is undefined,
return positions unchanged."
  (let* ((type (or type (evil-type properties)))
         (transform (when (and type transform)
                      (evil-type-property type transform))))
    (if transform
        (apply transform beg end properties)
      (apply #'evil-range beg end type properties))))

(defun evil-describe (beg end type &rest properties)
  "Return description of BEG and END with PROPERTIES.
If no description is available, return the empty string."
  (let* ((type (or type (evil-type properties)))
         (properties (plist-put properties :type type))
         (describe (evil-type-property type :string)))
    (or (when describe
          (apply describe beg end properties))
        "")))

;;; Ranges

(defun evil-range (beg end &optional type &rest properties)
  "Return a list (BEG END [TYPE] PROPERTIES...).
BEG and END are buffer positions (numbers or markers),
TYPE is a type as per `evil-type-p', and PROPERTIES is
a property list."
  (let ((beg (evil-normalize-position beg))
        (end (evil-normalize-position end)))
    (when (and (numberp beg) (numberp end))
      (append (list (min beg end) (max beg end))
              (when (evil-type-p type)
                (list type))
              properties))))

(defun evil-range-p (object)
  "Whether OBJECT is a range."
  (and (listp object)
       (>= (length object) 2)
       (numberp (nth 0 object))
       (numberp (nth 1 object))))

(defun evil-range-beginning (range)
  "Return beginning of RANGE."
  (when (evil-range-p range)
    (let ((beg (evil-normalize-position (nth 0 range)))
          (end (evil-normalize-position (nth 1 range))))
      (min beg end))))

(defun evil-range-end (range)
  "Return end of RANGE."
  (when (evil-range-p range)
    (let ((beg (evil-normalize-position (nth 0 range)))
          (end (evil-normalize-position (nth 1 range))))
      (max beg end))))

(defun evil-range-properties (range)
  "Return properties of RANGE."
  (when (evil-range-p range)
    (if (evil-type range)
        (nthcdr 3 range)
      (nthcdr 2 range))))

(defun evil-copy-range (range)
  "Return a copy of RANGE."
  (copy-sequence range))

(defun evil-set-range (range &optional beg end type &rest properties)
  "Set RANGE to have beginning BEG and end END.
The TYPE and additional PROPERTIES may also be specified.
If an argument is nil, it's not used; the previous value is retained.
See also `evil-set-range-beginning', `evil-set-range-end',
`evil-set-range-type' and `evil-set-range-properties'."
  (when (evil-range-p range)
    (let ((beg (or (evil-normalize-position beg)
                   (evil-range-beginning range)))
          (end (or (evil-normalize-position end)
                   (evil-range-end range)))
          (type (or type (evil-type range)))
          (plist (evil-range-properties range)))
      (evil-sort beg end)
      (setq plist (evil-concat-plists plist properties))
      (evil-set-range-beginning range beg)
      (evil-set-range-end range end)
      (evil-set-range-type range type)
      (evil-set-range-properties range plist)
      range)))

(defun evil-set-range-beginning (range beg &optional copy)
  "Set RANGE's beginning to BEG.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (setcar range beg)
  range)

(defun evil-set-range-end (range end &optional copy)
  "Set RANGE's end to END.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (setcar (cdr range) end)
  range)

(defun evil-set-range-type (range type &optional copy)
  "Set RANGE's type to TYPE.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (if type
      (setcdr (cdr range)
              (cons type (evil-range-properties range)))
    (setcdr (cdr range) (evil-range-properties range)))
  range)

(defun evil-set-range-properties (range properties &optional copy)
  "Set RANGE's properties to PROPERTIES.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (if (evil-type range)
      (setcdr (cdr (cdr range)) properties)
    (setcdr (cdr range) properties))
  range)

(defun evil-range-union (range1 range2 &optional type)
  "Return the union of the ranges RANGE1 and RANGE2.
If the ranges have conflicting types, use RANGE1's type.
This can be overridden with TYPE."
  (when (and (evil-range-p range1)
             (evil-range-p range2))
    (evil-range (min (evil-range-beginning range1)
                     (evil-range-beginning range2))
                (max (evil-range-end range1)
                     (evil-range-end range2))
                (or type
                    (evil-type range1)
                    (evil-type range2)))))

(defun evil-subrange-p (range1 range2)
  "Whether RANGE1 is contained within RANGE2."
  (and (evil-range-p range1)
       (evil-range-p range2)
       (<= (evil-range-beginning range2)
           (evil-range-beginning range1))
       (>= (evil-range-end range2)
           (evil-range-end range1))))

(defun evil-add-whitespace-to-range (range &optional dir pos regexp)
  "Add whitespace at one side of RANGE, depending on POS.
If POS is before the range, add trailing whitespace;
if POS is after the range, add leading whitespace.
If there is no trailing whitespace, add leading and vice versa.
If POS is inside the range, add trailing if DIR is positive and
leading if DIR is negative. POS defaults to point.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]+\"."
  (let* ((pos (or pos (point)))
         (dir (or (when (<= pos (evil-range-beginning range)) 1)
                  (when (>= pos (evil-range-end range)) -1)
                  dir 1))
         (regexp (or regexp "[ \f\t\n\r\v]+")))
    (save-excursion
      (save-match-data
        (goto-char pos)
        (cond
         ((if (< dir 0) (looking-back regexp) (not (looking-at regexp)))
          (or (evil-add-whitespace-after-range range regexp)
              (evil-add-whitespace-before-range range regexp)))
         (t
          (or (evil-add-whitespace-before-range range regexp)
              (evil-add-whitespace-after-range range regexp))))
        range))))

(defun evil-add-whitespace-before-range (range &optional regexp)
  "Add whitespace at the beginning of RANGE.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]+\".
Returns t if RANGE was successfully increased and nil otherwise."
  (let ((orig (evil-copy-range range))
        (regexp (or regexp "[ \f\t\n\r\v]+")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-beginning range))
        (when (looking-back regexp nil t)
          ;; exclude the newline on the preceding line
          (goto-char (match-beginning 0))
          (when (eolp) (forward-char))
          (evil-set-range range (point)))
        (not (evil-subrange-p range orig))))))

(defun evil-add-whitespace-after-range (range &optional regexp)
  "Add whitespace at the end of RANGE.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]+\".
Returns t if RANGE was successfully increased and nil otherwise."
  (let ((orig (evil-copy-range range))
        (regexp (or regexp "[ \f\t\n\r\v]+")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-end range))
        (when (looking-at regexp)
          (evil-set-range range nil (match-end 0)))
        (not (evil-subrange-p range orig))))))

(defun evil-adjust-whitespace-inside-range (range &optional shrink regexp)
  "Adjust whitespace inside RANGE.
Leading whitespace at the end of the line is excluded.
If SHRINK is non-nil, indentation may also be excluded,
and the trailing whitespace is adjusted as well.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]*\".
Returns t if RANGE was successfully adjusted and nil otherwise."
  (let ((orig (evil-copy-range range))
        (regexp (or regexp "[ \f\t\n\r\v]*")))
    (save-excursion
      (goto-char (evil-range-beginning range))
      (when (looking-at (concat regexp "$"))
        (forward-line)
        (if (and shrink evil-auto-indent)
            (back-to-indentation)
          (evil-move-beginning-of-line))
        (evil-set-range range (point) nil))
      (goto-char (evil-range-end range))
      (when (and shrink (looking-back (concat "^" regexp)))
        (evil-set-range range nil (line-end-position 0)))
      (not (evil-subrange-p orig range)))))

(defun evil-inner-object-range (count forward &optional backward type)
  "Return an inner text object range (BEG END) of COUNT objects.
If COUNT is positive, return objects following point;
if COUNT is negative, return objects preceding point.
FORWARD is a function which moves to the end of an object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument."
  (let* ((count (or count 1))
         (forward-func forward)
         (backward-func backward)
         (forward  (or forward
                       #'(lambda (count)
                           (funcall backward-func (- count)))))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward-func (- count)))))
         beg end)
    (when (< count 0)
      (evil-swap forward backward)
      (setq count (abs count)))
    (setq beg (save-excursion
                (funcall forward 1)
                (funcall backward 1)
                (point))
          end (save-excursion
                (funcall forward count)
                (point)))
    (evil-range beg end type)))

(defun evil-an-object-range (count forward &optional backward type newlines)
  "Return a text object range (BEG END) of COUNT objects with whitespace.
See `evil-inner-object-range' for more details."
  (let ((range (evil-inner-object-range count forward backward type)))
    (save-excursion
      (if newlines
          (evil-add-whitespace-to-range range count)
        (evil-with-restriction
            (save-excursion
              (goto-char (evil-range-beginning range))
              (line-beginning-position))
            (save-excursion
              (goto-char (evil-range-end range))
              (line-end-position))
          (evil-add-whitespace-to-range range count))))))

(defun evil-paren-range (count open close &optional exclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN is an opening character and CLOSE is a closing character.
If EXCLUSIVE is non-nil, OPEN and CLOSE are excluded from
the range; otherwise they are included.

This function uses Emacs' syntax table and can therefore only
handle single-character delimiters. To match whole strings,
use `evil-regexp-range'."
  (let ((open-regexp (regexp-quote (string open)))
        (close-regexp (regexp-quote (string close)))
        (count (or count 1))
        level beg end range)
    (save-excursion
      (if (or (evil-in-comment-p)
              (and (evil-in-string-p)
                   ;; TODO: this checks whether the current closing
                   ;; quote is indeed the end of a string. This is
                   ;; only a quick fix and should be done more
                   ;; carefully!
                   (or (/= (char-after) close)
                       (eobp)
                       (evil-in-string-p (1+ (point))))))
          ;; if in a comment, first look inside the comment only;
          ;; failing that, look outside it
          (or (evil-regexp-range count open-regexp close-regexp exclusive)
              (progn
                (evil-goto-min (evil-string-beginning)
                               (evil-comment-beginning))
                (evil-paren-range count open close exclusive)))
        (with-syntax-table (copy-syntax-table (syntax-table))
          (cond
           ((= count 0))
           ;; if OPEN is equal to CLOSE, handle as string delimiters
           ((eq open close)
            (modify-syntax-entry open "\"")
            (while (not (or (eobp) (evil-in-string-p)))
              (forward-char))
            (when (evil-in-string-p)
              (setq range (evil-range
                           (if exclusive
                               (1+ (evil-string-beginning))
                             (evil-string-beginning))
                           (if exclusive
                               (1- (evil-string-end))
                             (evil-string-end))))))
           (t
            ;; otherwise handle as open and close parentheses
            (modify-syntax-entry open (format "(%c" close))
            (modify-syntax-entry close (format ")%c" open))
            ;; Is point next to a parenthesis?
            (if (< count 0)
                (when (looking-back close-regexp)
                  (backward-char))
              (when (looking-at open-regexp)
                (forward-char)))
            ;; find OPEN
            (evil-motion-loop (nil count level)
              (condition-case nil
                  (while (progn
                           (backward-up-list 1)
                           (not (looking-at open-regexp))))
                (error nil)))
            (when (/= level count)
              (setq beg (if exclusive (1+ (point)) (point)))
              ;; find CLOSE
              (forward-sexp)
              (setq end (if exclusive (1- (point)) (point)))
              (setq range (evil-range beg end))
              (when exclusive
                (evil-adjust-whitespace-inside-range
                 range (not (eq evil-this-operator #'evil-delete)))))))
          range)))))

(defun evil-quote-range (count open close &optional exclusive)
  "Return a range (BEG END) of COUNT quotes.
OPEN is the opening quote, CLOSE is the closing quote (often both
are equal). If EXCLUSIVE is non-nil, OPEN and CLOSE are excluded
from the range unless COUNT is 2 in which case they are included;
otherwise they are included as well as any succeeding (or
preceding if no whitespace follows) white space."
  (if exclusive
      (if (and count (= count 2))
          (evil-paren-range 1 open close nil)
        (evil-paren-range count open close t))
    (let ((range (evil-paren-range count open close nil)))
      (save-excursion
        (if (progn
              (goto-char (evil-range-end range))
              (looking-at "[[:space:]]+"))
            (evil-range (evil-range-beginning range) (match-end 0))
          (goto-char (evil-range-beginning range))
          (skip-chars-backward "[:space:]")
          (evil-range (point) (evil-range-end range)))))))

(defun evil-regexp-range (count open close &optional exclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN is a regular expression matching the opening sequence,
and CLOSE is a regular expression matching the closing sequence.
If EXCLUSIVE is non-nil, OPEN and CLOSE are excluded from
the range; otherwise they are included. See also `evil-paren-range'."
  (evil-with-or-without-comment
    (let ((either (format "\\(%s\\)\\|\\(%s\\)" open close))
          (count (or count 1))
          (level 0)
          beg end range)
      (save-excursion
        (save-match-data
          ;; Is point inside a delimiter?
          (when (evil-in-regexp-p either)
            (if (< count 0)
                (goto-char (match-end 0))
              (goto-char (match-beginning 0))))
          ;; Is point next to a delimiter?
          (if (< count 0)
              (when (looking-back close)
                (goto-char (match-beginning 0)))
            (when (looking-at open)
              (goto-char (match-end 0))))
          ;; find beginning of range
          (while (and (< level (abs count))
                      (re-search-backward either nil t))
            (if (looking-at open)
                (setq level (1+ level))
              ;; found a CLOSE, so need to find another OPEN first
              (setq level (1- level))))
          ;; find end of range
          (when (> level 0)
            (forward-char)
            (setq level 1
                  beg (if exclusive
                          (match-end 0)
                        (match-beginning 0)))
            (while (and (> level 0)
                        (re-search-forward either nil t))
              (if (looking-back close)
                  (setq level (1- level))
                ;; found an OPEN, so need to find another CLOSE first
                (setq level (1+ level))))
            (when (= level 0)
              (setq end (if exclusive
                            (match-beginning 0)
                          (match-end 0)))
              (setq range (evil-range beg end))))
          range)))))

(defun evil-xml-range (&optional count exclusive)
  "Return a range (BEG END) of COUNT matching XML tags.
If EXCLUSIVE is non-nil, the tags themselves are excluded
from the range."
  (evil-regexp-range
   count "<\\(?:[^/ ]\\(?:[^>]*?[^/>]\\)?\\)?>" "</[^>]+?>" exclusive))

(defun evil-expand-range (range &optional copy)
  "Expand RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (evil-copy-range range)))
  (unless (plist-get (evil-range-properties range) :expanded)
    (setq range (evil-transform-range :expand range)))
  range)

(defun evil-contract-range (range &optional copy)
  "Contract RANGE according to its type.
Return a new range if COPY is non-nil."
  (evil-transform-range :contract range copy))

(defun evil-normalize-range (range &optional copy)
  "Normalize RANGE according to its type.
Return a new range if COPY is non-nil."
  (evil-transform-range :normalize range copy))

(defun evil-transform-range (transform range &optional copy)
  "Apply TRANSFORM to RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (evil-copy-range range)))
  (when (evil-type range)
    (apply #'evil-set-range range
           (apply #'evil-transform transform range)))
  range)

(defun evil-describe-range (range)
  "Return description of RANGE.
If no description is available, return the empty string."
  (apply #'evil-describe range))

;;; Undo

(defun evil-start-undo-step (&optional continue)
  "Start a undo step.
All following buffer modifications are grouped together as a
single action. If CONTINUE is non-nil, preceding modifications
are included. The step is terminated with `evil-end-undo-step'."
  (when (listp buffer-undo-list)
    (if evil-undo-list-pointer
        (evil-refresh-undo-step)
      (unless (or continue (null (car-safe buffer-undo-list)))
        (undo-boundary))
      (setq evil-undo-list-pointer (or buffer-undo-list t)))))

(defun evil-end-undo-step (&optional continue)
  "End a undo step started with `evil-start-undo-step'.
Adds an undo boundary unless CONTINUE is specified."
  (when evil-undo-list-pointer
    (evil-refresh-undo-step)
    (unless continue
      (undo-boundary))
    (remove-hook 'post-command-hook #'evil-refresh-undo-step t)
    (setq evil-undo-list-pointer nil)))

(defun evil-refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `evil-undo-list-pointer' are removed
to make the entries undoable as a single action.
See `evil-start-undo-step'."
  (when evil-undo-list-pointer
    (setq buffer-undo-list
          (evil-filter-list #'null buffer-undo-list
                            evil-undo-list-pointer)
          evil-undo-list-pointer (or buffer-undo-list t))))

(defmacro evil-with-undo (&rest body)
  "Execute BODY with enabled undo.
If undo is disabled in the current buffer, the undo information
is stored in `evil-temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun)
           (debug t))
  `(unwind-protect
       (let (buffer-undo-list)
         ,@body
         (setq evil-temporary-undo (cons nil buffer-undo-list)))
     (unless (eq buffer-undo-list t)
       ;; undo is enabled, so update the global buffer undo list
       (setq buffer-undo-list
             ;; prepend new undos (if there are some)
             (if (cdr evil-temporary-undo)
                 (nconc evil-temporary-undo buffer-undo-list)
               buffer-undo-list)
             evil-temporary-undo nil))))

(defmacro evil-with-single-undo (&rest body)
  "Execute BODY as a single undo step."
  (declare (indent defun)
           (debug t))
  `(let (evil-undo-list-pointer)
     (evil-with-undo
       (unwind-protect
           (progn
             (evil-start-undo-step)
             ,@body)
         (evil-end-undo-step)))))

(defun evil-undo-pop ()
  "Undo the last buffer change.
Removes the last undo information from `buffer-undo-list'.
If undo is disabled in the current buffer, use the information
in `evil-temporary-undo' instead."
  (let ((paste-undo (list nil)))
    (let ((undo-list (if (eq buffer-undo-list t)
                         evil-temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (error "Can't undo previous change"))
      (while (and undo-list (null (car undo-list)))
        (pop undo-list)) ; remove nil
      (while (and undo-list (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo)))
        (evil-save-echo-area
          (undo)))
      (if (eq buffer-undo-list t)
          (setq evil-temporary-undo nil)
        (setq buffer-undo-list undo-list)))))

;;; Substitute

(defun evil-downcase-first (str)
  "Return STR with the first letter downcased."
  (if (zerop (length str))
      str
    (concat (downcase (substring str 0 1))
            (substring str 1))))

(defun evil-upcase-first (str)
  "Return STR with the first letter upcased."
  (if (zerop (length str))
      str
    (concat (upcase (substring str 0 1))
            (substring str 1))))

(defun evil-compile-subreplacement (to &optional start)
  "Convert a regexp replacement TO to Lisp from START until \\e or \\E.
Returns a pair (RESULT . REST). RESULT is a list suitable for
`perform-replace' if necessary, the original string if not.
REST is the unparsed remainder of TO."
  (let ((regexp "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[^0-9&]")
        (rest "") char list)
    (save-match-data
      (if (not (string-match regexp to))
          (cons to "")
        (while (progn
                 (setq start (match-end 0))
                 (push (substring to 0 (- start 2)) list)
                 (setq char (aref to (1- start))
                       to (substring to start))
                 (cond
                  ((eq char ?#)
                   (push '(number-to-string replace-count) list))
                  ((memq char '(?e ?E))
                   (setq rest to to ""))
                  ((memq char '(?l ?L ?u ?U))
                   (let ((result (evil-compile-subreplacement to))
                         (func (cdr (assoc char
                                           '((?l . evil-downcase-first)
                                             (?L . downcase)
                                             (?u . evil-upcase-first)
                                             (?U . upcase))))))
                     (push `(,func
                             (replace-quote
                              (evil-match-substitute-replacement
                               ,(car result) t))) list)
                     (setq to (cdr result))))
                  ((eq char ?,)
                   (setq start (read-from-string to))
                   (push `(replace-quote ,(car start)) list)
                   (let ((end
                          ;; swallow a space after a symbol
                          (if (and (or (symbolp (car start))
                                       ;; swallow a space after 'foo,
                                       ;; but not after (quote foo)
                                       (and (eq (car-safe (car start)) 'quote)
                                            (not (= ?\( (aref to 0)))))
                                   (eq (string-match " " to (cdr start))
                                       (cdr start)))
                              (1+ (cdr start))
                            (cdr start))))
                     (setq to (substring to end))))
                  ((eq char ?\\)
                   (push "\\\\" list))
                  (t ; let Emacs unescape the character
                   (push (car-safe (read-from-string
                                    (format "\"\\%c\"" char))) list)))
                 (string-match regexp to)))
        (setq to (nreverse (delete "" (cons to list))))
        (replace-match-string-symbols to)
        (cons (if (cdr to)
                  (cons 'concat to)
                (car to))
              rest)))))

(defun evil-compile-replacement (to)
  "Maybe convert a regexp replacement TO to Lisp.
Returns a list suitable for `perform-replace' if necessary,
the original string if not."
  (when (stringp to)
    (save-match-data
      (cons 'replace-eval-replacement
            (car (evil-compile-subreplacement to))))))

(defun evil-replace-match (replacement &optional fixedcase string)
  "Replace text match by last search with REPLACEMENT.
If REPLACEMENT is an expression it will be evaluated to compute
the replacement text, otherwise the function behaves as
`replace-match'."
  (if (stringp replacement)
      (replace-match replacement fixedcase nil string)
    (replace-match (funcall (car replacement)
                            (cdr replacement)
                            0)
                   fixedcase nil string)))

(defun evil-match-substitute-replacement (replacement &optional fixedcase string)
  "Return REPLACEMENT as it will be inserted by `evil-replace-match'."
  (if (stringp replacement)
      (match-substitute-replacement replacement fixedcase nil string)
    (match-substitute-replacement (funcall (car replacement)
                                           (cdr replacement)
                                           0)
                                  fixedcase nil string)))

;;; Alignment

(defun evil-justify-lines (beg end justify position)
  "Justifes all lines in a range.
BEG and END specify the range of those lines to be
justified. JUSTIFY is either 'left, 'right or 'center according
to the justification type. POSITION is the maximal text width for
right and center justification or the column at which the lines
should be left-aligned for left justification."
  (let ((fill-column position)
        adaptive-fill-mode fill-prefix)
    (evil-with-restriction
        (save-excursion
          (goto-char beg)
          (line-beginning-position))
        (save-excursion
          (goto-char end)
          (if (bolp)
              (line-end-position 0)
            (line-end-position)))
      (goto-char (point-min))
      (while (progn
               (if (eq justify 'left)
                   (indent-line-to position)
                 (when (re-search-forward "^[[:space:]]*" nil t)
                   (delete-region (match-beginning 0)
                                  (match-end 0)))
                 (justify-current-line justify nil t))
               (and (zerop (forward-line)) (bolp))))
      (goto-char (point-min))
      (back-to-indentation))))

(provide 'evil-common)

;;; evil-common.el ends here
