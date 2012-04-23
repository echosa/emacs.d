(require 'emacs-setup-util)

;;; **************
;;; CUSTOMIZATIONS
;;; **************

(defgroup emacs-setup-layout nil
  "Emacs setup layout customizations."
  :group 'emacs-setup)

(defcustom emacs-setup-set-frame-size nil
  "Whether or not to set frame size when setting up emacs.
Uses emacs-setup-frame-height and emacs-setup-frame-width."
  :group 'emacs-setup-layout
  :type 'boolean)

(defcustom emacs-setup-frame-height 0
  "Height, in rows, of the emacs frame after setup. 0 means don't set the 
height."
  :group 'emacs-setup-layout
  :type 'integer)

(defcustom emacs-setup-frame-width 0
  "Width, in rows, of the emacs frame after setup. 0 means don't set the width."
  :group 'emacs-setup-layout
  :type 'integer)

(defcustom emacs-setup-frames nil
  "Whether or not to use emacs-setup-frame-configuration to setup frames."
  :group 'emacs-setup-layout
  :type 'boolean)

(defcustom emacs-setup-frame-configurations nil
  "Frame settings alist. First frame is the starting default frame."
  :group 'emacs-setup-layout
  :type '(alist :key-type (string :tag "Configuration Name: ")
                :value-type (repeat :tag "Frame Layout: " (string))))

(defcustom emacs-setup-default-frame-configuration nil
  "Frame configuration to load if emacs-setup-frames is t.
This may also be the name of a function that returns a configuration
name.  

Example:

\(defun emacs-setup-get-default-frame-configuration ()
  (cond ((= (x-display-pixel-width) 2560) \"Cinema Display\")
        ((string= system-name \"simon\") \"Dell 24\\\" Fullscreen\")
        ((member system-name '(\"Book.local\" \"book.ehtech.in\")) 
         \"MacBook Pro Fullscreen\")
        (t \"Dell 24\\\"\")))"
  :group 'emacs-setup-layout
  :type 'string)

(defcustom emacs-setup-layout-dir nil
  "Directory where frame revive layouts should be saved."
  :group 'emacs-setup-layout
  :type 'directory)

(defcustom emacs-setup-load-layout nil
  "If t, load emacs-setup-layout-file using revive.el."
  :group 'emacs-setup-layout
  :type 'boolean)

(defcustom emacs-setup-fullscreen nil
  "If t, make emacs fullscreen prior to loading layout (if set) using 
`ns-toggle-fullscreen`."
  :group 'emacs-setup-layout
  :type 'boolean)

(defcustom emacs-setup-layout-buffer-list nil
  "This is a list of buffer names to load in the windows setup by revive.el. 
The order in which the list is entered is the order the buffers will be placed 
in windows. For example, the second item in the list will be in the second 
window."
  :group 'emacs-setup-layout
  :type '(alist :key-type (string :tag "Layout File: ")
                :value-type (repeat :tag "Buffer Name: " (string))))

(defcustom emacs-setup-load-window-number-mode nil
  "If t, use window-number-mode."
  :group 'emacs-setup-layout
  :type 'boolean)

;;; *********
;;; FUNCTIONS
;;; *********

;; *******
;; LOADING
;; *******
(defun emacs-setup-get-layout-filename ()
  (let* ((screen-width (if (fboundp 'x-display-pixel-width)
                           (x-display-pixel-width)
                         (display-pixel-width)))
         (screen-height (if (fboundp 'x-display-pixel-height)
                            (x-display-pixel-height)
                          (display-pixel-height)))
         (top-pixel (assoc 'top (frame-parameters (selected-frame))))
         (top-pixel (if top-pixel (eval (cdr top-pixel)) 0))
         (left-pixel (assoc 'left (frame-parameters (selected-frame))))
         (left-pixel (if left-pixel (eval (cdr left-pixel)) 0)))
    (concat (number-to-string screen-width) "-" 
            (number-to-string screen-height) "-"
            (number-to-string (frame-width)) "-"
            (number-to-string (frame-height)) "-"
            (number-to-string top-pixel) "-"
            (number-to-string left-pixel) ".el")))

(defun emacs-setup-get-layout-file ()
  (convert-standard-filename
   (concat
    emacs-setup-layout-dir "/" (emacs-setup-get-layout-filename))))

(defun emacs-setup-layout ()
  (when emacs-setup-load-window-number-mode
    (unless (featurep 'window-number)
      (condition-case nil
          (require 'window-number)
        (error
         (message "Couldn't require window-number."))))
    (when (fboundp 'window-number-mode)
      (window-number-mode t)))
  (if (and emacs-setup-frames
           emacs-setup-frame-configurations
           emacs-setup-default-frame-configuration)
      (emacs-setup-frames)
    (emacs-setup-set-frame-size)
    (emacs-setup-windows)))

(defun emacs-setup-get-default-frame-configuration ()
  "This function will check to see if the value of
``emacs-setup-default-frame-configuration'' is a function.  If it
is, it will evalulate the function and return its value (if it is
a string).  Otherwise, it will return the base value."
  (let* ((func (intern emacs-setup-default-frame-configuration))
         (test-val (when (fboundp func) (funcall func))))
    (if (stringp test-val)
        test-val
      emacs-setup-default-frame-configuration)))

(defun emacs-setup-prompt-for-configuration ()
  "This function abstracts prompting for an emacs-setup frame
configuration."
  (let ((default-config (emacs-setup-get-default-frame-configuration)))
    (completing-read (concat "Configuration (" default-config "): ")
                     (mapcar 'first emacs-setup-frame-configurations)
                     nil 'confirm-after-completion nil nil default-config)))

(defun emacs-setup-frames (&optional config)
  (interactive `(,(emacs-setup-prompt-for-configuration)))
  (let* ((config (or config (emacs-setup-get-default-frame-configuration)))
         (config (rest (assoc config emacs-setup-frame-configurations)))
         (main (first config))
         (other (rest config)))
    (emacs-setup-frame main)
    (dolist (frame other)
      (emacs-setup-frame frame t))))

(defun emacs-setup-frame (frame-layout-filename &optional make-frame)
  (let* ((frame frame-layout-filename)
         (frame-info (split-string (substring frame 0 -3) "-" t))
         (frame-width (string-to-number (third frame-info)))
         (frame-height (string-to-number (fourth frame-info)))
         (frame-top (string-to-number (fifth frame-info)))
         (frame-left (string-to-number (sixth frame-info))))
    (when (and (fboundp 'ns-toggle-fullscreen)
               (or (and (zerop frame-top)
                        (not (zerop (frame-parameter (selected-frame) 'top))))
                   (and (not (zerop frame-top))
                        (zerop (frame-parameter (selected-frame) 'top)))))
      (ns-toggle-fullscreen))    
    (if make-frame
        (select-frame (make-frame `((width . ,frame-width)
                                    (height . ,frame-height))))
      (set-frame-size (selected-frame) frame-width frame-height))
    (set-frame-position (selected-frame) frame-left frame-top)
    (emacs-setup-windows)))

(defun emacs-setup-windows ()
  (when (and emacs-setup-load-layout
             (not (boundp 'aquamacs-version))
             (file-exists-p (emacs-setup-get-layout-file)))
    (emacs-setup-revive-resume (emacs-setup-get-layout-filename))
    (if (emacs-setup-thing-exists 'emacs-setup-layout-buffer-list)
        (if (fboundp 'window-number-goto)
            (condition-case nil
                (progn
                  (let ((window-count 1))
                    (dolist (window
                             (cdr (assoc (emacs-setup-get-layout-filename)
                                         emacs-setup-layout-buffer-list)))
                      (window-number-goto window-count)
                      (switch-to-buffer window)
                      (setq window-count (1+ window-count)))
                    (window-number-goto 1)))
              (error
               (message "Error setting up windows!")
               (delete-other-windows)
               (switch-to-buffer "*scratch*")))
          (message
           "Not setting up windows: window-number-goto function not found."))
      (message
       "Not setting up windows: emacs-setup-layout-buffer-list not set."))))

(defun emacs-setup-set-frame-size ()
  (if (and emacs-setup-fullscreen
           (fboundp 'ns-toggle-fullscreen))
      (ns-toggle-fullscreen)
    (when (and emacs-setup-set-frame-size
               (fboundp 'window-system)
               (not (boundp 'aquamacs-version)))
      (when emacs-setup-frame-height
        (set-frame-height (selected-frame) emacs-setup-frame-height))
      (when emacs-setup-frame-width
        (set-frame-width (selected-frame) emacs-setup-frame-width)))))

(defun emacs-setup-revive-resume (layout-file)
  (if (and (not (fboundp 'resume)) (not (require 'revive)))
      (message "Resume function not loaded. Please load revive.el.")
    (when (find-file (convert-standard-filename "~/.revive.el"))
      (write-file (convert-standard-filename "~/.revive.el.bak") nil)
      (kill-buffer))
    (find-file (emacs-setup-get-layout-file))
    (write-file (convert-standard-filename "~/.revive.el") nil)
    (kill-buffer)
    (resume)))

;; ******
;; SAVING
;; ******
(defun emacs-setup-save-frame ()
  "Save the current frame setup."
  (interactive)
  (emacs-setup-revive-save (emacs-setup-get-layout-file))
  (emacs-setup-save-frame-buffers))

(defun emacs-setup-save-frame-configuration ()
  (interactive)
  (let* ((config-name (catch 'valid
                        (while t
                          (let ((config-name 
                                 (emacs-setup-prompt-for-configuration)))
                            (when (not (zerop (length config-name)))
                              (throw 'valid config-name))))))
        user-frames
        (current-frame (selected-frame)))
    (dolist (frame (frame-list))
      (select-frame-by-name (car (emacs-setup-get-frame-info frame)))
      (message (emacs-setup-get-layout-filename))
      (add-to-list 'user-frames
                   (emacs-setup-get-layout-filename)
                   (eq frame current-frame))
      (emacs-setup-save-frame))
    (let* ((current-config emacs-setup-frame-configurations)
           (this-config (assoc config-name current-config)))
      (when this-config
        (setq current-config (remove this-config current-config)))
      (add-to-list 'current-config (cons config-name user-frames) t)
      (set-variable 'emacs-setup-frame-configurations current-config)
      (customize-save-variable 'emacs-setup-frame-configurations
                               emacs-setup-frame-configurations))
    (select-frame current-frame)))
    
(defun emacs-setup-revive-save (layout-file)
  (if (not (fboundp 'save-current-configuration))
      (message "Save configuration function not loaded. Please load revive.el.")
    (when (find-file (convert-standard-filename "~/.revive.el"))
      (write-file (convert-standard-filename "~/.revive.el.bak") nil)
      (kill-buffer))
    (save-current-configuration)
    (find-file (convert-standard-filename "~/.revive.el"))
    (write-file (convert-standard-filename layout-file) nil)
    (kill-buffer)))

(defun emacs-setup-save-frame-buffers ()
  (let* ((layout-buffer-list emacs-setup-layout-buffer-list)
         (this-buffer-list
          (assoc (emacs-setup-get-layout-filename) layout-buffer-list)))
    (when this-buffer-list
      (setq layout-buffer-list (remove this-buffer-list layout-buffer-list)))
    (add-to-list 'layout-buffer-list 
                 (cons (emacs-setup-get-layout-filename)
                       (emacs-setup-get-frame-buffers))
                 t)
    (set-variable 'emacs-setup-layout-buffer-list layout-buffer-list)
    (customize-save-variable 'emacs-setup-layout-buffer-list
                             emacs-setup-layout-buffer-list)))

(defun emacs-setup-get-frame-buffers ()
  (let (buffers
        (window-count 1)
        (current-window (window-number)))
    (condition-case nil
        (while (window-number-goto window-count)
          (setq buffers (append buffers (list (buffer-name))))
          (setq window-count (1+ window-count)))
      (error
       nil))
    (window-number-goto current-window)
    buffers))

(defun emacs-setup-get-frame-info (&optional frame)
  (unless frame
    (setq frame (selected-frame)))
  `(,(cdr (assoc 'name (frame-parameters frame))) .
    (,(cdr (assoc 'top (frame-parameters frame)))
     ,(cdr (assoc 'left (frame-parameters frame)))
     ,(cdr (assoc 'height (frame-parameters frame)))
     ,(cdr (assoc 'width (frame-parameters frame))))))

(provide 'emacs-setup-layout)