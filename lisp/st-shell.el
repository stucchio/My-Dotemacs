(defun if-void (arg default)
  (if (boundp arg)
      (eval arg)
    default
      )
)

(defgroup st-shell nil
  "Simple support for having multiple shells open."
  :group 'languages)

(defcustom st-shell-shell-function '(shell)
  "Command called to create shell"
  :group 'st-shell)

(defcustom st-shell-name "*shell*" "The name of the buffer opened by the shell command."
  :type 'string
  :group 'st-shell)

(defun st-shell-function () "This function opens the appropriate shell." (eval st-shell-shell-function) )
;;;(defvar st-shell-function `(shell) ) ;;; Defines the shell. ('shell) or ('eshell)
;(defvar st-shell-name "*eshell*") ;;; Name of default shell or eshell buffer

(defvar st-shell-ring (make-ring 100) "This stores a bunch of buffers, which are shells created by st-shell." )
(setq st-shell-index 0 )
(defvar st-shell-last-buffer nil)

(defun st-shell-is-current-buffer-current-st-shell (&optional ignored)
  "Checks if current buffer is the current st-shell."
  (eq (current-buffer) (ring-ref st-shell-ring st-shell-index))
)

(defun st-shell-switch-to-current-shell (&optional ignored)
  "Switch to shell buffer."
  (if (buffer-live-p (ring-ref st-shell-ring st-shell-index))
      (switch-to-buffer (ring-ref st-shell-ring st-shell-index))
    )
)

(defun st-shell-current-shell (&optional ignored)
  "Returns the current st-shell."
  (ring-ref st-shell-ring st-shell-index)
)

(defun st-shell-switch-to-next-live-shell (&optional ignored)
  "Switches to the next live shell. Creates one if none exists."
  (interactive "p")
  (let ((still-looking t)
	(empty nil))
    (while (and still-looking (not empty))
      (if (ring-empty-p st-shell-ring)
	  (progn
	    (setq empty t)
	    (st-shell 1)
	    )
	(progn
	  (if (buffer-live-p (ring-ref st-shell-ring st-shell-index))
	      (progn
		(setq st-shell-index (+ st-shell-index 1))
		(switch-to-buffer (ring-ref st-shell-ring st-shell-index))
		(setq still-looking nil)
		)
	    (ring-remove st-shell-ring st-shell-index)
	    )
	  )
	)
      )
    )
)

;;;###autoload
(defun st-shell-go-back (&optional ignored)
  "Switch to buffer st-shell-last-buffer."
  (interactive "p")
  (if (buffer-live-p st-shell-last-buffer)
      (switch-to-buffer st-shell-last-buffer)
    (message "Last buffer visited before st-shell is gone. Nothing to go back to..")
     ))


;;;###autoload
(defun st-shell-switch (&optional ignored)
  "If current buffer is not an st-shell, switch to current st-shell buffer. Otherwise, switch to next st-shell buffer."
  (interactive "p")
  (progn
    (setq st-shell-last-buffer (current-buffer))
    (let ((still-looking t)
	  (empty nil))
      (if (ring-empty-p st-shell-ring)
	  (st-shell 1)
	(if (and (buffer-live-p (st-shell-current-shell) ) 
	     (not (eq (st-shell-current-shell) (current-buffer))))
	(switch-to-buffer (st-shell-current-shell))
      (st-shell-switch-to-next-live-shell)
      )
    )
  )))



;;;###autoload
(defun st-shell (&optional numshells)
  "Creates a shell buffer. If one already exists, this creates a new buffer, with the name '*shell*<n>', where n is chosen by the function generate-new-buffer-name."
  (interactive "p")
  (progn 
    (setq st-shell-last-buffer (current-buffer))
    (dotimes (i (if-void 'numshells 1) nil)
      (let ( (tempname (generate-new-buffer-name "*tempshell*")) 
	     (new-buff-name (generate-new-buffer-name st-shell-name))
	     (localdir default-directory)
	     )
	(if (eq (get-buffer st-shell-name) nil) ;If a 
	    (progn
	      (st-shell-function)
	      (process-send-string (get-buffer-process new-buff-name) (concat "cd " localdir "\n"))
	      (ring-insert st-shell-ring (current-buffer) )
	      (setq st-shell-index (+ st-shell-index 1))
	      )
	  (progn
	    (interactive)
	    (st-shell-function)
	    (rename-buffer tempname)
	    (st-shell-function)
	    (rename-buffer new-buff-name )
	    (switch-to-buffer tempname)
	    (rename-buffer st-shell-name)
	(switch-to-buffer new-buff-name)
	(process-send-string (get-buffer-process new-buff-name) (concat "cd " localdir "\n"))
	(ring-insert st-shell-ring (current-buffer) )
	(setq st-shell-index (+ st-shell-index 1))
	)
	  )
	)
      )
    )
  )

(defun shell-with-name (name)
  "Creates a shell with name given by the first argument, and switches to it. If a buffer with name already exists, we simply switch to it."
  (let ((buffer-of-name (get-buffer name)) 
	(tempname (generate-new-buffer-name "*tempshell*") ) )
    (cond ((bufferp buffer-of-name) ;If the buffer exists, switch to it (assume it is a shell)
	   (switch-to-buffer name))
	  ( (bufferp (get-buffer st-shell-name))
	  (progn
	    (st-shell-function)
	    (rename-buffer tempname)
	    (st-shell-function)
	    (rename-buffer name)
	    (switch-to-buffer tempname)
	    (rename-buffer st-shell-name)
	    (switch-to-buffer name)))
	  ( t
	    (progn
	      (st-shell-function)
	      (rename-buffer name)
	      )
	    )
	  )
    )
  )

(provide 'st-shell)

