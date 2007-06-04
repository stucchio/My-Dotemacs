;;;###autoload
(defun zap-up-to-char (arg char) ;defined in xemacs, but not emacs
  "Kill up to the ARG'th occurrence of CHAR."
  (interactive "p\ncDelete to: ")
  (kill-region (point) 
	       (progn (search-forward (char-to-string char) nil nil arg)
		      (backward-char)
		      (point))))

;;;###autoload
(defun search-google ()
  "Prompt for a query in the minibuffer, launch the web browser and query google."
  (interactive)
  (let ((search (read-from-minibuffer "Google Search: ")))
    (browse-url (concat "http://www.google.com/search?q=" search))))
;;;###autoload
(defun backward-zap-to-char (c)
  "Just like zap-to-char, except works backwards."
  (interactive "cBackward Zap to Char: ")
  (zap-to-char -1 c)
  )
;;;###autoload
(defun backward-zap-up-to-char (c)
  "Just like zap-up-to-char, except works backwards."
  (interactive "cBackward Zap to Char: ")
  (zap-up-to-char -1 c)
  )

;;;###autoload
(defun insert-time-at-point ()
  "Inserts a current time-stamp (calculated by current-time-string) at point."
  (interactive)
  (insert (current-time-string)   )
  )

;;;###autoload
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun start-or-end-kbd-macro ()
  "Starts recording a keyboard macro, or if already recording, stops recording it."
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)
    )
  )

(defun lisp-files-in-directory (d)
  "Return a list of lisp files in the directory d. d must take the form '/blah/blah', not '/blah/blah/'."
  (split-string (shell-command-to-string (concat "ls " d "/*.el"))  "\n")
  )

(defun byte-compile-whole-directory (d)
  "This function byte compiles all the *.el files in directory d. d must take the form '/blah/blah', not '/blah/blah/'."
  (map 'list 'byte-compile-file
       (lisp-files-in-directory d)
     ))


;;;###autoload
(icicle-define-command
   icicle-recent-file "Open recently used files."
   find-file
   "Recent files: " recentf-list )

;;;###autoload
(defun kill-buffer-eventually ()
  "Thus function kills a buffer eventually."
  (progn
    )
  )

;;;###autoload
(defun ask-before-killing-buffer ()
  (let ((buffer (current-buffer)))
    (cond
     ((equal (buffer-name) "*scratch*")
      ;; Never kill *scratch*
      nil)
     ((and buffer-file-name (buffer-modified-p))
      ;; If there's a file associated with the buffer, 
      ;; make sure it's saved
      (y-or-n-p (format "Buffer %s modified; kill anyway? " 
			(buffer-name))))
     ((get-buffer-process buffer)
      ;; If there's a process associated with the buffer, 
      ;; make sure it's dead
      (y-or-n-p (format "Process %s active; kill anyway? "
			(process-name (get-buffer-process buffer)))))
     (t t))))
 
;;;###autoload
(defun kill-buffer-shortly (&optional un-tempbufify)
  "This function will turn on tempbuf mode in a buffer, and then bury it. The buffer will be killed shortly. When called with a non-nil argument, it will un-tempbuffer-ify the current buffer. (i.e. imagine we tempbufified something, and buried it."
  (interactive "P")
  (if un-tempbufify
      (progn
	(turn-off-tempbuf-mode)
	(message (concat "Saved buffer " (buffer-name (current-buffer)) " from imminent deletion."))
	)
    (if (ask-before-killing-buffer) 
	(progn
	  (turn-on-tempbuf-mode)
	  (message (concat "Buffer " (buffer-name (current-buffer)) " has been buried, will expire shortly.") )
	  (bury-buffer)
	  )))
  )

;;;###autoload
(defun ediff-regions-wordwise-this-buffer ()
  "Runs ediff-regions-wordwise with current buffer as both buffer A and buffer B."
  (interactive)
  (ediff-regions-wordwise (current-buffer) (current-buffer))
  )

(require 'tempbuff)
;;;###autoload
(defun find-file-temporary (filename)
  "Calls find-file, but in a temporary buffer."
  (interactive "f")
  (save-excursion
    (let ((old-buff (current-buffer)))
      (find-file filename)
      (tempbuf-mode t)
      (let ((new-buff (current-buffer)))
	(switch-to-buffer old-buff)
	(display-buffer new-buff)
	)
      )
    )
)

;;;###autoload
(defun windows-text-to-unix-text ()
  "Remove ^M (control-M, decimal 13) characters from the file"
  (interactive)
  (beginning-of-buffer)       ; start search from the top
  (while (re-search-forward "\015" nil t)
    (replace-match "" nil nil)
  )
  (beginning-of-buffer)       ; finish at top
)
