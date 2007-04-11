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
