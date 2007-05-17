(defun darcsum-no-duplicate-buffer ()
  "Opens new darcsum window, or jumps to current if it already exists. The new darcsum window is opened in the current directory (or parent directory if that's where darcs is)."
  (interactive)
  (let ( (working-dir default-directory) )
    (let ( (darcs-buffer-name (concat "*darcs-" working-dir "*")) )
      (let ( (darcs-buffer (get-buffer darcs-buffer-name)) )
	(if darcs-buffer
	    (progn 
	      (switch-to-buffer darcs-buffer)
	      (darcsum-refresh)
	      )
	  (if (darcsum-whatsnew working-dir)
	      (progn
		(switch-to-buffer (get-buffer "*darcs*"))
		(rename-buffer darcs-buffer-name)
		(switch-to-buffer (get-buffer darcs-buffer-name))
		)
	    (message "No changes")
	    ))))))


