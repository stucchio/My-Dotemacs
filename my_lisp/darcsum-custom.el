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


(defun darcsum-ediff ()
  "Like `darcsum-diff' but in an Ediff session."
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
	  (eq type 'change))
      (let ( (pristine-filename (darcsum-original-path (point)))
	     (working-filename (darcsum-path (point)))
	     (old-window-configuration (current-window-configuration))
	     )
      (progn
	(save-excursion
	  (find-file-read-only pristine-filename)
	  (rename-buffer (concat "*darcs pristine buffer:" pristine-filename "*"))
	  (rename-uniquely)
	  (tempbuf-mode) ;;Main difference between this and version in darcsum.el is this line
	  )
	(ediff pristine-filename
	       working-filename
	       (lambda () (progn
			    (make-variable-buffer-local 'pre-darcsum-ediff-window-configuration)
			    (make-variable-buffer-local 'darcsum-pristine-buffer)
			    (setq pre-darcsum-ediff-window-configuration old-window-configuration)
			    (make-local-hook 'ediff-quit-hook)
			    (add-hook 'ediff-quit-hook (lambda ()
							 (set-window-configuration pre-darcsum-ediff-window-configuration)
							  )
				      )
			    )))
	))))))