; Enable parse on load.
(setq TeX-parse-self t)

;auctex customizations
(custom-set-variables
 '(LaTeX-enable-toolbar nil);Turn off auctex toolbar
 '(TeX-electric-sub-and-superscript t)
)

;Make latex-mode fontify by default
(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
;Make LaTeX-math-mode turn on by default
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)


;;Chooses forward search based on xdvi or kdvi
;;enables forward search using xdvi
(require 'xdvi-search)
;;Binds C-q to xdvi forward search.
(add-hook 'LaTeX-mode-hook (lambda ()
			     (local-set-key "\C-q" 'xdvi-jump-to-line)))

;sets bookmark jump to call an xdvi forward search immediately afterward, in latex-mode only
(add-hook 'LaTeX-mode-hook (lambda ()
			     (local-set-key "\C-xrb"
					    '(bookmark-jump
					      xdvi-jump-to-line
					      )
					    )))
(add-hook 'LaTeX-mode-hook (lambda ()
			     (local-set-key [(f8)]
					    (lambda ()
					      (interactive)
					      (progn
						(bm-next)
						(xdvi-jump-to-line nil)
						)
					      )
					    )
			     ))



;;If xdvi-search doesn't work, then we will try to use kdvi-search as inferior alternative.
(if (not (boundp 'xdvi-search))
    (progn
      (require 'kdvi-search) ;Load kdvi-search
      (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\C-q" 'kdvi-jump-to-line)))

      (defun stucchio-bookmark-jump-with-kdvi-jump ()
	"Does bookmark-jump followed by kdvi-jump-to-line."
	(interactive)
	(progn
	  (call-interactively 'bookmark-jump)
	  (call-interactively 'kdvi-jump-to-line)
	  )
	)
      ;;Make bookmark-jump also do kdvi-jump-to-line in Latex mote
      (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\C-xrb" 'stucchio-bookmark-jump-with-kdvi-jump)))
      )
  )

;;;***************************************************************************
;;; Some basic function definitions
;;;***************************************************************************

(defvar environments-to-multi-lineify '( ("equation" . "multline") ("equation*" . "multline*")) )


(defun single-line-to-multiline ()
  "This function will turn the current single-line environment (e.g. equation) into a multi-line environment."
  (interactive)
  (let ( (new-environment (cdr (assoc (LaTeX-current-environment) environments-to-multi-lineify )))
	)
      (if new-environment
	  (progn
	    (LaTeX-modify-environment new-environment)
	    (message (concat "Changed environment to " new-environment))
	    ))))

;;******** Variables needed by LaTeX-insert-environment-interactive ********
(defvar LaTeX-equation-environments-to-insert-interactively '("equation" "multline" "equation*" "multline*" "eqnarray"))
(defvar LaTeX-theorem-environments-to-insert-interactively '("proposition" "lemma" "remark" "theorem"))

(defun LaTeX-insert-or-change-equation-environment-interactive ()
  (interactive)
  (LaTeX-insert-or-change-environment LaTeX-equation-environments-to-insert-interactively)
  )

(defun LaTeX-insert-or-change-theorem-environment-interactive ()
  (interactive)
  (LaTeX-insert-or-change-environment LaTeX-theorem-environments-to-insert-interactively)
  )

(defun LaTeX-insert-or-change-environment (possible-environments)
  "Inserts or changes latex environments. Checks if the current environment is a member of possible-environments. If so, changes the environment to the nesxt environment listed in  LaTeX-environments-to-insert-interactively."
  (let ( (env-list (member (LaTeX-current-environment) possible-environments))
	 )
    (if env-list ;;If current environment a member of LaTeX-environments-to-insert-interactively,
	(let ((env-list-tail (cdr env-list)))
	  (if env-list-tail
	      (LaTeX-modify-environment (car env-list-tail)) ;;then change environment to next environment in list
	    (LaTeX-modify-environment (car possible-environments)) ;;If at the end of the list, change to first element in the list
	    )
	)
      (LaTeX-insert-environment (car possible-environments)) ;;If not in an eq environment, just insert first environment from the list.
      )
    )
  )

(defun save-buffer-and-tex ()
  "This function saves the buffer and automagically runs latex on it."
  (interactive)
  (if (buffer-modified-p)
      (progn
	(save-buffer)
	(TeX-command "LaTeX" 'TeX-master-file nil)
	)
    (message "(No changes need to be saved)")
    )
)

(add-hook 'LaTeX-mode-hook (lambda ()
			     (local-set-key "\C-x\C-s"
					    'save-buffer-and-tex
					    )))

(require 'tex-buf)

(defun TeX-run-command (name command file)
  "Create a process for NAME using COMMAND to process FILE.
Return the new process. This command is already defined in tex-buf.el (part of auctex), but we redefine it here to make the buffer used into a temporary buffer."
  (let ((default TeX-command-default)
	(buffer (TeX-process-buffer-name file))
	(dir (TeX-master-directory))
	(command-buff (current-buffer)))
    (TeX-process-check file)		; Check that no process is running
    (setq-default TeX-command-buffer command-buff)
    (get-buffer-create buffer)
    (set-buffer buffer)
    (erase-buffer)
    (turn-on-tempbuf-mode)
    (set (make-local-variable 'line-number-display-limit) 0)
    (setq TeX-output-extension nil)
    (set (make-local-variable 'TeX-command-buffer) command-buff)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (setq mode-name name)
    (if TeX-show-compilation
	(display-buffer buffer)
      (message "Type `C-c C-l' to display results of compilation."))
    (setq TeX-parse-function 'TeX-parse-command)
    (setq TeX-command-default default)
    (setq TeX-sentinel-function
	  (lambda (process name)
	    (message (concat name ": done."))))
    (if TeX-process-asynchronous
	(let ((process (start-process name buffer TeX-shell
				      TeX-shell-command-option command)))
	  (if TeX-after-start-process-function
	      (funcall TeX-after-start-process-function process))
	  (TeX-command-mode-line process)
	  (set-process-filter process 'TeX-command-filter)
	  (set-process-sentinel process 'TeX-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (setq compilation-in-progress (cons process compilation-in-progress))
	  process)
      (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process TeX-shell nil buffer nil
		    TeX-shell-command-option command))))

(defun TeX-help-error (error output runbuffer)
  "Print ERROR in context OUTPUT from RUNBUFFER in another window."

  (let ((old-buffer (current-buffer))
	(log-file (with-current-buffer runbuffer
		    (with-current-buffer TeX-command-buffer
		      (expand-file-name (TeX-active-master "log")))))
	(TeX-error-pointer 0))

    ;; Find help text entry.
    (while (not (string-match (car (nth TeX-error-pointer
					TeX-error-description-list))
			      error))
      (setq TeX-error-pointer (+ TeX-error-pointer 1)))

    (pop-to-buffer (get-buffer-create "*TeX Help*"))
    (erase-buffer)
    (turn-on-tempbuf-mode)
    (insert "ERROR: " error
	    "\n\n--- TeX said ---"
	    output
	    "\n--- HELP ---\n"
	    (save-excursion
	      (if (and (string= (cdr (nth TeX-error-pointer
					  TeX-error-description-list))
				"No help available")
		       (let* ((log-buffer (find-buffer-visiting log-file)))
			 (if log-buffer
			     (progn
			       (set-buffer log-buffer)
			       (revert-buffer t t)
			       (turn-on-tempbuf-mode))
			   (setq log-buffer
				 (find-file-noselect log-file))
			   (progn (set-buffer log-buffer)
				  (turn-on-tempbuf-mode))
			   )
			 (auto-save-mode nil)
			 (setq buffer-read-only t)
			 (goto-line (point-min))
			 (search-forward error nil t 1)))
		  (progn
		    (re-search-forward "^l.")
		    (re-search-forward "^ [^\n]+$")
		    (forward-char 1)
		    (let ((start (point)))
		      (re-search-forward "^$")
		      (concat "From the .log file...\n\n"
			      (buffer-substring start (point)))))
		(cdr (nth TeX-error-pointer
			  TeX-error-description-list)))))
    (goto-char (point-min))
    (pop-to-buffer old-buffer)))