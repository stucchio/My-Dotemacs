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





