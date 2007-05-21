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
(defvar LaTeX-environments-to-insert-interactively '("equation" "multline" "equation*" "multline*" "eqnarray"))
(defvar LaTeX-environments-to-insert-ring (make-ring (length LaTeX-environments-to-insert-interactively)))
(mapc (lambda (obj) (ring-insert LaTeX-environments-to-insert-ring obj))
      LaTeX-environments-to-insert-interactively)
(defvar LaTeX-environments-to-insert-ring-index 1)

(defun LaTeX-insert-environment-interactive ()
  "Interactively inserts latex environments. \nCalled once, inserts (car LaTeX-environments-to-insert-interactively). Called repeatedly, changes current environment to cycle through that ring." 
  (interactive)
  (if (eq last-command 'LaTeX-insert-environment-interactive)
      (progn
	(setq LaTeX-environments-to-insert-ring-index (+ LaTeX-environments-to-insert-ring-index 1) )
	(LaTeX-modify-environment (ring-ref LaTeX-environments-to-insert-ring LaTeX-environments-to-insert-ring-index) )
      )
    (progn
      (setq LaTeX-environments-to-insert-ring-index 1)
      (LaTeX-insert-environment (ring-ref LaTeX-environments-to-insert-ring LaTeX-environments-to-insert-ring-index))
      )
    )
)


;;;***************************************************************************
;;; This part of the file defines some tempo-templates which are useful in auctex
;;;***************************************************************************

;When we load LaTeX, turn on abbrev-mode

;(require 'latex)

(add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode t)))


(defmacro make-LaTeX-env-abbrev (abb env)
  `(add-hook 'LaTeX-mode-hook (lambda ()
				(define-abbrev text-mode-abbrev-table ,abb "" 
				  (lambda () (LaTeX-insert-environment ,env)))))
)

(require 'tempo)

;;; Expands into a titled slide environment
(tempo-define-template "bslide"
		       '( "\\begin{slide}{" (p "Title: ") "}" n
			  p n
			  "\\end{slide}" n
			 )
		       )

;;; Expands into a titled slide environment, with N overlays, 
(tempo-define-template "bslideo"
		       '( "\\overlays{" (p "Number of Overlays: " ) "}{" n
			 "\\begin{slide}{" (p "Title: ") "}" n
			  p n
			  "\\begin{itemstep}" n
			  "\\end{itemstep}" n
			  "\\end{slide}" n "}" n
			 )
		       )



(make-LaTeX-env-abbrev "\\beq" "equation")
(make-LaTeX-env-abbrev "\\beqn" "equation*")
(make-LaTeX-env-abbrev "\\bem" "multline")
(make-LaTeX-env-abbrev "\\bemn" "multline*")
;;(make-LaTeX-env-abbrev "\\prop" "proposition")
;;(make-LaTeX-env-abbrev "\\thm" "theorem")
;;(make-LaTeX-env-abbrev "\\lem" "lemma")
(make-LaTeX-env-abbrev "\\bslide" "slide")

(define-abbrev text-mode-abbrev-table "bslide" "" 'tempo-template-bslide)
(define-abbrev text-mode-abbrev-table "bslideo" "" 'tempo-template-bslideo)
;;Expands "\n" to "\\". If the current environment is equation or equation*, turn it into multline or multline*. Otherwise, just replace \n by \\.
(add-hook 'LaTeX-mode-hook (lambda () 
			     (define-abbrev text-mode-abbrev-table "\\n" "" (lambda () 
									      (progn 
										(single-line-to-multiline)
										(insert "\\\\")
										)))))
