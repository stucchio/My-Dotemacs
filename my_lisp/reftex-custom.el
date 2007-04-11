(require 'reftex)

(turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;Customize variables
(custom-set-variables
 '(reftex-toc-split-windows-horizontally t)
)


(defun view-crossref-or-toc (&optional ignored-arg)
  (interactive "p")
  (or
   (not (reftex-view-crossref nil nil t)) 
   (reftex-toc) 
   )
)

(local-set-key "\M-j"
					    'view-crossref-or-toc)

(add-hook 'LaTeX-mode-hook (lambda ()
			     (local-set-key "\M-j"
					    'view-crossref-or-toc)))
