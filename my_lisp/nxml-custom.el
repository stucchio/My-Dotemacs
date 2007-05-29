(eval-after-load 'nxhtml
  '(define-key nxhtml-mode-map [(M /)] 'nxml-complete))

(defun xhtml-make-paragraph ()
  (interactive)
  (if mark-active
      (if (< (point) (mark))
	  (save-excursion
	    (insert "<p>")
	    (exchange-point-and-mark)
	    (insert "</p>")
	    )
	(save-excursion
	  (insert "</p>")
	  (exchange-point-and-mark)
	  (insert "<p>")
	  )
	)
    (tempo-template-nxml-paragraph)
    )
  )
  
(eval-after-load 'nxhtml
  '(define-key nxhtml-mode-map [(M p)] 'xhtml-make-paragraph))

(tempo-define-template "nxml-paragraph" '("<p>" p "</p>") )