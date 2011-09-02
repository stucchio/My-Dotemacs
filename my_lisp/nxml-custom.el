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

(defun nxml-edit-javascript ()
  "This function opens a new javascript-mode buffer based on the current <script></script element, in the js2-mode."
  (interactive)
  (let ((s "")
	(js-buffer (generate-new-buffer "inline-javascript.js"))
	(original-buffer (current-buffer))
	)

    (save-excursion ;Check if we are in a javascript block
      (condition-case nil
	  (nxml-up-element)
	  (error (error "Not in javascript block")))
      (nxml-backward-element)
      (unless (equal (nxml-parse-start-tag) '("script" (("type" . "text/javascript"))))
	(error "Not in javascript block")
	)
      )

      (save-excursion
	(nxml-mark-paragraph)
	(append-to-buffer js-buffer (point) (mark))
	) ;Create temporary javascript buffer, append region to it

      (switch-to-buffer js-buffer) ;Switch to that buffer
      (javascript-mode) ;turn on javascript mode

      ;remember where we came from
      (mapcar 'make-variable-buffer-local '(kill-buffer-hook from-buffer jstemp-buffer))
      (setq jstemp-buffer js-buffer)
      (setq from-buffer original-buffer)

      (add-hook 'kill-buffer-hook (lambda ()
				    (let ((s (buffer-string)))
				      (switch-to-buffer from-buffer)
				      (nxml-mark-paragraph)
				      (delete-region (point) (mark))
				      (insert s)
				      (switch-to-buffer from-buffer)
				      )
				    ))
      )
  )

(eval-after-load 'nxhtml
  '(progn
     (define-key nxhtml-mode-map [(M p)] 'xhtml-make-paragraph)
    (define-key nxhtml-mode-map "\C-j" 'nxml-edit-javascript))
  )