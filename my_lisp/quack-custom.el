(defun scheme-send-dwim (arg)
  "Send the appropriate forms to Scheme to be evaluated."
  (interactive "P")
  (save-excursion
    (cond
     ;;Region selected - evaluate region
     ((not (equal mark-active nil))
      (scheme-send-region (mark) (point)))
     ;; At/after sexp - evaluate last sexp
     ((or (looking-at "\\s\)")
	  (save-excursion
	    (backward-char 1)
	    (looking-at "\\s\)")))
      (if (looking-at "\\s\)")
	  (forward-char 1))
      (scheme-send-last-sexp))
     ;; At/before sexp - evaluate next sexp
     ((or (looking-at "\\s\(")
	  (save-excursion
	    (forward-char 1)
	    (looking-at "\\s\(")))
      (if (looking-at "\\s\(")
	  (forward-list 1))
      (scheme-send-last-sexp))
     ;; Default - evaluate enclosing top-level sexp
     (t (scheme-send-definition)))
    (if arg (switch-to-scheme t))))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (progn
	      (local-set-key [(f9)] 'scheme-send-dwim )
	      )
	    )
	  )

