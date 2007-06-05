(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
		   (previous-single-property-change point 'help-echo)
		   (next-single-property-change point 'help-echo))))
     
     (defun ted-eshell-ls-find-file-temporary-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file-temporary (buffer-substring-no-properties
			     (previous-single-property-change point 'help-echo)
			     (next-single-property-change point 'help-echo))))
     
     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))
     
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
	 (define-key map [(control return)]      'ted-eshell-ls-find-file-temporary-at-point)
         (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
         (defvar ted-eshell-ls-keymap map))
       
       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, mouse-2: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
	(beginning-of-line))))

