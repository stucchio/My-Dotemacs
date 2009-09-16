;Use f1 for help of various sorts
(global-set-key [(control f1)] 'apropos)
(global-set-key [(M f1)] 'describe-function)
;Makes f4 key switch to buffers, C-f4 kills buffer
(global-set-key [(control f4)] 'kill-buffer-shortly)
(global-set-key [(f4)] 'anything-for-buffers)
;; F5 does bubble-buffer
(global-set-key [(f5)] 'bubble-buffer-next)
(global-set-key [(control f5)] 'bubble-buffer-previous)
;;F6 controls the shell
(global-set-key [(f6)] 'multi-eshell-switch)
(global-set-key [(control f6)] 'multi-eshell)
(global-set-key [(M f6)] 'multi-eshell-go-back)
;;F7 turns on an interpreter reply, if one is available
(global-set-key [(f7)] 'turn-on-appropriate-repl)

;; F8 controls bookmarks
(global-set-key [(control f8)] 'bm-toggle)
(global-set-key [(f8)] 'bm-next)
;; F9 controls evaling lisp
(global-set-key [(f9)] 'eval-last-sexp)
(global-set-key [(control f9)] 'fc-eval-and-replace)
(global-set-key [(M f9)] 'execute-extended-command)
;; F10 controls window-config-ring
(global-set-key [(control f10)] 'window-config-ring-store)
(global-set-key [(M f10)] 'window-config-ring-remove)
(global-set-key [(f10)] 'window-config-ring-next)
;; F11 controls keyboard macros
(global-set-key [(control f11)] 'start-or-end-kbd-macro) ;One-button recording of kbd macros
(global-set-key [(control M f11)] 'name-last-kbd-macro) ;One button naming of last kbd macro
(global-set-key [(f11)] 'call-last-kbd-macro) ;one button calling of kbd macros
;; F12 controls darcs
(global-set-key [(f12)]
		(try-several-commands (darcsum-no-duplicate-buffer hg-commit-start ) "Failed to run darcs or hg")
		); Tries to run either darcsum-no-duplicate-buffer or hg-commit start, i.e. run darcs or mercurial. Raise error message if neither one works.

;;******** wrap-region stuff ********
(global-set-key "(" (wrap-region-with-function "(" ")"))
(global-set-key "{" (wrap-region-with-function "{" "}"))
(global-set-key "[" (wrap-region-with-function "[" "]"))


;;******** anything stuff ********
(global-set-key "\C-\M-a" 'anything)
(global-set-key [(f3)] 'anything)
(global-set-key "\C-x\C-f" 'anything-for-files-create-if-not-found)
(global-set-key "\C-x\C-r" 'anything-for-files)
(global-set-key "\C-xk" 'anything-kill-buffers-tweaked)
(global-set-key "\C-xb" 'anything-for-buffers)
(global-set-key [(M f8)] 'anything-for-bm)
(global-set-key "\M-y" 'anything-show-kill-ring-and-registers)
(global-set-key "\M-x" 'anything-M-x)

;;Far-search
(global-set-key "\M-s" 'far-search)

;;Bind M-j to imenu, for quick navigation.
(global-set-key "\M-j" 'imenu)

;;
(global-set-key [(M right)] 'forward-sexp)
(global-set-key [(M left)] 'backward-sexp)
(global-set-key [(M up)] 'backward-up-list)

;Zapping
(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key "\C-z" 'backward-zap-up-to-char)

;Eval
(global-set-key "\C-x\C-e" 'fc-eval-and-replace)
(global-set-key "\C-c\C-d" 'insert-time-at-point)

;unset f5 in icicle-mode
;; (add-hook 'icicle-mode-hook (lambda ()
;; 			      (define-key icicle-mode-map [(f5)] nil) ))
;; (add-hook 'icicle-mode-hook (lambda ()
;; 			      (define-key icicle-mode-map "\C-c`" nil) ))

;; Tabbar mode
(global-set-key [(control tab)] 'tabbar-forward-tab)
(global-set-key [(control M tab)] 'anything-for-buffers)

;; Lisp mode
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (progn
				    (define-key emacs-lisp-mode-map "\M-k" 'kill-sexp)
				    (define-key emacs-lisp-mode-map "\M-/" 'lisp-complete-symbol)
				    )))

(add-hook 'clojure-mode-hook (lambda ()
			       (progn
				 (define-key clojure-mode-map "\M-k" 'kill-sexp)
				 (define-key clojure-mode-map "\M-/" 'slime-complete-symbol)
				 (define-key clojure-mode-map [(f9)] 'slime-eval-last-expression)
				 (define-key clojure-mode-map [(M f9)] 'slime-eval-buffer)
				 )))

;; LaTeX-mode

(add-hook 'LaTeX-mode-hook (lambda ()
			     (progn
			       (define-key LaTeX-mode-map [(insert)] 'LaTeX-insert-or-change-equation-environment-interactive)
			       (define-key LaTeX-mode-map [(control insert)] 'LaTeX-insert-or-change-theorem-environment-interactive)
			       (define-key LaTeX-mode-map "$" (wrap-region-with-function "$" "$"))
			       )
			     )
	  )

;; Eshell-mode
(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (progn
	       (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)
	       (define-key eshell-mode-map [(control f4)] '(lambda () (interactive) (kill-buffer (current-buffer))))
	       )
	     )
	  )

;; Darcsum-mode
(add-hook 'darcsum-mode-hook
	  '(lambda ()
	     (define-key darcsum-mode-map [(control f4)] '(lambda () (interactive)
								     (progn
								       (kill-buffer (get-buffer-create "*darcs comment*"))
								       (kill-buffer (current-buffer))
								       )))))
(add-hook 'darcsum-comment-mode-hook
	  '(lambda () (define-key darcsum-comment-mode-map [(control f4)] '(lambda () (interactive) (kill-buffer (current-buffer))))))

;; C++ mode
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))