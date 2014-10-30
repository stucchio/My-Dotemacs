;Use f1 for help of various sorts
(global-set-key [(control f1)] 'apropos)
(global-set-key [(M f1)] 'describe-function)
; f2 for buffer revert
(global-set-key [(f2)] 'revert-buffer)
;Makes f4 key go back in buffer list, C-f4 kills buffer
(global-set-key [(f4)] `projectile-kill-buffers)
(global-set-key [(control f4)] 'kill-buffer-shortly)
;;F5 is available

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
(global-set-key [(control f10)] 'persp-kill)
;;(global-set-key [(M f10)] 'window-config-ring-remove)
(global-set-key [(f10)] 'persp-switch)
;; F11 controls keyboard macros
(global-set-key [(control f11)] 'start-or-end-kbd-macro) ;One-button recording of kbd macros
(global-set-key [(control M f11)] 'name-last-kbd-macro) ;One button naming of last kbd macro
(global-set-key [(f11)] 'call-last-kbd-macro) ;one button calling of kbd macros
;; F12 tries to vc commit
;; Tries to run either appropriate version control commit commanddarcsum-no-duplicate-buffer or hg-commit start, i.e. run darcs or mercurial. Raise error message if neither one works.
(global-set-key [(f12)] (try-several-commands
			 (darcsum-no-duplicate-buffer
			  hg-commit-start
			  (lambda () (git-status (buffer-file-name (current-buffer))))
			  )
		 "Failed to run version control commit")
		)
(global-set-key [(control f12)] 'git-push-ff-only)
(global-set-key [(M f12)] 'git-branch)

;;******** Helm ********
(global-set-key "\M-y" 'helm-show-kill-ring)

;;******** projectile ********
(global-set-key [(control tab)] 'helm-projectile)
(global-set-key "\M-s" 'projectile-multi-occur)
(global-set-key "\C-\M-s" 'projectile-replace)
(global-set-key "\C-\M-w" 'delete-region)

;;******** wrap-region stuff ********
(global-set-key "(" (wrap-region-with-function "(" ")"))
(global-set-key "{" (wrap-region-with-function "{" "}"))
(global-set-key "[" (wrap-region-with-function "[" "]"))



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

;; Org mode

(add-hook 'org-mode-hook (lambda ()
			   (progn
			     (define-key org-mode-map [(control shift right)] 'org-shiftright)
			     (define-key org-mode-map [(control shift left)] 'org-shiftleft)
			     (define-key org-mode-map [(control shift up)] 'org-shiftup)
			     (define-key org-mode-map [(control shift down)] 'org-shiftdown)
			     )
			   )
	  )
