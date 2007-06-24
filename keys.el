;Use f1 for help of various sorts
(global-set-key [(control f1)] 'apropos)
(global-set-key [(M f1)] 'describe-function)
;Makes f4 key switch to buffers, C-f4 kills buffer
(global-set-key [(control f4)] 'kill-buffer-shortly)
(global-set-key [(f4)] 'icicle-buffer)
;; F5 does bubble-buffer
(global-set-key [(f5)] 'bubble-buffer-next)
(global-set-key [(control f5)] 'bubble-buffer-previous)
;;F6 controls the shell
(global-set-key [(f6)] 'st-shell-switch)
(global-set-key [(control f6)] 'st-shell)
(global-set-key [(M f6)] 'st-shell-go-back)
;; F8 controls bookmarks
(global-set-key [(control f8)] 'bm-toggle) 
(global-set-key [(f8)] 'bm-next)
;; F9 controls evaling lisp
(global-set-key [(f9)] 'eval-last-sexp)
(global-set-key [(control f9)] 'fc-eval-and-replace)
(global-set-key [(M f9)] 'execute-extended-command)
;; F11 controls keyboard macros
(global-set-key [(control f11)] 'start-or-end-kbd-macro) ;One-button recording of kbd macros
(global-set-key [(control M f11)] 'name-last-kbd-macro) ;One button naming of last kbd macro
(global-set-key [(f11)] 'call-last-kbd-macro) ;one button calling of kbd macros
;; F12 controls darcs
(global-set-key [(f12)] 'darcsum-no-duplicate-buffer);This is the easiest way to switch to darcs mode


(global-set-key "\C-x\C-r" 'icicle-recent-file)

;;Bind M-j to imenu, for quick navigation.
(global-set-key "\M-j" 'imenu)

;;
(global-set-key [(M right)] 'forward-sexp)
(global-set-key [(M left)] 'backward-sexp)


;Zapping
(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key "\C-z" 'backward-zap-up-to-char)

;Eval
(global-set-key "\C-x\C-e" 'fc-eval-and-replace)
(global-set-key "\C-c\C-d" 'insert-time-at-point)

;unset f5 in icicle-mode
(add-hook 'icicle-mode-hook (lambda ()
			      (define-key icicle-mode-map [(f5)] nil) ))
(add-hook 'icicle-mode-hook (lambda ()
			      (define-key icicle-mode-map "\C-c`" nil) ))


;; Tabbar mode
(global-set-key [(control tab)] 'tabbar-forward-tab)
(global-set-key [(control M tab)] 'switch-to-buffer)

;; LaTeX-mode

(add-hook 'LaTeX-mode-hook (lambda ()
			     (define-key LaTeX-mode-map [(insert)] 'LaTeX-insert-or-change-environment-interactive) ))
(add-hook 'LaTeX-mode-hook (lambda ()
			     (define-key LaTeX-mode-map [(delete)] (lambda () (interactive) (message "Delete remapped. You use this key?")))))

(add-hook 'eshell-mode-hook
	  '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))