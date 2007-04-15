;Makes f5 key open a new shell
(global-set-key [(f5)] 'st-shell)
(global-set-key [(f6)] 'st-shell-switch)
(global-set-key [(control f6)] 'st-shell-go-back)
(global-set-key [(f10)] 'start-or-end-kbd-macro) ;One-button recording of kbd macros
(global-set-key [(f11)] 'call-last-kbd-macro) ;one button calling of kbd macros
(global-set-key [(f7)] 'bm-toggle) 
(global-set-key [(f8)] 'bm-next)
(global-set-key [(control f8)] 'bm-previous)
(global-set-key [(f12)] 'darcsum-no-duplicate-buffer);This is the easiest way to switch to darcs mode


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
