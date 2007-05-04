(load "icicles")
(icy-mode)

(add-hook 'icicle-mode-hook 'bind-my-icicles-keys)

(defun bind-my-icicles-keys ()
  "Replace some default Icicles bindings with others I prefer."
  (dolist (map (append (list minibuffer-local-completion-map
			     minibuffer-local-must-match-map)
		       (and (fboundp 'minibuffer-local-filename-completion-map)
			    (list minibuffer-local-filename-completion-map))))
    (when icicle-mode
      (define-key map [?\M-!] 'icicle-all-candidates-action))))
      
(defun bind-my-icicles-keys--for-all-minibuffer-map (map)
  (define-key map "\M-!" 'icicle-all-candidates-action)
  )

(icicle-mode 1)