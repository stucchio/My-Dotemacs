(add-hook 'c-mode-hook
          (lambda () (c-toggle-auto-hungry-state 1)))
(add-hook 'c++-mode-hook
          (lambda () (c-toggle-auto-hungry-state 1)))
(add-hook 'c++-mode-hook
          (lambda () (c-toggle-auto-newline 1)))

