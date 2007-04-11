(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value. The sexp is pushed to the kill ring."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	       (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

