(define-project-type mercurial (generic)
  (look-for ".hg")
  )

(define-project-type darcs (generic)
  (look-for "_darcs")
  )

(define-project-type svn (generic)
  (look-for ".svn")
  )

(define-project-type git (generic)
  (look-for ".git")
  )

(add-hook 'find-file-hook 'eproject-maybe-turn-on)

(provide 'eproject-stucchio-custom)