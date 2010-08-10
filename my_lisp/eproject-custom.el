(define-project-type mercurial (generic)
  (look-for ".hg")
  ;(irrelevant-files "*.aux" "*.pyc*" "*.eps" "*.dvi" "*.blg")
  )

(define-project-type darcs (generic)
  (look-for "_darcs")
  ;(irrelevant-files "*.aux" "*.pyc*" "*.eps" "*.dvi" "*.blg")
  )

(define-project-type git (generic)
  (look-for ".git")
  ;(irrelevant-files "*.aux" "*.pyc*" "*.eps" "*.dvi" "*.blg")
  )


(define-project-type svn (generic)
  (look-for ".svn/dir-prop-base")
  )

(define-project-type git (generic)
  (look-for ".git")
  )

(add-hook 'find-file-hook 'eproject-maybe-turn-on)

(provide 'eproject-stucchio-custom)

