(require 'tempo)
(setq tempo-interactive t)


(add-hook 'java-mode-hook
	  (lambda () 
	    (progn
	      (define-abbrev java-mode-abbrev-table "for" ""
		(tempo-define-template "for"
				       '(> "for (" (p "Loop condition:" condition) ") {" n> p n> "}" >)
				       "for"
				       "For loop template"))
	      
	      (define-abbrev java-mode-abbrev-table "try" ""
		(tempo-define-template "try"
				       '(> "try {" n> p n>
					   > "} catch (" (p "Exception: " exception) " e) {"
					   > n> "}" >
					   )
				       "try"
				       "Try-catch template"))
	      )
	    )
	  )
