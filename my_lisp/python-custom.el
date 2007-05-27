
;;;***************************************************************************
;;; This part of the file defines some tempo-templates which are useful in python
;;;***************************************************************************

(require 'tempo)

;;; Expands into a local exception, e.g. an exception named "NAME_exception"
(tempo-define-template "py-local-exception"
		       '( "class " (p "Name: ") "_exception(Exception):" n
			  "    def __init__(self,value):" n
			  "        self.value = value" n
			  "    def __str__(self):" n
			  "        return repr(self.value)" n
			  p
			 )
		       )
;;; A template which expands to a python class 
(tempo-define-template "py-class"
		       '(
			 "class " (p "Classname: ") ":" n>
			 "\"\"\"" (p "Docstring: " ) "\n"
			 "    \"\"\"" n>
			 "def __init__(self):" n>
			 r
			 )
		       )

;;; A template which expands to a python class, which inherits some other objects
(tempo-define-template "py-classi"
		       '(
			 "class " (p "Classname: ") "(" (p "Inherits: ") "):" n>
			 "\"\"\"" (p "Docstring: " ) "\n"
			 "    \"\"\"" n>
			 "def __init__(self):" n>
			 r
			 )
		       )

;;; A template which expands to a function definition.
(tempo-define-template "py-funcdef"
		       '("def " (p "Function name: ") "(" (p "Arguments" ) "):" n>
			 "\"\"\"" r "\"\"\""
			 ))


;;; Now turn on abbrev mode
(abbrev-mode t)

;;;And set these abbreviations to the above templates
;;(define-abbrev python-mode-abbrev-table "lclexpt" "" 'tempo-template-py-local-exception)
;;(define-abbrev python-mode-abbrev-table "class" "" 'tempo-template-py-class)
;;(define-abbrev python-mode-abbrev-table "classi" "" 'tempo-template-py-classi)
;;(define-abbrev python-mode-abbrev-table "def" "" 'tempo-template-py-funcdef)
