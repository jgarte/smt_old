;;;; smt.asd

(asdf:defsystem #:smt
  :description "Describe smt here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:xml #:alexandria #:split-sequence #:cl-ppcre)
  :components ((:file "package")
	       ((:file "package")
	       ;; (:module "xmlutils"
	       ;; 	:serial t
	       ;; 	:components ((:file "xml-base")
	       ;; 		     (:file "svg")))
	       (:module "engine"
		:serial t
		:components ((:file "setup")
			     (:file "utils")
			     (:file "fonts")
			     (:file "rules")   			     
			     (:file "canvas")
			     (:file "form")
			     (:file "glyph")
			     
			     (:file "accidentals")
			     (:file "tmpsyms")
			     (:file "clefs")
			     (:file "burin")
			     (:file "staff")
			     )
		)
	       (:module "rules"
		:serial t
		:components ((:file "types")
			     (:file "cwmn"))
		))))
