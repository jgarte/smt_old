;;;; smt.asd

(asdf:defsystem #:smt
  :description "Symbolic Music Typesetting"
  :author "Amir Teymuri"
  :version "0.0.1"
  :serial t
  :depends-on (#:xml #:alexandria #:split-sequence #:cl-ppcre)
  :components ((:file "package")
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
		)
	       (:file "test")))
