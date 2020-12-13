;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-




(asdf:defsystem "smtngn"
  :in-order-to ((test-op (test-op "smttst")))
  :serial t
  :depends-on ("smtxml" #:alexandria #:split-sequence #:cl-ppcre)
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
		)))
