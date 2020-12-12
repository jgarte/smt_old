;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(asdf:defsystem smttst
  :name "smttst"
  :serial t
  :depends-on ("smtngn" "fiveam")
  :components ((:file "package")
	       (:module "test"
			:serial t
			:components ((:file "ngn")))))
