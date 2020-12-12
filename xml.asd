;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(asdf:defsystem smtxml
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:module "xml"
		:components ((:file "xmlbase")
			     (:file "svg")))))
