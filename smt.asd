;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(asdf:defsystem smt
  :name "smt"
  :description "Symbolic Music Typesetting"
  :author "Amir Teymuri"
  :version "0.0.1"
  :serial t
  :depends-on ("smtngn")
  :components ((:file "package")
	       (:module "rules"
		:serial t
		:components ((:file "types")
			     (:file "cwmn")))))
