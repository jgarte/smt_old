;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(asdf:defsystem "smttst"
  :serial t
  :depends-on ("smtngn" "fiveam")
  :perform (test-op (o s)
  		    (uiop:symbol-call :fiveam '#:run!
  				      (uiop:find-symbol* '#:boundary-check :smttst))
  		    )
  :components ((:file "package")
	       (:module "tests"
		:serial t
		:components ((:file "engine")))))
