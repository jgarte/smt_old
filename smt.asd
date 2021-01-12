;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-



(defsystem "smt/xml"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:module "xmlutils"
		:components ((:file "xml")
			     (:file "svg")))))



(defsystem "smt/engine"
  :serial t
  :depends-on ("smt/xml" "alexandria" "split-sequence" "cl-ppcre")
  :components (;; (:file "package")	       
	       (:module "engine"
		:serial t
		:components ((:file "setup")
			     (:file "utils")
			     (:file "glyphs")
			     (:file "rules")     
			     (:file "canvas")
			     (:file "form")
			     (:file "mchar")
			     (:file "accidentals")
			     (:file "tmpsyms")
			     (:file "clefs")
			     
			     )
		))
  )


(defsystem "smt"
  :version #.(destructuring-bind (major minor patch)
		 (car (safe-read-file-form "./version"))
	       (format nil "~d.~d.~d" major minor patch))
  :serial t
  :in-order-to ((test-op (test-op "smt/test")))
  :depends-on ("smt/engine" (:version "asdf" "3.1.2"))
  :components ((:file "package")
	       (:module "rules"
		:serial t
		:components ((:file "types")
			     (:file "cwmn")))))

(asdf:defsystem "smt/test"
  :serial t
  :defsystem-depends-on ("fiveam" "fiveam-asdf")
  :class :fiveam-tester-system
  :depends-on ("smt")
  :components ((:file "package")
	       (:file "regtest"))
  :test-package :smt-test
  :test-names (
	       ;; fractional arithmetic rounding error
	       ;; #:fare
	       #:horizontal
	       )
  )
