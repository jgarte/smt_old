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
  :depends-on ("smt/xml"
	       "alexandria" "split-sequence" "cl-ppcre"
	       "flexi-streams" "cxml")
  :components (
	       (:module "engine"
		:serial t
		:components (
			     ;; Installing font before using
			     ;; Glyphs in CANVAS for .SCALE.
			     (:file "font")
			     
			     (:file "utils")
			     (:file "rules")     
			     (:file "canvas")
			     (:file "form")
			     (:file "mchar")
			     (:file "accidentals")
			     (:file "temps")
			     (:file "clefs")
			     ;;
			     (:file "lines")
			     )
		)
	       (:module "rules"
		:serial t
		:components ((:file "types")
			     (:file "cwmn")))
	       )
  )


(defsystem "smt"
  :version #.(destructuring-bind (major minor patch)
		 (car (safe-read-file-form "./version"))
	       (format nil "~d.~d.~d" major minor patch))
  :serial t
  :in-order-to ((test-op (test-op "smt/test")))
  :depends-on ("smt/engine" (:version "asdf" "3.1.2"))
  :components ((:file "package")	       
	       (:file "smt")
	       ))

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
