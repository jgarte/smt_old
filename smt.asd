;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-




(defsystem "smt/engine"
  :serial t
  :depends-on (
	       "alexandria" "split-sequence" "cl-ppcre"
	       "s-xml")
  :components ((:file "package")
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
			     (:file "mchar")))))


(defsystem "smt"
  :version #.(destructuring-bind (major minor patch)
		 (car (safe-read-file-form "./version"))
	       (format nil "~d.~d.~d" major minor patch))
  :serial t
  :in-order-to ((test-op (test-op "smt/test")))
  :depends-on ("smt/engine" (:version "asdf" "3.1.2"))
  :components (
	       ;; (:file "package")
	       (:file "smt")		;For rules useful Things might be defined here
	       (:module "rules"
		:serial t
		:components ((:file "cwmn")))
	       ))

(asdf:defsystem "smt/test"
  :serial t
  :defsystem-depends-on ("fiveam" "fiveam-asdf")
  :class :fiveam-tester-system
  :depends-on ("smt")
  :components (;; (:file "package")
	       (:file "regtest"))
  :test-package :smt-test
  :test-names (
	       ;; fractional arithmetic rounding error
	       ;; #:fare
	       #:horizontal
	       )
  )
(asdf:defsystem "smt/docs"
  :depends-on ("smt" "cl-markup")
  :components ((:module "docs"
		:components ((:file "doc")))))
