;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-



(defsystem smt/xml
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:module "xmlutils"
		:components ((:file "xmlbase")
			     (:file "svg")))))



(defsystem smt/engine
  :serial t
  :depends-on ("smt/xml" "alexandria" "split-sequence" "cl-ppcre")
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
			     
			     )
		))
  )


;;; Leave these funx to stay in ASDF-USER pkg
(defun smt-version-form (at)
  (uiop:safe-read-file-form "./version" :at at))

(defun smt-version-string (&optional (at 0))
  (destructuring-bind (major minor patch)
      (car (smt-version-form at))
    (format nil "~d.~d.~d" major minor patch)))

(defsystem smt
  :version #.(smt-version-string)
  :serial t
  ;; :in-order-to ((test-op (test-op "smt/test")))
  :depends-on ("smt/engine" (:version "asdf" "3.1.2"))
  :components ((:file "package")
	       (:module "rules"
		:serial t
		:components ((:file "types")
			     (:file "cwmn")))))


(defsystem smt/test
  :serial t
  ;; https://github.com/rpgoldman/fiveam-asdf.git
  :defsystem-depends-on ("fiveam-asdf")
  :depends-on ("smt" "fiveam")
  :class asdf::fiveam-tester-system
  :test-package #:smt-test
  :test-names (boundary-check)
  ;; :perform (test-op (o s)
  ;; 		    (uiop:symbol-call :fiveam '#:run!
  ;; 				      (uiop:find-symbol* '#:boundary-check
  ;; 							 :smttst))
  ;; 		    )
  :components ((:file "package")
	       (:file "regression-testing")
	       ))
