;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-



(asdf:defsystem "smt/xml"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:module "xmlutils"
		:components ((:file "xmlbase")
			     (:file "svg")))))

(defun smt-engine-version-form (at)
  (uiop:safe-read-file-form "./engine-version" :at at))

(defun smt-engine-version-string (&optional (at 0))
  (destructuring-bind (major minor patch)
      (car (smt-engine-version-form at))
    (format nil "~d.~d.~d" major minor patch)))

(asdf:defsystem "smt/ngn"
  ;; :in-order-to ((test-op (test-op "smttst")))
  :serial t
  :version #.(smt-engine-version-string)
  :depends-on ("smt/xml" #:alexandria #:split-sequence #:cl-ppcre)
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

(asdf:defsystem "smt"
  :serial t
  :depends-on ("smt/ngn")
  :components ((:file "package")
	       (:module "rules"
		:serial t
		:components ((:file "types")
			     (:file "cwmn")))))


(asdf:defsystem "smt/tst"
  :serial t
  :depends-on ("smt/ngn" "fiveam")
  :perform (test-op (o s)
  		    (uiop:symbol-call :fiveam '#:run!
  				      (uiop:find-symbol* '#:boundary-check :smttst))
  		    )
  :components ((:file "package")
	       (:module "tests"
		:serial t
		:components ((:file "engine")))))
