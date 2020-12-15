;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-



(defsystem "smt/xml"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:module "xmlutils"
		:components ((:file "xmlbase")
			     (:file "svg")))))



(defsystem "smt/engine"
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
  (safe-read-file-form "./version" :at at))

(defun smt-version-string (&optional (at 0))
  (destructuring-bind (major minor patch)
      (car (smt-version-form at))
    (format nil "~d.~d.~d" major minor patch)))

(defsystem "smt"
  :version #.(smt-version-string)
  :serial t
  :in-order-to ((test-op (test-op "smt/test")))
  :depends-on ("smt/engine" (:version "asdf" "3.1.2"))
  :components ((:file "package")
	       (:module "rules"
		:serial t
		:components ((:file "types")
			     (:file "cwmn")))))

;; (defmethod perform :before ((o test-op) (c (eql (asdf:find-system "smt/test"))))
;;   (print 'before))
;; (defmethod perform :after ((o test-op) (c (eql (asdf:find-system "smt/test"))))
;;   (print 'after))

(asdf:defsystem "smt/test"
  :serial t
  :depends-on ("smt")
  :components ((:file "package")
	       (:file "regression-testing")
	       )
  :perform (test-op (o s) (print s))
  )

#.(list 1 2 3 4 5)
;; (defmethod perform ((o test-op) (c (eql (asdf:find-system "smt/test"))))
;;   (print 'hastings))


