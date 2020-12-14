
;;; Key signature and single accidentals


(in-package :smt-engine)
(defclass accidental (pitched-glyph) ())
(defun accidental (spn label &rest initargs &key &allow-other-keys)
  (apply #'make-instance
	 'accidental
	 :spn spn
	 ;; :label label
	 initargs))




