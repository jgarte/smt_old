;;; Temporal ie notes (& it's components) and rests

(in-package #:smt-engine)


;;;;;;;;;;;;;;;;;; note

(defclass tmp ()
  ((dur :initarg :dur :accessor dur)))

(defclass note (stacked-form tmp)
  ((domain :initform nil
	   :documentation "We don't know what domain we want for note yet, 
thus this can't be set to STACKED!")
   (head :accessor head
	 :initarg :head)
   (spn :accessor spn
	:initarg :spn)))

(defun notep (obj) (typep obj 'note))

(defun make-note (spn dur &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'note :dur dur :spn spn
	 (alexandria:delete-from-plist initargs :dur :spn)))
