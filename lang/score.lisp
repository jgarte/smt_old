(in-package #:smt)

;;;;;;;;;;;;;;;;;; clocks
(defclass clocked ()
  ((dur :initarg :dur :accessor dur)))

(defclass note (stacked-form clocked)
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

(defclass accidental (mchar)
  ())
(defun make-acc (name)
  (make-instance 'accidental :name name))

(defclass line (stacked-form)
  ((starts :initarg :starts
	   :initform ()
	   :type list
	   :accessor starts)
   (ends :initarg :ends
	 :accessor ends)))

(defclass barline (line) ())

;;;;;;;;;;;;;;;;;;;;;;;;;
(export .installed-fonts. :smt-engine)
(export (font-glyphs) :smt-engine)



