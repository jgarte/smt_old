(in-package #:smt)


;; (push "/home/amir/Work/Lisp/smt/lang/cwmn.lisp" smt-engine::*rulepaths*)
;; (dolist (path smt-engine::*rulepaths*)
;;   (load path))
;;;;;;;;;;;;;;;;;;;;;;;;;
(export .installed-fonts. :smt-engine)
(export (font-glyphs) :smt-engine)


;;;;;;;;;;;;;;;;;; clocks
(defclass clock ()
  ((dur :initarg :duration :accessor duration)))

(defclass note (stacked-form clock)
  ((domain :initform nil
	   :documentation "We don't know what domain we want for note yet, 
thus this can't be set to STACKED!")
   (head :accessor head
	 :initarg :head)
   (spn :accessor spn
	:initarg :spn)))

(defun notep (obj) (typep obj 'note))

(defun make-note (spn duration &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'note :duration duration :spn spn
	 (alexandria:delete-from-plist initargs :duration :spn)))

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

