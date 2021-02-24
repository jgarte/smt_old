(in-package #:smt)


;; (push "/home/amir/Work/Lisp/smt/lang/cwmn.lisp" smt-engine::*rulepaths*)
;; (dolist (path smt-engine::*rulepaths*)
;;   (load path))
;;;;;;;;;;;;;;;;;;;;;;;;;
(export .installed-fonts. :smt-engine)
(export (font-glyphs) :smt-engine)


;;;;;;;;;;;;;;;;;; clocks
(defclass clock ()
  ((dur :initarg :dur :accessor dur)))
(defun clockp (obj) (typep obj 'clock))
(defclass pitch ()
  ((spn :accessor spn :initarg :spn
	:documentation "Scientific Pitch Notation")))

;;;

(defclass note (stacked-form clock pitch)
  ((domain :initform nil
	   :documentation "We don't know what domain we want for note yet, 
thus this can't be set to STACKED!")
   ;; Following slots are containers for objects
   (head :accessor head
	 :documentation "Mchar obj"
	 :initarg :head
	 :initform nil)
   (flag :accessor flag :initarg :flag
	 :initform nil)
   (stem :accessor stem
	 :initform nil :initarg :stem)
   (id :initform (gensym "NOTI"))))

(defun notep (obj) (typep obj 'note))
;;; make-sform to contain all note stuff
;; (defun make-note (&rest initargs &key &allow-other-keys)
;;   (apply #'make-instance 'note initargs))

(defclass accidental (stacked-form pitch)
  ;; set domain to NIL or it will be inherited from stacked-form
  ((domain :initform nil)
   (mchar :initarg :mchar
	  :accessor mchar)
   (id :initform (gensym "ACCI"))))
(defparameter *accidental-right-side-space* (mm-to-px .5))
;; (defun make-accidental (&rest initargs &key &allow-other-keys)
;;   (apply #'make-instance 'accidental initargs))

(defclass line (stacked-form)
  ((starts :initarg :starts
	   :initform ()
	   :type list
	   :accessor starts)
   (ends :initarg :ends
	 :accessor ends)))

(defmacro define-score-makers (&rest names)
  `(progn ,@(loop for name in names
		  collect `(defun ,(intern (format nil "MAKE-~A" (string name)))
			      (&rest initargs &key &allow-other-keys)
			     (apply #'make-instance ',name initargs)))))

(define-score-makers note accidental)

(defclass barline (line) ())

