;;; Temporal symbols ie notes (& it's components) and rests

(in-package #:smt-engine)

(defclass temporal ()
  ((duration :initarg :dur
	     :accessor dur)))

;;; Notehead is not pitched!!!
(defclass notehead (mchar)
  ((spn :accessor spn
	:initarg :spn
	:documentation "Hidden SPN!"
	:initform nil)
   ))

(defun make-notehead (label &rest initargs &key &allow-other-keys)
  (let ((family (getf initargs :family .font-family.)))
    (apply #'make-instance 'notehead
	   :family family
	   :code (mchar-label->mchar-code label "noteheads" family)
	   initargs)))


;;;;;;;;;;;;;;;;;; note


(defclass note (stacked-form temporal)
  ((domain :initform nil
	   :documentation "We don't know what domain we want for note yet, 
thus this can't be STACKED!")
   (head :accessor head
	 :initarg :head
	 ;; Can be empty too!
	 :initform nil)
   (head-color :accessor head-color
	      :initarg :head-color)
   (spn :accessor spn
	:initarg :spn
	:initform nil)))

(defun notep (obj) (typep obj 'note))

(defun duration->notehead-label (d)
  (cond ((= d 1/2) "s1")
	((= d 1) "s0")
	((<= d 1/4) "s2")))

(defun make-note (spn &rest initargs &key &allow-other-keys)
  ;; A certain notehead desired?
  (let* ((family (getf initargs :family .font-family.))
	 (head-color (getf initargs :head-color "black"))
	 (dur (getf initargs :dur 1/4))	 
	 ;; If head supplied it's color is inside of it
	 (head (lazy-getf initargs :head
			  (make-notehead (duration->notehead-label dur)
					 :family family
					 :mchar-color head-color)))
	 )
    ;; (when head-color (setf (glyph-color head) head-color))
    ;; head inherits spn from note (need spn for rules referring to spn)
    (unless (spn head) (setf (spn head) spn))    
    (apply #'make-instance
	   'note
	   :dur dur
	   :head-color head-color
	   :spn spn
	   :head head
	   :content (list head)
	   initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;; Rests
(defclass pause (mchar temporal)
  ())

(defun make-pause (&rest initargs &key &allow-other-keys)
  (let ((family (getf initargs :family .font-family.))
	(label (lazy-getf initargs :label (error "Making a pause requires label"))))
    (apply #'make-instance 'pause
	   :family family
	   :code (mchar-label->mchar-code label "rests" family)
	   initargs)))
(defun pausep (obj) (typep obj 'pause))


;;;;;;;;;;;;;;;;;;; Chord
(defclass chord (stacked-form)
  ((dur :initarg :dur :accessor dur :initform 1/4)
   (spns :initarg :spns :accessor spns :initform ())))

;; (defun make-chord (&rest initargs &key &allow-other-keys)
;;   (let ((family (getf initargs :family))
;; 	(dur (getf initargs :dur)))
    
;;     (apply #'make-instance 'chord
;; 	   initargs)))

(defun chordp (obj) (typep obj 'chord))
