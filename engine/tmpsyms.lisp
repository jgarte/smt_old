;;; Temporal symbols ie notes and rests

(in-package #:smtngn)



;;; Notehead is not pitched!!!
(defclass notehead (glyph)
  ((spn :accessor spn
	:initarg :spn
	:documentation "Hidden SPN!"
	:initform nil)
   ))

(defun notehead (label &rest initargs &key &allow-other-keys)
  (let ((family (getf initargs :family %font-family%)))
    (apply #'make-instance 'notehead
	   :family family
	   :code (glyph-label->glyph-code label "noteheads" family)
	   initargs)))


;;;;;;;;;;;;;;;;;; note


(defclass note (stacked-form)
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
	:initform nil)
   (dur :initarg :dur
	:accessor dur
	:initform 1/4)))

(defun duration->notehead-label (d)
  (cond ((= d 1/2) "s1")
	((= d 1) "s0")
	((<= d 1/4) "s2")))

(defun note (spn &rest initargs &key &allow-other-keys)
  ;; A certain notehead desired?
  (let* ((family (getf initargs :family %font-family%))
	 (head-color (getf initargs :head-color "black"))
	 (dur (getf initargs :dur 1/4))	 
	 ;; If head supplied it's color is inside of it
	 (head (lazy-getf initargs :head
			  (notehead (duration->notehead-label dur)
				    :family family
				    :glyph-color head-color)))
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
