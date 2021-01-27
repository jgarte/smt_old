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
	 :initarg :head
	 )
   (spn :accessor spn
	:initarg :spn)))

(defun notep (obj) (typep obj 'note))



;; (defun make-note (spn &rest initargs &key &allow-other-keys)
;;   ;; A certain notehead desired?
;;   (let* ((font (getf initargs :font *font*))
;; 	 (head-color (getf initargs :head-color "black"))
;; 	 (dur (getf initargs :dur 1/4))	 
;; 	 ;; If head supplied it's color is inside of it
;; 	 (head ;; (lazy-getf initargs :head
;; 	       ;; 		  (make-notehead ;; (duration->notehead-label dur)
;; 	       ;; 		   :font font
;; 	       ;; 		   :mchar-color head-color))
;; 	       )
;; 	 )
;;     ;; (when head-color (setf (glyph-color head) head-color))
;;     ;; head inherits spn from note (need spn for rules referring to spn)
;;     (apply #'make-instance
;; 	   'note
;; 	   :dur dur
;; 	   :head-color head-color
;; 	   :spn spn
;; 	   :head head
;; 	   :content (list head)
;; 	   initargs)))

(defun make-note (spn dur &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'note :dur dur :spn spn
	 (alexandria:delete-from-plist initargs :dur :spn)))
