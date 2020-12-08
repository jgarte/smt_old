;;; Common Western Music Notation

(in-package :smtngn)

;;; Give me at least a restart!!!
;; (setf sb-ext:*on-package-variance* '(:warn nil :error t))


;;; Create some default ruletables
;; (eval-when (:compile-toplevel :load-toplevel)
;;   (dolist (domain '(stacked horizontal vertical :treble))
;;     (register-domain domain)))
;; (register-domain :treble)







;;; PITCH CAUSING ERROR!!!!

;; (defrule :treble (:spn (cons symbol unsigned-byte)) (obj diranc)
;;   (let ((pitch (car (spn obj)))
;; 	(octave (cdr (spn obj))))
;;     (print (symbol-value pitch))
;;     (setf (y-offset obj)
;; 	  ;; Pitch
;; 	  (+ (* (/ *staff-space* 2)
;; 		(case pitch
;; 		  (b 0) (a 1) (g 2) (f 3))
;; 		)
;; 	     ;; Octaves
;; 	     (* 7/8 (fixed-height diranc) (- 4.0 octave)))
;; 	  ))
;;   )



;; (defrule :treble (:spn (cons (eql b) unsigned-byte)) (obj diranc)
;;   (setf (y obj)	
;;   	(+ (y diranc)
;;   	   (* (- 4.0 (cdr (spn obj)))
;; 	      7/8 (fixed-height diranc)))))

;; (defrule :treble (:spn (cons (eql g) unsigned-byte)) (o da)
;;   (setf (y o)
;; 	(+ (y da) (* .25 (fixed-height da)))))

;; (defrule :treble (:spn (cons (eql f) unsigned-byte)) (o da)
;;   (setf (y o)
;; 	(+ (y da) (* -.25 (fixed-height da)))))






;; (define-stacked-rule (:id) (v)
;;   (loop for i from -2 to 2
;; 	for y = (+ (y v) (* i (/ (fixed-height v) 4)))
;; 	do (push (svg:line (left v) y (+ (left v) (width v)) y
;; 			   :stroke-width 30
;; 			   :stroke-linecap "round"
;; 			   :stroke "black")
;; 		 (svglst v))))

