;;; in font



(in-package :smt-engine)

;; (setq n 
      
;;       )
;; (defparameter *n* )
;; (defparameter *s0* (form :stacked :chase-fill "green"
;; 				   :content (list *n*)))
;; (defparameter *s1* (form :stacked :toplevelp t
;; 				   :content (list *s0*)))
;; (let* ((n (notehead "s0" :id 'n))
;;        (s0 (sform
;; 	    :id 's0
;; 	    :content (list n)))
;;        (s1 (sform :toplevelp t
;; 		  :id 's1
;; 		  :content (list s0))))
;;   (print '__________)
;;   (incf (x n) -20)
;;   (render (list s1))
;;   ;; Print all left edges
;;   (print (mapcar #'left (list n s0 s1)))
;;   ;; Print all right edges
;;   (print (mapcar #'right (list n s0 s1)))
;;   )
(let* ((absx 40)
       (durs '(1 1/2 1/4))
       (h (hform
	   :id 'h
	   :ruler 'content
	   :width (mm-to-px 184)	;695.4331
	   :canvas-vis-p nil
	   :canvas-color "pink"
	   :canvas-opac 1
	   :origin-visible-p nil
	   :absx absx
	   :toplevelp t
	   :content (list
		     (sform :content (list (make-instance 'barline)))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-note '(b . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-instance 'barline)))
		     (sform :content (list (make-note '(d . 5) (nth (random (length durs)) durs))))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-instance 'barline)))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-instance 'barline)))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-instance 'barline)))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-note '(a . 4) (nth (random (length durs)) durs))))
		     (sform :content (list (make-instance 'barline)))
		     )
	   :preproc (preproc x
	  	      ((eq (class-name (class-of x)) 'note)
	  	       (setf
	  		;; Doubling the width temporarily to ease reading
	  		;; (ruler x) '(:spn)
	  		(domain x) :treble
	  		(canvas-vis-p x) nil
	  		(origin-visible-p x) nil
	  		))
	  	      ((or (eq (class-name (class-of x)) 'stacked-form)
			   (typep x 'barline))
	  	       (setf
	  		(canvas-vis-p x) nil
			(canvas-color x) "green"
	  		(origin-visible-p x) nil
	  		))
	  	      )
	   ))
       )
  (render (list h))
  (width h))
;;; Unit of Space width
(let* ((absx 40)
       (w 184)
       (absy 100)
       (h (hform
	   :id 'h
	   :absy absy
	   :ruler 'content
	   :width (mm-to-px w)
	   :canvas-vis-p nil
	   :canvas-color "pink"
	   :canvas-opac 1
	   :origin-visible-p nil
	   :absx absx
	   :toplevelp t
	   :content (list (sform :content (list (make-note '(a . 4) :dur 1/2)))
			  (sform :content (list (make-note '(b . 4) :dur 1/4)))
			  (sform :content (list (make-note '(c . 5) :dur 1/4)))
			  (sform :content (list (make-instance 'barline)))
			  (sform :content (list (make-note '(d . 5) :dur 1/2)))
			  (sform :content (list (make-note '(c . 5) :dur 1/2)))
			  (sform :content (list (make-instance 'barline)))
			  (sform :content (list (make-note '(d . 5) :dur 1/4)))
			  (sform :content (list (make-note '(c . 5) :dur 1/4)))
			  (sform :content (list (make-note '(b . 4) :dur 1/4)))
			  (sform :content (list (make-note '(a . 4) :dur 1/4)))
			  (sform :content (list (make-instance 'barline)))
			  (sform :content (list (make-note '(b . 4) :dur 1/4)))
			  (sform :content (list (make-note '(c . 5) :dur 1/4)))
			  (sform :content (list (make-note '(d . 5) :dur 1/4)))
			  (sform :content (list (make-note '(d . 5) :dur 1/4 :head-color "red")))
			  (sform :content (list (make-instance 'barline)))
			  (sform :content (list (make-note '(a . 4) :dur 1/2)))
			  (sform :content (list (make-note '(b . 4) :dur 1/4)))
			  (sform :content (list (make-note '(c . 5) :dur 1/4)))
			  (sform :content (list (make-instance 'barline)))
			  (sform :content (list (make-note '(d . 5) :dur 1/2)))
			  (sform :content (list (make-note '(c . 5) :dur 1/2)))
			  (sform :content (list (make-instance 'barline)))
			  )
	   :preproc (preproc x
	  	      ((typep x 'notehead)
		       ;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  	       (setf (ruler x) 'spn
	  		     (domain x) :treble
	  		     (canvas-vis-p x) nil
	  		     (origin-visible-p x) nil
	  		     ))
	  	      ((eq (class-name (class-of x)) 'note)
	  	       (setf
	  		;; Doubling the width temporarily to ease reading
	  		;; (ruler x) '(:spn)
	  		(domain x) :treble
	  		(canvas-vis-p x) nil
	  		(origin-visible-p x) nil
	  		))
	  	      ((or (eq (class-name (class-of x)) 'stacked-form)
			   (typep x 'barline))
	  	       (setf
	  		(canvas-vis-p x) nil
			(canvas-color x) "green"
	  		(origin-visible-p x) nil
	  		))
	  	      )
	   ))
       ;; H2 line 2
       (h2 (hform
	    :ruler 'content
	    :absy (incf absy 100)
	    :absx absx
	    :width (mm-to-px (- w 30))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(g . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25 :head-color "blue")))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(g . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; H3
       (h3 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 200
	    :width (mm-to-px (- w 60))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "green")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; ;;;;;;;;;;;;;;;;;
       (h4 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 0))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; ;;;;;;;;;;;;;;;;;;
       (h5 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 10))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; ;;;;;;;;;;;;;;;;;
       (h6 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 20))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; ;;;;;;;;;;;;;;;
       (h7 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 30))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; ;;;;;;;;;;;;;;;
       (h8 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 40))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; ;;;;;;;;;;;;;
       (h9 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 50))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       ;; ;;;;;;;;
       (h10 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 100)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 60))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       (h11 (hform
	    :ruler 'content
	    :absx absx
	    :absy (incf absy 70)
	    ;; :y-offset 300
	    :width (mm-to-px (- w 70))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :origin-visible-p nil
	    :toplevelp t
	    :content (list (sform :content (list (make-note '(a . 4) :dur .5)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .5)))
			   (sform :content (list (make-note '(c . 5) :dur .5)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(a . 4) :dur .25)))
			   (sform :content (list (make-instance 'barline)))
			   (sform :content (list (make-note '(b . 4) :dur .25)))
			   (sform :content (list (make-note '(c . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25)))
			   (sform :content (list (make-note '(d . 5) :dur .25 :head-color "gray")))
			   (sform :content (list (make-instance 'barline)))
			   )
	    :preproc (preproc x
	  	       ((typep x 'notehead)
			;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  		(setf (ruler x) 'spn
	  		      (domain x) :treble
	  		      (canvas-vis-p x) nil
	  		      (origin-visible-p x) nil
	  		      ))
	  	       ((eq (class-name (class-of x)) 'note)
	  		(setf
	  		 ;; Doubling the width temporarily to ease reading
	  		 ;; (ruler x) '(:spn)
	  		 (domain x) :treble
	  		 (canvas-vis-p x) nil
	  		 (origin-visible-p x) nil
	  		 ))
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (origin-visible-p x) nil
	  		 ))
	  	       )
	    ))
       )
  ;; (incf (left (car (content h))) 10)
  (render (list h h2 h3 h4 h5 h6 h7 h8 h9 h10 h11))
  )


;;;;;;;;;;;;;;;;;;;;;test
(let* ((n (make-note '(c . 4)
		     :id 'n
		     :head (make-mchar  'clefs.c :id 'nh)
		     :canvas-vis-p t))
       (n2 (make-note '(f . 4) :id 'n2
       			       ;; :x-offset 20
       			       :head (make-mchar  'clefs.g :id 'nh2)
       			       :canvas-vis-p t))
       (s (sform :content (list n )
		 :id 's
		 :canvas-vis-p t))
       (s2 (sform :content (list n2) :id 's2
       		  :canvas-color "green"

       		  ))
       (h (hform :content (list s s2) :id 'h
		 :canvas-color "black"
		 :toplevelp t)))
  (render (list h))
  )

(let* ((absx 30)
       (h (hform
	   :id 'h
	   :ruler 'content
	   :width (mm-to-px 50)		;188.97638
	   :canvas-vis-p nil
	   :canvas-color "pink"
	   :canvas-opac 1
	   :origin-visible-p nil
	   :absx absx
	   :toplevelp t
	   :content (append
		     (list (sform :content (list (make-instance 'barline))))
		     (loop repeat 20
			   for i = (nth (random 3) '(1/2 1/4 1))
			   collect (sform :content (list (make-note '(g . 5) i))))
		     (list (sform :content (list (make-instance 'barline))))
		     )
	   :preproc (preproc x
	  	      ((typep x 'mchar)
		       ;; (format t "~&Notehead W: ~D U ~D~%" (width x) u)
	  	       (setf 
	  		(domain x) :treble
	  		(canvas-vis-p x) nil
	  		(origin-visible-p x) t
	  		))
	  	      ((eq (class-name (class-of x)) 'note)
	  	       (setf
	  		;; Doubling the width temporarily to ease reading
	  		;; (ruler x) '(:spn)
	  		(domain x) :treble
	  		(canvas-vis-p x) nil
			;; (canvas-opac x) 1
	  		(origin-visible-p x) nil
	  		))
	  	      ((or (eq (class-name (class-of x)) 'stacked-form)
			   (typep x 'barline))
	  	       (setf
	  		(canvas-vis-p x) nil
			(canvas-color x) (nth (random 6) '("red" "green" "orange" "pink" "blue" "gray"))
			(canvas-opac x) 1
	  		(origin-visible-p x) nil
	  		)))))
       )
  (render (list h))
  )

;;; ;;;;;;;;;;;;;;;;
(let* (
       (h (hform
	   ;; :absy absy
	   ;; :ruler 'content
	   :width (mm-to-px 40)		;151.1811
	   :canvas-vis-p nil
	   ;; :canvas-color "pink"
	   ;; :canvas-opac 1
	   :origin-visible-p nil
	   :absx 30
	   :toplevelp t
	   :content
	   (loop for x to 10
		 for d = (nth (random 3) '(1 1/2 1/4))
		 collect (sform :content (list (make-note '(b . 4) d
							  :domain :treble
							  :canvas-vis-p t
							  :origin-visible-p nil))
				:canvas-vis-p nil
				:origin-visible-p nil))
	   ;; (list (sform :content (list (make-note '(b . 4) 1 :domain :treble)))
	   ;; 	 (sform :content (list (make-note  '(b . 4) 1 :domain :treble)))
	   ;; 	 (sform :content (list (make-note  '(c . 5) 1 :domain :treble)))
	   ;; 	 ;; (sform :content (list (make-instance 'barline)))
	   ;; 	 (sform :content (list (make-note  '(d . 5) 1 :domain :treble)))
	   ;; 	 (sform :content (list (make-note  '(c . 5) 1 :domain :treble)))
	   ;; 	 ;; (sform :content (list (make-instance 'barline)))
	   ;; 	 )
	   ;; :preproc (preproc x
	   ;; 	      ((typep x 'note)
	   ;; 	       (setf
	   ;; 		;; Doubling the width temporarily to ease reading
	   ;; 		;; (ruler x) '(:spn)
	   ;; 		(domain x) :treble
	   ;; 		(canvas-vis-p x) nil
	   ;; 		(origin-visible-p x) nil
	   ;; 		;; (origin-visible-p (car (content x))) nil
	   ;; 		))
	   ;; 	      ;; ((or (eq (class-name (class-of x)) 'stacked-form)
	   ;; 	      ;; 	   (typep x 'barline))
	   ;; 	      ;;  (let ((colors '("green" "red" "gray" "orange" "pink" "blue")))
	   ;; 	      ;; 	 (setf
	   ;; 	      ;; 	  (canvas-vis-p x) t
	   ;; 	      ;; 	  (canvas-color x) (nth (random (length colors)) colors)
	   ;; 	      ;; 	  (origin-visible-p x) nil
	   ;; 	      ;; 	  )))
	   ;; 	      )
	   ))
       )
  ;; (incf (left (car (content h))) 10)
  (render (list h))
  (print (children h))
  )

(let ((h (hform
	  :toplevelp t
	  :id 'h
	  :content
	  (list (sform :id 's
		 :content
		 (list (make-note '(b . 4) 1/2
				  :id 'n
				  :domain :treble))
		 ))
	  )
	 ))

  (render (list h))
    (enumerate-generations h 0)
  (mapcan #'cdr (children h ))


  )


;;; Setf content
(dotimes (i (length (ruledocs)))
  (remrule i))
