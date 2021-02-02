;;; in font



(in-package :smt)
(ruledocs)
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
(setf l '(1 2 3 4 5 6)
      r (remove-if #'oddp l))
(dolist (x r)
  (incf x))


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

;;; Kann sowas benutzen als ruler
(typep (funcall (compfunc #'children #'(lambda (l)
					 (mapcar #'type-of
						 (mapcar #'second (sort l #'< :key #'car)))))
		(hform :toplevelp t
		       :content (list (sform :content (list (hform))))))
       '(cons (eql stacked-form) (cons (eql HORIZONTAL-form) null) ))


(flet ((rndacc ()
	 (nth (random 3) '(accidentals.flat accidentals.natural accidentals.sharp)))
       (foo (n)
	 (loop repeat n
	       for d = (nth (random 3) '(1 1/2 1/4))
	       for a = (nth (random 3) '(accidentals.flat accidentals.natural accidentals.sharp))
	       when (zerop (random 2))
		 collect (sform :content (list (make-acc a)))
	       collect (sform :content (list (make-note (cons 'b 4) d)))
	       )))
  (let* ((absx 30)
	 (dur 1/4)
	 (h (hform
	     :id 'seq
	     :ruler 'content
	     :width (mm-to-px 150)	;566.92914
	     ;; :canvas-vis-p nil
	     :canvas-color "black"
	     :canvas-opac .1
	     ;; :origin-visible-p nil
	     :absx absx
	     :toplevelp t
	     :content (let ((f (foo 40)))
			(print (mapcar #'content f)) f)
	     ;; (list (sform :content (list (make-note (cons 'b 4) dur)))		   
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (sform :content (list (make-acc 'accidentals.sharp)))
	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) dur))))
	     
	     ;; (list (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-acc (rndacc))))

	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-acc 'accidentals.natural)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-acc 'accidentals.natural)))
	     ;; 	   (sform :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-acc (rndacc))))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-acc 'accidentals.sharp)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (sform :content (list (make-acc 'accidentals.sharp)))
	     ;; 	   (sform :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1))))))
	     
	     ;; (append
	     ;;  ;; (list (sform :content (list (make-instance 'barline))))
	     ;;  (loop repeat 20
	     ;; 	  for i = (nth (random 3) '(1/2 1/4 1))
	     ;; 	  for p = (nth (random 3) '(d f g))
	     ;; 	  for o = (nth (random 2) '(4 5))
	     ;; 	  collect (sform :content (list (make-note (cons 'b 4) i)))
	     ;; 	  collect (sform :content (list (make-acc 'accidentals.sharp)))
	     ;; 	  )
	     ;;  ;; (list (sform :content (list (make-instance 'barline))))
	     ;;  )
	     :preproc (preproc x
	  		((typep x 'mchar)
	  		 (setf 
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
			  ;; (canvas-opac x) 1
	  		  (origin-visible-p x) nil
	  		  ))
	  		((or (eq (class-name (class-of x)) 'stacked-form)
			     (typep x 'barline))
	  		 (setf
			  (domain x) 's
	  		  (canvas-vis-p x) nil
			  (canvas-color x)
			  (nth (random 13) '("lightgreen" "yellow" "springgreen" "lightblue" "deeppink" "red" "green" "orange" "pink" "blue" "gray" "cyan"
					     "darkkhaki"))
			  (canvas-opac x) .8
	  		  (origin-visible-p x) nil
	  		  )))))
	 )
    (render h)
    ))

;;; ;;;;;;;;;;;;;;;;
(render (hform
	 :id 'seq
	   ;; :ruler 'content
	 :width (mm-to-px 40)		;151.1811
	 :canvas-vis-p nil
	 ;; :canvas-color "pink"
	 ;; :canvas-opac 1
	 :origin-visible-p nil
	 :absx 30
	 :toplevelp t
	 :content

	 ;; (loop for x to 10
	 ;; 	 for d = (nth (random 3) '(1 1/2 1/4))
	 ;; 	 collect (sform :content (list (make-note '(b . 4) d
	 ;; 						  :domain :treble
	 ;; 						  :canvas-vis-p t
	 ;; 						  :origin-visible-p nil))
	 ;; 			:canvas-vis-p nil
	 ;; 			:origin-visible-p nil))
	 
	 (list (sform :content (list (make-note '(b . 4) 1 :domain :treble)))
	       (sform :content (list (make-note  '(b . 4) 1 :domain :treble)))
	       (sform :content (list (make-note  '(c . 5) 1 :domain :treble)))
	       ;; (sform :content (list (make-instance 'barline)))
	       (sform :content (list (make-note  '(d . 5) 1 :domain :treble)))
	       (sform :content (list (make-note  '(c . 5) 1 :domain :treble)))
	       ;; (sform :content (list (make-instance 'barline)))
	       )
	 :preproc (preproc x
	   	    ((typep x 'note)
	   	     (setf
	   	      ;; Doubling the width temporarily to ease reading
	   	      ;; (ruler x) '(:spn)
	   	      (domain x) :treble
	   	      (canvas-vis-p x) nil
	   	      (origin-visible-p x) nil
	   	      ;; (origin-visible-p (car (content x))) nil
	   	      ))
	   	    ;; ((or (eq (class-name (class-of x)) 'stacked-form)
	   	    ;; 	   (typep x 'barline))
	   	    ;;  (let ((colors '("green" "red" "gray" "orange" "pink" "blue")))
	   	    ;; 	 (setf
	   	    ;; 	  (canvas-vis-p x) t
	   	    ;; 	  (canvas-color x) (nth (random (length colors)) colors)
	   	    ;; 	  (origin-visible-p x) nil
	   	    ;; 	  )))
	   	    )
	 )
  )


;;; Kann nicht?
(render (hform :toplevelp t) :apprulp nil)


(let ((h (hform
	  :toplevelp t
	  :id 'seq
	  ;; :domain nil
	  :origin-visible-p nil
	  :canvas-vis-p nil
	  :canvas-color "green"
	  :content
	  (list (sform :id 's
	  	 :content
	  	 (list (make-mchar 'clefs.c))
	  	 ))
	  )
	 ))

  (render h :apprulp nil))



(ql:quickload :cxml)
(in-package :cxml)
(defun make-line (x y)
  (list "name" "line" "x" x "y" y))
(setq l (list (make-line 1 2) (make-line 3 4)))
(with-open-file (s "/tmp/stream" :direction :output
				 :if-does-not-exist :create
				 :if-exists :supersede)
  (with-xml-output (make-character-stream-sink s :indentation 2 :canonical nil)
    (with-element "svg"
      (attribute "xmlns" "http://www.w3.org/2000/svg")
      (attribute "xmlns:xlink" "http://www.w3.org/1999/xlink")
      (attribute "width" "793.7008")
      (attribute "height" "1122.5198")
      (with-element "rect"
	(attribute "transform" "scale(1 -1)")
	(attribute "fill-opacity" "1")))
    
    ))

(ql:quickload :s-xml)
(in-package :s-xml)

(cdr (assoc "x"(xml-element-attributes (make-xml-element :name "line" :attributes '(("x" . 23.1423)))
				   )
	:test #'string=))

(print-xml
 (xml-element-name (car )) :pretty t :input-type :xml-struct)
((NS-0:|svg| :|xmlns| "http://www.w3.org/2000/svg" :|xmlns:xlink|
  "http://www.w3.org/1999/xlink" :|version| "1.1" :|viewBox|
  "-129 -529 200 1000")
 ((NS-0:|g| :|transform| "matrix(1 0 0 -1 0 800)")
  ((NS-0:|path| :|fill| "currentColor" :|d|
    "M18 -175v-121c0 -13 5 -18 14 -18c17 0 49 21 77 37c8 5 16 -5 11 -13c-50 -72 -79 -154 -112 -235c-1 -3 -4 -4 -7 -4s-5 1 -6 4c-33 81 -62 163 -112 235c-5 8 3 18 11 13c28 -16 60 -37 77 -37c9 0 14 5 14 18v158c-3 187 -5 375 -8 562c0 13 -5 18 -13 18
c-17 0 -48 -23 -76 -39c-8 -5 -17 2 -17 10c0 2 1 4 2 6c50 71 86 151 118 231c2 4 5 6 9 6s7 -2 9 -6c32 -80 68 -160 118 -231c7 -10 -4 -22 -15 -16c-28 16 -59 39 -76 39c-8 0 -13 -5 -13 -18c-1 -104 -4 -208 -5 -312c26 21 59 34 93 34c52 0 89 -48 89 -102
c0 -80 -86 -117 -147 -169c-16 -14 -35 -29 -35 -50zM27 41l-1 -66v-11c0 -22 1 -44 4 -66c45 38 93 80 93 139c0 33 -14 67 -43 67c-31 0 -52 -30 -53 -63z"))))


(symbol-name (xml-element-name
	      (parse-xml-file "/home/amir/haydn/svg/accidentals.flat.arrowboth_haydn-11.svg"
		  :output-type :xml-struct)))

(symbol-package '*package*)
(with-open-file (s "/tmp/s.xml"
		   :if-exists :supersede
		   :direction :output)
  (format s "<?xml version=\"1.0\" standalone=\"no\"?>~%")
  (print-xml-dom (make-xml-element :name "svg"
				   :attributes '(("xmlns" . "http://www.w3.org/2000/svg")
						 ("xmlns:xlink" . "http://www.w3.org/1999/xlink")
						 ("viewBox" . "-129 -529 200 1000")
						 ("version" . "1.1")
						 )
				   :children (list (make-xml-element :name "g"
								:attributes '(("transform" . "matrix(1 0 0 -1 0 800)"))
								:children (list (make-xml-element :name "path"
												  :attributes '(("d" . "M18 -175v-121c0 -13 5 -18 14 -18c17 0 49 21 77 37c8 5 16 -5 11 -13c-50 -72 -79 -154 -112 -235c-1 -3 -4 -4 -7 -4s-5 1 -6 4c-33 81 -62 163 -112 235c-5 8 3 18 11 13c28 -16 60 -37 77 -37c9 0 14 5 14 18v158c-3 187 -5 375 -8 562c0 13 -5 18 -13 18
c-17 0 -48 -23 -76 -39c-8 -5 -17 2 -17 10c0 2 1 4 2 6c50 71 86 151 118 231c2 4 5 6 9 6s7 -2 9 -6c32 -80 68 -160 118 -231c7 -10 -4 -22 -15 -16c-28 16 -59 39 -76 39c-8 0 -13 -5 -13 -18c-1 -104 -4 -208 -5 -312c26 21 59 34 93 34c52 0 89 -48 89 -102
c0 -80 -86 -117 -147 -169c-16 -14 -35 -29 -35 -50zM27 41l-1 -66v-11c0 -22 1 -44 4 -66c45 38 93 80 93 139c0 33 -14 67 -43 67c-31 0 -52 -30 -53 -63z")
														("fill" . "currentColor")
														("transform" . "translate(1 2) scale(2 1) scale(3 4)")
														))))))
		 :xml-struct s t 0)
  (terpri s)
  (print-xml-dom (make-xml-element :name "fooo"
				   :attributes '(("x" . "123"))) :xml-struct s t 0))

(cdr (car (xml-element-attributes (car (xml-element-children (car (xml-element-children (parse-xml-file "/home/amir/haydn/svg/accidentals.doublesharp_haydn-11.svg" :output-type :xml-struct))))))))
