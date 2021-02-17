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
;;        (s0 (make-form :s
;; 	    :id 's0
;; 	    :content (list n)))
;;        (s1 (make-form :s :toplevelp t
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


;;;;;;;;;;;;;;;;;;;;;test
(let* ((n (make-note '(c . 4)
		     :id 'n
		     :head (make-mchar  'clefs.c :id 'nh)
		     :canvas-vis-p t))
       (n2 (make-note '(f . 4) :id 'n2
       			       ;; :x-offset 20
       			       :head (make-mchar  'clefs.g :id 'nh2)
       			       :canvas-vis-p t))
       (s (make-form :s :content (list n )
		 :id 's
		 :canvas-vis-p t))
       (s2 (make-form :s :content (list n2) :id 's2
       		  :canvas-color "green"

       		  ))
       (h (hform :content (list s s2) :id 'h
		 :canvas-color "black"
		 :toplevelp t)))
  (render (list h))
  )

;;; Kann sowas benutzen als ruler
(typep (funcall (comp #'children #'(lambda (l)
					 (mapcar #'type-of
						 (mapcar #'second (sort l #'< :key #'car)))))
		(hform :toplevelp t
		       :content (list (make-form :s :content (list (hform))))))
       '(cons (eql stacked-form) (cons (eql HORIZONTAL-form) null) ))


(flet ((rndacc ()
	 (nth (random 3) '(accidentals.flat accidentals.natural accidentals.sharp)))
       (foo (n)
	 (loop for i below n
	       for d = (nth (random 3) '(1 1/2 1/4))
	       for p = (nth (random 5) '(a b e f g))
	       for a = (nth (random 3) '(accidentals.flat accidentals.natural accidentals.sharp))
	       for c = (nth (random 3) '(clefs.c clefs.g clefs.f ))
	       collect (make-sform :content (list (make-note (cons 'b 4) d)))
	       ;; when (and (zerop (random 2)) (/= i 0) (/= i (1- n)))
	       ;; collect (make-sform :content (list (make-acc 'accidentals.sharp)))
	       ;; when (and (zerop (random 4)) (/= i 0) (/= i (1- n)))
	       ;; 	 collect(make-form :s :content (list (make-mchar c)))

	       )))
  (let* ((absx 30)
	 (h (make-hform
	     :id 'normseq
	     :domain 'julian
	     ;; :ruler 'content
	     :width (mm-to-px 104)	;393.07086 695.4331
	     ;; :canvas-vis-p nil
	     :canvas-color "black"
	     :canvas-opac 1
	     ;; :origin-visible-p nil
	     :absx absx
	     :toplevelp t
	     :content (foo 20)
	     ;; (list (make-form :s :content (list (make-note (cons 'b 4) dur)))		   
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.sharp)))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur)))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) dur))))
	     
	     ;; (list (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))

	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.natural)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.natural)))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.flat)))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-acc (rndacc))))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.sharp)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1)))))
	     ;; 	   (make-form :s :content (list (make-acc 'accidentals.sharp)))
	     ;; 	   (make-form :s :content (list (make-note (cons 'b 4) (nth (random 3) '(1/2 1/4 1))))))
	     
	     ;; (append
	     ;;  ;; (list (make-form :s :content (list (make-instance 'barline))))
	     ;;  (loop repeat 20
	     ;; 	  for i = (nth (random 3) '(1/2 1/4 1))
	     ;; 	  for p = (nth (random 3) '(d f g))
	     ;; 	  for o = (nth (random 2) '(4 5))
	     ;; 	  collect (make-form :s :content (list (make-note (cons 'b 4) i)))
	     ;; 	  collect (make-form :s :content (list (make-acc 'accidentals.sharp)))
	     ;; 	  )
	     ;;  ;; (list (make-form :s :content (list (make-instance 'barline))))
	     ;;  )
	     :preproc (preproc x
	  		((typep x 'mchar)
	  		 (setf 
	  		  (domain x) :treble
	  		  ;; (canvas-vis-p x) nil
	  		  (origin-visible-p x) nil
	  		  ))
	  		((eq (class-name (class-of x)) 'note)
	  		 (setf
	  		  ;; Doubling the width temporarily to ease reading
	  		  ;; (ruler x) '(:spn)
	  		  (domain x) :treble
	  		  ;; (canvas-vis-p x) nil
			  ;; (canvas-opac x) 1
	  		  (origin-visible-p x) nil
	  		  ))
	  		((or (eq (class-name (class-of x)) 'stacked-form)
			     (typep x 'barline))
	  		 (setf
			  (domain x) 's
	  		  ;; (canvas-vis-p x) nil
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
	 ;; 	 collect (make-form :s :content (list (make-note '(b . 4) d
	 ;; 						  :domain :treble
	 ;; 						  :canvas-vis-p t
	 ;; 						  :origin-visible-p nil))
	 ;; 			:canvas-vis-p nil
	 ;; 			:origin-visible-p nil))
	 
	 (list (make-form :s :content (list (make-note '(b . 4) 1 :domain :treble)))
	       (make-form :s :content (list (make-note  '(b . 4) 1 :domain :treble)))
	       (make-form :s :content (list (make-note  '(c . 5) 1 :domain :treble)))
	       ;; (make-form :s :content (list (make-instance 'barline)))
	       (make-form :s :content (list (make-note  '(d . 5) 1 :domain :treble)))
	       (make-form :s :content (list (make-note  '(c . 5) 1 :domain :treble)))
	       ;; (make-form :s :content (list (make-instance 'barline)))
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
	  (list (make-form :s :id 's
	  	 :content
	  	 (list (make-mchar 'clefs.c))
	  	 ))
	  )
	 ))

  (render h :apprulp nil))

(let ((h (hform :toplevelp t
		:lineup nil
		:content (loop for i to 20
			       collect (let ((s (make-form :s :x-offset (* 60 (cos i))
						       :y-offset (* 60 (sin i))
						       :content (list (make-mchar 'scripts.stopped
										  )))))
					 (smt-engine::x-offset s)
					 s)))))
  (render h)
)


(render
 (let* ((s (make-form :s :s
		     :x-offset 10
		     :content (list (make-mchar 'clefs.g
						))))
       
       (h (make-form :s :h :toplevelp t
		     ;; :domain nil
		     ;; :canvas-vis-p nil
			:content (list s))))
   (clock-heads (content h))
   )
 :drawp nil
 :apprulp nil)

