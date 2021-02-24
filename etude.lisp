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
(remrule 0.1) (ruledocs)

(flet ((rndacc ()
	 (nth (random 3) '(accidentals.flat accidentals.natural accidentals.sharp)))
       (foo (n)
	 (loop with x = 1
	       for i below n
	       for d = (nth (random 3) '(1 1/2 1/4))
	       for p = (nth (random 5) '(a b e f g))
	       for a = (nth (random 3) '(accidentals.flat accidentals.natural accidentals.sharp))
	       for c = (nth (random 2) '(clefs.c clefs.g clefs.f ))
	       
	       when (and (zerop (+ 0 (random 5))) (/= i 0) (/= i (1- n)))
		 collect (make-accidental :mchar 'bemol :spn (cons 'b 4)
					  :domain :treble
					  :right-side-space
					  *accidental-right-side-space*)

	       collect (make-note :spn (cons 'b 4) :dur d :domain :treble)
	       
	       
	       )))
  (let* ((absx 30)
	 (h (make-hform
	     :id 'normseq
	     :domain 'julian
	     ;; :ruler 'content
	     :width (mm-to-px 184)	;695.4331
	     ;; :canvas-vis-p nil
	     :canvas-color "black"
	     :canvas-opac .21
	     ;; :origin-visible-p nil
	     :absx absx
	     :toplevelp t
	     :content (foo 6)
	     :preproc (preproc x
	  		((typep x 'mchar)
	  		 (setf 
	  		  (domain x) :treble
	  		  (canvas-vis-p x) nil
	  		  (origin-visible-p x) nil
	  		  ))
	  		;; ((eq (class-name (class-of x)) 'note)
	  		;;  (setf
	  		;;   ;; Doubling the width temporarily to ease reading
	  		;;   ;; (ruler x) '(:spn)
	  		;;   (domain x) :treble
	  		;;   ;; (canvas-vis-p x) nil
			;;   ;; (canvas-opac x) 1
	  		;;   ;; (origin-visible-p x) nil
	  		;;   ))
	  		((or (typep x 'stacked-form) (typep x 'barline))
	  		 (setf
			  ;; (domain x) 's
	  		  (canvas-vis-p x) nil
			  (canvas-color x)
			  (nth (random 13)
			       '("lightgreen" "yellow" "springgreen" "lightblue" "deeppink"
				 "red" "green" "orange" "pink" "blue" "gray" "cyan"
				 "darkkhaki"))
			  (canvas-opac x) .8
	  		  (origin-visible-p x) nil
	  		  )))))
	 )
    (render h)

    ))

;;; ;;;;;;;;;;;;;;;;



;;; Kann nicht?
(render (hform :toplevelp t) :apprulp nil)




