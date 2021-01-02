;;; Testing main xmlUTILS added locally


(in-package :ngn)

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
(defparameter *staff-line-thickness* 30)

;;; Stave
(defrule null (stacked-form) (stacked) ("Drawing staff lines")
  (null (me)
	(loop
	  :for line-idx :from -2 :to 2
	  :for line-y = (+ (* line-idx *staff-space*) (y me))
	  :do (packsvg me (svg:line (left me) line-y (+ (left me) (width me)) line-y
				    :stroke-width *staff-line-thickness*
				    :stroke-linecap "round"
				    :stroke "black")))))
(ruledocs)
(defrule spn (notehead) (:treble)
    ("Assigns correct vertical positions to note-heads,
 based on their pitch-name and their octave.")
  ((cons symbol unsigned-byte)
   (me parent)
   (let ((pitch-name (car (spn me)))
	 (octave (cdr (spn me))))
     (setf (y me)
           (+ (- (fixed-bottom parent)
		 (case pitch-name
		   (c (- *staff-space*))
		   (d (- (* .5 *staff-space*)))
		   (e 0)
		   (f (* .5 *staff-space*))
		   (g *staff-space*)
		   (a (* 1.5 *staff-space*))
		   (b (* 2 *staff-space*))))
	      (* (- 4 octave) 7/8 (fixed-height parent)))))))




(defparameter *octave-space* (* 3.5 *staff-space*))
(defun down-stem-p (spn)
  "Decides about the direction of a stem,
 based on the pitch-name and the octave of SPN."
  (let ((pitch-name (car spn))
	(octave (cdr spn)))
    (or (>= octave 5)
	(and (eq pitch-name 'b) (= octave 4)))))

(defrule null (note) (:treble)
    ("Draws stem lines on the <correct> side of the note N.")  
  (null (n)
	;; Give the note object N a stem only when it's dur < whole-note
	(when (< (dur n) 1)
	  (let* ((dx .44)
		 (dy 1.3)
		 (head-top (top (head n)))
		 (head-center (* (height (head n)) .5))
		 (stem-onset (+ head-top head-center)))
	    (packsvg n
		      (svg:line (if (down-stem-p (spn n))
				    (+ dx (x (head n)))
				    (- (right (head n)) dx)
				    )
				(if (down-stem-p (spn n))
				    (+ stem-onset dy)
				    stem-onset
				    )
				(if (down-stem-p (spn n))
				    (+ dx (x (head n)))
				    (- (right (head n)) dx)
				    )
				(if (down-stem-p (spn n))
				    (+ stem-onset *octave-space*)
				    (- stem-onset *octave-space*)
				    ) 
				:stroke-width (- *staff-line-thickness* 20)
				:stroke-linecap "round"
				:stroke "black"))))))


(defun contnotep (f) (typep (car (content f)) 'note))
(deftype snote () '(and stacked-form (satisfies contnotep)))
(defun allsn (l)
  (every #'(lambda (x) (typep x 'snote)) (content l)))
(deftype hsnote () '(and horizontal-form (satisfies allsn)))


(ruledocs)
*ruleidx*

(remrules 1)

(defrule content (horizontal-form) (t)
    ("Compute widths ")
  ((cons snote)(hf) 
   (dolist (d (content hf))
     (let ((n (car (content d))))
       (cond ((= (dur n) .25)
	      (incf (width d) (width d) ))
	     ((= (dur n) .5) (incf (width d) (* 2 (width d))))
	     ((= (dur n) 1) (incf (width d) (* 3 (width d)))))))
   (hlineup hf)))



(let ((h (hform
	  :ruler 'content
	  :canvas-vis-p t
	  :marker-vis-p t
	  :toplevelp t
	  :content (loop for dur in (loop repeat 20 collect (expt 2 (nth (random 3) '(0 -1 -2))))
			 for pitch in '(b a d f e a g f g c e d c)
			 for oct in '(4 4 4 5 4 4 5 4 4 5 5 5 5)
			 for color = "black"
			 ;; (nth (random 5)
				     ;; 	  '("green" "orange" "red" "pink" "blue"))
			 collect (sform :content (list ;; (make-note (cons pitch
						       ;; 		   (1+ oct))
						       ;; 	     :dur dur
						       ;; 	     ;; :x-offset (if (member dur '(1/2 .25) :test #'=) (- 5) 0)
						       ;; 	     :head-color color)
						       (make-note (cons pitch oct)
							     :dur dur
							     :head-color color)
						       (make-note (cons pitch
								   (1- oct))
							     :dur dur
							     ;; :x-offset (if (member dur '(1/2 .25) :test #'=) (- 5) 0)
							     :head-color color))
					))
	  :preproc (preproc x
	  	     ((typep x 'notehead)
	  	      (setf (ruler x) 'spn
	  		    (domain x) :treble
	  		    (canvas-vis-p x) nil
	  		    (marker-vis-p x) nil
	  		    ))
	  	     ((eq (class-name (class-of x)) 'note)
	  	      (setf
	  	       ;; Doubling the width temporarily to ease reading
	  	       ;; (ruler x) '(:spn)
	  	       (domain x) :treble
	  	       (canvas-vis-p x) nil
	  	       (marker-vis-p x) nil
	  	       ))
	  	     ((eq (class-name (class-of x)) 'stacked-form)
	  	      (setf
		       (canvas-color x) (nth (random 5)
					  '("green" "orange" "red" "cyan" "blue"))
	  	       (canvas-vis-p x) t
	  	       (marker-vis-p x) t
	  	       ))
	  	     )
	  )))
  ;; (incf (left (car (content h))) 10)
  (render (list h))
  )

;;;;;;;;;;;;;;;;;;;;;test
(let* ((n (make-note '(c . 4)
		     :id 'n
		     :head (make-notehead "s0" :id 'nh)
		     :canvas-vis-p nil))
       (n2 (make-note '(f . 4) :id 'n2
			       :head (make-notehead "s0" :id 'nh2)
			       :canvas-vis-p nil))
       (s (sform :content (list n (make-note nil :id 'n3

					     :head (make-notehead "s0" :id 'nh3)))
		 :id 's
		 :canvas-vis-p t))
       (s2 (sform :content (list n2) :id 's2
		  :canvas-color "green"))
       (h (hform :content (list s s2) :id 'h
		 :canvas-color "green"
		 :canvas-vis-p nil
		 :toplevelp t)))
  (render (list h))
  ;; (mapcar #'id (descendants h))
   )


(let* ((n (make-notehead "s1" :id 'n
			 :canvas-color "green"))
       (s (sform :content (list n) :id 's :toplevelp t)))
  ;; (print (mapcar #'top (list n s)))

  (render (list s))
  )



(ql:quickload "cxml")

(with-open-file (stream "/tmp/etude.xml" :direction :output :if-exists :supersede)
  (cxml:with-xml-output (cxml:make-character-stream-sink
			 stream :indentation 2 :canonical nil)
    (cxml:with-element "foo"
      (cxml:attribute "xyz" "abc")
      (cxml:attribute "asd" "8.123")
      (cxml:with-element "bar"
	(cxml:attribute "blub" "bla"))
      (cxml:text "Hi there."))))
