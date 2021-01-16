;;; Testing main xmlUTILS added locally



(in-package :ngn)
(subtypep (typep (make-note nil) 'temporal) )
(mapcar #'id (remove-if-not #'(lambda (x) (typep x 'temporal))
		(list (make-note nil :id 'n1) (make-note nil :id 'n2)
		      'asd (make-note nil :id 'n3) 3)
		))
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
 based on their pitch-name and their octave." 1)
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
(ruledocs)
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
				:stroke-width (- *staff-line-thickness* 5)
				:stroke-linecap "round"
				:stroke "black"))))))
(ruledocs)
(remrule 2)

;; (defun contnotep (f) (typep (car (content f)) 'note))
;; (deftype snote () '(and stacked-form (satisfies contnotep)))

(defun only-type-p (lst type)
  (and (= (list-length lst) 1)
       (typep (car lst) type)))

(defun pure-temp-sform-seq-p (content)
  "Einstimmig Noten oder Pausen"
  (every #'(lambda (x) (and (sformp x)
			    (only-type-p (content x)
					 '(or note pause))))
	 content))

(deftype pure-temp-sform-seq () '(satisfies pure-temp-sform-seq-p))

(ruledocs)
*ruleidx*
(remrule 6)

(defrule content (horizontal-form) (t)
    ("Compute widths so dass jede Note bzw.  Pause die dafür geltenden
Bereich oder Platz oder Raum in Anspruch nimmt. Dies wird dann
hilfreich sein, wenn Horizontale Form das Zeug verarbeiten soll."  0)
  (pure-temp-sform-seq (hf)
		       (let* ((u (mm-to-pxl (/ 160 15.72)))
			      (uh (* u (/ 5 3.5)))
			      (uw (* u (/ 7 3.5))))
			 (dolist (d (content hf))
			   (let ((n (car (content d))))
			     (cond ((= (dur n) .25) (setf (width d) u))
				   ((= (dur n) .5) (setf (width d) uh))
				   ((= (dur n) 1) (setf (width d) uw))))))
		       (hlineup hf))
  (t (hf)
     (let* ((note-sforms (remove-if-not #'(lambda (x) (and (sformp x)
							   (only-type-p (content x)
									'note)))
					(content hf)))
	    (blinesforms (remove-if-not #'(lambda (x) (and (sformp x)
							   (only-type-p (content x)
									'barline)))
					(content hf)))
	    (qnote-sforms (remove-if-not #'(lambda (x) (= .25 (dur (car (content x))))) note-sforms))
	    (hnote-sforms (remove-if-not #'(lambda (x) (= .5 (dur (car (content x))))) note-sforms))
	    (u (/ (- (width hf)
		     (* 2 (length blinesforms)))
		  (+ (length qnote-sforms)
		     (* (length hnote-sforms) (/ 5 3.5)))
		  ;; 15.72
		  ))
	    (uh (* u (/ 5 3.5)))
	    (uw (* u (/ 7 3.5)))
	    (l (length (content hf))))
       (dotimes (i l)
	 (let* ((d (nth i (content hf)))
		(n (car (content d))))
	   (typecase n
	     (note (cond ((= (dur n) .25) (setf (width d) u))
			 ((= (dur n) .5) (setf (width d) uh))
			 ((= (dur n) 1) (setf (width d) uw))))
	     (barline (when (< i (1- l))
			(setf (width d) (mm-to-pxl 2)))))))
       )
     (hlineup hf))
  )

(defrule null (barline) (t)
    ("Barline" 8)
  (null (me parent)
	(packsvg parent
		 (svg:line (left parent) (top parent)
			   (left parent) (bottom parent)
			   :stroke-width (+ *staff-line-thickness* 10)
			   :stroke-linecap "square"
			   :stroke "black")))
  )
(ruledocs)
(remrule 7)
;;; Unit of Space width
(let* ((absx 60)
       (w 160)
       (h (hform
	   :id 'h
	   :ruler 'content
	   :width (mm-to-pxl w)
	   :canvas-vis-p nil
	   :canvas-color "pink"
	   :canvas-opac 1
	   :marker-vis-p nil
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
	  	      ((or (eq (class-name (class-of x)) 'stacked-form)
			   (typep x 'barline))
	  	       (setf
	  		(canvas-vis-p x) nil
			(canvas-color x) "green"
	  		(marker-vis-p x) nil
	  		))
	  	      )
	   ))
       ;; H2 line 2
       (h2 (hform
	    :ruler 'content
	    :y-offset 100
	    :absx absx
	    :width (mm-to-pxl (- w 30))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :marker-vis-p nil
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
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (marker-vis-p x) nil
	  		 ))
	  	       )
	    ))
       ;; H3
       (h3 (hform
	    :ruler 'content
	    :absx absx
	    :y-offset 200
	    :width (mm-to-pxl (- w 60))
	    :canvas-vis-p nil
	    :canvas-color "pink"
	    :canvas-opac 1
	    :marker-vis-p nil
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
	  	       ((or (eq (class-name (class-of x)) 'stacked-form)
			    (typep x 'barline))
	  		(setf
	  		 (canvas-vis-p x) nil
			 (canvas-color x) "green"
	  		 (marker-vis-p x) nil
	  		 ))
	  	       )
	    ))
       )
  ;; (incf (left (car (content h))) 10)
  (render (list h h2 h3))
  )

;;;;;;;;;;;;;;;;;;;;;test
(let* ((n (make-note '(c . 4)
		     :id 'n
		     :head (make-notehead "s0" :id 'nh)
		     :canvas-vis-p nil))
       (n2 (make-note '(f . 4) :id 'n2
			       ;; :x-offset 20
			       :head (make-notehead "s0" :id 'nh2)
			       :canvas-vis-p nil))
       (s (sform :content (list n (make-note nil :id 'n3

					     :head (make-notehead "s0" :id 'nh3)))
		 :id 's
		 :canvas-vis-p t))
       (s2 (sform :content (list n2) :id 's2
		  :canvas-color "green"
		  ))
       (h (hform :content (list s s2) :id 'h
		 :canvas-color "black"  :width 15
		 :toplevelp t)))
  (render (list h))
  (width h)
  )


(let* ((n (make-notehead "s1" :id 'n
			 :canvas-color "green"))
       (s (sform :content (list n) :id 's :toplevelp t)))
  ;; (print (mapcar #'top (list n s)))

  (render (list s))
  )

(bcr-height (get-bcr "clefs.C" .font-family.))
(- 191 (+ 14 12 2.5 4.5
    1 1.5 2 1.5 2.5 2 2.5 1
    2.5 2
    4 1.5
	  1))133.0
;;; Achtel
(/ (+ 3 4 3 3 4 4 3 4 4 3.5 3.5 4 3.5 5)
   14)3.6785715 mm
;;; Viertel
(/ (+ 4 4 6 6 4 5.5 6 6)
   8)5.1875 mm
(/ 5.1875 3.6785715)1.4101942		;Ein Viertel ist so viel mal größer als ein Achtel
;;; Halbe
(/ (+ 8 7 7.5) 3)7.5
(/ 7.5 5.1875)1.4457831			;Halbe / Viertel
;;; Halbe punktiert
(/ (+ 9 9) 2)9
(/ 9 7.5)1.2				;PHalbe / Halbe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.4101942 : 1.4457831 : 1.2 ;;;
#|
1 Viertel 1.4101942 Achtelabstände
1 Halbe  (* 1.4101942 1.4457831) => 2.038835 Achtelabstände
1 Punktierte Halbe (* 2.038835 1.2) => 2.446602 Achtelabstände
|#
;;; Summe
(+ (+ 3 4 3 3 4 4 3 4 4 3.5 3.5 4 3.5 5)
       (+ 4 4 6 6 4 5.5 6 6) (+ 8 7 7.5) (+ 9 9))133.5
(let ((u (/ 133 29.0)))
  (+ (* 14 u) (* 8.8 u) (* 3.6 u) (* 2.6 u)))
;;;;;;;;;;;
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



