;;; Common Western Music Notation

(in-package :smt)




(defun only-type-p (lst type)
  (and (= (list-length lst) 1)
       (typep (car lst) type)))

(defun pure-temp-sform-seq-p (content)
  "Einstimmig Noten oder Pausen"
  (every #'(lambda (x) (and (sformp x)
			    (only-type-p (content x)
					 '(or note))))
	 content))

(deftype pure-temp-sform-seq () '(satisfies pure-temp-sform-seq-p))

(defrule identity (note) (:treble)
    (-1 "Deduces the notehead symbol from note's duration.")
  (note
   (noteobj)
   (let* ((dur (dur noteobj))
	  (hd (make-mchar (ecase dur
			    (1 'noteheads.s0)
     			    ((h 1/2) 'noteheads.s1)
			    (1/4 'noteheads.s2))
			  :id (gensym "NOTE")
     			  :origin-visible-p nil
     			  ;; :canvas-vis-p nil
			  ;; :font (case dur
     			  ;; 	  (1 (second .installed-fonts.))
			  ;; 	  (otherwise *font*))
			  )))
     (push hd (content noteobj))
     (setf (head noteobj) hd)
     )
   ))

(defrule mchar (accidental) (:treble)
    (-0.5 "Deduces the accidental symboil")
  (string
   (a)
   (cond ((string= (mchar a) "#")
	  (let ((c (make-mchar 'accidentals.sharp
			       )))
	    (push c (content a))
	    (setf (mchar a) c)))
	 ((string= (mchar a) "b")
	  (push (make-mchar 'accidentals.flat) (content a)))
	 ((string= (mchar a) "bekar")
	  (push (make-mchar 'accidentals.natural) (content a)))))
  (symbol
   (a)
   (cond ((eq(mchar a) 'sharp)
	  (let ((c (make-mchar 'accidentals.sharp)))
	    (push c (content a))
	    (setf (mchar a) c)))
	 ((eq (mchar a) 'bemol)
	  (let ((c (make-mchar 'accidentals.flat
			       :mchar-opac .5
			       :origin-visible-p nil
     			       ;; :canvas-vis-p nil
			       )))
	    (push c (content a))
	    (setf (mchar a) c))))))

(defun fspace-alloc (space widths diff-alloc-ratios)
  (let* ((sum (apply #'+ widths))
	 (r (- space sum))
	 (a (append
	     (list (* r (car diff-alloc-ratios)))
	     (loop for x in (cdr diff-alloc-ratios)
		   for w in widths
		   append (list w (* r x))))))
    (values a (= (apply #'+ a) space))))
;; (fspace-alloc 20 '(2 2 2) '(.25 .25 .25 .25))
(defparameter *guards*
  (list
   ;; It is not allowed to lean on right sides of the objects, their
   ;; guards must be strictly respected by all other characters!
   'barline (* *space* .5)
   'note (* *space* .1)
   ;; These rely only on their floating spaces to keep apart from other characters.
   'accidental 0)
  "Right-side guards in pixels (DANGER: Electric shock!).")

(defparameter *fspaces*
  (list 'barline 0 
	'accidental (* 1 *space*)
	)
  "Floating spaces; these are of fixed pixels only for non-clock characters.
Fspaces for clock characters are computed during punctuation.
These, if guard = 0, are at the same time the only space keeping
the types of mchars from next items.")

(defun decide-unit (durs)
  ""
  (let* ((counted (sort (mapcar #'(lambda (d) (cons (count d durs :test #'=) d))
				(remove-duplicates durs)) #'> :key #'car))
	 )
    ;; Es kann mehrere Dauren geben mit dergleichen Anzahl
    (flet ((look-at-first-counts (x)
	     (let ((max (first x)) l)
	       (dolist (i (cdr x) (push max l))
		 (when (= (car i) (car max))
		   (push i l))))))
      ;; Immer nimm die kleinste Dauer als unit (wenn mehrere Dauern gibt die
      ;; gleich viel vorkommen
      (apply #'min (mapcar #'cdr (sort (look-at-first-counts counted) #'< :key #'cdr)))
      )
    
    )
  ) 
(defparameter *duration-unit-space-reference*  
  '((1 . 7) (.5 . 5) (.25 . 3.5) (1/8 . 2.5) (1/16 . 2))
  )
(defparameter *minimum-legible-space* (* .3 *space*)
  "Distances smaller than this value (in pixels) are considered as collisions.")




(defun clock-heads (lst)
  "Subdivides lst into sublists where the head of each sublist
is a CLOCKED."
  (let (indices)
    (dotimes (i (list-length lst)
		(loop :for (start end) :on (nreverse indices)
		      :collect (subseq lst start end)))
      (when (typep (nth i lst) 'clock) (push i indices)))))

;;; This should ideally happen on the fly!
(defparameter *punctuation-units*
  '((1 . 7) (.5 . 5) (.25 . 3.5) (1/8 . 2.5) (1/16 . 2))
  "Proportions btwn durations expressed as ...")
(defun ufactor (udur dur2)
  (/ (cdr (assoc dur2 *punctuation-units* :test #'=))
     (cdr (assoc udur *punctuation-units* :test #'=))))


(defun n-compute-perfect-clock-punctuation (clocks-lst desired-width)
  (let* ((durs (mapcar #'dur clocks-lst))
	 (count-durs (mapcar #'(lambda (d)
				 (cons (count d durs :test #'=) d))
			     (remove-duplicates durs)))
	 (udur (decide-unit durs))	  ;Which of the durations is the unit?
	 (uwidth (/ desired-width
		    (apply #'+ (mapcar #'(lambda (x)
					   ;; car x=tedade clockedha, cdr dur
					   (* (car x) (ufactor udur (cdr x))))
				       count-durs))))
	 )
    ;; (print (mapcar #'(lambda (x)
    ;; 		       (list x (dur x)
    ;; 			     (* uwidth (ufactor udur (dur x)))
    ;; 			     (width x)
    ;; 			     (- (* uwidth (ufactor udur (dur x))) (width x))))
    ;; 		   clocks-lst))
    (dolist (c clocks-lst)
      (setf (right-side-space c)
	    ;; Exklusive width
	    (- (* uwidth (ufactor udur (dur c)))
	       (width c))))))

;(ngn::enumerate-generations (content (make-note :head (make-mchar 'clefs.c))) 0)
(defrule id (horizontal-form) (julian) (.1 "Punktieren Durchlauf 1:
Findet das idealle Spacing zwischen Clocks, tut so al ob gäbe es gaar keine
Non-clocks!")
  ((eql normseq) (h)
   ;; Change the funcall directions of COMP
   (let* ((items (content h))
	  )
     ;; (print (mapcar #'(lambda (i) (list i (+ (width i) (right-side-space i)))) items))
     (n-compute-perfect-clock-punctuation (mapcar #'car (clock-heads items)) (width h))
     (dolist (lst (clock-heads items)
		  ;; Am ende add all together
		  (dolist (s (content h)
			     (progn
			       ;; (print (mapcar #'(lambda (i) (list i (+ (width i) (right-side-space i)))) items))
			       (nlineup h)
			       (format t "~%>>>~F~&" (left h))))
		    ;; (print (list s (width s)
		    ;; 		 (right-side-space s)))
		    
		    (incf (width s) (right-side-space s))
		    
		    ;; (print (list s (width s)))
		    )
		  )
       
       (when (> (length lst) 1)		;note+accidental

	 (if 
	  ;; Passt rein? setz es von clock's fspace ab
	  (< (apply #'+ (mapcar #'(lambda (nonclock)
				    (+ (width nonclock) (right-side-space nonclock)))
				(cdr lst)))
	     (right-side-space (car lst)))
	  (progn
	    
	    (decf (right-side-space (car lst))
		  (apply #'+ (mapcar #'(lambda (nonclock)
					 (+ (width nonclock)
					    (right-side-space nonclock)))
				     (cdr lst))))
	    ;; (print (right-side-space (car lst)))
	    )
	  (progn (print 'größer))))))
   ))



;; (defrule spn (note) (:treble)
;;     (1 "Assigns correct vertical positions to note' heads,
;;  based on their pitch-name and their octave.")
;;   ((cons symbol unsigned-byte)
;;    (me parent)
;;    (let ((pitch-name (car (spn me)))
;; 	 (octave (cdr (spn me))))
;;      (setf (y (head me))
;;            (+ (- (fixed-bottom parent)
;; 		 (case pitch-name
;; 		   (c (- *space*))
;; 		   (d (- (* .5 *space*)))
;; 		   (e 0)
;; 		   (f (* .5 *space*))
;; 		   (g *space*)
;; 		   (a (* 1.5 *space*))
;; 		   (b (* 2 *space*))))
;; 	      (* (- 4 octave) 7/8 (fixed-height parent)))))))

(defrule spn (note) (:treble)
    (1 "Assigns correct vertical positions to note' heads,
 based on their pitch-name and their octave.")
  ((cons symbol unsigned-byte) (me)
   (let ((pitch-name (car (spn me)))
	 (octave (cdr (spn me))))
     (setf (y (head me))
           (+ (- (fixed-bottom me)
		 (case pitch-name
		   (c (- *space*))
		   (d (- (* .5 *space*)))
		   (e 0)
		   (f (* .5 *space*))
		   (g *space*)
		   (a (* 1.5 *space*))
		   (b (* 2 *space*))))
	      (* (- 4 octave) 7/8 (fixed-height me)))))))

(defrule spn (accidental) (:treble)
    (1.1 "Assigns correct vertical positions to Accidental
 based on their pitch-name and their octave.")
  ((cons symbol unsigned-byte) (me)
   (let ((pitch-name (car (spn me)))
	 (octave (cdr (spn me))))
     (setf (y (mchar me))
           (+ (- (fixed-bottom me)
		 (case pitch-name
		   (c (- *space*))
		   (d (- (* .5 *space*)))
		   (e 0)
		   (f (* .5 *space*))
		   (g *space*)
		   (a (* 1.5 *space*))
		   (b (* 2 *space*))))
	      (* (- 4 octave) 7/8 (fixed-height me)))))))

(defparameter *octave-space* (* 3.5 *space*))
(defun down-stem-p (spn)
  "Decides about the direction of a stem,
 based on the pitch-name and the octave of SPN."
  (let ((pitch-name (car spn))
	(octave (cdr spn)))
    (or (>= octave 5)
	(and (eq pitch-name 'b) (= octave 4)))))

(defrule null (note) (:treble)
    (2 "Draws stem lines on the <correct> side of the note N. aber nicht für rests" )
  (null (n)
	;; Give the note object N a stem only when it's dur < whole-note
	(when (and (< (dur n) 1) (not (eq (id n) 'r)))
	  (let* ((dx .44)
		 (dy 1.3)
		 (head-top (top (head n)))
		 (head-center (* (height (head n)) .5))
		 (stem-onset (+ head-top head-center)))
	    (packsvg n
		     (ngn::svgline (if (down-stem-p (spn n))
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
				   "stroke-width" (- *staff-line-thickness* .1)
				   "stroke-linecap" "round"
				   "stroke" "black"))))))
(defparameter *staff-line-thickness* 1.)

;;; Stave
(defrule null (note accidental) (:treble) (3 "Drawing staff lines")
  (null (me)
	(loop
	  :for line-idx :from -2 :to 2
	  :for line-y = (+ (* line-idx *space*) (y me))
	  :do (packsvg me
		       (ngn::svgline (left me) line-y
				     (+ (left me) (width me)) line-y
				     "stroke-width" *staff-line-thickness*
				     "stroke-linecap" "sqaure"
				     "stroke" (if (clockp me) "black"
						  "red"))
		       
		       )
	  )
	;; (print (list me (when (clockp me) (dur me)) (width me)))
	;;       (print '_________)
	))

(defrule null (barline) (t)
    (4 "Barline")
  (null (me parent)
	(packsvg parent
		 (ngn::svgline (left parent) (top parent)
			   (left parent) (bottom parent)
			   "stroke-width" (+ *staff-line-thickness* .1)
			   "stroke-linecap" "square"
			   "stroke" "black")))
  )




#|
S.52 Hader
Jeder musikalishce Wert, ob Noten oder Pausen, gegenüber
seinem kleineren nicht vergrößert, sondern verkleinert wird:
1 Viertel = 1.5 Achtel
1 Halbe = 2 Achtel
1 Ganze = 3 Achtel
|#
