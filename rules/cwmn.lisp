;;; Common Western Music Notation

(in-package :smt-engine)


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

(defrule content (horizontal-form) (t)
    ("Compute widths so dass jede Note bzw.  Pause die dafür geltenden
Bereich oder Platz oder Raum in Anspruch nimmt. Dies wird dann
hilfreich sein, wenn Horizontale Form das Zeug verarbeiten soll."  0)
  ;; (pure-temp-sform-seq (hf)
  ;; 		       (let* ((u (mm-to-px (/ 160 15.72)))
  ;; 			      (uh (* u (/ 5 3.5)))
  ;; 			      (uw (* u (/ 7 3.5))))
  ;; 			 (dolist (d (content hf))
  ;; 			   (let ((n (car (content d))))
  ;; 			     (cond ((= (dur n) .25) (setf (width d) u))
  ;; 				   ((= (dur n) .5) (setf (width d) uh))
  ;; 				   ((= (dur n) 1) (setf (width d) uw))))))
  ;; 		       (hlineup hf))
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
	    (wnote-sforms (remove-if-not #'(lambda (x) (= 1 (dur (car (content x))))) note-sforms))
	    ;; Width ist selber in px , also braucht keine Konvertierung
	    (u (/ (- (width hf)
		     (* (mm-to-px 2) (1- (length blinesforms))))
		  (+ (length qnote-sforms)
		     (* (length hnote-sforms) (/ 5 3.5))
		     (* (length wnote-sforms) (/ 7 3.5)))
		  ;; 15.72
		  ))
	    (uh (* u (/ 5 3.5)))
	    (uw (* u (/ 7 3.5)))
	    (l (length (content hf))))
       (dotimes (i l)
	 (let* ((d (nth i (content hf)))
		(n (find-if #'(lambda (x) (typep x '(or barline note))) (content d))))
	   (typecase n
	     (note (cond ((= (dur n) .25) (setf (width d) u))
			 ((= (dur n) .5) (setf (width d) uh))
			 ((= (dur n) 1) (setf (width d) uw))))
	     (barline (when (< i (1- l))
			(setf (width d) (mm-to-px 2)))))))
       )
     (hlineup hf))
  )

(defrule spn (note) (:treble)
    ("Assigns correct vertical positions to note' heads,
 based on their pitch-name and their octave." 1)
  ((cons symbol unsigned-byte)
   (me parent)
   (let ((pitch-name (car (spn me)))
	 (octave (cdr (spn me))))
     (setf (y (head me))
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
    ("Draws stem lines on the <correct> side of the note N. aber nicht für rests" 2)
  (null (n)
	;; Give the note object N a stem only when it's dur < whole-note
	(when (and (< (dur n) 1) (not (eq (id n) 'r)))
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
(defparameter *staff-line-thickness* 30)

;;; Stave
(defrule null (stacked-form) (stacked) ("Drawing staff lines" 3)
  (null (me)
	(loop
	  :for line-idx :from -2 :to 2
	  :for line-y = (+ (* line-idx *staff-space*) (y me))
	  :do (packsvg me (svg:line (left me) line-y (+ (left me) (width me)) line-y
				    :stroke-width *staff-line-thickness*
				    :stroke-linecap "round"
				    :stroke "black")))))

(defrule null (barline) (t)
    ("Barline" 4)
  (null (me parent)
	(packsvg parent
		 (svg:line (left parent) (top parent)
			   (left parent) (bottom parent)
			   :stroke-width (+ *staff-line-thickness* 10)
			   :stroke-linecap "square"
			   :stroke "black")))
  )



(defrule identity (note) (:treble)
    ("Deduces the notehead symbol from note's duration." -1)
  (note
   (noteobj)
   (let* ((dur (dur noteobj))
	  (hd (make-mchar (ecase dur
			    (1 'noteheads.s0)
     			    (1/2 'noteheads.s1)
     			    (1/4 'noteheads.s2))
     			  :origin-visible-p nil
     			  :canvas-vis-p nil)))
     (push hd (content noteobj))
     (setf (head noteobj) hd)
     
     )
   ))
