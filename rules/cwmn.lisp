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
   (let* ((dur (duration noteobj))
	  (hd (make-mchar (ecase dur
			    (1 'noteheads.s0)
     			    ((h 1/2) 'noteheads.s1)
     			    (1/4 'noteheads.s2))
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
(defparameter *minimum-legible-space* (* .01 *space*)
  "Distances smaller than this value are considered as collisions.")




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

;;; Wenn die Rules eine Art IO hätten, könnte man das O von diesem einen
;;; ins I vom nächsten stecken??? Dann müsste ich die widths nicht hier setzen
(defrule id (horizontal-form) (julian) (0 "Punktieren Durchlauf 1:
Findet das idealle Spacing zwischen Clocks, tut so al ob gäbe es gaar keine
Non-clocks!")
  ((eql normseq) (hform)
   ;; Content besteht aus gefüllten Sforms
   (let* ((items (mapcar (comp #'content #'car) (content hform)))
	  (clocks (mapcar #'car (clock-heads items)))
	  (durs (mapcar #'duration clocks))
	  (count-durs (mapcar #'(lambda (d)
				  (cons (count d durs :test #'=) d))
			      (remove-duplicates durs)))
	  (u (decide-unit durs))	;Which of the durations is the unit?
	  (uwidth (/ (width hform)
		     (apply #'+ (mapcar #'(lambda (x)
					    ;; car x=tedade clockedha, cdr dur
					    (* (car x) (ufactor u (cdr x))))
					count-durs))))
	  )
     (dolist (c clocks (hlineup hform))
       (setf (width c) (* uwidth (ufactor u (duration c))))))))





;; (defrule id (horizontal-form) (julian)
;;     (0 "Punctuation:
;; 1. Subtrahieret alle nonclock-sforms von der Bereite der Zeile = B,
;; 2. Findet das unit heraus = U,
;; 3. Findet die Bereite vom U und anderen Notendauern heruas, sodass:
;; NU*U+NX*(X/U)")
;;   ((eql lst) (h)
;;    (let* ((items (mapcar #'(lambda (s) (car (content s))) (content h)))
;; 	  (clocks (remove-if-not #'(lambda (x) (typep x 'clocked)) items))		  
;; 	  (aux (remove-if #'(lambda (x) (typep x 'clocked)) items))
;; 	  (nonclocks-width-sum (apply #'+ (mapcar #'(lambda (x) (punctuation-width x))
;; 					    aux)))
;; 	  (useful-width (- (width h) nonclocks-width-sum))
;; 	  (durs (mapcar #'(lambda (x) (dur x)) clocks))
;; 	  ;; clocks are sforms, which contait notes = car s
;; 	  (u (decide-unit durs))
;; 	  (count-durs (mapcar #'(lambda (d) (cons (count d durs :test #'=) d))
;; 			      (remove-duplicates durs)))
;; 	  (uwidth (/ useful-width
;; 		     (apply #'+ (mapcar #'(lambda (x)
;; 					    ;; car x=tedade clockedha, cdr dur
;; 					    (* (car x) (ufactor u (cdr x))))
;; 					count-durs))))
;; 	  (remain 0))
     
;;      ;; (format t "~&[Width ~d] [AuxWidthSum ~d] [UsefulWidth ~d] [Durs ~a] ~%[Unit ~d] [UnitDur ~d]"
;;      ;; 	   (width h)
;;      ;; 	   nonclocks-width-sum
;;      ;; 	   useful-width
;;      ;; 	   count-durs u (assoc u *duration-unit-space-reference* :test #'=))

;;      ;; X is 1 group
;;      (dolist (x (clock-groups
;; 		 (content h)
;; 		 #'(lambda (s) (typep (car (content s)) 'clocked))))     
;;        ;; Vorgweschlagenes Width for this clock
;;        (let ((ideal-width (* uwidth
;; 			     (ufactor u
;; 				      (dur (car (content (car x))))
;; 				      ))))
;; 	 (if (> (length x) 1)
;; 	     ;; group of clock etc.
;; 	     (let* ((etcw (mapcar #'punctuation-width (mapcar
;; 						       #'(lambda (s)
;; 							   (car (content s)))
;; 						       (cdr x)))))
;; 	       (if (> (apply #'+ etcw)
;; 		      ;; hat x width?
;; 		      (- ideal-width
;; 			 (width (head (car (content (car x)))))
;; 			 ;; (width (car x))
;; 			 (getf *guards* 'note)))
;; 		   (setf remain (+ remain
;; 				   (- ideal-width
;; 				      (width (car x))
;; 				      (getf *guards* 'note)))
;; 			 (width (car x))
;; 			 ;; (- ideal-width
;; 			 ;; 	  (width (car x))
;; 			 ;; 	  (getf *guards* 'note))
				       
;; 			 (+ (width (car x))
;; 			    (getf *guards* 'note) 0)
;; 			 )
;; 		   (setf remain (+ remain (apply #'+ etcw))
;; 			 (width (car x))
;; 			 (- ideal-width (apply #'+ etcw)))))
;; 	     (setf (width (car x)) ideal-width)))
;;        (dolist (r (rest x))
;; 	 (setf (width r) (punctuation-width (car (content r))))))
		   
;;      ;; (dolist (c (content h))
;;      ;;   (incf (width c) (/ remain (length (content h)))))
		   
;;      (format t "~&~D Remains" remain )
		   
;;      (when (not (zerop remain))
;;        (let* ((clks (remove-if-not #'(lambda (x) (typep x 'clocked))
;; 		   		   (mapcar #'(lambda (s)
;; 		   			       (car (content s)))
;; 		   			   (content h))))
;; 	      )
;; 	 ;; (dolist (x (content h))
;; 	 ;; 	 (when (typep (car (content x))'clocked)
;; 	 ;; 	   (setf (width x)
;; 	 ;; 		 (+ (width x) (print (/ remain (length clks)))))
;; 	 ;; 	   ;; (format t "~&~a ~d~%__________" x (width x))
;; 	 ;; 	   ))
		       
;; 	 (dolist (x (remove-if-not #'(lambda (s) (typep (car (content s)) 'clocked))
;; 		   		   (content h)
;; 				   ))
;; 	   (setf (width x)
;; 		 (+ (width x) (/ remain (length clks)))
;; 		 )
;; 	   )
		       
;; 	 ))
		   
;;      (hlineup h))))







;; (defrule content (horizontal-form) (t)
;;     (0 "Compute widths so dass jede Note bzw.  Pause die dafür geltenden
;; Bereich oder Platz oder Raum in Anspruch nimmt. Dies wird dann
;; hilfreich sein, wenn Horizontale Form das Zeug verarbeiten soll.")
;;   ;; (pure-temp-sform-seq (hf)
;;   ;; 		       (let* ((u (mm-to-px (/ 160 15.72)))
;;   ;; 			      (uh (* u (/ 5 3.5)))
;;   ;; 			      (uw (* u (/ 7 3.5))))
;;   ;; 			 (dolist (d (content hf))
;;   ;; 			   (let ((n (car (content d))))
;;   ;; 			     (cond ((= (dur n) .25) (setf (width d) u))
;;   ;; 				   ((= (dur n) .5) (setf (width d) uh))
;;   ;; 				   ((= (dur n) 1) (setf (width d) uw))))))
;;   ;; 		       (hlineup hf))
;;   (t (hf)
;;      (let* ((note-sforms (remove-if-not #'(lambda (x) (and (sformp x)
;; 							   (only-type-p (content x)
;; 									'note)))
;; 					(content hf)))
;; 	    (blinesforms (remove-if-not #'(lambda (x) (and (sformp x)
;; 							   (only-type-p (content x)
;; 									'barline)))
;; 					(content hf)))
;; 	    (qnote-sforms (remove-if-not #'(lambda (x) (= .25 (dur (car (content x))))) note-sforms))
;; 	    (hnote-sforms (remove-if-not #'(lambda (x) (= .5 (dur (car (content x))))) note-sforms))
;; 	    (wnote-sforms (remove-if-not #'(lambda (x) (= 1 (dur (car (content x))))) note-sforms))
;; 	    ;; Width ist selber in px , also braucht keine Konvertierung
;; 	    (u (/ (- (width hf)
;; 		     (* (mm-to-px 2) (1- (length blinesforms))))
;; 		  (+ (length qnote-sforms)
;; 		     (* (length hnote-sforms) (/ 5 3.5))
;; 		     (* (length wnote-sforms) (/ 7 3.5)))
;; 		  ;; 15.72
;; 		  ))
;; 	    (uh (* u (/ 5 3.5)))
;; 	    (uw (* u (/ 7 3.5)))
;; 	    (l (length (content hf))))
;;        (dotimes (i l)
;; 	 (let* ((d (nth i (content hf)))
;; 		(n (find-if #'(lambda (x) (typep x '(or barline note))) (content d))))
;; 	   (typecase n
;; 	     (note (cond ((= (dur n) .25) (setf (width d) u))
;; 			 ((= (dur n) .5) (setf (width d) uh))
;; 			 ((= (dur n) 1) (setf (width d) uw))))
;; 	     (barline (when (< i (1- l))
;; 			(setf (width d) (mm-to-px 2)))))))
;;        )
;;      (hlineup hf))
;;   )

(defrule spn (note) (:treble)
    (1 "Assigns correct vertical positions to note' heads,
 based on their pitch-name and their octave.")
  ((cons symbol unsigned-byte)
   (me parent)
   (let ((pitch-name (car (spn me)))
	 (octave (cdr (spn me))))
     (setf (y (head me))
           (+ (- (fixed-bottom parent)
		 (case pitch-name
		   (c (- *space*))
		   (d (- (* .5 *space*)))
		   (e 0)
		   (f (* .5 *space*))
		   (g *space*)
		   (a (* 1.5 *space*))
		   (b (* 2 *space*))))
	      (* (- 4 octave) 7/8 (fixed-height parent)))))))
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
	(when (and (< (duration n) 1) (not (eq (id n) 'r)))
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
(defrule null (stacked-form) (s) (3 "Drawing staff lines")
  (null (me)
	(loop
	  :for line-idx :from -2 :to 2
	  :for line-y = (+ (* line-idx *space*) (y me))
	  :do (packsvg me (ngn::svgline (left me) line-y (+ (left me) (width me)) line-y
				    "stroke-width" *staff-line-thickness*
				    "stroke-linecap" "round"
				    "stroke" "black")))))

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
