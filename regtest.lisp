
;;; Regression testing for SMT and it's components

(in-package #:tst)

(defparameter *tolerance* .1)

(defun ~ (a b &optional (tol *tolerance*))
  (<= (abs (- a b)) tol))

(defun ~~ (&rest r)
  (loop for (x . y) on r
	always (loop for z in y always (~ x z))))


(def-suite fare)

(def-test test-f= (:suite fare)
  (setf *num-trials* 100000000
	*max-trials* 100000000)
  (for-all ((f1 #'(lambda () (random 2001)))
	    (f2 #'(lambda () (random .000499))))
    (is (~ f1 (+ f1 f2)))))

;; (loop for f from .5 to 10 by .1
;;       unless
;;       (loop repeat 100000
;; 	    for f2 = (+ f (* (random .0005) (if (zerop (random 2)) 1 -1)))
;; 	    do (print (list f f2))
;; 	    always (~ f f2)
;; 	    )
;;       do (return (list f))
;;       )

;; (~ 0 0.000000000001 10)



(defmacro are (&rest iss)
  `(progn ,@(mapcar #'(lambda (x) `(is ,x)) iss)))

(def-suite horizontal
  :description "")

;; First how is the construction of the glyph??????
;; X is more right than right? (BR-right is neg), then
;; is left also more left than x (Maybe NOT!)

(setf *num-trials* 10000
       *max-trials* *num-trials*)

;;; These assertions come from inspecting the non-broken Engine!
;;; I know from INSPECTING that eg xs of notehead & it's parent
;;; sform are the same.
;;; x, l, r, w
(def-test notehead-in-sform
    (:suite horizontal)
  (let* ((n (make-notehead "s0"
			   :marker-vis-p t
			   :id 'n))
	 (s (sform :content (list n)
		   :marker-vis-p t
		   :toplevelp t
		   :id 's))
	 ;; Record initials for comparison & resetting
	 (nx (x n))
	 (nl (left n))
	 (nr (right n))
	 (nw (width n))
	 (sx (x s))
	 (sl (left s))
	 (sr (right s))
	 (sw (width s))
	 )
    (are (= nr sr)
	 (= nx nl sx sl)
	 ;; Width for form is right - left (floating arithmetic involved)
	 (~ sw nw))
    ;; Changing x of notehead must:
    (for-all ((d (gen-integer :min -10000 :max 10000)))
	     (incf (x n) d)
	     ;; 1. not touch
	     (is (= sx (x s)))
	     (cond ((or (plusp d) (zerop d))
		    (are (= (right n) (right s))
			 (> (right s) sx)
			 (~ (right n) (+ (x n) (width n)))
			 (~ sl (left s))
			 (= (x n) (left n))
			 (~ (width s) (+ (width n) (- (left n) (left s))))))
		   ((< (abs d) (width n))
		    (are (= (right n) (right s))
			 (~ (width n) (width s))
			 (> (right s) sx)
			 (~ (right n) (+ (x n) (width n)))
			 (= (left n) (left s))))
		   (t (are (= (width s) (- (x s) (left n)))
			   (= (right s) (x s))
			   (>= (x s) (right n)) 
			   (= (left n) (left s))
			   (~ (width s) (+ (width n)
					   (- (right s) (right n)))))))
	     ;; Reset to initials after each iteration
	     ;; to avoid incremental changing!
	     ;; This setfing involves incfing (floating arithmetic),
	     ;; Thus (x n/s) is never gonna be exact N/SX again (unless I don't incf when setfing)
	     (setf (x s) sx 
		   ;; LR are incfed by setf x, W should be done manually
		   (width s) sw
		   (x n) nx
		   )
	     )
    ;; Direct Inheritance of coords by child
    (for-all ((d (gen-integer :min -10000 :max 10000)))
	     (incf (x s) d)
	     (are (~ (x s) (x n))
    		  (~ (left s) (left n))
    		  (~ (right s) (right n))
		  (~ (width n) (width s))
    		  )
	     (setf (x s) sx 
		   ;; LR are incfed by setf x, W should be done manually
		   (width s) sw
		   (x n) nx
		   )
	     ;; We do this Although for now it's like incf x ;;;;;;;;;;;;;;;
	     (incf (right s) d)
	     (are (~ (x s) (x n))
    		  (~ (left s) (left n))
    		  (~ (right s) (right n))
		  (~ (width n) (width s))
    		  )
	     (setf (x s) sx 
		   ;; LR are incfed by setf x, W should be done manually
		   (width s) sw
		   (x n) nx
		   )
	     (incf (left s) d)
	     (are (~ (x s) (x n))
    		  (~ (left s) (left n))
    		  (~ (right s) (right n))
		  (~ (width n) (width s))
    		  )
	     (setf (x s) sx 
		   ;; LR are incfed by setf x, W should be done manually
		   (width s) sw
		   (x n) nx
		   )
	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     (incf (width s) d)
	     (are (~ sl (left s))
		  (~ nx (x n))
		  (~ nr (right n)) (~ nl (left n)))
	     (setf (x s) sx 
		   ;; LR are incfed by setf x, W should be done manually
		   (width s) sw
		   (x n) nx
		   )
	     )
    )
  )
(defmacro with-horizontal-reset-check (places resetvals &body b)
  `(progn ,@b
	  (setf ,@(mapcan #'(lambda (x y) `(,x ,y)) places resetvals))
	  (ARE
	   (EVERY #'IDENTITY
		  (MAPCAR #'(LAMBDA (A B) (~ (X A) B)) (LIST H S1 S2 N1 N2 NH1 NH2)
			  (LIST HX S1X S2X N1X N2X NH1X NH2X)))
	   (EVERY #'IDENTITY
		  (MAPCAR #'(LAMBDA (A B) (~ (LEFT A) B)) (LIST H S1 S2 N1 N2 NH1 NH2)
			  (LIST HL S1L S2L N1L N2L NH1L NH2L)))
	   (EVERY #'IDENTITY
		  (MAPCAR #'(LAMBDA (A B) (~ (RIGHT A) B)) (LIST H S1 S2 N1 N2 NH1 NH2)
			  (LIST HR S1R S2R N1R N2R NH1R NH2R)))
	   (EVERY #'IDENTITY
		  (MAPCAR #'(LAMBDA (A B) (~ (WIDTH A) B)) (LIST H S1 S2 N1 N2 NH1 NH2)
			  (LIST HW S1W S2W N1W N2W NH1W NH2W))))))




(def-test 2notes-in-2sforms-in-hform (:suite horizontal)
  (let* (
	 (nh1 (make-notehead "s0"))	 
	 (nh2 (make-notehead "s0"))
	 (n1 (make-note nil :head nh1))
	 (n2 (make-note nil :head nh2))
	 ;; (nh1 (head n1)) (nh2 (head n2))
	 (s1 (sform :content (list n1)))
	 (s2 (sform :content (list n2)))
	 (h (hform :content (list s1 s2) :toplevelp t))
	 ;; Record initials
	 (nh1x (x nh1)) (nh1r (right nh1)) (nh1l (left nh1)) (nh1w (width nh1))
	 (nh2x (x nh2)) (nh2r (right nh2)) (nh2l (left nh2)) (nh2w (width nh2))
	 (n1x (x n1)) (n1r (right n1)) (n1l (left n1)) (n1w (width n1))
	 (n2x (x n2)) (n2r (right n2)) (n2l (left n2)) (n2w (width n2))
	 (s1x (x s1)) (s1r (right s1)) (s1l (left s1)) (s1w (width s1))
	 (s2x (x s2)) (s2r (right s2)) (s2l (left s2)) (s2w (width s2))
	 (hx (x h)) (hr (right h)) (hl (left h)) (hw (width h))
	 )
    ;; Relationship of initials
    (are (= nh1x nh2x n1x n2x s1x s2x hx
	    nh1l nh2l n1l n2l s1l s2l hl)
	 (= nh1r nh2r n1r n2r s1r s2r hr)
	 (= nh1w nh2w)
	 (~~ nh1w n1w n2w s1w s2w hw))
    (for-all ((d (gen-integer :min -10000 :max 10000)))          
      (with-horizontal-reset-check ((x h)) (hx)
	(incf (x h) d)
	;; X &left is only for Noteheads the same!
	(are (apply #'= (append (mapcar #'x (list h s1 s2 n1 n2 nh1 nh2))
				(mapcar #'left (list h s1 s2 n1 n2 nh1 nh2))))
	     (apply #'~~ (mapcar #'right (list h s1 s2 n1 n2 nh1 nh2)))	   
	     ;; Widths haven't changed
	     (every #'identity
		    (mapcar #'=
			    (mapcar #'width (list h s1 s2 n1 n2 nh1 nh2))
			    (list hw s1w s2w n1w n2w nh1w nh2w)))
	     ))
      ;; ;;;;;;;;;;;;;;;;;;;;
      (with-horizontal-reset-check ((left h)) (hl)	
	(incf (left h) d)
	(are (apply #'= (append (mapcar #'x (list h s1 s2 n1 n2 nh1 nh2))
				(mapcar #'left (list h s1 s2 n1 n2 nh1 nh2))))
	     (apply #'~~ (mapcar #'right (list h s1 s2 n1 n2 nh1 nh2)))
	     ;; Widths haven't changed
	     (every #'identity
		    (mapcar #'=
			    (mapcar #'width (list h s1 s2 n1 n2 nh1 nh2))
			    (list hw s1w s2w n1w n2w nh1w nh2w)))
	     ))      
      ;; ;;;;;;;;;;;;;;;;;
      (with-horizontal-reset-check ((right h)) (hr)
	(incf (right h) d)
	(are (apply #'= (append (mapcar #'x (list h s1 s2 n1 n2 nh1 nh2))
				(mapcar #'left (list h s1 s2 n1 n2 nh1 nh2))))
	     (apply #'~~ (mapcar #'right (list h s1 s2 n1 n2 nh1 nh2)))
	     ;; Widths haven't changed
	     (every #'identity
		    (mapcar #'=
			    (mapcar #'width (list h s1 s2 n1 n2 nh1 nh2))
			    (list hw s1w s2w n1w n2w nh1w nh2w)))
	     ))
      ;; ;;;;;;;;;;;;;;;;;;;width of h changes
      (with-horizontal-reset-check ((width h)) (hw)
	(incf (width h) d)
	(are (every #'identity
		    (mapcar #'(lambda (a b) (~ (x a) b))
			    (list h s1 s2 n1 n2 nh1 nh2)
			    (list hx s1x s2x n1x n2x nh1x nh2x)))
	     (every #'identity
		    (mapcar #'(lambda (a b) (~ (left a) b))
			    (list h s1 s2 n1 n2 nh1 nh2)
			    (list hl s1l s2l n1l n2l nh1l nh2l)))
	     ;; Only r of h has changed
	     (~ (right h) (+ hl (width h)))
	     (every #'identity
		    (mapcar #'(lambda (a b) (~ (right a) b))
			    (list s1 s2 n1 n2 nh1 nh2)
			    (list s1r s2r n1r n2r nh1r nh2r)))
	     (every #'identity
		    (mapcar #'(lambda (a b) (~ (width a) b))
			    (list s1 s2 n1 n2 nh1 nh2)
			    (list s1w s2w n1w n2w nh1w nh2w)))	     
	     )))
    ;; ;;;;;;;;;;;;;;Sform2 an sofrm1 hängen
    ;; s2 grenzt genau an die rechte Seite vom s1 ein.
    (setf (left s2) (right s1))
    ;; Widths
    (is (~~ s1w (width s1) n1w (width n1) nh1w (width nh1)))
    (is (~~ s2w (width s2) n2w (width n2) nh2w (width nh2)))
    (is (~ (width h) (+ (width s1) (width s2))))
    ;; Rights
    (is (~~ s1r (right s1) n1r (right n1) nh1r (right nh1)))
    (is (~~ (right s2) (right n2) (right nh2) (right h)))
    ;; Left, X
    (is (~~ (left s1) (left n1) (left nh1) (left h)
	    s1l n1l nh1l hl
	    (x s1) (x n1) (x nh1) (x h)
	    s1x n1x nh1x hx))
    (is (~~ (left s2) (left n2) (left nh2)
	    (x s2) (x n2) (x nh2)))
    ;; Exemplarisch
    (is (~~ (x s1) (- (left s2) (width s1)) (- (right h) (width s2) (width s1))))
    (is (~~ (x s2) (+ (left h) (width nh1))))
    ;; Reinit & test it    
    (flet ((is-like-init-p ()
	     ;; Drehe (x h) und hx um
	     (are (~ (x h) hx) (~ (width h) hw) (~ (left h) hl) (~ (right h) hr)
		  (~ (x s1) s1x) (~ (width s1) s1w) (~ (left s1) s1l) (~ (right s1) s1r)
		  (~ (x n1) n1x) (~ (width n1) n1w) (~ (left n1) n1l) (~ (right n1) n1r)
		  (~ (x nh1) nh1x) (~ (width nh1) nh1w) (~ (left nh1) nh1l) (~ (right nh1) nh1r)
		  (~ (x s2) s2x) (~ (width s2) s2w) (~ (left s2) s2l) (~ (right s2) s2r)
		  (~ (x n2) n2x) (~ (width n2) n2w) (~ (left n2) n2l) (~ (right n2) n2r)
		  (~ (x nh2) nh2x) (~ (width nh2) nh2w) (~ (left nh2) nh2l) (~ (right nh2) nh2r))))
      (setf (left s2) s2l)
      (is-like-init-p)
      ;; Sform2 Koordinaten versetzen
      (for-all ((d (gen-integer :min -10000 :max 10000)))
	(incf (x S2) d)
	;; these all move together
	(is (~~ (x s2) (x n2) (x nh2) (left s2) (left n2) (left nh2)))
	;; Wenn d + ist oder 0, sind die lefts nicht angefasst,
	;; aber rechts wird erweitert (+) oder bleibt wo es war (0)
	(cond ((zerop d) (is-like-init-p))
	      ((plusp d) (are (~~ (right h) (right s2) (right n2) (right nh2))
	      		      (~ (+ s2w d) (width h))
	      		      ;; shouldn't have changed
	      		      (~~ hl hx
	      		      	  (left h) (left s1) (left n1) (left nh1)
	      		      	  (x h) (x s1) (x n1) (x nh1))
	      		      (~~ s1w (width s1) (width n1) (width nh1)
				  s2w (width s2) (width n2) (width nh2))
			      (~~ s1r (right s1) (right n1) (right nh1))
	      		      ))
	      ;; d is -
	      (t (Are
		  ;; right of whole is still at where s1r is (since it still ist the rightmost one)
		  (~~ s1r (right h) (right s1) (right n1) (right nh1))
		  ;; shouldn't have changed: x,left of s1 family and x of h
		  (~~ s1l n1x (left s1) (left n1) (left nh1)
		      (x s1) (x n1) (x nh1)
		      (x h))
		  ;; shouldn't have changed: all widths except with H
		  (~~ s1w (width s1) (width n1) (width nh1)
		      s2w (width s2) (width n2) (width nh2))
		  ;; Lefts of h and s2 family or drawn back
		  (~~ (left h) (left s2) (left n2) (left nh2)
		      (x s2) (x n2) (x nh2))
		  (~ (- s2w d) (width h))
		  ))
	      )
	(setf (x S2) S2X)
	(is-like-init-p)
	)
      ;; Left ,right muss genau wie x sein; überspringen
      ;; Width von s2 ändern
      (for-all ((d (gen-integer :min 0 :max 10000)))
      	(incf (width s2) d)	       
	;; Sollten sich nicht verändert haben
	(are (~~ hr (right n2) (right nh2)
		 (right s1) (right n1) (right nh1))
	     (~~ hw (width n2) (width nh2)
		 (width s1) (width n1) (width nh1))
	     (~~ hl (left s2) (left n2) (left nh2)
		 (left s1) (left n1) (left nh1)
		 (left h)
		 (x s2) (x n2) (x nh2) (x s1) (x n1) (x nh1) (x h)))
	(if (zerop d)
	    (is-like-init-p)
	    (are (~ (right s2) (right h)) (~ (width s2) (width h))))
	(setf (width s2) s2w)
	(is-like-init-p))
      ;; x von n1, ähnlich wird es sein mit left , right
      (for-all ((d (gen-integer :min -10000 :max 10000)))
	(incf (x n1) d)
	;; Sollten sich nicht verändert haben:
	;; die ganze x,left Koordinaten außer n1 und sein baby
	(are (~~ s2x (x s2) (x n2) (x nh2)
		 (left s2) (left n2) (left nh2)
		 (x h)
		 ;; (left h)
		 (x s1)
		 ;; (left s1)
		 )
	     (~~ s2r (right s2) (right n2) (right nh2))
	     ;; alle widths außer von h und s1
	     (~~ s2w (width s2) (width n2) (width nh2)
		 (width n1) (width nh1))
	     )
	(cond ((zerop d) (is-like-init-p))
	      ((plusp d)
	       (is (~~ (right h) (right s1)
		       (right n1) (right nh1)))
	       (is (~~ (+ (width n1) d) (width h) (width s1)))
	       (is (~~ hl (left h) (left s1)))
	       )
	      (t (is (~~ hr (right h) (right s2) (right n2) (right nh2)))
		 (is (~~ (+ hl d) (left h) (left s1) (left n1) (left nh1)
			 (x n1) (x nh1)))
		 ;; Right vom h hängt noch fest an right vom n2
		 (is (~ (- n2w d) (width h)))
		 ;; Wie viel sind wir von der X Achse
		 ;; nach links gerütscjht?
		 (if (<= (- d) s1w)
		     ;; Noch die X achse nicht unterschritten
		     ;; Width ist wie das alte
		     (is (~ s1w (width s1)))
		     ;; X Achse wurdew unterschritten
		     (is (~ (- d) (width s1)))
		     )
		 ))
	(setf (x n1) n1x)
	(is-like-init-p)
	)
      (for-all ((d (gen-integer :min 0 :max 10000)))
      	(incf (width n1) d)
	;; Darf sich nicht verändert haben
	(is (~~ s2x n2l (x s2) (x n2) (x nh2)
		(left s2) (left n2) (left nh2)
		(left s1) (left n1) (left nh1) (left h)
		(x s1) (x n1) (x nh1) (x h)))
	;; S2 ist unberührt
	(is (~~ s2r (right s2) (right n2) (right nh2)))
	(is (~~ (right n1) (right s1) (right h)))
	(is (~~ (width n1) (width s1) (width h)))
	(unless (zerop d)
	  ;; Width von n1's baby darf sich nicht verändert haben!
	  (is-false (~ (width n1) (width nh1))))
	(setf (width n1) n1w)
	)
      )
    ))

