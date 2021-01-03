
;;; Regression testing for SMT and it's components

(in-package #:tst)

;; (setf *read-default-float-format* 'double-float)

(defparameter *tolerance* .0005)

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

(setf *num-trials* 100000
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
    		  (= (right s) (right n))
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
    		  (= (right s) (right n))
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
    		  (= (right s) (right n))
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
	 (= nh1r nh2r n1r n2r s1r s2r hr) (= nh1w nh2w)
	 (~~ nh1w n1w n2w s1w s2w hw)
	 )
    (for-all ((d (gen-integer :min -10000 :max 10000)))
      (incf (x h) d)
      (are (apply #'= (mapcar #'x
			      (list h s1 s2 n1 n2 nh1 nh2)))
	   (apply #'= (mapcar #'right
			      (list h s1 s2 n1 n2 nh1 nh2)))
	   (apply #'= (mapcar #'left
			      (list h s1 s2 n1 n2 nh1 nh2)))
	   ;; Widths haven't changed
	   (every #'identity
		  (mapcar #'=
			  (mapcar #'width (list h s1 s2 n1 n2 nh1 nh2))
			  (list hw s1w s2w n1w n2w nh1w nh2w)))
	   ))))

