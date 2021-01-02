
;;; Regression testing for SMT and it's components

(in-package #:tst)

;; (setf *read-default-float-format* 'double-float)

;; (defun close-enough-p (f1 f2 &optional (epsilon 0.0005))
;;   (declare (type real f1 f2 epsilon))
;;   (let ((delta (* (abs f1) epsilon)))
;;     (<= (- f1 delta)
;;         f2
;;         (+ f1 delta))))
(defparameter *tolerance* .0005)
(defun ~ (a b &optional (tol *tolerance*))
  (<= (abs (- a b)) tol))

;; (setf *read-default-float-format* 'double-float)
;;; https://floating-point-gui.de/errors/comparison/
;; (defun ~ (a b &optional (epsilon .01))
;;   (let* ((absa (abs a))
;; 	 (absb (abs b))
;; 	 (diff (abs (- a b))))
;;     (cond ((= a b) t)
;; 	  ((or (zerop a) (zerop b)
;; 	       (< (+ absa absb) least-positive-normalized-single-float))
;; 	   (< diff (* epsilon least-positive-normalized-single-float)))
;; 	  (t (< (/ diff (min (+ absa absb) most-positive-single-float))
;; 		epsilon)))))



(def-suite fare)

(def-test test-f= (:suite fare)
  (setf *num-trials* 100000000
	*max-trials* 100000000)
  (for-all ((f1 #'(lambda () (random 2000.0)))
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



;;; These assertions come from inspecting the non-broken Engine!
;;; I know from INSPECTING that eg xs of notehead & it's parent
;;; sform are the same.
;;; x, l, r, w
(def-test notehead-in-sform
    (:suite horizontal)
  (let* ((trials 1000000)
	 (*num-trials* trials)
	 (*max-trials* trials)
	 (n (make-notehead "s0"
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
	    (x n) nx
	    ;; LR are incfed by setf x, W should be done manually
	    (width s) sw)
      )
    ;; Direct Inheritance of coords by child
    ;; (for-all ((d (gen-integer :min -1000 :max 1000)))
    ;;   (incf (x s) d)
    ;;   (are (~ (x s) (x n))
    ;; 	   (= (left s) (left n))
    ;; 	   (= (right s) (right n))	   
    ;; 	   ))

    (render (list s))
    )
  )
