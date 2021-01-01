
;;; Regression testing for SMT and it's components

(in-package #:tst)
(setf *read-default-float-format* 'double-float)
;;; https://floating-point-gui.de/errors/comparison/
(defun nearly-equal-p (a b &optional (epsilon .01))
  (let* ((absa (abs a))
	 (absb (abs b))
	 (diff (abs (- a b))))
    (cond ((= a b) t)
	  ((or (zerop a) (zerop b)
	       (< (+ absa absb) least-positive-normalized-single-float))
	   (< diff (* epsilon least-positive-normalized-single-float)))
	  (t (< (/ diff (min (+ absa absb) most-positive-single-float))
		epsilon)))))



(def-suite fare)

(def-test test-f= (:suite fare)
  (setf *num-trials* 100000
	*max-trials* 100000)
  (for-all ((f1 #'(lambda () (random 2000.0)))
	    (f2 #'(lambda () (random .0005))))
    (print (list f1 (+ f1 f2)))
    (is (nearly-equal-p f1 (+ f1 f2)))))

;; (loop for f from .5 to 10 by .1
;;       unless
;;       (loop repeat 100000
;; 	    for f2 = (+ f (* (random .0005) (if (zerop (random 2)) 1 -1)))
;; 	    do (print (list f f2))
;; 	    always (nearly-equal-p f f2)
;; 	    )
;;       do (return (list f))
;;       )

;; (nearly-equal-p 0 0.000000000001 10)



(defmacro are (&rest iss)
  `(progn ,@(mapcar #'(lambda (x) (list 'is x)) iss)))

(def-suite notehead-on-sform
  :description "")

(setf *num-trials* 10000000
      *max-trials* 10000000)

;;; These assertions come from inspecting the non-broken Engine!
;;; I know from INSPECTING that eg xs of notehead & it's parent
;;; sform are the same.
;;; x, l, r, w
(def-test on-x-axis
    (:suite notehead-on-sform)
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
    (are (= nx sx) (= nl sl) (= nr sr)
	 (nearly-equal-p sw nw))
    ;; Changing x of notehead must:
    (setf i 1)
    (for-all ((d (gen-integer :min -10000 :max 10000)))
      (format t "~&NX ~F" (x n))
      (incf (x n) d)
      ;; 1. not touch x of parent
      ;; (is (= sx (x s)))
      ;; ;; 2. Parent's left-right are setfed appropriately:
      ;; (is (if (or (plusp d) (zerop d) (< (abs d) (width n)))
      ;; 	      (= (right n) (right s))
      ;; 	      ;; abs d >= width of n
      ;; 	      (= sx (right s))))
      
      (is (if (or (plusp d) (zerop d))
	      (progn
		(print (list (incf i) d (x n) nx
			     (x s)
			     (left n) (left s) sl
			     (= (left n) (left s))))
		
	       (nearly-equal-p sl (left s)))
	      (= (left n) (left s))))
      ;; Reset to initials after each iteration
      ;; to avoid incremental changing!
      (psetf (x n) nx
	     (x s) sx
	     ;; LR are incfed by setf x, W should be done manually
	     (width s) sw)
      )
    ;; ;; Direct Inheritance of coords by child
    ;; (for-all ((d (gen-integer :min -1000 :max 1000)))
    ;;   (format t "Ghabl LS:~F LN:~F~%"(left s) (left n))
    ;;   (incf (x s) d)
    ;;   (format t "Bad LS:~F LN:~F~%+++++++++~%"(left s) (left n))
    ;;   (are (nearly-equal-p (x s) (x n))
    ;; 	   (= (left s) (left n))
    ;; 	   (= (right s) (right n))
	   
    ;; 	   ))

    ;; (render (list s))
    )
  )
