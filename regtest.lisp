
;;; Regression testing for SMT and it's components

(in-package #:tst)

;;; Floating Point error restrained =, f2 is equal to f1
;;; within the deviation +-DEV
(defun non-flarer= (f1 f2 &optional (dev 0.0005))
  (and (<= (- f1 dev) f2 (+ f1 dev))
       (<= (- f2 dev) f1 (+ f2 dev))))

(defmacro are (&rest iss)
  `(progn ,@(mapcar #'(lambda (x) (list 'is x)) iss)))

(setf *num-trials* 1000)

(def-suite boundary-check)

;;; These assertions come from inspecting the non-broken Engine!
;;; I know from INSPECTING that eg xs of notehead & it's parent
;;; sform are the same.
(def-test notehead-default-coords-inheritance
    (:suite boundary-check)
  (let* (
	 (n (make-notehead "s0"
			   :marker-vis-p t
			   :id 'n))
	 (s (sform :content (list n)
		   :marker-vis-p t
		   :toplevelp t
		   :id 's))
	 ;; These are affected by N being a child of S,
	 ;; since happening after having declared them son&father
	 (nx (x n))
	 (ny (y n))
	 (nt (top n))
	 (nb (bottom n))
	 (nl (left n))
	 (nr (right n))
	 (nh (height n))
	 (nw (width n))
	 (sx (x s))
	 (sy (y s))
	 (st (top s))
	 (sft (fixed-top s))	 
	 (sb (bottom s))
	 (sfb (fixed-bottom s))
	 (sl (left s))
	 (sr (right s))
	 (sh (height s))
	 (sfh (fixed-height s))
	 (sw (width s))
	 )
    ;; Initials
    (is (= nx sx))
    (is (= ny sy))
    (is (= st sft))
    ;; > means lower on the page!
    (is (> nt st))
    (is (= sb sfb))
    (is (< nb sb))
    (is (= nl sl))
    (is (= nr sr))
    (is (non-flarer= sh sfh))
    (is (non-flarer= sw nw))
    ;; (is (= sh sfh))
    ;; (is (= (width s) (width n)))
    ;; Changing xy of notehead may not touch xy of sform
    (for-all ((d (gen-integer :min -1000 :max 1000)))
      (incf (x n) d)
      (is (= sx (x s)))
      (incf (y n) d)
      (is (= sy (y s))))
    (psetf (x n) nx (y n) ny)
    ;; The opposite is doesn't hold; changing xy of sform moves
    ;; xy of it's child too.
    (for-all ((d (gen-integer :min -1000 :max 1000)))
      (incf (x s) d)
      (is (non-flarer= (x n) (x s)))
      (incf (y s) d)
      (is (= (y n) (y s))))
    (psetf (x s) sx (y s) sy (x n) nx (y n) ny)

    ;; (inspect-br n)
    ;; (inspect-br s)
    ;; (render (list s))
    )
  )


(def-test multi-note-default-coords-inheritance
    (:suite boundary-check)
  (let* ((n1 (make-note nil))
	 (s (sform :content (list n1)))
	 (h (hform :content (list s) :toplevelp t))
	 ;; Record the initial coords
	 (n1hx (x (head n1))) (n1hy (y (head n1)))
	 (n1x (x n1)) (n1y (y n1))
	 (sx (x s)) (sy (y s))
	 (hx (x h)) (hy (y h))
	 )
    ;; changing xy of notes must:
    (for-all ((d1 (gen-integer :min -1000 :max 1000)))
      (incf (x n1) d1) (incf (y n1) d1)
      ;; 1- take xy of their heads along
      (are (= (x (head n1)) (x n1))
	(= (y (head n1)) (y n1)))
      ;; 2- not change xy of parents
      (are (= sx (x s)) (= hx (x h))
	(= sy (y s)) (= hy (y h))))
    (psetf (x n1) n1x (y n1) n1y
	   (x (head n1)) n1hx (y (head n1)) n1hy)
    ;; Changing XY of sform must:
    (for-all ((d1 (gen-integer :min -1000 :max 1000)))
      (incf (x s) d1) (incf (y s) d1)
      (are
	;; 1. take xy of children along
	(non-flarer= (x s) (x n1))
	(non-flarer= (y s) (y n1))
	(non-flarer= (x s) (x (head n1)))
	(non-flarer= (y s) (y (head n1)))
	;; 2. not change xy of parent
	(= hx (x h)) (= hy (y h)))     
      )
    ;; Reset s first, for not to double reset n & it's head!
    ;; Look at (setf x) of FORM!
    (psetf (x s) sx (y s) sy
	   (x n1) n1x (y n1) n1y
	   (x (head n1)) n1hx (y (head n1)) n1hy
	   )
    ;; Changing xy of hform must:
    (for-all ((d (gen-integer :min -1000 :max 1000)))
      (incf (x h) d) (incf (y h) d)
      (are
    	;; 1. take xy of all children layers along
    	(non-flarer= (x h) (x s))
    	(non-flarer= (y h) (y s))
    	(non-flarer= (x h) (x n1))
    	(non-flarer= (y h) (y n1))
    	(non-flarer= (x h) (x (head n1)))
    	(non-flarer= (y h) (y (head n1)))
    	)
      )
    (render (list h))))
