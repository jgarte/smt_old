
;;; Regression testing for SMT and it's components

(in-package #:tst)


(def-suite boundary-check)

;;; These assertions come from inspecting the non-broken Engine!
;;; I know from INSPECTING that eg xs of notehead & it's parent
;;; sform are the same.
(def-test notehead-default-coords-inheritance
    (:suite boundary-check)
  (let* ((n (make-notehead "s0"
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
    (is (= nx sx))
    (is (= ny sy))
    (is (= st sft))
    (is (> nt st))
    (is (= sb sfb))
    (is (< nb sb))
    (is (= nl sl))
    (is (= nr sr))
    ;; (is (= sh sfh))
    ;; (is (= (width s) (width n)))	;
    ;; Changing xy of notehead may not touch xy of sform
    (progn
      (for-all ((d (gen-integer :min -1000 :max 1000)))
	(progn (incf (x n) d)
	       (is (= (x s) sx)))
	(progn (incf (y n) d)
	       (is (= (y s) sy))))
      (psetf (x n) nx
      	     (y n) ny)
      )
    ;; The opposite is not true; changing xy of sform moves
    ;; xy of it's child too.
    (progn
      (for-all ((d (gen-integer :min -100 :max 100)))
	(progn (incf (x s) d)
	       (is (= (x n) (x s)))
	       (incf (y s) d)
	       (is (= (y n) (y s))))))
    (inspect-br n)
    (inspect-br s)
    (render (list s)))
  )
