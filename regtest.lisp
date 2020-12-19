
;;; Regression testing for SMT and it's components

(in-package #:tst)


(def-suite boundary-check)

;;; These assertions come from inspecting the non-broken Engine!
;;; I know from INSPECTING that eg xs of notehead & it's parent
;;; sform are the same.
(def-test notehead-default-coords-inheritance
    (:suite boundary-check)
  (let* ((n (make-notehead "s0"
			   :marker-vis-p nil
			   :id 'n))
	 (s (sform :content (list n)
		   :marker-vis-p nil
		   :toplevelp t
		   :id 's)))
    (inspect-br n)
    (inspect-br s)
    (is (= (x n) (x s)))
    (is (= (y n) (y s)))
    (is (> (top n) (top s)))
    (is (< (bottom n) (bottom s)))
    (is (= (left n) (left s)))
    (is (= (right n) (right s)))
    (is (= (width s) (width n)))
    (render (list s)))
  )


