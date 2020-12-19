
;;; Regression testing for SMT and it's components

(in-package #:tst)


(def-suite boundary-check)

(def-test head-in-sform (:suite boundary-check)
  (let* ((n (make-notehead "s0"))
	 (s (sform :content (list n) :toplevelp t)))
    (is (= (x n) (x s)))
    ;; Top of notehead is by default lower than
    ;; the top of Sform
    (is (> (top n) (top s)))
    )
  )
