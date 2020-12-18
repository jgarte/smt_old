
;;; Regression testing for SMT and it's components

(in-package #:smt-test)


(def-suite boundary-checks)
(def-test bch1 (:suite boundary-checks)
  (is (= 2 2))
  (is (= 2 2))
  (is (= 2 2))
  (is (= 2 2))
  (is (= 2 2))
  (is (= 2 2))
  (is (= 2 2))
  (is (= 2 2))
  (pass) (fail)
  )
