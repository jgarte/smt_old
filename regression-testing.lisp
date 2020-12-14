

(in-package #:smt-test)
;; (5am:def-suite foo)
;; (5am:in-suite foo)


;; (5am:test dummy-tests
;;   "Just a placeholder."
;;   (5am:is (listp (list 1 2)))
;;   (5am:is (= 5 (+ 2 3))))



;; ;; (5am:signals simple-type-error (error 'simple-type-error
;; ;; 				      :format-control "???"))
;; (5am:pass)

(parachute:define-test foo
  (parachute:is = 2 2)
  (parachute:is = 2 2)
  (parachute:is = 2 2)
  (parachute:is = 2 2)
  (parachute:is = 2 2)
  (parachute:is = 2 2)
  (parachute:is = 2 29))
(defun runtst ()
  (parachute:test 'foo :report 'parachute:interactive))
