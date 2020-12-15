(in-package #:smt-test)
(parachute:define-test foo
  (parachute:true nil))
(defun goon ()
  (parachute:test 'foo :report 'parachute:interactive))
