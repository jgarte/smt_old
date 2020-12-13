

(in-package #:smttst)


(5am:def-suite boundary-check
  :description "")
(5am:in-suite boundary-check)

(5am:test foo
  ""
  (let* ((n (smtngn::make-note nil))
	 (s (smtngn::sform :content (list n)))
	 (h (smtngn::hform :content (list s) :toplevelp t))
	 ;; make a record of n positions
	 (nl (left n)))
    (5am:is (= 'sd (left h)))))


