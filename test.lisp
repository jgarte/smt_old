(in-package #:smtngn)

(print '==========RUntests)
;;; Trap 1:
(let* ((n (make-note '(c . 4)))
       (n2 (make-note '(f . 4)))
       (s (sform :content (list n)))
       (s2 (sform :content (list n2)))
       (h (hform :content (list s s2)
		 :toplevelp t))
       ;; test vals
       (n2xrec (x n2))
       )
  (print (list (x n2) n2xrec))
  (decf (x n2) 20)
  (print (list (x n2) n2xrec))
  ;; (render (list h) :drawp nil :apprulp nil)
  (print (list (x n2) n2xrec))
  (print (= (x n2) (- n2xrec 20))))


