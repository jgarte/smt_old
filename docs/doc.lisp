(in-package #:smt-docs)

 (defmacro with-page ((&key title) &body body)
   `(spinneret:with-html-string
      (:doctype)
      (:html
        (:head
         (:title ,title)
	 (:link :href #p"/home/amir/Work/Lisp/smt/docs/doc.css" :rel :stylesheet :type "text/css"))
        (:body ,@body))))

(sb-impl::defconstant-eqx +day-names+ 
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday")
  #'equal)

(defun shopping-list ()
  (with-page (:title "SMT")
    (:header
     (:h1 "Symbolic Music Typesetting"))
    (:section
     (documentation 'smt-engine::children 'function))
    (:footer ("Last login: ~A" (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
				   (get-decoded-time)
				 (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
					 hour
					 minute
					 second
					 (nth day-of-week +day-names+)
					 month
					 date
					 year
					 (- tz)))))))


(with-open-file (s "/home/amir/Work/Lisp/smt/docs/index.html"
		   :direction :output :if-does-not-exist :create
		   :if-exists :supersede)
  (format s (shopping-list)))

