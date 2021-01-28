(in-package #:smt-docs)
(in-package :cl-markup)

(sb-impl::defconstant-eqx +day-names+ 
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday")
  #'equal)



(with-open-file (s "/home/amir/Work/Lisp/smt/docs/index.html"
		   :direction :output :if-does-not-exist :create
		   :if-exists :supersede)
  (format s (html :lang "eng"
		  (:head (:meta :charset "UTF-8")	     
			 (:title "SMT")
			 (:style :type "text/css"
				 "footer{ 
            position: fixed; 
            padding: 10px 10px 0px 10px; 
            bottom: 0; 
            width: 100%; 
            /* Height of the Footer*/  
            height: 40px; 
            background: #acadac; 
        }"))
		  (:body
		   (:header (:h1 "Symbolic Music Typesetting"))
		   (:section "Hello MArkup")
		   (:footer (format nil "Last modified: ~A" (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
								(get-decoded-time)
							      (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
								      hour
								      minute
								      second
								      (nth day-of-week +day-names+)
								      month
								      date
								      year
								      (- tz)))))
		   ))))
