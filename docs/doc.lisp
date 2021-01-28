(in-package #:smt-docs)

(sb-impl::defconstant-eqx +day-names+ 
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday")
  #'equal)



(with-open-file (s "/home/amir/Work/Lisp/smt/docs/index.html"
		   :direction :output :if-does-not-exist :create
		   :if-exists :supersede)
  (format s (cl-markup:html :lang "eng"
			    (:head (:meta :charset "UTF-8")	     
				   (:title "SMT")
				   (:style :type "text/css"
					   "html { background-color:#3df3;
          padding: 0; margin: 0; } footer { position: fixed; padding:
          10px 10px 0px 10px; bottom: 0; width: 100%; height: 20px;
          background: #acadac; }"))
			    (:body
			     (:header (:h1 "Symbolic Music Typesetting"))
			     (:section (:p (documentation 'smt-engine::children 'function)))
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
