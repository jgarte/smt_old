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
					   "html { background-color:#c9c3c3;
          padding: 0; margin: 0; } footer { position: fixed; padding:
          1px 10px 10px 3px; bottom: 0; width: 100%; height: 16px;
          background: #acadac; }"))
			    (:body
			     (:header (:h1 "Symbolic Music Typesetting"))
			     (:section (:p (documentation 'smt-engine::children 'function)))
			     (:footer (format nil "Last modified: ~A" (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
									  (get-decoded-time)
									
									
									(format nil "~a, ~d/~2,'0d/~d, ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
										(nth day-of-week +day-names+)
										month
										date
										year
										hour
										minute
										second
										(- tz)))))

			     ))))

