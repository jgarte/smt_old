(in-package :smt-docs)




;;; Called first when we have all pages registered.
(defmacro generate-pages ()
  (let ((version-dir (ensure-directories-exist (format nil "/home/amir/Work/Lisp/smt/docs/~A/" *current-version-dir-name*))))
    `(progn
       ,@(loop for (pg bd-lst) in (reverse *menu*)
	       :collecting
	       `(with-open-file
		    (s ,(format nil "~A~A.html" version-dir (string-downcase pg))
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :supersede)
		  (format s (cl-markup:html :lang "en"
					    (:head (:meta :charset "UTF-8"
							  :name "SMT" :content "Symbolic Music Typesetting")
						   (:title "SMT")
						   (:style :type "text/css" *css*)
						   )
					    (:body
					     ;; Navigation menu comes over each page
					     (:div :class "navigation"
						   ,@(append
						      '((:a :href "/home/amir/Work/Lisp/smt/docs/index.html" (:b "Home")))
						      '((list *bullet*))
						      (loop for i below (list-length *menu*)
							    for p = (nth i (reverse *menu*))
							    :collecting `(:a :href ,(format nil "~A~A.html" version-dir (string-downcase (car p)))
									     (:b ,(car p)))
							    :when (/= i (1- (list-length *menu*))) :collect '(list *bullet*)))
						   )
					     (:hr)
					     ,@bd-lst
					     (:hr)
					     (:p (:i (format nil "Last modified on ~A"
							     (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
								 (get-decoded-time)
							       (format nil "~a, ~d/~2,'0d/~d, ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
								       (nth day-of-week +day-names+)
								       month
								       date
								       year
								       hour
								       minute
								       second
								       (- tz))))))
					     ))))))))

;;; This must be called only after all version pages created.
(progn
  ;; This is the home page, with all version listed in it
  (with-open-file (home "/home/amir/Work/Lisp/smt/docs/index.html"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :Create)
    (format home (cl-markup:html :lang "en"
				 (:head (:meta :charset "UTF-8" :name "SMT" ;; :content "Symbolic Music Typesetting"
					       )
					(:title "SMT")
					(:style :type "text/css" *css*)
					)
				 (:body 
				  (:hr)
				  (:div :class "page_title" (:h1 "Symbolic Music Typesetting"))
				  (:ul
				   ;; List all versions on the home page (newest on top)
				   (loop for vlst in (mapcar #'car (uiop:with-safe-io-syntax (:package :smt-docs)
								     (uiop:read-file-forms "/home/amir/Work/Lisp/smt/version")))
					 collect (cl-markup:markup
						  (:li (:a :href (format nil "/home/amir/Work/Lisp/smt/docs/~A/index.html"
									 (destructuring-bind (major minor patch) vlst
									   (format nil "~D~D~D" major minor patch)))
							   (:b (format nil "v. ~A"
								       (destructuring-bind (major minor patch) vlst
									 (format nil "~D.~D.~D" major minor patch)))))))
					 ))
				  (:hr)
				  (:p (:i (format nil "Last modified on ~A"
						  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
						      (get-decoded-time)
						    (format nil "~a, ~d/~2,'0d/~d, ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
							    (nth day-of-week +day-names+)
							    month
							    date
							    year
							    hour
							    minute
							    second
							    (- tz))))))
				  ))))
  (generate-pages))
