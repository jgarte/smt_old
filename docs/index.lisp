(in-package #:smt-docs)

(sb-impl::defconstant-eqx +day-names+ 
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday")
  #'equal)




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *css*
    "/* $Id: tic.css,v 1.4 2007-03-05 02:19:32 smoot Exp $ */

body {
	font-family: Georgia, \"Times New Roman\", Times, serif;
	color: black;
	background-color: #ffffff;
}

div.page_title {
	align: left;
}

/* Navigation bar */
div.navigation {
 font-family: Helvetica, Geneva, Arial, SunSans-Regular, sans-serif;
}

h1.title {
	display: inline;
	font-family: Helvetica, Geneva, Arial, SunSans-Regular, sans-serif;
}

/* Highlight hrefs with color */
a:hover {
 background-color: red; foreground-color: yellow;
}

/* Partner page has icons for bullets */
ul.partners {
	list-style: none;
}

hr {
	height: 4px;
	background: orange;
}

img {
	border: 0;
}
")
  (defparameter *menu*
    '())
  (defparameter *bullet* "&nbsp;&bull;&nbsp;"))

(defmacro register-page (name &body bd)
  `(push '(,name ,bd) *menu*))

(defparameter *current-version-dir-name*
  (apply #'uiop:strcat
	 (split-sequence:split-sequence "."
					(asdf:component-version (asdf:find-system "smt"))
					:test #'string=))
  "Current SMT version")

;;; Index page of version x.x.x
(register-page "Index" (:div :class "page_title"
			     (:h1 (format nil "Symbolic Music Typesetting ~A"
					  (asdf:component-version (asdf:find-system "smt"))))))
