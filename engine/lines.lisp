
(in-package #:ngn)

(defstruct point x y)

(defclass line (stacked-form)
  ((starts :initarg :starts
	   :initform ()
	   :type list
	   :accessor starts)
   (ends :initarg :ends
	 :accessor ends)))

(defclass barline (line) ())
