
(in-package :smtngn)


(defparameter *staff-line-thickness* 50)

(defclass staff (burin)
  ((count :initarg :count
	  :accessor staff-count
	  :initform 5)
   (color :initarg :color
	  :accessor staff-color)
   (space :initarg :space
	  :initform *staff-space*
	  :accessor staff-space)
   (coords :initarg :coords
	   :type list
	   :documentation "((:x1 :x2 :y1 :y2)1 ... (:x1 :x2 :y1 :y2)N)"
	   :accessor staff-coords)))


;;; &allow-other-keys is a bad idea, you can specify any keyword
;;; and nothing happens, which is weird!
(defun make-staff (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'staff initargs))



(defmethod pack-svglst ((obj staff))
  (push
   (svg:g
    :content (loop for i below (staff-count obj)
		   for y = (* i (staff-interspace obj))
		   collect (svg:line 0 y 20 y :fill "blue")))
   (svglst obj)))
