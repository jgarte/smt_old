
(in-package :smtngn)

;;; Stichel
(defclass burin (smtobj)
  ((rtid :initarg :rtid
	 :initform nil
	 :accessor burin-rtid)
   (start :initarg :start
	  :accessor burin-start)
   (end :initarg :end
	:accessor burin-end)
   (direction :initarg :direction
	      :accessor burin-direction)))
