
(in-package #:smtngn)


;;; The chase-content is of this type? This can basically be note or rest.
;;; IS THE CONTENT OF THIS TYPE?
(defun content-is-singleton-temporal-mtype-p (contlst)
  (and (typep (car contlst) 'temporal-mtype)
       (null (cdr contlst))))

(defun staff-container-p (cntlst)
  (member 'staff cntlst :key #'type-of))
(defun temporal-mtype-container-p (cntlst)
  (member 'temporal-mtype cntlst :key #'type-of))
(defun all-staff-or-tmtype-containers-p (cntlst)
  (every #'(lambda (x) (or (typep x 'note)))
	 cntlst))
(deftype s-tm () '(satisfies all-staff-or-tmtype-containers-p))

(deftype singleton-temporal-mtype-content ()
  "The content of this stick is a list of a single TEMPORAL MTYPE"
  '(satisfies content-is-singleton-temporal-mtype-p))



(defun contents-r-stacked-singleton-temporal-mtype-p (contlst)
  (every #'(lambda (x) (typep (content x) 'singleton-temporal-mtype-content)) contlst))

;;; This is the content
(deftype stacked-singleton-temporal-mtype-content ()
  '(or (satisfies contents-r-stacked-singleton-temporal-mtype-p) s-tm))
(defun foo (cntlst)
  (every #'(lambda (a) (typep a 'stacked-composing-stick)) cntlst))
(deftype xt () '(satisfies foo))




