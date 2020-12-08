;;; (asdf:load-system "smt")
(in-package :smtngn)


(defparameter *ruleidx* -1 "Rule Index")
(defparameter *ruledocs* (make-hash-table))
(defparameter *ruletable* (make-hash-table))

(defun ruledocs ()
  (sort (alexandria:hash-table-alist *ruledocs*) #'<
	:key #'car))

(defun remrules (idx &rest idcs)
  (remhash idx *ruletable*)
  (remhash idx *ruledocs*)
  (dolist (i idcs)
    (remhash i *ruletable*)
    (remhash i *ruledocs*)))

(defun examine-clauses (clauses)
  (dolist (clause clauses)
    (assert (consp clause) (clause)
	    "Malformed rule clause: ~A" clause)
    (assert (> (list-length clause) 1) (clause)
	    "Malformed rule clause: ~A, missing lambda-list & body" clause)
    (assert (listp (cadr clause)) (clause)
	    "Malformed rule clause lambda-list: ~A" (cadr clause))))

(defmacro defrule (ruler trgts dmns (&optional doc (idx (incf *ruleidx*))) &body clauses)
  (examine-clauses clauses)
  (let ((rlrval (gensym)))
    `(prog1 ',ruler
       (setf (gethash ,idx *ruledocs*) ,doc
	     (gethash ,idx *ruletable*)	   
    	     (list :rlrfn #'(lambda (,rlrval)
			      (etypecase ,rlrval
				,@(loop for clause in clauses
		  			collect (destructuring-bind (typespec lambda-lst . body)
						    clause
						  `(,typespec (list :clsfn #'(lambda ,lambda-lst ,@body)
								    :clsll ',lambda-lst
								    :clsbd ',body))))))
		   :trgts ',trgts :dmns ',dmns
		   :rlr (function ,ruler)
		   :clss ',clauses)
	     ))))


;;; Top level objects may not have any ancestors
(defun assert-toplevel-lamlstlen (obj lamlstlen)
  "Asserts an appropriate lambda list length of a rule for the toplevel."
  (when (toplevelp obj)
    (assert (or (zerop lamlstlen) (= lamlstlen 1))
	    (obj)
	    "Toplevel ~A can't access non-existing ancestor parameters in it's rule's lambda-list!"
	    (type-of obj))))

(defun pass-args (obj fn lamlstlen)
  (cond
    ((zerop lamlstlen) (funcall fn))
    ((= lamlstlen 1) (funcall fn obj))
    (t (apply fn obj
	      ;; Since sticks are PUSHED to ancestors-lst of
	      ;; other objs, the nearest parent is at the end of anc-lst,
	      ;; hence the reverse
	      (reverse (last (ancestors obj) (1- lamlstlen)))))))

(defun apply-rules (all)
  (dolist (idx (sort (alexandria:hash-table-keys *ruletable*) #'<))
    (let* ((plst (gethash idx *ruletable*))	   
	   ;; pick up objs with their types corresponding to trgts
	   (trgts (getf plst :trgts))
	   (trgobjs (remove-if-not #'(lambda (x) (find x trgts :test #'typep)) all))
	   ;; pick up trgobjs with their dmns corresponding to dmns
	   (dmns (getf plst :dmns))
	   (dmnobjs (remove-if-not  #'(lambda (x)
					(typep (domain x) (if (find t dmns)
							      t
							      `(member ,@dmns))))
				    trgobjs)))
      (dolist (obj dmnobjs)
	(let* ((clause-plst (funcall (getf plst :rlrfn)
				     (funcall (getf plst :rlr) obj)))
	       (clause-lamlstlen (list-length (getf clause-plst :clsll)))
	       (clause-fn (getf clause-plst :clsfn)))
	  (assert-toplevel-lamlstlen obj clause-lamlstlen)
	  (pass-args obj clause-fn clause-lamlstlen))))))

