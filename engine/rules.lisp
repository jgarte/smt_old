;;; (asdf:load-system "smt")
(in-package :smt-engine)


(defparameter *ruleidx* -1 "Rule Index")
(defparameter *ruledocs* (make-hash-table))
(defparameter *ruletable* (make-hash-table))

(defun ruledocs ()
  (sort (alexandria:hash-table-alist *ruledocs*) #'<
	:key #'car))

(defun remrule (idx)
  (remhash idx *ruletable*)
  (remhash idx *ruledocs*))

(defun examine-clauses (clauses)
  (dolist (clause clauses)
    (assert (consp clause) (clause)
	    "Malformed rule clause: ~A" clause)
    (assert (> (list-length clause) 1) (clause)
	    "Malformed rule clause: ~A, missing lambda-list & body" clause)
    (assert (listp (cadr clause)) (clause)
	    "Malformed rule clause lambda-list: ~A" (cadr clause))))

(defmacro defrule (ruler targets domains (&optional doc (idx (incf *ruleidx*))) &body clauses)
  (examine-clauses clauses)
  (let ((rlrval (make-symbol "RLRVAL")))
    `(prog1 ',ruler
       (psetf (gethash ,idx *ruledocs*) ,doc
	      (gethash ,idx *ruletable*)
	      ;; Rule's plist
    	      (list :rlrfn #'(lambda (,rlrval)
			       (etypecase ,rlrval
				 ,@(loop for clause in clauses
		  			 collect (destructuring-bind (typespec lambda-lst . body)
						     clause
						   `(,typespec
						     ;; Clause' Plist
						     (list :clsfn #'(lambda ,lambda-lst ,@body)
							   :clsll ',lambda-lst
							   :clsbd ',body))))))
		    :trgts ',targets :dmns ',domains :rlr (function ,ruler) :clss ',clauses)
	      ))))





;;; Top level objects may not have any ancestors
(defun assert-toplevel-lamlstlen (obj lamlstlen)
  "Asserts an appropriate lambda list length of a rule for the toplevel."
  (when (toplevelp obj)
    (assert (or (zerop lamlstlen) (= lamlstlen 1))
	    (obj)
	    "Toplevel ~A can't access non-existing ancestor parameters in it's rule's lambda-list!"
	    (type-of obj))))

(defun pass-args-to-clause-func (obj fn lamlstlen)
  (cond
    ((zerop lamlstlen) (funcall fn))
    ((= lamlstlen 1) (funcall fn obj))
    (t (apply fn obj
	      ;; Since sticks are PUSHED to ancestors-lst of
	      ;; other objs, the nearest parent is at the end of anc-lst,
	      ;; hence the reverse
	      (reverse (last (ancestors obj) (1- lamlstlen)))))))

(defun apply-rules (flatt-doc-objs)
  (dolist (idx (sort (alexandria:hash-table-keys *ruletable*) #'<))    
    (let* ((plist (gethash idx *ruletable*)) ;Rule's plist
	   ;; pick up objs with their types corresponding to trgts
	   (trgts (getf plist :trgts))
	   (trgobjs (remove-if-not #'(lambda (x) (find x trgts :test #'typep)) flatt-doc-objs))
	   ;; pick up trgobjs with their dmns corresponding to dmns
	   (dmns (getf plist :dmns))
	   ;; If domains contains T, all objects no matter in which domain, are adressed.
	   (dmnobjs (if (find t dmns)
			;; No removings!
			trgobjs
			(remove-if-not #'(lambda (x) (typep (domain x) `(member ,@dmns))) trgobjs)))
	   ;; (dmnobjs (remove-if-not  #'(lambda (x)
	   ;; 				(typep (domain x)
	   ;; 				       (if (find t dmns) t `(member ,@dmns))))
	   ;; 			    trgobjs))
	   )
      (dolist (obj dmnobjs)
	(let* ((clause-plist (funcall (getf plist :rlrfn)
				      (funcall (getf plist :rlr) obj)))
	       (clause-lamlstlen (list-length (getf clause-plist :clsll)))
	       (clause-fn (getf clause-plist :clsfn)))
	  (assert-toplevel-lamlstlen obj clause-lamlstlen)
	  (pass-args-to-clause-func obj clause-fn clause-lamlstlen))))))

