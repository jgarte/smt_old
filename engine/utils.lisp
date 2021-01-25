;;; In this file come functions which prepare things
;;; for use by system, like prepearing funcs for svgi etc.
;;; Every thing which is needed by others in advance!
(in-package #:smt-engine)


(defconstant +px-per-mm+ 3.7795275591 "Pixels per mm")
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Need this for the margins constants later in the file
  ;; (DEFCONSTANT wants to know about constant's value at compile-time too)
  (defun mm-to-px (mm) (* mm +px-per-mm+)))

;;; Page margines: Measured from Schubert Sonate, Henle
(defconstant +right-margin+ (mm-to-px 25))
(defconstant +left-margin+ (mm-to-px 36))
(defconstant +top-margin+ (mm-to-px 56))



(defgeneric pack-svglst (obj)
  (:documentation "Pushes all SVG elements to the SVGLST of obj."))

(defclass smtobj ()
  ((id :initarg :id
       :initform (gensym "SMTOBJ")
       :reader id)
   (ruler :initarg :ruler
	  :accessor ruler)
   ;; (ruleids :initarg :ruleids
   ;; 	    :initform ()
   ;; 	    :type list
   ;; 	    :documentation "(spn etc."
   ;; 	    :accessor ruleids)
   (svglst :initform ()
	   :accessor svglst)
   ;; Ruletable domain
   (domain :initarg :domain
	   :initform nil
	   :documentation "Ruletable domain"
	   :accessor domain)
   (ancestors :accessor ancestors
	      :initform ())   
   (origin-visible-p :initarg :origin-visible-p
		     :initform t
		     :accessor origin-visible-p)))



(defun root (obj)
  "Returns the farthest parent (toplevel) of obj"
  (let ((root (car (ancestors obj))))
    (assert (toplevelp root))
    root))

(defun parent (obj)
  "Returns the direct parent of obj."
  (alexandria:lastcar (ancestors obj)))



(defun register-object (obj)
  "Registers OBJ."
  (setf (gethash (id obj) *central-registry*) obj))

(defun remsmtobj (id)
  (remhash id *central-registry*))



(defmethod initialize-instance :after ((obj smtobj) &key)
  (register-object obj)
  )



;;; A substitute for MEMBER-IF
(defun delimit-ancestors (ancestors-list cutoff-pred upwardp)
  (when ancestors-list			;NIL if ancestors-list empty
    ;; upwardp = starting from the next ancestor up to the top-most
    (let ((pos (position-if cutoff-pred ancestors-list :from-end upwardp)))
      (unless pos
	(error "Predicate ~A yielded NIL on ancestors-list ~A" cutoff-pred ancestors-list))
      (nthcdr pos ancestors-list))))

;; (defun parse-float (string)
;;   "Returns a float read from string, and the index to the remainder of string."
;;   (multiple-value-bind (integer i)
;;       (parse-integer string :junk-allowed t)
;;     (multiple-value-bind (fraction j)
;;         (parse-integer string :start (+ i 1) :junk-allowed t)
;;       (values (float (+ integer (/ fraction (expt 10 (- j i 1))))) j))))


;; (or (typecase obj
;;       (mchar %mchar-origin-circle-color%)
;;       (stacked-form %sform-origin-circle-color%)
;;       (horizontal-form %hform-origin-circle-color%)
;;       (vertical-form %vform-origin-circle-color%))
;;     "none")


(defun svgize-origin (obj)
  (let ((half-line (/ *origin-cross-length* 2))
	(circle-fill (typecase obj
		       (mchar %mchar-origin-circle-color%)
		       (stacked-form %sform-origin-circle-color%)
		       (horizontal-form %hform-origin-circle-color%)
		       (vertical-form %vform-origin-circle-color%)))
	(circle-stroke (typecase obj
			 (mchar %mchar-origin-circle-contour-color%)
			 (stacked-form %sform-origin-circle-contour-color%)
			 (horizontal-form %hform-origin-circle-contour-color%)
			 (vertical-form %vform-origin-circle-contour-color%)))
	(cross-stroke (typecase obj
			(mchar *mchar-origin-cross-color*)
			(stacked-form *sform-origin-cross-color*)
			(horizontal-form *hform-origin-cross-color*)
			(vertical-form *vform-origin-cross-color*)))
	(comment-str (typecase obj
		       (mchar (format nil "Character ~A Origin Point" (id obj)))
		       (stacked-form (format nil "Sform ~A Origin Point" (id obj)))
		       (horizontal-form (format nil "Hform ~A Origin Point" (id obj)))
		       (vertical-form (format nil "Vform ~A Origin Point" (id obj))))))
    (list
     ;; circle
     (svg:circle (x obj) (y obj) *origin-circle-r*
		 :fill circle-fill
		 :fill-opacity *origin-circle-opac*
		 :stroke circle-stroke
		 :stroke-width *origin-line-thickness*)
     ;; cross
     (svg:line (- (x obj) half-line) (y obj) (+ (x obj) half-line) (y obj)
	       :stroke cross-stroke
	       :fill "none"
	       :stroke-width *origin-line-thickness*)
     (svg:line (x obj) (- (y obj) half-line) (x obj) (+ (y obj) half-line)
	       :stroke cross-stroke
	       :fill "none"
	       :stroke-width *origin-line-thickness*)
     (xml-base::comment comment-str))))


;; (defun svgize-origin (obj)
;;   "Pinpointe awaliye!"
;;   (let ((half-marker-line-length (/ *origin-cross-length* 2)))
;;     (list	   ;list upside-down, since pushing reverses the order
;;      (svg:circle (x obj) (y obj) *origin-circle-r*
;; 		 :fill (typecase obj
;; 			 (mchar %mchar-origin-circle-color%)
;; 			 (stacked-form %sform-origin-circle-color%)
;; 			 (horizontal-form %hform-origin-circle-color%)
;; 			 (vertical-form %vform-origin-circle-color%))
;; 		 :fill-opacity *origin-circle-opac*
;; 		 :stroke *marker-circle-line-color*
;; 		 :stroke-width *origin-line-thickness*)
;;      (xml-base::comment (format nil "~A, Origin Point Center" (id obj)))
;;      (svg:line (- (x obj) half-marker-line-length) (y obj) (+ (x obj) half-marker-line-length) (y obj)
;; 	       :stroke (marker-stroke obj)
;; 	       :fill "none"
;; 	       :stroke-width *origin-line-thickness*)
;;      (xml-base::comment (format nil "~A, Origin Point Horizontal Line" (id obj)))
;;      (svg:line (x obj) (- (y obj) half-marker-line-length) (x obj) (+ (y obj) half-marker-line-length)
;; 	       :stroke (marker-stroke obj)
;; 	       :fill "none"
;; 	       :stroke-width *origin-line-thickness*)
;;      (xml-base::comment (format nil "~A, Origin Point Vertical Line" (id obj)))
;;      )))



 




;; (defun maybe-apply-rules (obj)
;;   (let ((rule-labels (pick-hash-keys *ruletable*
;; 				     #'(lambda (k)
;; 					 (find obj (getf k :targets) :test #'typep)))))
;;     ;; When do we endeavor to apply rules? (Absolutely needed ones)
;;     (when (and rule-labels (domain obj))
;;       (assert (member (domain obj) (getf *registered-domains* (type-of obj))) (obj)
;; 	      "No rules are defined for the combination of object ~A & domain ~A,
;; consult *registered-domains*" obj (domain obj))
;;       (let ((obj-ruleids (ruleids obj)))	
;; 	(dotimes (rule-idx (list-length obj-ruleids))
;; 	  (dolist (rlabel rule-labels)
;; 	    (when (and (eql (getf rlabel :domain) (domain obj))
;; 		       (eql (getf rlabel :id) (nth rule-idx obj-ruleids))
;; 		       (eql (getf rlabel :idx) rule-idx))
;; 	      ;; Then have found our rule!
;; 	      (let* ((rule-plist (gethash rlabel *ruletable*))
;; 		     (type-checker (getf rule-plist :ruleid-type-checker))
;; 		     (rulefn (funcall type-checker (ruleidval (nth rule-idx obj-ruleids) obj)))
;; 		     (lambda-lst-len (list-length (getf rule-plist :lambda-list))))
;; 		;; Top-level mchar can't have access to non-existing ancestors,
;; 		;; so args can be 0 or 1 (the obj itself)	  	    
;; 		(assert-toplevel-lll obj lambda-lst-len)
;; 		(cond
;; 		  ((zerop lambda-lst-len) (funcall rulefn))
;; 		  ((= lambda-lst-len 1) (funcall rulefn obj))
;; 		  (t (apply rulefn obj
;; 			    ;; Since sticks are PUSHED to ancestors-lst of
;; 			    ;; other objs, the nearest parent is at the end of anc-lst,
;; 			    ;; hence the reverse
;; 			    (reverse (last (ancestors obj) (1- lambda-lst-len))))))))))))))



(defun inverse-toplvl-scale-posidims! (xmlelem)
  (dolist (attr-val (xml-base::elmattrs xmlelem))
    (when (member (car attr-val) *posidim-attrs* :test #'string=)
      (setf (cdr attr-val) (inv-toplvl-scale (cdr attr-val))))))

(defun replace-with-transform! (xml-elem)  
  (multiple-value-bind (trns indxd) (svg::extract-transformations xml-elem)
    (when trns
      (setf (xml-base::elmattrs xml-elem) (set-difference (xml-base::elmattrs xml-elem) trns))
      (push (cons "transform"
		  (let ((s ""))
		    (dolist (l indxd s) ;fängt mit 0 an
		      (let ((ts (remove-if-not #'(lambda (av) (string= "t" (car av) :end2 1)) (cadr l)))
			    (ss (remove-if-not #'(lambda (av) (string= "s" (car av) :end2 1)) (cadr l))))
			(when ts
			  (setf s (concatenate 'string s (format nil "translate(~D ~D) " (cdr (first ts)) (cdr (second ts))))))
			(when ss
			  (setf s (concatenate 'string s (format nil "scale(~D ~D)" (cdr (first ss)) (cdr (second ss))))))))))
	    (xml-base::elmattrs xml-elem)))))



(defun packsvg (object &rest svg-elements)
  ""
  (dolist (e (reverse svg-elements) (svglst object))
    (push e (svglst object))))



(defmacro lazy-getf (place indicator default)
  `(or (getf ,place ,indicator) ,default))


(defun inspect-br (obj)
  (let ((o obj))
    (format t "~%===================== BR ~A" (id o))
    (format t "~% X: ~D Y: ~D" (x o) (y o))
    (format t "~% Top: ~D Fixed Top: ~D" (top o) (when (formp o) (fixed-top o)))
    (format t "~% Bottom: ~D Fixed Bottom: ~D" (bottom o) (when (formp o) (fixed-bottom o)))
    (format t "~% Left: ~D Right: ~D" (left o) (right o))
    (format t "~% Width: ~D Height: ~D Fixed Height: ~D" (width o) (height o) (when (formp o) (fixed-height o)))
    (format t "~%=========================")))
