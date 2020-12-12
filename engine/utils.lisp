;;; In this file come functions which prepare things
;;; for use by system, like prepearing funcs for svgi etc.
;;; Every thing which is needed by others in advance!
(in-package #:smtngn)

;; (defgeneric svgize (obj)
;;   (:documentation "Returns a list of svg elements to be written to the document.
;; Svgizing happens in the RENDER function after all (possible) rules have been applied
;; to the obj (and possibly it's descendants)."))


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
   (marker-vis-p :initarg :marker-vis-p
		     :initform t
		     :accessor marker-vis-p)))

(defun root (obj)
  "Returns the farthest parent (toplevel) of obj"
  (let ((root (car (ancestors obj))))
    (assert (toplevelp root))
    root))

(defun parent (obj)
  "Returns the direct parent of obj."
  (alexandria:lastcar (ancestors obj)))

;;; Toplevel scale
(defun toplvl-scale (r) (* r %scale%))
;;; Inverse toplevel scale
(defun inv-toplvl-scale (r) (/ r %scale%))


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
;;       (glyph %glyph-marker-circle-color%)
;;       (stacked-form %sform-marker-circle-color%)
;;       (horizontal-form %hform-marker-circle-color%)
;;       (vertical-form %vform-marker-circle-color%))
;;     "none")


(defun svgize-marker (obj)
  (let ((half-line (/ *marker-cross-length* 2))
	(circle-fill (typecase obj
		       (glyph %glyph-marker-circle-color%)
		       (stacked-form %sform-marker-circle-color%)
		       (horizontal-form %hform-marker-circle-color%)
		       (vertical-form %vform-marker-circle-color%)))
	(circle-stroke (typecase obj
			 (glyph %glyph-marker-circle-contour-color%)
			 (stacked-form %sform-marker-circle-contour-color%)
			 (horizontal-form %hform-marker-circle-contour-color%)
			 (vertical-form %vform-marker-circle-contour-color%)))
	(cross-stroke (typecase obj
			(glyph *glyph-marker-cross-color*)
			(stacked-form *sform-marker-cross-color*)
			(horizontal-form *hform-marker-cross-color*)
			(vertical-form *vform-marker-cross-color*)))
	(comment-str (typecase obj
		       (glyph (format nil "Glyph ~A Marker" (id obj)))
		       (stacked-form (format nil "Sform ~A Marker" (id obj)))
		       (horizontal-form (format nil "Hform ~A Marker" (id obj)))
		       (vertical-form (format nil "Vform ~A Marker" (id obj))))))
    (list
     ;; circle
     (svg:circle (x obj) (y obj) *marker-circle-r*
		 :fill circle-fill
		 :fill-opacity *marker-circle-opac*
		 :stroke circle-stroke
		 :stroke-width *marker-line-thickness*)
     ;; cross
     (svg:line (- (x obj) half-line) (y obj) (+ (x obj) half-line) (y obj)
	       :stroke cross-stroke
	       :fill "none"
	       :stroke-width *marker-line-thickness*)
     (svg:line (x obj) (- (y obj) half-line) (x obj) (+ (y obj) half-line)
	       :stroke cross-stroke
	       :fill "none"
	       :stroke-width *marker-line-thickness*)
     (xmlbase::comment comment-str))))


;; (defun svgize-marker (obj)
;;   "Pinpointe awaliye!"
;;   (let ((half-marker-line-length (/ *marker-cross-length* 2)))
;;     (list	   ;list upside-down, since pushing reverses the order
;;      (svg:circle (x obj) (y obj) *marker-circle-r*
;; 		 :fill (typecase obj
;; 			 (glyph %glyph-marker-circle-color%)
;; 			 (stacked-form %sform-marker-circle-color%)
;; 			 (horizontal-form %hform-marker-circle-color%)
;; 			 (vertical-form %vform-marker-circle-color%))
;; 		 :fill-opacity *marker-circle-opac*
;; 		 :stroke *marker-circle-line-color*
;; 		 :stroke-width *marker-line-thickness*)
;;      (xmlbase::comment (format nil "~A, Marker Center" (id obj)))
;;      (svg:line (- (x obj) half-marker-line-length) (y obj) (+ (x obj) half-marker-line-length) (y obj)
;; 	       :stroke (marker-stroke obj)
;; 	       :fill "none"
;; 	       :stroke-width *marker-line-thickness*)
;;      (xmlbase::comment (format nil "~A, Marker Horizontal Line" (id obj)))
;;      (svg:line (x obj) (- (y obj) half-marker-line-length) (x obj) (+ (y obj) half-marker-line-length)
;; 	       :stroke (marker-stroke obj)
;; 	       :fill "none"
;; 	       :stroke-width *marker-line-thickness*)
;;      (xmlbase::comment (format nil "~A, Marker Vertical Line" (id obj)))
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
;; 		;; Top-level glyph can't have access to non-existing ancestors,
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
  (dolist (attr-val (xmlbase::elmattrs xmlelem))
    (when (member (car attr-val) *posidim-attrs* :test #'string=)
      (setf (cdr attr-val) (inv-toplvl-scale (cdr attr-val))))))

(defun replace-with-transform! (xml-elem)  
  (multiple-value-bind (trns indxd) (svg::extract-transformations xml-elem)
    (when trns
      (setf (xmlbase::elmattrs xml-elem) (set-difference (xmlbase::elmattrs xml-elem) trns))
      (push (cons "transform"
		  (let ((s ""))
		    (dolist (l indxd s) ;fängt mit 0 an
		      (let ((ts (remove-if-not #'(lambda (av) (string= "t" (car av) :end2 1)) (cadr l)))
			    (ss (remove-if-not #'(lambda (av) (string= "s" (car av) :end2 1)) (cadr l))))
			(when ts
			  (setf s (concatenate 'string s (format nil "translate(~D ~D) " (cdr (first ts)) (cdr (second ts))))))
			(when ss
			  (setf s (concatenate 'string s (format nil "scale(~D ~D)" (cdr (first ss)) (cdr (second ss))))))))))
	    (xmlbase::elmattrs xml-elem)))))

;; (mapcan #'(lambda (x)
;; 	    (if (formp x)
;; 		(cons x (desc x))
;; 		(list x)))
;; 	(list (sform) (vform) (notehead "s0") (note 3)))



(defun render (lst &key (apprulp t) (drawp t) (page-format *page-format*))
  ;; Vorbereitungen
  (dolist (obj lst)
    (when (formp obj)
      ;; perform pre-processings
      (preprocess obj)
      ;; Line-ups
      (hlineup obj)
      ))
  (when apprulp
    (apply-rules (mapcan #'(lambda (x)
			     (if (formp x)
				 (cons x (desc x))
				 (list x)))
			 lst)))
  ;; This part must ONLY do the drawing stuff!!!
  (when drawp
    ;;  Pack svg lists
    (dolist (obj lst)      
      (pack-svglst obj)
      (dolist (elem (svglst obj))
	(inverse-toplvl-scale-posidims! elem)
	(replace-with-transform! elem)))
    (svg:write-svg (svg:g
		    ;; Setting the toplevel scaling of the score
		    :attributes `(("transform" . ,(svg:transform (svg:scale %scale% %scale%))))
		    :content (append (list (mapcar #'svglst lst)
					   (svg:rect 0 0 1 6
						     :fill "red"
						     :fill-opacity .7))))
		   :width (getf (page-size page-format) :w)
		   :height (getf (page-size page-format) :h)))
  (uiop:run-program "rsvg-convert --format=pdf --output=/tmp/smt.pdf /tmp/smt.svg"))

(defun packsvg (object &rest svg-elements)
  ""
  (dolist (e (reverse svg-elements) (svglst object))
    (push e (svglst object))))



(defmacro lazy-getf (place indicator default)
  `(or (getf ,place ,indicator) ,default))
