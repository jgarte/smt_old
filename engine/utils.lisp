;;; In this file come functions which prepare things
;;; for use by system, like prepearing funcs for svgi etc.
;;; Every thing which is needed by others in advance!
(in-package #:smt-engine)

(defconstant +px-per-mm+ 3.7795275591 "Pixels per mm")
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Need this for the margins constants later in the file
  ;; (DEFCONSTANT wants to know about constant's value at compile-time too)
  (defun mm-to-px (mm) (* mm +px-per-mm+)))

(defparameter *svg-count-decimal* 10
  "Specifies the number of decimal points in printed SVG numericals.")

(defmacro defsvg (name &rest obattrs)
  (let ((stringized (gensym)))
    `(defun ,(intern (format nil "SVG~A" (symbol-name name)))
	 ;; Obligatory attributes and Optional attributes
	 (,@obattrs &rest opattrs &key &allow-other-keys)
       (s-xml:make-xml-element :name ,(string-downcase (string name))
			       :attributes (append
					    `(,,@(mapcar #'(lambda (att)
							   ;; Attribute names always downcase?
							   `(cons ,(string-downcase (string att))
								  (format nil "~A" ,att)))
						       obattrs))
					    ;; Convert every attribute value to ASCII
					    (let (,stringized)
					      (alexandria:doplist (key val opattrs ,stringized)
						(push (cons key (format nil "~A" val)) ,stringized))
					      ))))))

(defsvg line x1 y1 x2 y2)
(defsvg rect x y width height)
(defsvg circle cx cy r)
(defsvg path d)
(defun svgcomment (string)
  (s-xml:make-xml-element :name "!--" :children (list string)))



;;; Reversed order of funcs application than defined by RH
(defun comp-reduction (f1 f2)
  (lambda (&rest args) (funcall f2 (apply f1 args))))
;;; 
(defun comp (&rest functions)
  "Takes a set of functions and returns a fn that is the composition
of those fns.  The returned fn takes a variable number of args,
applies the leftmost of fns to the args, the next
fn (left-to-right) to the result, etc. ©Rich Hickey"
  (when functions (reduce #'comp-reduction functions)))


;;; Staff Space & Scaling
(defun chlapik-staff-space (rastral-nr)
  "Rastral Größen wie bei Chlapik S. 32 beschrieben."
  (ecase rastral-nr
    (zwei (mm-to-px 1.88))
    (drei (mm-to-px 1.755))
    (vier (mm-to-px 1.6))
    (fuenf (mm-to-px 1.532))
    (sechs (mm-to-px 1.4))
    (sieben (mm-to-px 1.19))
    (acht (mm-to-px 1.02))))

(defparameter *space* (chlapik-staff-space 'zwei)
  "Measure of the current staff space in pixels.")

;;; This is the user-interface,
(defparameter *scale* 1
  "Global scaling factor for X and Y coordinates.")
(defparameter *vertical-space-reference-glyph* 'clefs.c
  "The relation between 4 staff spaces and the vertical dimension 
of this glyph is used to find the global internal factor, by which all
glyphs are scaled to ... By convention the vertical space of 
stave is equal to the height of the alto clef, hence the default glyph.")

;;; The actual internal factor, I need this factor to get
;;; uniformly sized characters, disregarding their design dimensions.
(define-symbol-macro .scale. (* *scale*	;This internal factor is always affected by the user global factor *SCALE*.
				;; Chlapik p. 33: The symbol C-clef is 4 staff-spaces height.
				(/ (* 4 *space*)
				   ;; Height of the alto clef (in current font)
				   (bbox-height
				    (glyph-bbox
				     (get-glyph *vertical-space-reference-glyph*)))
				   )))

;;; Toplevel scale
(defun toplvl-scale (r) (* r .scale.))
;;; Inverse toplevel scale
(defun inv-toplvl-scale (r) (/ r .scale.))




;;; Page margines: Measured from Schubert Sonate, Henle

(defconstant +right-margin+ (mm-to-px 25))
(defconstant +left-margin+ (mm-to-px 36))
(defconstant +top-margin+ (mm-to-px 56))

;;; ;;;;;;;;;;;;;;ORIGIN
;;; Line thickness for both cross and circle's contour
(defparameter *origin-line-thickness* .06)
;;; Origin's Cross
(defparameter *mchar-origin-cross-color* "deeppink")
(defparameter *sform-origin-cross-color* "tomato")
(defparameter *hform-origin-cross-color* "green")
(defparameter *vform-origin-cross-color* "blue")
(defparameter *origin-cross-length* 20 "Line length")

;;; Origin's Circle
(defparameter *origin-circle-r* 4)
(defparameter *origin-circle-opac* .3)
(define-symbol-macro %mchar-origin-circle-color% *mchar-origin-cross-color*)
(define-symbol-macro %mchar-origin-circle-contour-color% *mchar-origin-cross-color*)
(define-symbol-macro %sform-origin-circle-color% *sform-origin-cross-color*)
(define-symbol-macro %sform-origin-circle-contour-color% *sform-origin-cross-color*)
(define-symbol-macro %hform-origin-circle-color% *hform-origin-cross-color*)
(define-symbol-macro %hform-origin-circle-contour-color% *hform-origin-cross-color*)
(define-symbol-macro %vform-origin-circle-color% *vform-origin-cross-color*)
(define-symbol-macro %vform-origin-circle-contour-color% *vform-origin-cross-color*)

;;; Chase backgrounds follow their marker counterparts
(define-symbol-macro %mchar-canvas-color% *mchar-origin-cross-color*)
(define-symbol-macro %sform-canvas-color% *sform-origin-cross-color*)
(define-symbol-macro %hform-canvas-color% *hform-origin-cross-color*)
(define-symbol-macro %vform-canvas-color% *vform-origin-cross-color*)



(defstruct page-format width height)
(defparameter *a4* (make-page-format :width (mm-to-px 210) :height (mm-to-px 297)))

(defun page-size (format)
  "Returns format's page size in pixels. BB p.481"
  (ecase format
    (:a3 (list :h (mm-to-px 420) :w (mm-to-px 297)))
    (:b4 (list :h (mm-to-px 353) :w (mm-to-px 250)))
    (:a4 (list :h  :w ))))

(defparameter *page-format* *a4*)


(defparameter *posidim-attrs*
  '("x" "y" "width" "height" "left" "right" "top" "bottom"
    ;; transform/translate xy
    "tx" "ty"
    ;; circle
    "cx" "cy" "r"
    "x1" "x2" "y1" "y2")
  "These positional-dimensional attributes should be inverse-toplevel-scaled in all SVGLSTs before 
writing the svg doc.")

(defparameter *ticket-output* nil)
(defparameter *output-counter* 0)
(define-symbol-macro %output-path%
    (cond
      (*ticket-output* (incf *output-counter*)
		       (format nil "/tmp/smt-etude-~D.svg" *output-counter*))
      (t (format nil "/tmp/smt-etude.svg"))))


(defgeneric pack-svglst (obj)
  (:documentation "Pushes all SVG elements to the SVGLST of obj."))

(defun smteq (obj1 obj2)
  ""
  (and (typep obj1 (type-of obj2))
       ;; If not same type, they may have same ID???
       (eq (id obj1) (id obj2))))

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
   (svg-list :initform ()
	     :accessor svg-list)
   ;; Ruletable domain
   (domain :initarg :domain
	   :initform nil
	   :documentation "Ruletable domain"
	   :accessor domain)
   (ancestors :accessor ancestors
	      :documentation "The nearest of the parents to the object
in this list is the LAST one, so if u want to start from the nearest
use the reversed of this list."
	      :initform ())   
   (origin-visible-p :initarg :origin-visible-p
		     :initform t
		     :accessor origin-visible-p)))





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




;; (defun svgize-origin (obj)
;;   (let ((half-line (/ *origin-cross-length* 2))
;; 	(circle-fill (typecase obj
;; 		       (mchar %mchar-origin-circle-color%)
;; 		       (stacked-form %sform-origin-circle-color%)
;; 		       (horizontal-form %hform-origin-circle-color%)
;; 		       (vertical-form %vform-origin-circle-color%)))
;; 	(circle-stroke (typecase obj
;; 			 (mchar %mchar-origin-circle-contour-color%)
;; 			 (stacked-form %sform-origin-circle-contour-color%)
;; 			 (horizontal-form %hform-origin-circle-contour-color%)
;; 			 (vertical-form %vform-origin-circle-contour-color%)))
;; 	(cross-stroke (typecase obj
;; 			(mchar *mchar-origin-cross-color*)
;; 			(stacked-form *sform-origin-cross-color*)
;; 			(horizontal-form *hform-origin-cross-color*)
;; 			(vertical-form *vform-origin-cross-color*)))
;; 	(comment-str (typecase obj
;; 		       (mchar (format nil "Character ~A Origin Point" (id obj)))
;; 		       (stacked-form (format nil "Sform ~A Origin Point" (id obj)))
;; 		       (horizontal-form (format nil "Hform ~A Origin Point" (id obj)))
;; 		       (vertical-form (format nil "Vform ~A Origin Point" (id obj))))))
;;     (list
;;      ;; circle
;;      (svg:circle (x obj) (y obj) *origin-circle-r*
;; 		 :fill circle-fill
;; 		 :fill-opacity *origin-circle-opac*
;; 		 :stroke circle-stroke
;; 		 :stroke-width *origin-line-thickness*)
     
;;      ;; cross
;;      (svg:line (- (x obj) half-line) (y obj) (+ (x obj) half-line) (y obj)
;; 	       :stroke cross-stroke
;; 	       :fill "none"
;; 	       :stroke-width *origin-line-thickness*)
;;      (svg:line (x obj) (- (y obj) half-line) (x obj) (+ (y obj) half-line)
;; 	       :stroke cross-stroke
;; 	       :fill "none"
;; 	       :stroke-width *origin-line-thickness*)
;;      (xml-base::comment comment-str))
;;     ))


;; (defun inverse-toplvl-scale-posidims! (xmlelem)
;;   (dolist (attr-val (xml-base::elmattrs xmlelem))
;;     (when (member (car attr-val) *posidim-attrs* :test #'string=)
;;       (setf (cdr attr-val) (inv-toplvl-scale (cdr attr-val))))))

;; (defun replace-with-transform! (xml-elem)  
;;   (multiple-value-bind (trns indxd) (svg::extract-transformations xml-elem)
;;     (when trns
;;       (setf (xml-base::elmattrs xml-elem) (set-difference (xml-base::elmattrs xml-elem) trns))
;;       (push (cons "transform"
;; 		  (let ((s ""))
;; 		    (dolist (l indxd s) ;fängt mit 0 an
;; 		      (let ((ts (remove-if-not #'(lambda (av) (string= "t" (car av) :end2 1)) (cadr l)))
;; 			    (ss (remove-if-not #'(lambda (av) (string= "s" (car av) :end2 1)) (cadr l))))
;; 			(when ts
;; 			  (setf s (concatenate 'string s (format nil "translate(~D ~D) " (cdr (first ts)) (cdr (second ts))))))
;; 			(when ss
;; 			  (setf s (concatenate 'string s (format nil "scale(~D ~D)" (cdr (first ss)) (cdr (second ss))))))))))
;; 	    (xml-base::elmattrs xml-elem)))))

(defun render (score &key (apprulp t) (drawp t) (page-format *page-format*))
  ;; Vorbereitungen
  (when (formp score)
    ;; perform pre-processings
    (preprocess score)
    ;; Line-ups
    (hlineup score))
  ;; (dolist (obj score)
  ;;   (when (formp obj)
  ;;     ;; perform pre-processings
  ;;     (preprocess obj)
  ;;     ;; Line-ups
  ;;     (hlineup obj)
  ;;     ))
  
  (when apprulp (apply-rules score))
  ;; (when apprulp
  ;;   (apply-rules (mapcan #'(lambda (x)
  ;; 			     (if (formp x)
  ;; 				 (cons x (descendants x))
  ;; 				 (list x)))
  ;; 			 score)))
  ;; Using s-xml
  (when drawp
    (with-open-file (s "/tmp/smt.svg"
  		       :if-does-not-exist :create
  		       :if-exists :supersede
  		       :direction :output)
      (format s "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>~%")
      (nlayer-svg-list score)
      (s-xml:print-xml-dom
       (s-xml:make-xml-element :name "svg"
			       :attributes `(("xmlns" . "http://www.w3.org/2000/svg")
					     ("xmlns:xlink" . "http://www.w3.org/1999/xlink")
					     ("width" . ,(format nil "~VF" *svg-count-decimal*
								 (page-format-width page-format)))
					     ("height" . ,(format nil "~VF" *svg-count-decimal*
								  (page-format-height page-format))))
			       :children (svg-list score))
       :xml-struct s t 1)))
  )


(defun packsvg (object &rest svg-elements)
  ""
  (dolist (e (reverse svg-elements) (svg-list object))
    (push e (svg-list object))))




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
