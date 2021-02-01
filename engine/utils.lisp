;;; In this file come functions which prepare things
;;; for use by system, like prepearing funcs for svgi etc.
;;; Every thing which is needed by others in advance!
(in-package #:smt-engine)

;;; Reversed order of funcs application than defined by RH
(defun comp-reducer (f1 f2)
  (lambda (&rest args) (funcall f2 (apply f1 args))))
;;; 
(defun compfunc (&rest functions)
  "Takes a set of functions and returns a fn that is the composition
of those fns.  The returned fn takes a variable number of args,
applies the leftmost of fns to the args, the next
fn (left-to-right) to the result, etc. ©Rich Hickey"
  (when functions (reduce #'comp-reducer functions)))

(defun enumerate-generations (x n)
  ""
  (if (or (mcharp x) (null (content x)))
      ()
      ;; X is a form with children
      (mapcan #'(lambda (k) (cons (cons n k) (enumerate-generations k (1+ n))))
	      ;; Content beinhaltet immer nur die erste Generation der Kinder,
	      ;; (die Oberfläche des Nachwuchses)
	      (content x))
      ))

(defun children (x &optional (lastgen-first t))
  "Returns a list of (0 CHILD01 CHILD02 ...) (1 CHILD11 CHILD12 ...) 
with 0, 1, ... being the number of generation. LASTGEN-FIRST= <a href=\"http://www.lispworks.com/documentation/HyperSpec/Body/a_t.htm#t\">T</a> sorts
the farthest & youngest children to appear first in the list,
LASTGEN-FIRST=NIL the eldest and next one."
  (let* ((enum (enumerate-generations x 0))
	 (max-gen (apply #'max (mapcar #'car enum)))
	 generations)
    (dotimes (i (1+ max-gen) (sort generations
				   (if lastgen-first #'> #'<)
				   :key #'car))
      (push (cons i (mapcar #'cdr
			    (remove-if-not
			     #'(lambda (gen) (= (car gen) i))
			     enum)))
	    generations))))



;;; Staff Space & Scaling
(defun chlapik-staff-space (rastral-no)
  "Rastral Größen wie bei Chlapik S. 32 beschrieben."
  (ecase rastral-no
    (2 (mm-to-px 1.88))
    (3 (mm-to-px 1.755))
    (4 (mm-to-px 1.6))
    (5 (mm-to-px 1.532))
    (6 (mm-to-px 1.4))
    (7 (mm-to-px 1.19))
    (8 (mm-to-px 1.02))))
(defparameter *staff-space* (chlapik-staff-space 2))
;;; This is the user-interface,
(defparameter *scale* 1
  "Global scaling factor for X and Y coordinates.")
(defparameter *vertical-space-reference-glyph* 'clefs.c
  "The relation between 4 staff spaces and the vertical dimension 
of this glyph is used to find the global internal factor, by which all
glyphs are scaled to ... By convention the vertical space of 
stave is equal to the height of the alto clef, hence the default glyph.")

;;; and the actual internal factor
(define-symbol-macro .scale. (* *scale*
				;; Chlapik p. 33: The symbol C-clef is 4 staff-spaces height.
				(/ (* 4 *staff-space*)
				   (bbox-height
				    (glyph-bbox
				     (get-glyph *vertical-space-reference-glyph*)))
				   )))

;;; Toplevel scale
(defun toplvl-scale (r) (* r .scale.))
;;; Inverse toplevel scale
(defun inv-toplvl-scale (r) (/ r .scale.))

(defconstant +px-per-mm+ 3.7795275591 "Pixels per mm")
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Need this for the margins constants later in the file
  ;; (DEFCONSTANT wants to know about constant's value at compile-time too)
  (defun mm-to-px (mm) (* mm +px-per-mm+)))


;;; Page margines: Measured from Schubert Sonate, Henle

(defconstant +right-margin+ (mm-to-px 25))
(defconstant +left-margin+ (mm-to-px 36))
(defconstant +top-margin+ (mm-to-px 56))

;;; Line thickness for both cross and circle's contour
(defparameter *origin-line-thickness* 3)
;;; Marker's Cross
(defparameter *mchar-origin-cross-color* "deeppink")
(defparameter *sform-origin-cross-color* "tomato")
(defparameter *hform-origin-cross-color* "green")
(defparameter *vform-origin-cross-color* "blue")
(defparameter *origin-cross-length* 40 "Line length")

(defparameter *origin-circle-r* 4)
(defparameter *origin-circle-opac* .3)
;;; Marker's Circle
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




(defun page-size (format)
  "Returns format's page size in pixels. BB p.481"
  (ecase format
    (:a3 (list :h (mm-to-px 420) :w (mm-to-px 297)))
    (:b4 (list :h (mm-to-px 353) :w (mm-to-px 250)))
    (:a4 (list :h (mm-to-px 297) :w (mm-to-px 210)))))

(defparameter *page-format* :a4)


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
   (svglst :initform ()
	   :accessor svglst)
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



(defun root (obj)
  "Returns the farthest parent (toplevel) of obj"
  (let ((root (car (ancestors obj))))
    (assert (toplevelp root))
    root))

(defun parent (obj)
  "Returns the direct parent of obj."
  (alexandria:lastcar (ancestors obj)))
(defun grandparent (obj)
  "Returns the eldest of the parents"
  (car (ancestors obj)))

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
     (xml-base::comment comment-str))
    ))


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
				 (cons x (descendants x))
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
    (svg:write-svg
     (svg:g
      ;; Setting the toplevel scaling of the score
      :attributes `(("transform" . ,(svg::transform (svg:scale .scale. .scale.))))
      :content (append (list (mapcar #'svglst lst)
			     (svg:rect 0 0 50 50
				       :fill "red"
				       :fill-opacity .7))))
     :width (getf (page-size page-format) :w)
     :height (getf (page-size page-format) :h))))

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
