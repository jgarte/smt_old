
(in-package :smt-engine)


(defun make-mchar (name &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'mchar
	 :name name
	 initargs))


;;; Music Character
(defclass mchar (canvas)
  ((bcr :accessor bcr)   
   (canvas-color :initform %mchar-canvas-color%
		 :documentation "Background color")
   (code :initarg :code
	 :accessor code)
   (name :initarg :name :reader name)
   (mchar-vis-p :initarg :mchar-vis-p
		:initform t
		:accessor mchar-vis-p)   
   (mchar-color :initarg :mchar-color
		:initform "black"
		:accessor mchar-color
		:documentation "Glyph's Face Color")
   (mchar-opac :initarg :mchar-opac
	       :initform 1
	       :accessor mchar-opac)
   )
  (:documentation "A Movable Type is the smallest unit
representing a single musical symbol. It's content consists
of a canvas filled with one glyph.
Mtypes should be printable both as standalone and as part of
Composing Sticks."))



(defun mcharp (obj) (typep obj 'mchar))

;;; A distinction should be made between su & uu:
;;; uu needs to be involving the scaling, for su which is used for putting svg elements
;;; together is unscaled, since the scaling is written to the transFORM attribute
(defmethod initialize-instance :after ((obj mchar) &key)
  (setf 
	;; Height and Width can be computed for Mtypes right away, since not
	;; dependant on x or y!
	(slot-value obj 'hslot) (refresh-height obj)
	(slot-value obj 'wslot) (compwidth obj))
  ;; Allow a mchar obj to be rendered as standalone when it's toplevel.
  ;; No need for :h :w, since already computed above!

  (when (toplevelp obj)
    (refresh-bcr! obj :x t :y t :l t :r t :t t :b t))
  )



(defmethod (setf x) (newx (obj mchar))
  (let ((dx (- newx (x obj))))
    (incf (slot-value obj 'xslot) dx)
    (incf (slot-value obj 'lslot) dx)
    (incf (slot-value obj 'rslot) dx))
  ;; Direction of setfing: from innermost to outermost
  (dolist (anc (reverse (ancestors obj)))
    (setf (slot-value anc 'rslot) (calc-right anc)
	  (slot-value anc 'lslot) (calc-left anc)
	  (slot-value anc 'wslot) (compwidth anc)))
  newx)

(defmethod (setf y) (newy (obj mchar))  
  (setf (slot-value obj 'yslot) newy)
  (refresh-bcr! obj :t t :b t)
  ;; Ancestors of a Glyph can ONLY be sticks!!
  (dolist (anc (reverse (ancestors obj)))
    ;; When the top of ancestor is more to the bottom than mine.
    ;; Happens when we have moved upwards.
    ;; (when (> (top anc) (top obj))
    ;;   (setf (slot-value anc 'tslot) (top obj))
    ;;   (refresh-bcr! anc :h t))
    ;; ;; When the b of ancestor is more to the top than mine!
    ;; ;; Happens when we have moved downwards!
    ;; (when (< (bottom anc) (bottom obj))
    ;;   (setf (slot-value anc 'bslot) (bottom obj))
    ;;   (refresh-bcr! anc :h t))
    (refresh-bcr! anc :t t :b t :h t))
  newy)


;;; Faghat baraye CS!!!!
(defmethod calc-left ((obj mchar))
  (+ (x obj) (toplvl-scale (bbox-left (glyph-bbox (get-glyph (name obj) (font obj))))
			   ;; (getf (bcr obj) :left)
			   ;; (bcr-left (bcr obj))
			   )))

(defmethod calc-right ((obj mchar))
  (+ (x obj) (toplvl-scale (bbox-right (glyph-bbox (get-glyph (name obj) (font obj))))
	      ;; (getf (bcr obj) :right)
	      ;; (bcr-right (bcr obj))
	      )))


(defmethod width ((obj mchar))
  (slot-value obj 'wslot))
(defmethod compwidth ((obj mchar))
  (toplvl-scale (bbox-width (glyph-bbox (get-glyph (name obj) (font obj))))
   ;; (getf (bcr obj) :width)
   ;; (bcr-width (bcr obj))
   ))

(defmethod refresh-top ((obj mchar))
  (+ (y obj) (toplvl-scale (bbox-top (glyph-bbox (get-glyph (name obj) (font obj))))
	      ;; (getf (bcr obj) :top)
	      ;; (bcr-top (bcr obj))
	      )))

(defmethod refresh-height ((obj mchar))
  (toplvl-scale (bbox-height (glyph-bbox (get-glyph (name obj) (font obj))))
   ;; (getf (bcr obj) :height)
   ;; (bcr-height (bcr obj))
   ))

(defmethod refresh-bottom ((obj mchar))
  (+ (y obj) (toplvl-scale (bbox-bottom (glyph-bbox (get-glyph (name obj) (font obj))))
	      ;; (getf (bcr obj) :bottom)
	      ;; (bcr-bottom (bcr obj))
	      )))


;;; Will be written to SVG
(defmethod svgize-bcr ((obj mchar))
  (svg:rect (left obj)			;Is dependent on x
	    (top obj)
	    (width obj)
	    (height obj)
	    :id (format nil "~A-bcr" (string-downcase (symbol-name (id obj))))
	    :fill (or (canvas-color obj) "none")
	    :fill-opacity (canvas-opac obj)))

(defmethod pack-svglst ((obj mchar))  
  ;; Marker  
  (when (origin-visible-p obj)    
    ;; Since svgize-marker consists of more than one svg-element,
    ;; push each one seperately into SVGLST
    (dolist (elem (svgize-marker obj)) (push elem (svglst obj)))
    (push (xml-base::comment (format nil "Character ~A, Marker" (id obj))) (svglst obj)))  
  (push (svg:path ;; (mchar-path-d (code obj) (family obj))
	 ;; (get-glyph-d (name obj))
	 (glyph-pathd (get-glyph (name obj) (font obj)))
	 :fill (mchar-color obj) 
	 :fill-opacity (mchar-opac obj)
	 :id (symbol-name (id obj))
	 ;; Flip the mchar path vertically here (SY)
	 :tx (x obj) :ty (y obj) :sx (x-scaler obj) :sy (- (y-scaler obj)))
	(svglst obj))
  (push (xml-base::comment (format nil "Character ~A" (id obj))) (svglst obj))
  ;; BCR Rect
  (when (canvas-vis-p obj)
    (push (svgize-bcr obj) (svglst obj))
    (push (xml-base::comment (format nil "Character ~A, BCR" (id obj))) (svglst obj)))  
  )
