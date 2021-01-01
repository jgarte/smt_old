
(in-package :smt-engine)



(defclass glyph (canvas)
  ((bcr :accessor bcr)   
   (canvas-color :initform %glyph-canvas-color%
		 :documentation "Background color")
   (code :initarg :code
	 :accessor code)
   (glyph-vis-p :initarg :glyph-vis-p
		:initform t
		:accessor glyph-vis-p)   
   (glyph-color :initarg :glyph-color
		:initform "black"
		:accessor glyph-color
		:documentation "Glyph's Face Color")
   (glyph-opac :initarg :glyph-opac
	       :initform 1
	       :accessor glyph-opac)
   )
  (:documentation "A Movable Type is the smallest unit
representing a single musical symbol. It's content consists
of a canvas filled with one glyph.
Mtypes should be printable both as standalone and as part of
Composing Sticks."))



(defun glyphp (obj) (typep obj 'glyph))

;;; A distinction should be made between su & uu:
;;; uu needs to be involving the scaling, for su which is used for putting svg elements
;;; together is unscaled, since the scaling is written to the transFORM attribute
(defmethod initialize-instance :after ((obj glyph) &key)
  (setf (bcr obj) (get-bcr (code obj) (family obj))
	;; Height and Width can be computed for Mtypes right away, since not
	;; dependant on x or y!
	(slot-value obj 'hslot) (refresh-height obj)
	(slot-value obj 'wslot) (calc-width obj))
  ;; Allow a glyph obj to be rendered as standalone when it's toplevel.
  ;; No need for :h :w, since already computed above!
  (when (toplevelp obj)
   (refresh-bcr! obj :x t :y t :l t :r t :t t :b t))
  )

(defmethod (setf x) (newx (obj glyph))
  (let ((dx (- newx (x obj))))
    (incf (slot-value obj 'xslot) dx)
    (incf (slot-value obj 'lslot) dx)
    (incf (slot-value obj 'rslot) dx))
  ;; Direction of setfing: from innermost to outermost
  (dolist (anc (reverse (ancestors obj)))
    (setf (slot-value anc 'rslot) (calc-right anc)
	  (slot-value anc 'lslot) (calc-left anc)
	  (slot-value anc 'wslot) (calc-width anc)))
  newx)

(defmethod (setf y) (newy (obj glyph))  
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
(defmethod calc-left ((obj glyph))
  (+ (x obj) (toplvl-scale (bcr-left (bcr obj)))))

(defmethod calc-right ((obj glyph))
  (+ (x obj) (toplvl-scale (bcr-right (bcr obj)))))

(defmethod calc-width ((obj glyph))
  (toplvl-scale (bcr-width (bcr obj))))

(defmethod refresh-top ((obj glyph))
  (+ (y obj) (toplvl-scale (bcr-top (bcr obj)))))

(defmethod refresh-height ((obj glyph))
  (toplvl-scale (bcr-height (bcr obj))))

(defmethod refresh-bottom ((obj glyph))
  (+ (y obj) (toplvl-scale (bcr-bottom (bcr obj)))))


;;; Will be written to SVG
(defmethod svgize-bcr ((obj glyph))
  (svg:rect (left obj)			;Is dependent on x
	    (top obj)
	    (width obj)
	    (height obj)
	    :id (format nil "~A-bcr" (string-downcase (symbol-name (id obj))))
	    :fill (or (canvas-color obj) "none")
	    :fill-opacity (canvas-opac obj)))

(defmethod pack-svglst ((obj glyph))  
  ;; Marker  
  (when (marker-vis-p obj)    
    ;; Since svgize-marker consists of more than one svg-element,
    ;; push each one seperately into SVGLST
    (dolist (elem (svgize-marker obj)) (push elem (svglst obj)))
    (push (xml-base::comment (format nil "Glyph ~A, Marker" (id obj))) (svglst obj)))  
  (push (svg:path (glyph-path-d (code obj) (family obj))
		  :fill (glyph-color obj) 
		  :fill-opacity (glyph-opac obj)
		  :id (symbol-name (id obj))
		  :tx (x obj) :ty (y obj) :sx (x-scaler obj) :sy (y-scaler obj))
	(svglst obj))
  (push (xml-base::comment (format nil "Glyph ~A" (id obj))) (svglst obj))
  ;; BCR Rect
  (when (canvas-vis-p obj)
    (push (svgize-bcr obj) (svglst obj))
    (push (xml-base::comment (format nil "Glyph ~A, BCR" (id obj))) (svglst obj)))  
  )
