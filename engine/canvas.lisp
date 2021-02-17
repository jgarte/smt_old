
;; (asdf:load-system "smt")
(in-package :smt-engine)

;;; All new canvases can be checked for collisions
;;; against other canvas bounding boxes.
(defparameter *covered-areas*
  (make-hash-table :test #'equalp)
  "Areas are bounding boxes on the page in the form
(:X1 R :X2 R :Y1 R :Y2 R) which are laready covered by
some canvases.")



;;; unit of space= width char+guard+floating space
(defun unit-of-space (x)
  (+ (width x) (guard x) (fspace x)))


;;; Chase ist der Rahmen, also das Canvas einfach.
(defclass canvas (smtobj)
  (
   
   (rotate-degrees :initarg :rotate-degrees
		   :initform 0
		   :accessor chase-rotate-degrees)
   
   ;; Move this to a FORM
   (font :initarg :font
	 :initform *font*
	 :accessor font
	 :documentation "Wenn man den Font vom Inhalt des
Ganzen Stick z.B. auf einmal ändern möchte.")
   (toplevelp :initarg :toplevelp
	      :initform nil
	      :documentation "If no parents vorhanden,
dann ist dieses Chase Root Element vom SVG Dokument.
Ein Mtype kann toplevel sein oder nicht. 
Dieses Slot wird nicht vom System geändert, es ist
gesetzt nur vom User."
	      :accessor toplevelp)
   ;; Call the initarg :canvas-color to avoid conflict with mtype's :fill
   (canvas-color :initarg :canvas-color
		 :accessor canvas-color
		 :documentation "This is the color of canvas's bg.")
   (canvas-opac :initarg :canvas-opac
		:documentation "Canvas' background opacity"
		:initform 0.3
		:accessor canvas-opac)
   (canvas-vis-p :initarg :canvas-vis-p
		 :initform t
		 ;; Forget chase-chase-visible-p!!! :-0
		 :accessor canvas-vis-p)
   ;; Diese überholen die Eltern Koordinaten und sind
   ;; stets in ABSOLUT Form zu verstehen, i.e. without scalings
   ;; being applied to them.
   (absx :initarg :absx
	 :accessor absx
	 :initform nil
	 :documentation "")
   (absy :initarg :absy
	 :accessor absy
	 :initform nil)
   ;; These are subject to scalings before being added to xy.
   (x-offset :initarg :x-offset
	     :initform 0
	     :documentation "Extra moving left/right under
scalings."
	     :type number
	     :accessor x-offset)
   (y-offset :initarg :y-offset
	     :documentation "Extra moving down/up"
	     :initform 0
	     :accessor y-offset
	     :type number)
   ;; Scales are overriden by mtype, if it has parent.
   (x-scaler :initarg :x-scaler
	     ;; Set to nil for doing conditionals by scaling toplevel etc.
	     :initform *scale*
	     :accessor x-scaler)
   (y-scaler :initarg :y-scaler
	     :initform *scale*
	     :documentation "Für Mtypes das Umkehren passiert
in denen selbst. Das horizonale Umkehren braucht nämlich ein Stick
nicht! Ausserdem diese für ein mtype innerhalb eines
 Elterns werden zum 1, -1."
	     :accessor y-scaler)
   (xslot :accessor xslot :initform nil)
   (yslot :accessor yslot :initform nil)
   ;; Associated with margines for initial values,
   ;; might be dangarous, but since used only to init
   ;; values (changed later) maybe not dangarous???????
   (lslot :accessor lslot :initform nil) ;init horizontal pnt to +LEFT-MARGIN+ [DEPRECATED, MAYBE SET BACK TO NIL??]
   (rslot :accessor rslot :initform nil)
   (tslot :accessor tslot :initform nil) ;init vertical pnt
   (bslot :accessor bslot :initform nil)
   (wslot :accessor wslot :initform nil)
   (hslot :accessor hslot :initform nil)
   ))

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

(defmethod y ((obj canvas)) (slot-value obj 'yslot))
(defmethod top ((obj canvas)) (slot-value obj 'tslot))
(defmethod bottom ((obj canvas)) (slot-value obj 'bslot))
(defmethod height ((obj canvas)) (slot-value obj 'hslot))
;;; Data level coordinates (for user's satisfaction!!!)
(defmethod x ((obj canvas)) (slot-value obj 'xslot))
(defmethod right ((obj canvas)) (slot-value obj 'rslot))
(defmethod left ((obj canvas)) (slot-value obj 'lslot))
;; (defmethod width ((obj canvas)) (slot-value obj 'wslot))

(defmethod refresh-bcr! ((obj canvas) &key x y l r ((:t top)) b w h)
  "Storing rectangular coordinates"
  ;; X&y first, this order of computation is VERY important:
  ;; e.g. for a stick, (re)computing width happens based on it's r & l etc.
  (when x (setf (slot-value obj 'xslot) (calc-x obj)))
  (when y (setf (slot-value obj 'yslot) (calc-y obj)))  
  ;; LR and then W
  (when l (setf (slot-value obj 'lslot) (calc-left obj)))
  (when r (setf (slot-value obj 'rslot) (calc-right obj)))  
  (when w (setf (slot-value obj 'wslot) (compwidth obj)))
  ;; TB and then H
  (when top (setf (slot-value obj 'tslot) (refresh-top obj)))
  (when b (setf (slot-value obj 'bslot) (refresh-bottom obj)))
  (when h (setf (slot-value obj 'hslot) (refresh-height obj))))


(defmethod initialize-instance :after ((obj canvas) &key)
  (when (toplevelp obj)
    (assert (null (ancestors obj)) () "Toplevel canvas can not have ancestors!")
    (unless (absx obj) (setf (absx obj) +left-margin+))
    (unless (absy obj) (setf (absy obj) +top-margin+)))
  
  )
(defmethod calc-x ((obj canvas))
  ;; UPWARDP T = start from obj itself
  (let* ((absx-tail (delimit-ancestors (append (ancestors obj) (list obj)) #'absx t))
	 ;; This is the one with a sure ABSX!
	 (tailcar (car absx-tail))
	 ;; These all have had ABSX = NIL (empty list if OBJ itslef has ABSX).
	 (tailcdr (cdr absx-tail)))
    (apply #'+ (absx tailcar) (x-offset tailcar) (mapcar #'x-offset tailcdr))))

(defmethod calc-y ((obj canvas))
  (let* ((absy-tail (delimit-ancestors (append (ancestors obj) (list obj)) #'absy t))
	 ;; This is the one with a sure ABSY!
	 (tailcar (car absy-tail))
	 ;; These all have had ABSY = NIL (empty list if OBJ itslef has ABSY).
	 (tailcdr (cdr absy-tail)))
    (apply #'+ (absy tailcar) (y-offset tailcar) (mapcar #'y-offset tailcdr))))

