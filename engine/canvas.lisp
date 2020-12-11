
;; (asdf:load-system "smt")
(in-package :smtngn)

;;; Keeps track of UPDATES for an object
;; (defstruct ud
;;   left)

(defun cascaded-x-scaler (canvobj self-scaling-p cutoff-pred upwardp)
  (apply #'*
	 ;; This is the internal global calc-x scaling
	 %scale%
	 (if self-scaling-p (x-scaler canvobj) 1)
	 (mapcar #'x-scaler (delimit-ancestors (ancestors canvobj) cutoff-pred upwardp))))

(defun cascaded-y-scaler (canvobj self-scaling-p cutoff-pred upwardp)
  (apply #'*
	 ;; This is the internal global calc-y scaling
	 %scale%
	 (if self-scaling-p (y-scaler canvobj) 1)
	 (mapcar #'y-scaler (delimit-ancestors (ancestors canvobj) cutoff-pred upwardp))))

(defun %x-scale (n canvobj)
  (* n %scale% (x-scaler canvobj)))
(defun %inverse-x-scale (n canvobj)
  (/ n %scale% (x-scaler canvobj)))
(defun y-scale (n canvobj)
  (* n %scale% (y-scaler canvobj)))
;;; Cascaded X scaled
(defun embedding-x-scale (n canvobj &key cutoff-pred self-scaling-p (upwardp t))
  (* n (cascaded-x-scaler canvobj self-scaling-p cutoff-pred upwardp)))
;;; Cascaded X scalings reversed
(defun embedding-inverse-x-scale (n canvobj &key cutoff-pred self-scaling-p (upwardp t))
  "Transforms N (as inputed by user) in such a way that after the system-computations
SVG, N results. Preserves N through the svg-calculations!"
  (/ n (cascaded-x-scaler canvobj self-scaling-p cutoff-pred upwardp)))

(defun embedding-y-scale (n canvobj &key cutoff-pred self-scaling-p (upwardp t))
  (* n (cascaded-y-scaler canvobj self-scaling-p cutoff-pred upwardp)))
(defun embedding-inverse-y-scale (n canvobj &key cutoff-pred self-scaling-p (upwardp t))
  "Transforms N (as inputed by user) in such a way that after the system-computations
SVG, N results. Preserves N through the svg-calculations!"
  (/ n (cascaded-y-scaler canvobj self-scaling-p cutoff-pred upwardp)))



;;; Chase ist der Rahmen, also das Canvas einfach.
(defclass canvas (smtobj)
  ((rotate-degrees :initarg :rotate-degrees
		   :initform 0
		   :accessor chase-rotate-degrees)
   
   ;; Move this to a FORM
   (family :initarg :family
	   :initform %font-family%
	   :accessor family
	   :documentation "Wenn man den Font vom
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
(defmethod y ((obj canvas)) (slot-value obj 'yslot))
(defmethod top ((obj canvas)) (slot-value obj 'tslot))
(defmethod bottom ((obj canvas)) (slot-value obj 'bslot))
(defmethod height ((obj canvas)) (slot-value obj 'hslot))
;;; Data level coordinates (for user's satisfaction!!!)
(defmethod x ((obj canvas)) (slot-value obj 'xslot))
(defmethod right ((obj canvas)) (slot-value obj 'rslot))
(defmethod left ((obj canvas)) (slot-value obj 'lslot))
(defmethod width ((obj canvas)) (slot-value obj 'wslot))

(defmethod refresh-bcr! ((obj canvas) &key x y l r ((:t top) nil) b w h)
  "Storing rectangular coordinates"
  ;; X&y first, this order of computation is VERY important:
  ;; e.g. for a stick, (re)computing width happens based on it's r & l etc.
  (when x (setf (slot-value obj 'xslot) (calc-x obj)))
  (when y (setf (slot-value obj 'yslot) (calc-y obj)))  
  ;; LR and then W
  (when l (setf (slot-value obj 'lslot) (calc-leftmost obj)))
  (when r (setf (slot-value obj 'rslot) (calc-rightmost obj)))  
  (when w (setf (slot-value obj 'wslot) (calc-width obj)))  
  ;; TB and then H
  (when top (setf (slot-value obj 'tslot) (refresh-top obj)))
  (when b (setf (slot-value obj 'bslot) (refresh-bottom obj)))
  (when h (setf (slot-value obj 'hslot) (refresh-height obj))))


(defmethod initialize-instance :after ((obj canvas) &key)
  (when (toplevelp obj)
    (assert (null (ancestors obj)) () "Toplevel canvas can not have ancestors!")
    (unless (absx obj) (setf (absx obj) +left-margin+))
    (unless (absy obj) (setf (absy obj) +top-margin+)))
  ;; Allow these slot-readers to be used as rulers for this object
  
  )




(defmethod calc-x ((obj canvas))
  ;; UPWARDP T = start from obj itself
  (let* ((absx-tail (delimit-ancestors (append (ancestors obj) (list obj)) #'absx t))
	 ;; This is the one with a sure ABSX!
	 (tailcar (car absx-tail))
	 ;; These all have had ABSX = NIL (empty list if OBJ itslef has ABSX).
	 (tailcdr (cdr absx-tail)))
    (apply #'+ (absx tailcar) (x-offset tailcar) (mapcar #'x-offset tailcdr))))
;;; Avoid recursion here, it will have double setfing effects





;; (defmethod reprx ((obj canvas))
;;   "Expresses x in terms of the (X) function"
;;   (let* ((absx-tail (delimit-ancestors (ancestors obj) #'absx t))
;; 	 ;; This is the one with absx
;; 	 (tailcar (car absx-tail))
;; 	 ;; These all have had absx = NIL
;; 	 (tailcdr (cdr absx-tail)))
;;     (embedding-inverse-x-scale (if (absx obj)
;; 				   (- (rx obj) ;;=> (+ (absx obj) (x-offset obj))
;; 				      (if absx-tail
;; 					  (apply #'+ (absx tailcar) (x-offset tailcar) (mapcar #'x-offset tailcdr))
;; 					  0))
;; 				   ;; Drucke x-offset aus, in terms of (X)
;; 				   (- (rx obj)
;; 				      (apply #'+ (absx tailcar) (x-offset tailcar) (mapcar #'x-offset tailcdr)))
;; )
;; 			       obj :cutoff-pred #'identity :upwardp nil))
;;   )

(defmethod calc-y ((obj canvas))
  (let* ((absy-tail (delimit-ancestors (append (ancestors obj) (list obj)) #'absy t))
	 ;; This is the one with a sure ABSY!
	 (tailcar (car absy-tail))
	 ;; These all have had ABSY = NIL (empty list if OBJ itslef has ABSY).
	 (tailcdr (cdr absy-tail)))
    (apply #'+ (absy tailcar) (y-offset tailcar) (mapcar #'y-offset tailcdr))))


;; (defmethod repry ((obj canvas))
;;   "Expresses x in terms of the (X) function"
;;   (let* ((absy-tail (delimit-ancestors (ancestors obj) #'absy t))
;; 	 ;; This is the one with absx
;; 	 (tailcar (car absy-tail))
;; 	 ;; These all have had absx = NIL
;; 	 (tailcdr (cdr absy-tail)))
;;     (embedding-inverse-y-scale (if (absy obj)
;; 				   (- (y obj) ;;=> (+ (absx obj) (x-offset obj))
;; 				      (if absy-tail
;; 					  (apply #'+ (absy tailcar) (y-offset tailcar) (mapcar #'y-offset tailcdr))
;; 					  0))
;; 				   ;; Drucke x-offset aus, in terms of (X)
;; 				   (- (y obj) (apply #'+ (absy tailcar) (y-offset tailcar) (mapcar #'y-offset tailcdr))))
;; 			       obj :cutoff-pred #'identity :upwardp nil))
;;   )

;; (defmethod ry ((obj canvas))
;;   (if (absy obj)
;;       ;; When abs exists, all xs before should be invalidated
;;       (let* ((absy-tail (delimit-ancestors (ancestors obj) #'absy t))
;; 	     ;; This is the one with absy
;; 	     (tailcar (car absy-tail))
;; 	     ;; These all have had absy = NIL
;; 	     (tailcdr (cdr absy-tail)))
;; 	(embedding-inverse-y-scale (- (+ (absy obj) (y-offset obj))
;; 				      (if absy-tail
;; 					  (apply #'+
;; 						 (absy tailcar) (y-offset tailcar)
;; 						 (mapcar #'y-offset tailcdr))
;; 					  0))
;; 				   obj :cutoff-pred #'identity :upwardp nil))
;;       ;; Just retain the correct amount of x-offset      
;;       (embedding-inverse-y-scale (y-offset obj) obj :cutoff-pred #'identity :upwardp nil)
;;       ))


(defun observe-hedge (val obj)
  "Computes the representational value of n from the standpoint of obj."
  ;; Find the nearest parent with an absx
  (let* ((absx-tail (delimit-ancestors (append (ancestors obj) (list obj)) #'absx t))
	 ;; This is the one with absx
	 (tailcar (car absx-tail))
	 ;; These all have had absx = NIL
	 (tailcdr (cdr absx-tail)))
    (embedding-inverse-x-scale (- val (apply #'+ (absx tailcar) (x-offset tailcar)
					     (mapcar #'x-offset tailcdr)))
			       obj
			       :cutoff-pred #'identity
			       :upwardp nil
			       :self-scaling-p t)))
(defun observe-vedge (val obj)
  "Computes the representational value of n from the standpoint of obj."
  ;; Find the nearest parent with an absx
  (let* ((absy-tail (delimit-ancestors (append (ancestors obj) (list obj)) #'absy t))
	 ;; This is the one with absy
	 (tailcar (car absy-tail))
	 ;; These all have had absy = NIL
	 (tailcdr (cdr absy-tail)))
    (embedding-inverse-y-scale (- val (apply #'+ (absy tailcar) (y-offset tailcar)
					     (mapcar #'y-offset tailcdr)))
			       obj
			       :cutoff-pred #'identity
			       :upwardp nil
			       :self-scaling-p t)))





(defun inspbcr (obj)
  (let ((o obj))
    (format t "~&>>> [~A]~% x:~D y:~D ~% t:~D ft:~D ~% l:~D r:~D ~% b:~D fb:~D ~% h:~D fh:~D ~% w:~D"
	    (id o)
	    (x o) (y o)
	    (top o) (when (formp o) (fixed-top o))
	    (left o) (right o)
	    (bottom o) (when (formp o) (fixed-bottom o))
	    (height o) (when (formp o) (fixed-height o))
	    (width o)
	    )))

