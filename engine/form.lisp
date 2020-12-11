

;;; Diese Datei hat NIX mit mtype.lisp zu tun,
;;; also kann Sachen daraus sich holen!
;;; In ASDF dies muss NACH mtype.lisp compiliert werden! 
;; (asdf:load-system "smt")
(in-package :smtngn)


(defmacro preproc (var &body cases)
  `(list ,@(loop for x in cases
		 collect `#'(lambda (,var)
			      (when ,(car x) ,@(cdr x))))))



;;; Bruach ich das, kann direkt von canvas erben?
(defclass form (canvas)
  ((preproc :initarg :preproc
     :initform nil
     :accessor form-preproc)   
   (content :initarg :content
	    :initform ()
	    :type list
	    :accessor content)
   ))



(defun formp (obj) (typep obj 'form))



;; (defun unfold-contents-helper (cs-obj acc)
;;   (dolist (child (content cs-obj) acc)
;;     (typecase child
;;       (form (setf acc (unfold-contents-helper child (push child acc))))
;;       (glyph (setf acc (push child acc)))
;;       ;; (burin (setf acc (push child acc)))
;;       )))

;; (defun unfold-contents (obj)
;;   "Discloses all children of the canvas flattened.
;; This is used by sticks to push themselves to their
;; descendant's ANCESTORS list."
;;   (unfold-contents-helper obj ()))

(defun desc (form)
  "Returns form's descendants."
  (append (content form)
	  (mapcan #'(lambda (x) (when (formp x) (desc x)))
		  (content form))))

(defun rdesc (form)
  "Reverses the descendants list."
  (nreverse (desc form)))

(defun preprocess (form)
  (when (form-preproc form)
    (dolist (d (rdesc form))
      (dolist (pp-func (form-preproc form))
	;; Call the preproc func on all descendants
	(funcall pp-func d))))
  (dolist (d (content form))
    (when (formp d) (preprocess d))))

;;; Bei Mtype sind es alle so , vlcht sollte ich diese bei Mtype
;;; alle fixed-nennen????????????
(defmethod fixed-top ((obj form))
  (+ (y obj) (toplvl-scale (bcr-top (get-bcr "clefs.C" (family obj))))))

(defmethod fixed-bottom ((obj form))
  (+ (y obj) (toplvl-scale (bcr-bottom (get-bcr "clefs.C" (family obj))))))

(defmethod fixed-height ((obj form))
  (toplvl-scale (bcr-height (get-bcr "clefs.C" (family obj)))))



(defmethod initialize-instance :after ((obj form) &key)
  (let ((unfolded (rdesc obj)))
    (dolist (desc unfolded)
      ;; Tell all my kids about that im their Parent.
      ;; Don't push if obj already in the parents list,
      ;; otherwise there will be duplicates of obj there!
      ;; Nur ein Stick kann Vorfahren anderer Dinge sein (ein glyph nicht).
      ;; The Farthest of ancestors is in the front of the list.
      (pushnew obj (ancestors desc)))
    ;; At this time every obj has it's full ancestors list
    (when (toplevelp obj)
      ;; (dolist (d (append (remove-if-not #'glyphp (unfold-contents obj))
      ;; 		       (remove-if-not #'formp (unfold-contents obj))))   
      ;;   (refresh-bcr! d :x t :y t :l t :r t :t t :b t :w t :h t))
      ;; Mtype init get-bcr first, da diese unabhängig von Allem sind
      (dolist (g (remove-if-not #'glyphp unfolded))      
	(refresh-bcr! g :x t :y t :l t :r t :t t :b t :w t :h t))
      (dolist (f (remove-if-not #'formp unfolded))
	(refresh-bcr! f :x t :y t :l t :r t :t t :b t :w t :h t))
      (refresh-bcr! obj :x t :y t :l t :r t :t t :b t :w t :h t))))


(defmethod (setf x) (newx (obj form))
  (let ((dx (- newx (x obj))))
    (incf (slot-value obj 'xslot) dx)
    (incf (slot-value obj 'lslot) dx)
    (incf (slot-value obj 'rslot) dx)
    (dolist (d (rdesc obj))
      ;; Treat for children as setfing would be offsetting???????
      (incf (slot-value d 'xslot) dx)
      (incf (slot-value d 'lslot) dx)
      (incf (slot-value d 'rslot) dx)))
  (dolist (anc (reverse (ancestors obj)))
    ;; It is always safe to set the right edge of a form to the rightmost
    ;; of it's children.
    (setf (slot-value anc 'rslot) (calc-right anc)
	  (slot-value anc 'lslot) (calc-left anc)
	  (slot-value anc 'wslot) (calc-width anc)))
  newx)

(defmethod (setf y) (newy (obj form))  
  (let ((dy (- newy (slot-value obj 'yslot))))
    (dolist (d (rdesc obj))
      (incf (slot-value d 'yslot) dy)))
  (setf (slot-value obj 'yslot) newy)
  (dolist (mt (remove-if-not #'glyphp (desc obj)))
    (refresh-bcr! mt :t t :b t))
  (dolist (cs (remove-if-not #'formp (desc obj)))
    (refresh-bcr! cs :t t :b t))
  (refresh-bcr! obj :t t :b t)
  (dolist (anc (reverse (ancestors obj)))    
    (refresh-bcr! anc :t t :b t :h t))
  newy)

;;; Horizontal stuff → Initial
(defmethod calc-left ((obj form))
  ;; This recursion goes down possibly to a containing MTYPE.
  ;; left could have changed to be less than x, so include it in the calc too.
  (apply #'min (x obj) (mapcar #'left (rdesc obj))))

;;; Beware! Changing left doesn't impact right
(defmethod (setf left) (newl (obj form))
  ;; (SETF X) also takes care of re-computing LR
  (incf (x obj) (- newl (slot-value obj 'lslot)))
  newl)

(defmethod calc-right ((obj form))
  (apply #'max (x obj) (mapcar #'right (rdesc obj))))

(defmethod (setf right) (newr (obj form))
  ;; increment by delta-right
  (incf (x obj) (- newr (slot-value obj 'rslot)))
  newr)

(defmethod calc-width ((obj form))
  (- (right obj) (left obj)))

;;; Changing WIDTH moves the RIGHT edge, LEFT remains untouched!
(defmethod (setf width) (neww (obj form))
  ;; So setzen wir gleichzeitig das richtige neue right
  (setf (slot-value obj 'rslot) (+ (left obj) neww)
	(slot-value obj 'wslot) neww)
  ;; Imapcts the width of it's parents,
  ;; start from the direct parent upwards
  (dolist (anc (reverse (ancestors obj)))
    ;; It is always safe to set the right edge of a FORM to the rightmost
    ;; of it's children.
    (setf (slot-value anc 'rslot) (apply #'max (mapcar #'right (rdesc anc)))
	  (slot-value anc 'wslot) (calc-width anc)))
  neww)

;;; Vertical stuff ↓
;;; Design decision: don't compare with TOP, but with FIXED-TOP:
;;; think of a staff system which should always have the same top margin...
(defmethod refresh-top ((obj form))
  (apply #'min (fixed-top obj) (mapcar #'refresh-top (content obj))))

(defmethod (setf top) (newt (obj form))
  (incf (y obj) (- newt (slot-value obj 'tslot)))
  newt)

(defmethod refresh-bottom ((obj form))
  (apply #'max (fixed-bottom obj) (mapcar #'refresh-bottom (content obj))))

(defmethod (setf bottom) (newb (obj form))
  (incf (y obj) (- newb (slot-value obj 'bslot)))
  newb)

(defmethod refresh-height ((obj form))
  (- (bottom obj) (top obj)))

;;; Setfing height pushes the BOTTOM
(defmethod (setf height) (newh (obj form))
  (setf (slot-value obj 'bslot) (+ (top obj) newh)
	(slot-value obj 'hslot) newh)
  (dolist (anc (reverse (ancestors obj)))
    ;; Ancestor's bottom is higher than obj's
    (when (< (bottom anc) (bottom obj))
      (setf (slot-value anc 'bslot) (bottom obj))
      (refresh-bcr! anc :h t)))
  newh)




(defclass stacked-form (form)
  ((domain :initform 'stacked)
   (canvas-color :initform %sform-canvas-color%)))
(defun sformp (obj) (typep obj 'stacked-form))
(defun sform (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'stacked-form initargs))

(defclass horizontal-form (form)
  ((domain :initform 'horizontal)
   (canvas-color :initform %hform-canvas-color%)
  ))
(defun hformp (obj) (typep obj 'horizontal-form))
(defun hform (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'horizontal-form initargs))

(defclass vertical-form (form)
  ((domain :initform 'vertical)
   (canvas-color :initform %vform-canvas-color%)
  ))
(defun vformp (obj) (typep obj 'vertical-form))
(defun vform (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'vertical-form initargs))

(defmethod svgize-bcr ((obj form))
  "Evalutaion time: Rendering"
  (svg:rect (left obj)
	    (top obj)
	    (width obj)
	    (height obj)
	    :sx (x-scaler obj) :sy (y-scaler obj)
	    :id (format nil "~A-bcr" (id obj))
	    :fill (canvas-color obj)
	    :fill-opacity (canvas-opac obj)))

;;; horizontal-form lines up it's direct-children horizontally side by side
;;; Takes place BEFORE applying rules (muss man davon ausgehen, wenn man Rules schreibt)!
(defun hlineup (obj)
  ;; Start off lining up from the innermost child
  (when (formp obj)
    (dolist (d (content obj))
      (hlineup d)))
  ;; The line up the outer most
  (when (hformp obj)
    (loop for a in (butlast (content obj))
	  for b in (cdr (content obj))
	  ;; When I setf left, (setf x) is called which
	  ;; moves XLR
	  do (setf (left b) (right a)))))

(defmethod pack-svglst ((obj form))
  ;; Marker
  (when (marker-vis-p obj)    
    (dolist (elem (svgize-marker obj)) (push elem (svglst obj)))
    ;; (push (svgize-marker obj) (svglst obj))
    (push (xml::comment (format nil "Composing Stick ~A, Marker" (id obj))) (svglst obj)))
  ;; (push (xml::comment (format nil "Composing Stick ~A" (id obj))) (svglst obj))  
  (dolist (d (content obj))
    ;; Children's scales are multiplied by parent's scales
    (psetf (x-scaler d) (* (x-scaler d) (x-scaler obj))
	   (y-scaler d) (* (y-scaler d) (y-scaler obj)))
    (pack-svglst d)
    (setf (svglst obj) (append (svglst obj) (svglst d)))
    )
  ;; BCR Rect
  (when (canvas-vis-p obj)
    (push (svgize-bcr obj) (svglst obj))
    (push (xml::comment (format nil "Composing Stick ~A, BCR" (id obj))) (svglst obj)))  
  )
