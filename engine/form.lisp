

;;; Diese Datei hat NIX mit mtype.lisp zu tun,
;;; also kann Sachen daraus sich holen!
;;; In ASDF dies muss NACH mtype.lisp compiliert werden! 
;; (asdf:load-system "smt")
(in-package :smt-engine)


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
   (user-width :accessor user-width
	       :initform nil
	       :documentation "This is never touched and will
be the value of WIDTH if non-nil."
	       :initarg :width)
   ))

(defun formp (obj) (typep obj 'form))

;;; Ersetzen wenn verstanden was mit den descendants Alles passiert
(defun descendants0 (x)
  (if (or (mcharp x) (null (content x)))
      ()
      (mapcan #'(lambda (a) (cons a (f a))) (content x))
      ))


;;; Children
(defun descendants (form)
  "Returns form's descendants."
  (append (content form)
	  (mapcan #'(lambda (x)
		      (when (formp x) (descendants x)))
		  (content form))))

;; (defun reversed-descendants (form)
;;   "Reverses the descendants list."
;;   (nreverse (descendants form)))

(defun preprocess (form)
  (when (form-preproc form)
    (dolist (d (reverse (descendants form)))
      (dolist (pp-func (form-preproc form))
	;; Call the preproc func on all descendants
	(funcall pp-func d))))
  (dolist (d (content form))
    (when (formp d) (preprocess d))))

;;; Bei Mtype sind es alle so , vlcht sollte ich diese bei Mtype
;;; alle fixed-nennen????????????
(defmethod fixed-top ((obj form))
  (+ (y obj) (toplvl-scale
	      ;; Which Font?????
	      (bbox-top (glyph-bbox (get-glyph *vertical-space-reference-glyph* (font obj))))
	      ;; (getf (get-glyph-bbox *vertical-space-reference-glyph*) :top)
	      ;; (bcr-top
	      ;; 		    (get-glyph-bbox "uniE05C")
	      ;; 		    ;; (get-bcr "clefs.C" (family obj))
	      ;; 		    )
	      )))

(defmethod fixed-bottom ((obj form))
  ;; "uniE05C" is bravura alto, should find a solution to
  ;; find out the alto clef of a font!!!
  (+ (y obj) (toplvl-scale
	      (bbox-bottom (glyph-bbox (get-glyph *vertical-space-reference-glyph* (font obj))))
	      ;; (getf (get-glyph-bbox *vertical-space-reference-glyph*) :bottom)
	      ;; (bcr-bottom (get-glyph-bbox "uniE05C")
	      ;; 		    ;; (get-bcr "clefs.C" (family obj))
	      ;; 		    )
	      )))

(defmethod fixed-height ((obj form))
  ;; This is bravura alto clef
  (toplvl-scale
   (bbox-height (glyph-bbox (get-glyph *vertical-space-reference-glyph* (font obj))))
   ;; (getf (get-glyph-bbox *vertical-space-reference-glyph*) :height)
   ;; (bcr-height (get-glyph-bbox "uniE05C")
		;; 	    ;; (get-bcr "clefs.C" (family obj))
		;; 	    )
		))



(defmethod initialize-instance :after ((obj form) &key)
  (let ((desc (reverse (descendants obj))))
    (dolist (d desc)
      ;; is pushnew safe for this???
      (pushnew obj (ancestors d)))
    ;; At this time every obj has it's full ancestors list
    (when (toplevelp obj)
      ;; Mtype init get-bcr first, da diese unabhängig von Allem sind
      (dolist (g (remove-if-not #'mcharp desc))      
	(refresh-bcr! g :x t :y t :l t :r t :t t :b t :w t :h t))
      (dolist (f (remove-if-not #'formp desc))
	(refresh-bcr! f :x t :y t :l t :r t :t t :b t :w t :h t))
      (refresh-bcr! obj :x t :y t :l t :r t :t t :b t :w t :h t))))

;;; Nehmen na nur wurde gepushed->Anfang der liste
(defmethod (setf content) (new-content (obj form))
  "Establishes new parental relations to possibly newly added
new objects"
  (if new-content
      ;; The old content is modified and/or new stuff is added.
      ;; Add parent-child relations only to NEWLY added items
      (dolist (direct-new-child (set-difference new-content (content obj) :test #'smteq))
	;; OBJ is the first & nearest parent, so PUSH first
	(pushnew obj (ancestors direct-new-child))
	(when (formp direct-new-child)
	  (dolist (new-grandchild (mapcan #'cdr (children direct-new-child)))
	    (pushnew obj (ancestors new-grandchild))
	    ;; I suppose that parent-child dependency btwn. new
	    ;; items is already stablished by them themselves, and
	    ;; hence don't go into stablishing parental relations
	    ;; between them.
	    ))
	;; Now add all parents (starting from the next up to top, hence REVERSE)
	;; to parent lists of new items (old ones have these already!)
	(dolist (parent (reverse (ancestors obj)))
	  (pushnew parent (ancestors direct-new-child))
	  (when (formp direct-new-child)
	    (dolist (new-grandchild (mapcan #'cdr (children direct-new-child)))
	      (pushnew parent (ancestors new-grandchild)))))
	)      
      ;; Content has been removed all in all,
      ;; withdraw parent-child relations?
      nil
      )
  ;; ;;; this part should run also when content has been emptied; ie.
  ;; dimensions must be recomputed.
  ;; First set obj's new content, to have
  ;; valid updated ANCESTOR lists below.
  (setf (slot-value obj 'content) new-content)
  (dolist (child (mapcan #'cdr (children obj)))
    ;; Actually need only do this on new items??
    (refresh-bcr! child :x t :y t :l t :r t :t t :b t :w t :h t))
  (refresh-bcr! obj :x t :y t :l t :r t :t t :b t :w t :h t)
  (dolist (anc (reverse (ancestors obj)))
    ;; It is always safe to set the right edge of a form to the rightmost
    ;; of it's children.
    ;; Use SETF, for (COMPWIDTH obj) depends on (RIGHT obj) & (LEFT obj)
    (refresh-bcr! anc :x t :y t :l t :r t :t t :b t :w t :h t)
    )
  ;; Just keeping to the semantics of SETF; return NEW-CONTENT
  new-content)

(defmethod (setf x) (newx (obj form))
  (let ((dx (- newx (x obj))))
    (incf (slot-value obj 'xslot) dx)
    (incf (slot-value obj 'lslot) dx)
    (incf (slot-value obj 'rslot) dx)
    (dolist (d (reverse (descendants obj)))
      ;; Treat for children as setfing would be offsetting???????
      (incf (slot-value d 'xslot) dx)
      (incf (slot-value d 'lslot) dx)
      (incf (slot-value d 'rslot) dx)))
  (dolist (anc (reverse (ancestors obj)))
    ;; It is always safe to set the right edge of a form to the rightmost
    ;; of it's children.
    ;; Use SETF, for (COMPWIDTH obj) depends on (RIGHT obj) & (LEFT obj)
    (setf (slot-value anc 'rslot) (calc-right anc)
	  (slot-value anc 'lslot) (calc-left anc)
	  (slot-value anc 'wslot) (compwidth anc)
	  )
    )
  newx)


(defmethod (setf y) (newy (obj form))  
  (let ((dy (- newy (slot-value obj 'yslot))))
    (dolist (d (reverse (descendants obj)))
      (incf (slot-value d 'yslot) dy)))
  (setf (slot-value obj 'yslot) newy)
  (dolist (mt (remove-if-not #'mcharp (descendants obj)))
    (refresh-bcr! mt :t t :b t))
  (dolist (cs (remove-if-not #'formp (descendants obj)))
    (refresh-bcr! cs :t t :b t))
  (refresh-bcr! obj :t t :b t)
  (dolist (anc (reverse (ancestors obj)))    
    (refresh-bcr! anc :t t :b t :h t))
  newy)

;;; Horizontal stuff → Initial
(defmethod calc-left ((obj form))
  ;; Don't need reversing here!!!
  (apply #'min
	 (x obj)
	 (mapcar #'left (reverse (descendants obj)))))

;;; Beware! Changing left doesn't impact right
(defmethod (setf left) (newl (obj form))
  ;; (SETF X) also takes care of re-computing LR
  (incf (x obj) (- newl (slot-value obj 'lslot)))
  newl)

(defmethod calc-right ((obj form))
  (apply #'max
	 (x obj)
	 ;; (mapcar #'right (reverse (descendants obj)))
	 (mapcar #'right (descendants obj))))

(defmethod (setf right) (newr (obj form))
  ;; increment by delta-right
  (incf (x obj) (- newr (slot-value obj 'rslot)))
  newr)


(defmethod width ((obj form))
  "only This is in the public interface for getting width,
it takes care of chooising btwn userinputed width or system 
computed width."
  (or (user-width obj)
      (slot-value obj 'wslot)))
(defmethod compwidth ((obj form))
  ;; This subtraction will cause Floating Nightmare! :-S
  ;; #'right #'left are slot-value readers.
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
    (setf (slot-value anc 'rslot)
	  (apply #'max (mapcar #'right (reverse (descendants anc))))
	  (slot-value anc 'wslot)
	  (compwidth anc)))
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
   (canvas-color :initform %hform-canvas-color%)))
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
  (when (origin-visible-p obj)    
    (dolist (elem (svgize-origin obj)) (push elem (svglst obj)))
    ;; (push (svgize-origin obj) (svglst obj))
    (push (xml-base::comment (format nil "Composing Stick ~A, Origin Point" (id obj))) (svglst obj)))
  ;; (push (xml-base::comment (format nil "Composing Stick ~A" (id obj))) (svglst obj))  
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
    (push (xml-base::comment (format nil "Composing Stick ~A, BCR" (id obj))) (svglst obj)))  
  )
