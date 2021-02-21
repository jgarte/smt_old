

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
  ((right-side-space :accessor right-side-space
		     :initform 0
		     :initarg :right-side-space
		     :documentation "Vereinfacht das Punktieren
massiv, das ist der freie Platz nach einem Canvasobject. Ich brauchte
eine extra Einheit damit ich während der Rule-definitionen nicht das
Width direkt anfassen muss...")
   (preproc :initarg :preproc
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


(defun enumerate-generations (obj n)
  ""
  (if (or (mcharp obj) (null (content obj)))
      ()
      ;; OBJ is a form with children
      (mapcan #'(lambda (k) (cons (cons n k) (enumerate-generations k (1+ n))))
	      ;; Content beinhaltet immer nur die erste Generation der Kinder,
	      ;; (die Oberfläche des Nachwuchses)
	      (content obj))
      ))

(defun generation-children-lists (form lastgen-first)
  "Returns a list of (0 CHILD01 CHILD02 ...) (1 CHILD11 CHILD12 ...) 
with 0, 1, ... being the number of generation. LASTGEN-FIRST=T sorts
the farthest & youngest of children to appear first in the list,
LASTGEN-FIRST=NIL the eldest and next one. So it is safe to e.g. MAPCAN
CDRs to get children listed in the desired order specified by lastgEN-first."
  (let* ((enums (enumerate-generations form 0))
	 generations)
    (when enums	  ;Proceed only if FORM has content & not e.g. a Note object,
      ;; a problematic case would be this function used in lining-ups (eg in RENDER), where
      ;; it could be applied on a Note (which is a also form) before rules are applied on the Note,
      ;; which fill out it's content, where this causes an error (because of the next line APPLY MAX
      ;; on NIL)!
      (dotimes (i (1+ (apply #'max (mapcar #'car enums)))
		  (sort generations (if lastgen-first #'> #'<)
			:key #'car))
	(push (cons i (mapcar #'cdr
			      (remove-if-not
			       #'(lambda (gen) (= (car gen) i))
			       enums)))
	      generations)))))

(defun children (form &optional (lastgen-first t))
  "Returns the list of children ordered based on LASTGEN-FIRST 
,see GENERATION-CHILDREN-LISTS)"
  (mapcan #'cdr (generation-children-lists form lastgen-first)))

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
	      (bbox-top (glyph-bbox (get-glyph *staff-height-reference-glyph* (font obj))))
	      ;; (getf (get-glyph-bbox *staff-height-reference-glyph*) :top)
	      ;; (bcr-top
	      ;; 		    (get-glyph-bbox "uniE05C")
	      ;; 		    ;; (get-bcr "clefs.C" (family obj))
	      ;; 		    )
	      )))

(defmethod fixed-bottom ((obj form))
  ;; "uniE05C" is bravura alto, should find a solution to
  ;; find out the alto clef of a font!!!
  (+ (y obj) (toplvl-scale
	      (bbox-bottom (glyph-bbox (get-glyph *staff-height-reference-glyph* (font obj))))
	      ;; (getf (get-glyph-bbox *staff-height-reference-glyph*) :bottom)
	      ;; (bcr-bottom (get-glyph-bbox "uniE05C")
	      ;; 		    ;; (get-bcr "clefs.C" (family obj))
	      ;; 		    )
	      )))

(defmethod fixed-height ((obj form))
  ;; This is bravura alto clef
  (toplvl-scale
   (bbox-height (glyph-bbox (get-glyph *staff-height-reference-glyph* (font obj))))
   ;; (getf (get-glyph-bbox *staff-height-reference-glyph*) :height)
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
	  (dolist (new-grandchild (children direct-new-child))
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
	    (dolist (new-grandchild (children direct-new-child))
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
  (dolist (child (children obj))
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
    ;; Use sequential-SETF, for (COMPWIDTH obj) depends on (RIGHT obj) & (LEFT obj)
    (setf (slot-value anc 'rslot) (compute-right anc)
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
	 (x obj)			;OBJ's own x is the leftmost?
	 ;; Then since OBJ only functions as a container for other stuff it
	 ;; takes on the leftmost side of it's content.
	 (mapcar #'left (reverse (descendants obj)))))


(defmethod (setf left) (newl (obj form))
  ;; (SETF X) also takes care of re-computing LR
  (incf (x obj) (- newl (slot-value obj 'lslot)))
  newl)

(defmethod compute-right ((obj form))
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
  (or (user-width obj) (slot-value obj 'wslot)))

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
(defun make-sform (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'stacked-form initargs))

(defclass horizontal-form (form)
  ((domain :initform 'horizontal)
   (canvas-color :initform %hform-canvas-color%)))
(defun hformp (obj) (typep obj 'horizontal-form))
(defun make-hform (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'horizontal-form initargs))

(defclass vertical-form (form)
  ((domain :initform 'vertical)
   (canvas-color :initform %vform-canvas-color%)
  ))
(defun vformp (obj) (typep obj 'vertical-form))
(defun make-vform (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'vertical-form initargs))

(defmethod bounding-box-rect ((obj form))
  (svgrect (left obj) (top obj) (width obj) (height obj)
	   "fill" (canvas-color obj)
	   "fill-opacity" (canvas-opac obj)))



;;; horizontal-form lines up it's direct-children horizontally side by side
;;; Takes place BEFORE applying rules (muss man davon ausgehen, wenn man Rules schreibt)!
(defun hlineup (obj)

  (when (formp obj)
    (dolist (d (content obj))
      (hlineup d)))
  ;; The line up the outer most
  (when (and (hformp obj) )
    (loop for a in (butlast (content obj))
	  for b in (cdr (content obj))
	  ;; When I setf left, (setf x) is called which
	  ;; moves XLR
	  do (setf (left b) (right a)))))

;;; Call this once on the toplevel form to recursively line-up it's content
(defun nlineup (form)
  "Lines up all form's children horizontally or vertically. This
applies also to the Stacked Forms, they might also have
horizontal/vertical children which should be lined up.  Without this
all the content would get the same starting horizontal/vertical
coordinates as the FORM itself."
  ;; Start lining up from within!
  (dolist (child (remove-if-not #'formp (children form)))
    (nlineup child))
  (typecase form
    (horizontal-form
     (let ((starting-left (left form)))
       ;; The first direct child takes on the left-coord of the parent form and it's own
       ;; right coord, so we start lining up from the second direct child.
       (dotimes (i (list-length (cdr (content form))))
	 ;; For the rest of direct children we compute their difference from
	 ;; the starting left coordinate, then add that diff to the right-coord
	 ;; of the last sibling.
	 ;; (print (content (nth i (cdr (content form)))))
	 (setf (left (nth i (cdr (content form))))
	       (+ (- (left (nth i (cdr (content form)))) starting-left)
		  (right (nth i (content form))))))))))



(defmethod n-layer-svg-list ((obj form))
  (let ((formtype (typecase obj
		    (stacked-form "Stacked")
		    (horizontal-form "Horizontal")
		    (vertical-form "Vertical"))))
    ;; Origin
    (when (origin-visible-p obj)    
      (dolist (elem (origin-cross-elements obj))
	(push elem (svg-list obj)))
      (push (svgcomment (format nil "~AForm ~A Origin" formtype (id obj)))
	    (svg-list obj)))
    ;; Content
    (dolist (direct-child (content obj))
      (psetf (x-scaler direct-child) (* (x-scaler direct-child)
					(x-scaler obj))
	     (y-scaler direct-child) (* (y-scaler direct-child)
					(y-scaler obj)))
      (n-layer-svg-list direct-child)
      (setf (svg-list obj) (append (svg-list obj) (svg-list direct-child))))
    ;; Canvas
    (when (canvas-vis-p obj)
      (push (bounding-box-rect obj) (svg-list obj))
      (push (svgcomment (format nil "~AForm ~A BBox" formtype (id obj))) (svg-list obj))))
  )
