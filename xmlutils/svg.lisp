
(in-package #:svg)



;;; SVG colors
;;; https://www.december.com/html/spec/colorsvg.html
(defparameter *colors*
  '("aliceblue" "antiquewhite" "aqua(Safe 16 Hex3)" "aquamarine"
    "azure" "beige" "bisque" "black(Safe 16 Hex3)" "blanchedalmond"
    "blue(Safe 16 Hex3)" "blueviolet" "brown" "burlywood" "cadetblue"
    "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk" "crimson"))
					
#|	
cyan(Safe 16=aqua Hex3)	
 	darkblue		darkcyan		darkgoldenrod	
darkgray	
 	darkgreen		darkgrey		darkkhaki	
darkmagenta	
 	darkolivegreen		darkorange		darkorchid	
darkred	
 	darksalmon		darkseagreen		darkslateblue	
darkslategray	
 	darkslategrey		darkturquoise		darkviolet	
deeppink	
 	deepskyblue		dimgray		dimgrey	
dodgerblue	
 	firebrick		floralwhite		forestgreen	
fuchsia(Safe 16 Hex3)	
 	gainsboro		ghostwhite		gold	
goldenrod	
 	gray(16)		green(16)		greenyellow	
grey(16)	
 	honeydew		hotpink		indianred	
indigo	
 	ivory		khaki		lavender	
lavenderblush	
 	lawngreen		lemonchiffon		lightblue	
lightcoral	
 	lightcyan		lightgoldenrodyellow		lightgray	
lightgreen	
 	lightgrey		lightpink		lightsalmon	
lightseagreen	
 	lightskyblue		lightslategray(Hex3)		lightslategrey(Hex3)	
lightsteelblue	
 	lightyellow		lime(Safe 16 Hex3)		limegreen	
linen	
 	magenta(Safe 16=fuchsia Hex3)		maroon(16)		mediumaquamarine	
mediumblue	
 	mediumorchid		mediumpurple		mediumseagreen	
mediumslateblue	
 	mediumspringgreen		mediumturquoise		mediumvioletred	
midnightblue	
 	mintcream		mistyrose		moccasin	
navajowhite	
 	navy(16)		oldlace		olive(16)	
olivedrab	
 	orange		orangered		orchid	
palegoldenrod	
 	palegreen		paleturquoise		palevioletred	
papayawhip	
 	peachpuff		peru		pink	
plum	
 	powderblue		purple(16)		red(Safe 16 Hex3)	
rosybrown	
 	royalblue		saddlebrown		salmon	
sandybrown	
 	seagreen		seashell		sienna	
silver(16)	
 	skyblue		slateblue		slategray	
slategrey	
 	snow		springgreen		steelblue	
tan	
 	teal(16)		thistle		tomato	
turquoise	
 	violet		wheat		white(Safe 16 Hex3)	
whitesmoke	
 	yellow(Safe 16 Hex3)		yellowgreen

|#

(defparameter *decimal-attributes*
  '("x" "y" "width" "height" "x1" "x2" "y1" "y2")
  "These attributes must be non-rational numbers")

(defun ensure-decimal (attr-name attr-value)
  (if (and (member attr-name *decimal-attributes* :test #'string=)
	   (not (typep attr-value '(or float integer))))
      (coerce attr-value 'float)
      attr-value))

;;; Provide floating points
(defun translate (tx &optional (ty tx))
  (format nil "translate(~D ~D)" (ensure-decimal "x" tx) (ensure-decimal "y" ty)))
;; (defun translate (tx &optional (ty tx))
;;   `("translate" . ,(list (ensure-decimal "x" tx) (ensure-decimal "y" ty))))
(defun scale (sx &optional (sy sx))
  "Scale vertically(x) and/or horizonally(y)."
  (format nil "scale(~D ~D)" (ensure-decimal "x" sx) (ensure-decimal "y" sy)))
;; (defun scale (sx &optional (sy sx))
;;   "Scale vertically(x) and/or horizonally(y)."
;;   `("scale" . ,(list (ensure-decimal "x" sx) (ensure-decimal "y" sy))))
(defun rotate (a &optional (x 0) (y 0))
  (format nil "rotate(~D ~D ~D)" a x y))
;;; Always put translate first!
(defun transform (&rest transformations)
  (format nil "~{~A~^ ~}" transformations))


;;; Gilt als allgemeiner???
(defparameter *optional-attributes*
  '(:fill :fill-opacity
    :stroke :stroke-width :stroke-linecap :STROKE-OPACITY
    :stroke-dasharray
    :tx :ty :sx :sy :transform
    :id))

;;; Since downcase-sym-name is needed only for defshape,
;;; make it available only at compilation time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun downcase-sym-name (sym)
    (string-downcase (symbol-name sym)))  
  (defun assert-registered-optional-attribute (attribute)
    (assert (member attribute *optional-attributes*) (attribute)
	    "Invalid optional attribute ~A." attribute)))



(defmacro defshape (name &rest obligatory-attributes)
  "Defines a basic shape with it's obligatory & optional attributes."
  `(defun ,name (,@obligatory-attributes &rest optional-attributes &key &allow-other-keys)
     (xmlbase::make-element ,(downcase-sym-name name)
			:attrs (append
				;; Obligatory attributes
				(list ,@(loop :for s :in obligatory-attributes
					      ;; Save attribute name as lower case string.
					      :for sname = (downcase-sym-name s)
					      :collect `(list* ,sname
							       (ensure-decimal ,sname ,s))))
				;; Optional attributes
				(loop :for (attr val) :on optional-attributes :by #'cddr
				      :for sname = (downcase-sym-name attr)
				      :do (assert-registered-optional-attribute attr)
				      :collect (list* sname (ensure-decimal sname val)))))))

(defun replace-with-transform (element)
  (let* ((transformations (remove-if-not
			   #'(lambda (av) ;attribute & value
			       (member (subseq (car av) 0 2)
				       '("tx" "ty" "sx" "sy")
				       :test #'string=))
			   (remove-if #'(lambda (av) (< (length (car av)) 2))
				      (xmlbase::elmattrs element)))))
    (when transformations
      (let* ((biggest-idx (values
			   (read-from-string
			    (subseq (caar (sort transformations #'>
						:key #'(lambda (av)
							 (values
							  (read-from-string (subseq (car av) 2) nil 0)))))
				    2)
			    nil 0)))
	     (indexed (loop :for i :to biggest-idx
			    :collect (list i
					   (remove-if-not
					    #'(lambda (av) (= i (values (read-from-string (subseq (car av) 2) nil 0))))
					    transformations)))))
	;; rm
	(setf (xmlbase::elmattrs element) (set-difference (xmlbase::elmattrs element) transformations))
	;; add one transfORM to attrs lst
	(push (cons "transform"
		    (let ((s ""))
		      (dolist (l indexed s) ;fängt mit 0 an
			(let ((ts (remove-if-not #'(lambda (av) (string= "t" (car av) :end2 1)) (cadr l)))
			      (ss (remove-if-not #'(lambda (av) (string= "s" (car av) :end2 1)) (cadr l))))
			  (setf s (concatenate 'string s (format nil "translate(~D ~D) " (cdr (first ts)) (cdr (second ts)))))
			  (setf s (concatenate 'string s (format nil "scale(~D ~D)" (cdr (first ss)) (cdr (second ss)))))))))
	      (xmlbase::elmattrs element))))))

(defun extract-transformations (element)
  (let ((transformations (remove-if-not
			  #'(lambda (av) ;attribute & value
			      (member (subseq (car av) 0 2)
				      '("tx" "ty" "sx" "sy")
				      :test #'string=))
			  (remove-if #'(lambda (av) (< (length (car av)) 2))
				     (xmlbase::elmattrs element)))))
    (when transformations
      (let ((biggest-idx (values
			  (read-from-string
			   (subseq (caar (sort transformations #'>
					       :key #'(lambda (av)
							(values
							 (read-from-string (subseq (car av) 2) nil 0)))))
				   2)
			   nil 0))))
	(values
	 transformations
	 ;; indexed transformations from 0th upwards, e.g. (0 (("tx" . 5177.6094) ("ty" . 1524.9124) ("sx" . 1) ("sy" . 1)))
	 (loop :for i :to biggest-idx
	       :collect (list i
			      (remove-if-not
			       #'(lambda (av) (= i (values (read-from-string (subseq (car av) 2) nil 0))))
			       transformations))))
	))))


;;; Define basic shapes svg.lisp cl-svg
(defshape rect x y width height)
(defshape line x1 y1 x2 y2)
(defshape path d)
(defshape circle cx cy r)
(defshape ellipse cx cy rx ry)

;;; Group
(defun g (&key attributes content)
  (xmlbase::make-element "g"
		     :attrs attributes
		     :cnt content))

(defparameter *svg-ns* "http://www.w3.org/2000/svg")
(defparameter *xlink-ns* "http://www.w3.org/1999/xlink")

(defun write-svg (content &key
			    width height
			    (path #P"/tmp/smt.svg")
			    (ind-offset 0)
			    (ind-depth 1)
			    (ind-type 'space))
  "Writes the content to the specified path."
  (xmlbase::write-to-path (xmlbase::make-element "svg"
						 :attrs
						 `(("xmlns" . ,*svg-ns*)
						   ("xmlns:xlink" . ,*xlink-ns*)
						   ("width" . ,width)
						   ("height" . ,height))
						 :cnt content)
			  :path path
			  :ind-offset ind-offset
			  :ind-depth ind-depth
			  :ind-type ind-type))

;;; Path string parser
(defparameter *path-cmd-regex* "[A-Za-z][0-9 -.]*")
(defun parse-path-d (d)
  "Returns a list of path commands parsed from the path data d,
where the car is the command name and the rest are coordinates."
  (let ((d-len (length d)))
    (labels ((ppd (parsed start)
	       (multiple-value-bind (start end)
		   (cl-ppcre:scan *path-cmd-regex* d :start start)
		 (if (null end)
		     (nreverse parsed)
		     (ppd (cons (cl-ppcre:scan-to-strings *path-cmd-regex* d :start start) parsed) end)
		     )))
	     (group-cmds (parsed)
	       (loop for cmd in parsed
		     collect (cons (subseq cmd 0 1)
				   (mapcar #'read-from-string
					   (cl-ppcre:split "[\n ]" (subseq cmd 1)))))))
      (group-cmds (ppd () 0)))))

(defun name->keyword (name)
  (values (intern (string-upcase name) :keyword)))

(defun cmd-head (cmd) (car cmd))
(defun cmd-body (cmd) (cdr cmd))
(defun cmd-rel-p (command) (getf command :relp))
(defun cmd-name (command) (getf command :name))
(defun cmd-coords (command) (getf command :coords))
(defun endx (cmd) (getf (cmd-coords cmd) :xn))
(defun endy (cmd) (getf (cmd-coords cmd) :yn))

#| Draw a line from the current point to the given (x,y) coordinate
which becomes the new current point. L (uppercase) indicates that
absolute coordinates will follow; l (lowercase) indicates that
relative coordinates will follow. A number of coordinates pairs may be
specified to draw a polyline. At the end of the command, the new
current point is set to the final set of coordinates provided. |#
(defun lineto (cmd)
  (let* ((head (cmd-head cmd))
	 (body (cmd-body cmd))
	 (body-len (list-length body)))
   (assert (>= body-len 2) (cmd) "Lineto command should have at least 1 pair of coordinates.")
   (assert (evenp body-len) (cmd) "Lineto command should have an even number pairs of coordinates.")
   (list :name :l
	 :relp (if (string= head "l") t nil)
	 :coords
	 (append 
	  (loop for xy on (butlast body 2) by #'cddr
		;; Keep the zero idx for making absolute coord later.
		for idx :from 1
		nconc (list (name->keyword (format nil "x~D" idx)) (first xy)
			    (name->keyword (format nil "y~D" idx)) (second xy)))
	  (list :xn (first (last body 2))
		:yn (second (last body 2)))))))

(defun broken-lineto (lineto-cmd)
  "As https://github.com/hughsk/svg-path-parser"
  (loop for pnt on (cmd-coords lineto-cmd) by #'(lambda (x) (nthcdr 4 x))
	collect (list :name :l :relp (cmd-rel-p lineto-cmd)
		      :coords (list :xn (nth 1 pnt) :yn (nth 3 pnt)))))


#| Draws a horizontal line from the current point. H (uppercase)
indicates that absolute coordinates will follow; h (lowercase)
indicates that relative coordinates will follow. Multiple x values can
be provided (although usually this doesn't make sense). An H or h
command is equivalent to an L or l command with 0 specified for the y
coordinate. At the end of the command, the new current point is taken
from the final coordinate value.  |#

(defun hlineto (cmd)
  (list :name :h :relp (if (string= (cmd-head cmd) "h") t nil)
	:coords
	(append
	 (loop for x in (butlast (cmd-body cmd))
	       for idx from 1
	       nconc (list (name->keyword (format nil "x~D" idx)) x
			   (name->keyword (format nil "y~D" idx)) 0))
	 (list :xn (car (last (cmd-body cmd))) :yn 0))))

(defun broken-hlineto (hlineto-cmd)
  (loop for pnt on (cmd-coords hlineto-cmd) by #'(lambda (l) (nthcdr 4 l))
	collect (list :name :h :relp (cmd-rel-p hlineto-cmd)
		      :coords (list :xn (second pnt)
				    :yn (fourth pnt)))))


#| Draws a vertical line from the current point. V (uppercase)
indicates that absolute coordinates will follow; v (lowercase)
indicates that relative coordinates will follow. Multiple y values can
be provided (although usually this doesn't make sense). A V or v
command is equivalent to an L or l command with 0 specified for the x
coordinate. At the end of the command, the new current point is taken
from the final coordinate value.  |#

(defun vlineto (cmd)
  (list :name :v :relp (if (string= (cmd-head cmd) "v") t nil)
	:coords
	(append
	 (loop for y in (butlast (cmd-body cmd))
	       ;; Keep 0 idx for making absolute data
	       for idx from 1
	       nconc (list (name->keyword (format nil "x~D" idx)) 0
			   (name->keyword (format nil "y~D" idx)) y))
	 (list :xn 0 :yn (car (last (cmd-body cmd)))))))

(defun broken-vlineto (vlineto-cmd)
  (loop for pnt on (cmd-coords vlineto-cmd) by #'(lambda (l) (nthcdr 4 l))
	collect (list :name :v :relp (cmd-rel-p vlineto-cmd)
		      :coords (list :xn (second pnt) :yn (fourth pnt)))))

#| Start a new sub-path at the given (x,y) coordinates. M (uppercase)
indicates that absolute coordinates will follow; m (lowercase)
indicates that relative coordinates will follow. If a moveto is
followed by multiple pairs of coordinates, the subsequent pairs are
treated as implicit lineto commands. Hence, implicit lineto commands
will be relative if the moveto is relative, and absolute if the moveto
is absolute. If a relative moveto (m) appears as the first element of
the path, then it is treated as a pair of absolute coordinates. In
this case, subsequent pairs of coordinates are treated as relative
even though the initial moveto is interpreted as an absolute moveto.
|#

(defun moveto (cmd)
  (let* ((head (cmd-head cmd))
	 (body (cmd-body cmd))
	 (body-len (list-length body))
	 (relp (if (string= head "m") t nil)))
    (assert (>= body-len 2) (cmd) "Moveto command needs at least 1 pair of coordinates.")
    (assert (evenp body-len) (cmd) "Moveto command needs an even number of coordinates pairs.")
    (if (= body-len 2)
    	(list (list :name :m
		    :coords (list :xn (first body)
				      :yn (second body))
		    :relp relp))
    	(list (list :name :m
    		    :coords (list :xn (first body)
				      :yn (second body))
    		    :relp relp)
    	      (lineto (cons (if relp "l" "L") (nthcdr 2 body)))))
    ))


#| Draws a cubic Bézier curve from the current point to (x,y)
using (x1,y1) as the control point at the beginning of the curve
and (x2,y2) as the control point at the end of the
curve. C (uppercase) indicates that absolute coordinates will follow;
c (lowercase) indicates that relative coordinates will
follow. Multiple sets of coordinates may be specified to draw a
polybézier. At the end of the command, the new current point becomes
the final (x,y) coordinate pair used in the polybézier.  |#
(defun curveto (cmd)
  (let* ((head (cmd-head cmd))
	 (body (cmd-body cmd))
	 (body-len (list-length body)))
    (assert (>= body-len 6) (cmd) "Curveto command should have at least 3 coordinates pairs.")
    (assert (evenp body-len) (cmd) "Curveto command should have an even number pairs of coordinates.")
    (list :name :c :relp (if (string= head "c") t nil)	
	     :coords
	     (append
	      (loop for xy on (butlast body 2) by #'cddr
		    ;; Save 0 for making absolute coordinates later.
		    for idx from 1
		    nconc (list (name->keyword (format nil "x~D" idx)) (first xy)
				(name->keyword (format nil "y~D" idx)) (second xy)))
	      ;; Last two
	      (list :xn (first (last body 2))
		    :yn (second (last body 2))))
	     )))


#| Close the current subpath by connecting it back to the current
subpath's initial point (see prose above). Since the Z and z commands
take no parameters, they have an identical effect.  |#
(defun closepath (cmd) '(:name :z))


;;; Svg collision detection
;;; http://jsfromhell.com/math/is-point-in-poly
