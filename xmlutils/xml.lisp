
;;; Use for implementation:
;;; https://www.tei-c.org/release/doc/tei-p5-doc/en/html/SG.html


(in-package #:xml-base)

(deftype real-atom ()
  "HyperSpec: atom n. any object that is not a cons."
  '(and atom (not null)))
(deftype dotted-pair ()
  "HyperSpec: dotted pair n. 1. a cons whose cdr is a non-list. "
  '(cons * real-atom))

(defun dotted-pairs-p (lst)
  (every #'(lambda (x) (typep x 'dotted-pair)) lst))

(deftype dotted-pairs ()
  `(satisfies dotted-pairs-p))

(defun stringize-attribute (pair)
  (check-type pair dotted-pair)
  (format nil "~a='~a'" (car pair) (cdr pair))
  ;; (let ((name (car pair))
  ;; 	(type (cdr pair)))
  ;;   (uiop:strcat
  ;;    ;; Thing is converted to uppercase by princ-to-string.
  ;;    (if (stringp name) name (princ-to-string name))
  ;;    "=\""
  ;;    (if (stringp type) type (princ-to-string type))
  ;;    "\""
  ;;    ))
  )
;; (stringize-attribute '("foo" . ("java")))
;; (princ-to-string 'j)
;; (format nil "~a='~a'" :j "sd")


(defclass element ()
  ((name :initarg :name :reader element-name)
   (attrs :initarg :attrs
	  ;; Compiler doesn't care about this?
   	  ;; :type dotted-pairs
   	  :initform ()
	  :documentation "Attributes list"
   	  :accessor elmattrs)
   (cnt :initarg :cnt
	:initform ()
	:documentation "Content list"
	:accessor elmcnt)
   (open-mark 
    :initarg :open-mark
    :type (simple-array character (*))
    :accessor element-open-mark)
   (close-mark 
    :initarg :close-mark
    :type (simple-array character (*))
    :accessor element-close-mark)
   (empty-close-mark 
    :type (simple-array character (*))
    :initarg :empty-close-mark
    :accessor element-empty-close-mark)))

(defun make-element (name &key attrs cnt
			    (open-mark "<") (close-mark ">")
			    (empty-close-mark "/>"))
  (check-type attrs dotted-pairs)
  (make-instance 'element :name name :attrs attrs
		 :cnt cnt :open-mark open-mark
		 :close-mark close-mark :empty-close-mark empty-close-mark))

(defmethod opening-tag ((obj element))
  "Returns the <elem attrs...> string."
  (uiop:strcat (element-open-mark obj) (element-name obj)
  	       ;; Join strings
  	       (if (elmattrs obj)
  		   (format nil " ~{~A~^ ~}" (mapcar #'stringize-attribute (elmattrs obj)))
  		   "")
  	       (if (null (elmcnt obj)) (element-empty-close-mark obj) (element-close-mark obj)))
  )
;; (let ((obj (make-element "a" :attrs '(("viewBox" . "asd")))))
;;  (uiop:strcat
;;   (element-open-mark obj) (element-name obj)
;;   (format nil
;; 	  " ~{~A~^ ~}"
;; 	  (mapcar #'stringize-attribute (elmattrs obj)))
;;   (if (null (elmcnt obj)) (element-empty-close-mark obj) (element-close-mark obj))))
;; (opening-tag (make-element "a" :attrs '(("viewBox" . "asd"))))
(defmethod closing-tag ((obj element))
  "Returns the closing </elem> tag string."
  (uiop:strcat "</" (element-name obj) ">"))

(defgeneric elementp (obj))
(defmethod elementp ((obj element))
  (declare (ignore obj))
  t)
(defmethod elementp (obj)
  (declare (ignore obj)))

(defgeneric check-attribute-property (obj attribute-indicator))
(defmethod check-attribute-property ((obj element) attribute-indicator)
  (cdr (assoc attribute-indicator (elmattrs obj)
	      :test (etypecase attribute-indicator
		      (string #'string=)))))
(defmethod (setf check-attribute-property) (new-property (obj element) attribute-indicator)
  (setf (cdr (assoc attribute-indicator (elmattrs obj)))
	new-property))


;; ;;; Non-recursive implementation?
;; ;;; Or tail-recursive?
;; (defun compute-indentation (elements curr-ind ind-offset ind-depth)
;;   (cond
;;     ((null elements) nil)
;;     ;; A string needs no new indentation (comes at the same level
;;     ;; it'S parent, but on a new line).????
;;     ((stringp elements)     
;;      (list (cons curr-ind elements)))
;;     ;; An empty element (May not double-checking on elements, for empty or otherwise?)
;;     ;; Also no new indentation level is required, but on a new line.
;;     ((and (elementp elements) (null (elmcnt elements)))
;;      (list (cons curr-ind (opening-tag elements))))
;;     ;; A element type is like (element-form="<onset>" element-form=content element-form="</termination>")
;;     ((elementp elements) (append (list (cons curr-ind (opening-tag elements)))
;; 				 (compute-indentation (elmcnt elements)
;; 					 (* ind-depth (1+ ind-offset))
;; 					 (1+ ind-offset)
;; 					 ind-depth)
;; 				 (list (cons curr-ind (closing-tag elements)))))
;;     ;; Else its a list of elements and/or strings
;;     (t (apply #'append (mapcar
;; 			#'(lambda (element)
;; 			    (compute-indentation element curr-ind ind-offset ind-depth))
;; 			elements)))))


(defun compute-indentation (elements curr-ind ind-offset ind-depth)
  "Creates dotted pairs, where car is the number of indentations and
cdr is the elements/strings to be written to the output file."
  (cond
    ((null elements) nil)
    ;; An empty element (May not double-checking on elements, for empty or otherwise?)
    ;; Also no new indentation level is required, but on a new line.
    ((and (elementp elements) (null (elmcnt elements)))
     (list (cons curr-ind (opening-tag elements))))
    ;; If cnt is a string, create the whole string at once,
    ;; which forces no new indentation level for the content.
    ((and (elementp elements) (stringp (elmcnt elements)))
     (list (cons curr-ind (uiop:strcat (opening-tag elements) (elmcnt elements) (closing-tag elements)))))
    ;; A element type is like (element-form="<onset>" element-form=content element-form="</termination>")
    ((elementp elements) (append (list (cons curr-ind (opening-tag elements)))
				 (compute-indentation (elmcnt elements)
					 (* ind-depth (1+ ind-offset))
					 (1+ ind-offset)
					 ind-depth)
				 (list (cons curr-ind (closing-tag elements)))))
    ;; Else its a list of elements and/or strings
    (t (apply #'append (mapcar
			#'(lambda (element)
			    (compute-indentation element curr-ind ind-offset ind-depth))
			elements)))))




(defun make-lines (indented-elements ind-type)
  ;; ind-elem-pair = (indentation-level . element-object)
  (mapcar #'(lambda (ind-elem-pair)
	      (destructuring-bind (indentation . element) ind-elem-pair
		(format nil "~A~A~%"
			(make-string indentation :initial-element (ecase ind-type
								    (space #\SPACE)
								    (tab #\TAB)))
			element)))
	  indented-elements))


;;; https://xmlwriter.net/xml_guide/xml_declaration.shtml
(defparameter *declaration-attributes*
  '(("version" . "1.0")
    ("encoding" . "UTF-8")
    ("standalone" . "yes")))

(defun %declaration ()
  (make-element "xml"
		:attrs *declaration-attributes*
		:open-mark "<?"
		:empty-close-mark "?>"))

(defun comment (text)
  (make-element text
		:open-mark "<!-- "
		:empty-close-mark " -->"))



;;; Do indentation depth changes somewhere else more efficient?
(defun write-to-path (elements &key
				 (path "/tmp/etude.xml")
				 (ind-offset 0)
				 (ind-depth 1)
				 (ind-type 'space)
				 )
  "Writes all elements (possibly very nested!) into a file."
  (let ((indented-lines
	 (make-lines (compute-indentation
		      (list (%declaration)
			    (comment "Generated by SMT/XMLUTILS")
			    elements)
		      ind-offset ind-offset ind-depth) ind-type)))    
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (loop :for line :in indented-lines
	 :do (format stream line)))))
