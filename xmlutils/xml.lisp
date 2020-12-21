
;;; https://www.w3.org/TR/xml/

(in-package #:xml)

(defclass element ()
  ((generic-identifier :initarg :gi
		       :documentation "Each element has a type, identified by name, 
sometimes called its generic identifier (GI)"
		       :reader gi)
   (attributes :initarg :attributes
	       :type list
	       :accessor attributes)))

(defun attribute-pair (name val &key (nm-case :down)
				    (val-case :down))
  (format nil "~A=\"~A\""
	  (etypecase name
	    (string name)
	    (symbol (ecase nm-case
		      (:down (string-downcase (string name)))
		      (:up (string name))
		      (:cap (string-capitalize (string name))))))
	  (etypecase val
	    (number (princ-to-string val))
	    (string val)
	    (symbol (ecase val-case
		      (:down (string-downcase (string val)))
		      (:up (string val))
		      (:cap (string-capitalize (string-val)))))
	    )
	  ))

(defclass empty-element (element) ())

(defun empty-element-p (obj) (typep obj 'empty-element))

(defun make-empty-element (gi attributes)
  (make-instance 'empty-element :gi gi :attributes attributes))

(defclass non-empty-element (element)
  ((content :initarg :content
	    :type (or string list)
	    :accessor content)))

(defun make-non-empty-element (gi attributes content)
  (make-instance 'non-empty-element :gi gi :attributes attributes :content content))

(defun non-empty-element-p (obj) (typep obj 'non-empty-element))

(defun etag (obj)
  (format nil "</~A>" (gi obj)))

(defparameter *sdelims*
  '(empty-element "<"
    non-empty-element "<"
    comment "<!--"
    decl "<?xml"
    cdata "<![CDATA[")
  "Start Delimiters for Stag")

(defparameter *edelims*
  '(empty-element "/>"
    non-empty-element ">"
    comment "-->"
    decl "?>"
    cdata "]]>")
  "End Delimiters for Stag")

;;; Start tag wird gleichzeitig für empty & non-empty Elemente benutzt.
(defun stag (obj)
  (with-output-to-string (s)
    (write-string (format nil "~A~A" (getf *sdelims* (type-of obj)) (gi obj)) s)
    (do ((lst (attributes obj) (cddr lst)))
	((null lst) (write-string (getf *edelims* (type-of obj)) s))
      (destructuring-bind (name val) (subseq lst 0 2)
	(write-char #\Space s)
	(write-string (attribute-pair name val) s)
	))))



 ;;; From the perspective of data structure,
;;; more clear to have an extra class for unnamed
;;; text blocks, ...
;;; Unnamed objects get no GI and thus no tags.
(defclass unnamed ()
  ((attributes :initarg :attributes
	       :initform ()
	       :accessor attributes)
   (content :initarg :content
	    :initform ()
	    :accessor content)))
(defun unnamedp (obj) (typep obj 'unnamed))
(defclass comment (unnamed) ())

(defun make-comment (content)
  (make-instance 'comment :content content))
(defun commentp (obj) (typep obj 'comment))

(defmethod delimit ((obj comment))
  (format nil "~A ~A ~A"
	  (getf *sdelims* 'comment)
	  (content obj)
	  (getf *edelims* 'comment)))


;;; https://xmlwriter.net/xml_guide/xml_declaration.shtml
(defparameter *xmldecl*
  '(version "1.0" encoding "UTF-8" standalone yes))

(defun declp (obj) (typep obj 'decl))

(defclass decl (unnamed) ())

(defmethod delimit ((obj decl))
  (with-output-to-string (s)
    (write-string (getf *sdelims* 'decl) s)
    (write-char #\space s)
    (do ((lst (attributes obj) (cddr lst)))
	((null lst)
	 (write-string (getf *edelims* 'decl) s))
      (destructuring-bind (name val) (subseq lst 0 2)
	(write-string (attribute-pair name val) s)
	(write-char #\Space s)
	))))

(defun make-decl ()
  (make-instance 'decl :attributes *xmldecl*))

(defun indent (doc curr-ind ind-depth ind-offset)
  "Creates dotted pairs, where car is the number of indentations and
cdr is the elements/strings to be written to the output file."
  (cond
    ((null doc) ())
    ((listp doc) (apply #'append (mapcar
				  #'(lambda (elem)
				      (indent elem curr-ind ind-depth ind-offset))
				  doc)))
    ((unnamedp doc) (list (cons curr-ind (delimit doc))))
    ((empty-element-p doc) (list (cons curr-ind (stag doc))))
    ;; Implies that doc at point is a non-empty-element, check whether
    ;; we can make the whole string right away
    ((stringp (content doc))
     (list (cons curr-ind (concatenate 'string (stag doc) (content doc) (etag doc)))))
    (t (append (list (cons curr-ind (stag doc)))
	       (indent (content doc) (* ind-offset ind-depth) ind-depth (1+ ind-offset))
	       (list (cons curr-ind (etag doc)))))))

(defun make-line (pair)
  (concatenate 'string
	       (make-string (car pair) :initial-element #\space)
	       (cdr pair)))

(defun write-xml (doc &key (path "/tmp/etude.xml") (if-exists :supersede)
			(ind-depth 1) (curr-ind 0) (ind-offset 1))
  (with-open-file (s path :direction :output
			  :if-exists if-exists)
    (dolist (pair (indent (list (make-decl) doc) curr-ind ind-depth ind-offset))
      (write-line (make-line pair) s))))


