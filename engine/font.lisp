(in-package :smt-engine)

(defparameter *fonts-hashtable* (make-hash-table))
(define-symbol-macro .installed-fonts. (alexandria:hash-table-keys *fonts-hashtable*))
(defparameter *font* 'haydn-11)


;;; uninstall-font
(defun uninstall-font (font-name)
  (delete font-name .installed-fonts.)
  (remhash font-name *fonts-hashtable*))

(defstruct bbox x y left right top bottom width height)
;;; This is one glyph exported by Fontforge
(defstruct glyph name bbox pathd)

(defun install-font (srcpath &optional (vsrg 'clefs.c) (update-current-font t))
  (setf *vertical-space-reference-glyph* vsrg)
  ;; Prepare data
  (let* ((font-name (pathname-name srcpath))
	 (font-sym (intern (string-upcase font-name)))
	 (exportpath (format nil "/tmp/~AEXPORT/" font-name))
	 (bboxpath (format nil "/tmp/~A~A" font-name (string (gensym "BBOX")))))    
    (when update-current-font (setf *font* font-sym))
    (ensure-directories-exist exportpath)
    (sb-ext:run-program
     "/usr/bin/fontforge"
     (list "-script" "/home/amir/Work/Lisp/smt/fontinstprep.ff"
    	   srcpath
	   ;; $2 Where glyphs are exported (directory)
	   (namestring exportpath)
	   ;; $3 Where bboxes are written to (file),
	   ;; this file will be created by Fontforge.
	   bboxpath)
     :output *standard-output*)
    (setf (gethash font-sym *fonts-hashtable*) (make-hash-table))
    (let ((xx
	    ;; List name ,@bbox
	    (with-open-file (bbox bboxpath)
    	      (loop for ln = (read-line bbox nil)
    		    for trimln = (string-trim '(#\Space) ln)
    		    for lst = (split-sequence:split-sequence
    			       #\Space trimln)
    		    while (and ln (not (string= (car lst) "")))
		    ;; Keep the name of the glyph as string
    		    collect (list (car lst) (mapcar #'read-from-string lst)))
    	      )))
      (loop for (str (name minx miny maxx maxy w h)) in xx
	    for d = (cdr
		     (car
		      (s-xml:xml-element-attributes
		       (car
			(s-xml:xml-element-children
			 (car
			  (s-xml:xml-element-children
			   (s-xml:parse-xml-file (format nil "~A~A.svg" exportpath str) :output-type :xml-struct))))))))
	    do (setf (gethash (intern (string-upcase name))
			      (gethash (intern (string-upcase font-name)) *fonts-hashtable*))
		     (make-glyph :name name :pathd d
				 :bbox (make-bbox :x minx :y miny
						  :left minx :right maxx
						  :top miny :bottom maxy
						  :width w :height h))
		     )
	    )
      )
    )
  )

(defun font-hashtable (&optional (font *font*))
  (gethash font *fonts-hashtable*))
(defun font-glyphs (&optional (font *font*))
  "Returns the list of glyph names of FONT."
  (alexandria:hash-table-keys (font-hashtable font)))
(defun get-glyph (glyph-name &optional (font *font*))
  "Returns the from Fontforge exported data of the glyph (a structure)."
  (gethash glyph-name (font-hashtable font)))


;;; 
(install-font "/home/amir/gutenberg1939/svg/gutenberg1939-11.svg")
(install-font "/home/amir/haydn/svg/haydn-11.svg")
