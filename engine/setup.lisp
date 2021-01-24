
;;; This file contains only common cross-module parameters. More
;;; specific module-level parameters go directly inside their own
;;; files.

(in-package :smt-engine)

;; (load-time-value (defparameter *take* 0))


;; (defstruct bcr x y width height top right bottom left)

(defparameter *central-registry* (make-hash-table)
  "Keep track of objects created")


;; (defparameter *xml-indentation-type* #\space)
;; (defparameter *xml-indentation-depth* 3)

;;; Default font
(defparameter *fonts-hash-table* (make-hash-table))
(define-symbol-macro .installed-fonts. (alexandria:hash-table-keys *fonts-hash-table*))
(defparameter *font* 'haydn-11)


;;; Converting mm to pixel and vv.
;;; https://www.unitconverters.net/typography/millimeter-to-pixel-x.htm

(defconstant +px-per-mm+ 3.7795275591 "Pixels per mm")

(eval-when (:compile-toplevel :load-toplevel)
  ;; Need this for the margins constants later in the file
  ;; (DEFCONSTANT wants to know about constant's value at compile-time too)
  (defun mm-to-px (mm) (* mm +px-per-mm+)))


(defun chlapik-staff-space (rastral-no)
  "Rastral Größen wie bei Chlapik S. 32 beschrieben."
  (ecase rastral-no
    (2 (mm-to-px 1.88))
    (3 (mm-to-px 1.755))
    (4 (mm-to-px 1.6))
    (5 (mm-to-px 1.532))
    (6 (mm-to-px 1.4))
    (7 (mm-to-px 1.19))
    (8 (mm-to-px 1.02))))

(defparameter *staff-space* (chlapik-staff-space 2))


;;; This is the user-interface,
(defparameter *scale* 1
  "Global scaling factor for X and Y coordinates.")

(defparameter *vertical-space-reference-glyph* 'clefs.c
  "The relation between 4 staff spaces and the vertical dimension 
of this glyph is used to find the global internal factor, by which all
glyphs are scaled to ... By convention the vertical space of 
stave is equal to the height of the alto clef, hence the default glyph.")

;;; and the actual internal factor
(define-symbol-macro .scale. (* *scale*
				;; Chlapik p. 33: The symbol C-clef is 4 staff-spaces height.
				(/ (* 4 *staff-space*)
				   (bbox-height
				    (glyph-bbox
				     (get-glyph *vertical-space-reference-glyph*)))
				   ;; (getf (get-glyph-bbox *vertical-space-reference-glyph*) :height)
				   ;; (bcr-height (get-glyph-bbox "uniE05C")
				   ;; 	       ;; (get-bcr "clefs.C" .font.)
				   ;; 	       )
				   )))

;;; Line thickness for both cross and circle's contour
(defparameter *origin-line-thickness* 10)
;;; Marker's Cross
(defparameter *mchar-origin-cross-color* "deeppink")
(defparameter *sform-origin-cross-color* "tomato")
(defparameter *hform-origin-cross-color* "green")
(defparameter *vform-origin-cross-color* "blue")
(defparameter *origin-cross-length* 40 "Line length")

(defparameter *origin-circle-r* 4)
(defparameter *origin-circle-opac* .3)
;;; Marker's Circle
(define-symbol-macro %mchar-origin-circle-color% *mchar-origin-cross-color*)
(define-symbol-macro %mchar-origin-circle-contour-color% *mchar-origin-cross-color*)
(define-symbol-macro %sform-origin-circle-color% *sform-origin-cross-color*)
(define-symbol-macro %sform-origin-circle-contour-color% *sform-origin-cross-color*)
(define-symbol-macro %hform-origin-circle-color% *hform-origin-cross-color*)
(define-symbol-macro %hform-origin-circle-contour-color% *hform-origin-cross-color*)
(define-symbol-macro %vform-origin-circle-color% *vform-origin-cross-color*)
(define-symbol-macro %vform-origin-circle-contour-color% *vform-origin-cross-color*)

;;; Chase backgrounds follow their marker counterparts
(define-symbol-macro %mchar-canvas-color% *mchar-origin-cross-color*)
(define-symbol-macro %sform-canvas-color% *sform-origin-cross-color*)
(define-symbol-macro %hform-canvas-color% *hform-origin-cross-color*)
(define-symbol-macro %vform-canvas-color% *vform-origin-cross-color*)

;;; Just name the origin-point of the sticks (no magical numbers),
;;; XY origins are always 0!
(defconstant +stick-x-origin+ 0)
(defconstant +stick-y-origin+ 0)

;;; Page margines: Measured from Schubert Sonate, Henle
(defconstant +right-margin+ (mm-to-px 25))
(defconstant +left-margin+ (mm-to-px 36))
(defconstant +top-margin+ (mm-to-px 56))



(defun page-size (format)
  "Returns format's page size in pixels. BB p.481"
  (ecase format
    (:a3 (list :h (mm-to-px 420) :w (mm-to-px 297)))
    (:b4 (list :h (mm-to-px 353) :w (mm-to-px 250)))
    (:a4 (list :h (mm-to-px 297) :w (mm-to-px 210)))))

(defparameter *page-format* :a4)


(defparameter *posidim-attrs*
  '("x" "y" "width" "height" "left" "right" "top" "bottom"
    ;; transform/translate xy
    "tx" "ty"
    ;; circle
    "cx" "cy" "r"
    "x1" "x2" "y1" "y2")
  "These positional-dimensional attributes should be inverse-toplevel-scaled in all SVGLSTs before 
writing the svg doc.")



(defparameter *ticket-output* nil)
(defparameter *output-counter* 0)
(define-symbol-macro %output-path%
    (cond
      (*ticket-output* (incf *output-counter*)
		       (format nil "/tmp/smt-etude-~D.svg" *output-counter*))
      (t (format nil "/tmp/smt-etude.svg"))))
