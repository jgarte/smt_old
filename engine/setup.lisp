
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
(define-symbol-macro .fonts. (alexandria:hash-table-keys *fonts-hash-table*))
(define-symbol-macro .font. (car .fonts.))

;;; Converting mm to pixel and vv.
;;; https://www.unitconverters.net/typography/millimeter-to-pixel-x.htm

(defconstant +pxl-per-mm+ 3.7795275591 "Pixels per mm")

(eval-when (:compile-toplevel :load-toplevel)
  ;; Need this for the margins constants later in the file
  ;; (DEFCONSTANT wants to know about constant's value at compile-time too)
  (defun mm-to-pxl (mm) (* mm +pxl-per-mm+)))


(defun chlapik-staff-space (rastral-no)
  "Rastral Größen wie bei Chlapik S. 32 beschrieben."
  (ecase rastral-no
    (2 (mm-to-pxl 1.88))
    (3 (mm-to-pxl 1.755))
    (4 (mm-to-pxl 1.6))
    (5 (mm-to-pxl 1.532))
    (6 (mm-to-pxl 1.4))
    (7 (mm-to-pxl 1.19))
    (8 (mm-to-pxl 1.02))))

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
				   (getf (mcharbb *vertical-space-reference-glyph*) :height)
				   ;; (bcr-height (mcharbb "uniE05C")
				   ;; 	       ;; (get-bcr "clefs.C" .font.)
				   ;; 	       )
				   )))

;;; Line thickness for both cross and circle's contour
(defparameter *marker-line-thickness* 4)
;;; Marker's Cross
(defparameter *mchar-marker-cross-color* "deeppink")
(defparameter *sform-marker-cross-color* "tomato")
(defparameter *hform-marker-cross-color* "green")
(defparameter *vform-marker-cross-color* "blue")
(defparameter *marker-cross-length* 40 "Line length")

(defparameter *marker-circle-r* 2)
(defparameter *marker-circle-opac* .2)
;;; Marker's Circle
(define-symbol-macro %mchar-marker-circle-color% *mchar-marker-cross-color*)
(define-symbol-macro %mchar-marker-circle-contour-color% *mchar-marker-cross-color*)
(define-symbol-macro %sform-marker-circle-color% *sform-marker-cross-color*)
(define-symbol-macro %sform-marker-circle-contour-color% *sform-marker-cross-color*)
(define-symbol-macro %hform-marker-circle-color% *hform-marker-cross-color*)
(define-symbol-macro %hform-marker-circle-contour-color% *hform-marker-cross-color*)
(define-symbol-macro %vform-marker-circle-color% *vform-marker-cross-color*)
(define-symbol-macro %vform-marker-circle-contour-color% *vform-marker-cross-color*)

;;; Chase backgrounds follow their marker counterparts
(define-symbol-macro %mchar-canvas-color% *mchar-marker-cross-color*)
(define-symbol-macro %sform-canvas-color% *sform-marker-cross-color*)
(define-symbol-macro %hform-canvas-color% *hform-marker-cross-color*)
(define-symbol-macro %vform-canvas-color% *vform-marker-cross-color*)

;;; Just name the origin-point of the sticks (no magical numbers),
;;; XY origins are always 0!
(defconstant +stick-x-origin+ 0)
(defconstant +stick-y-origin+ 0)

;;; Page margines: Measured from Schubert Sonate, Henle
(defconstant +right-margin+ (mm-to-pxl 25))
(defconstant +left-margin+ (mm-to-pxl 36))
(defconstant +top-margin+ (mm-to-pxl 56))



(defun page-size (format)
  "Returns format's page size in pixels. BB p.481"
  (ecase format
    (:a3 (list :h (mm-to-pxl 420) :w (mm-to-pxl 297)))
    (:b4 (list :h (mm-to-pxl 353) :w (mm-to-pxl 250)))
    (:a4 (list :h (mm-to-pxl 297) :w (mm-to-pxl 210)))))

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
