
;;; This file contains only common cross-module parameters. More
;;; specific module-level parameters go directly inside their own
;;; files.

(in-package :smt-engine)

;; (load-time-value (defparameter *take* 0))


(defstruct bcr x y width height top right bottom left)

(defparameter *central-registry* (make-hash-table)
  "Keep track of objects created")


;; (defparameter *xml-indentation-type* #\space)
;; (defparameter *xml-indentation-depth* 3)

;;; Default font
(defparameter *font-families* '(:haydn-11))
(define-symbol-macro .font-family. (car *font-families*))

;;; Converting mm to pixel and vv.
;;; https://www.unitconverters.net/typography/millimeter-to-pixel-x.htm

(defconstant +px/mm+ 3.7795275591 "Pixels per mm")

(eval-when (:compile-toplevel :load-toplevel)
  ;; Need this for the margins constants later in the file
  ;; (DEFCONSTANT wants to know about constant's value at compile-time too)
  (defun mm->px (mm) (* mm +px/mm+)))


(defun chlapik-staff-space (rastral-no)
  "Rastral Größen wie bei Chlapik S. 32 beschrieben."
  (ecase rastral-no
    (2 (mm->px 1.88))
    (3 (mm->px 1.755))
    (4 (mm->px 1.6))
    (5 (mm->px 1.532))
    (6 (mm->px 1.4))
    (7 (mm->px 1.19))
    (8 (mm->px 1.02))))

(defparameter *staff-space* (chlapik-staff-space 2))


;;; This is the user-interface,
(defparameter *scale* 1
  "Global scaling factor for X and Y coordinates.")

;;; and the actual internal factor
(define-symbol-macro .scale. (* *scale*
				;; Chlapik p. 33: The symbol C-clef is 4 staff-spaces height.
				(/ (* 4 *staff-space*)
				   (bcr-height (get-bcr "clefs.C" .font-family.)))))

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
(defconstant +right-margin+ (mm->px 25))
(defconstant +left-margin+ (mm->px 36))
(defconstant +top-margin+ (mm->px 56))



(defun page-size (format)
  "Returns format's page size in pixels. BB p.481"
  (ecase format
    (:a3 (list :h (mm->px 420) :w (mm->px 297)))
    (:b4 (list :h (mm->px 353) :w (mm->px 250)))
    (:a4 (list :h (mm->px 297) :w (mm->px 210)))))

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
