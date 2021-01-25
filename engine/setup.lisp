
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
(defparameter *fonts-hashtable* (make-hash-table))
(define-symbol-macro .installed-fonts. (alexandria:hash-table-keys *fonts-hashtable*))
(defparameter *font* 'haydn-11)

















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
