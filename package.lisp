;;;; package.lisp


(defpackage #:xmlbase
  (:use #:cl))

(defpackage #:svg
  (:use #:cl #:xmlbase)
  (:export #:circle #:line #:write-svg #:g #:transform #:scale
	   #:rect #:path))


(defpackage #:smtngn
  (:use #:cl)
  (:export #:sform #:hform #:defrule #:packsvg
	   #:*staff-line-thickness*
	   #:left #:width #:y #:x #:height
	   #:spn #:top #:right #:stacked-form #:horizontal-form
	   #:vertical-form #:ruledocs #:remrules #:content #:dur
	   #:hlineup #:preproc #:domain #:ruler #:canvas-vis-p #:marker-vis-p
	   #:head #:canvas-color #:glyph-color #:render
	   #:*staff-space* #:fixed-bottom #:fixed-height))


