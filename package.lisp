;;;; package.lisp


(defpackage #:xml-base
  (:use #:cl))

(defpackage #:svg
  (:use #:cl #:xml-base)
  (:export #:circle #:line #:write-svg #:g #:transform #:scale
	   #:rect #:path))


(defpackage #:smt-engine
  (:use #:cl)
  (:export #:sform #:hform #:defrule #:packsvg
	   #:*staff-line-thickness*
	   #:left #:width #:y #:x #:height
	   #:spn #:top #:right #:stacked-form #:horizontal-form
	   #:vertical-form #:ruledocs #:remrules #:content #:dur
	   #:hlineup #:preproc #:domain #:ruler #:canvas-vis-p #:marker-vis-p
	   #:head #:canvas-color #:glyph-color #:render
	   #:*staff-space* #:fixed-bottom #:fixed-height))

(defpackage #:smt
  (:use #:cl #:smt-engine))

(defpackage #:smt-test
  (:use #:cl #:smt)
  )
