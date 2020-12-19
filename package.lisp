
(defpackage #:xml-base
  (:use #:cl))

(defpackage #:svg
  (:use #:cl #:xml-base)
  (:export #:circle #:line #:write-svg #:g #:transform #:scale
	   #:rect #:path))


(defpackage #:smt-engine
  (:use #:cl) (:nicknames "NGN")
  (:export #:sform #:hform #:defrule #:packsvg
	   #:*staff-line-thickness*
	   #:left #:width #:y #:x #:height
	   #:spn #:top #:right #:stacked-form #:horizontal-form
	   #:vertical-form #:ruledocs #:remrules #:content #:dur
	   #:hlineup #:preproc #:domain #:ruler #:canvas-vis-p #:marker-vis-p
	   #:head #:canvas-color #:glyph-color #:render
	   #:*staff-space* #:fixed-bottom #:bottom #:fixed-height #:fixed-top
	   #:make-note #:make-notehead #:inspect-br
	   ))

(defpackage #:smt
  (:use #:cl #:ngn)
  )

(defpackage #:smt-test
  (:nicknames "TST")
  (:use #:cl "NGN" #:it.bese.fiveam)
  
  )
