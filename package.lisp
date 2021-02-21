

(defpackage #:smt-engine
  (:use #:cl) (:nicknames "NGN")
  (:export
   #:id
   #:make-mchar #:mchar #:comp #:children
   #:sformp #:make-sform #:make-hform #:make-vform
   #:defrule #:packsvg
   #:*staff-line-thickness*
   #:left #:width #:y #:x #:height
   #:top #:right #:stacked-form #:horizontal-form
   #:vertical-form #:ruledocs #:remrule #:content
   #:right-side-space #:nlineup
   #:hlineup #:preproc #:domain #:ruler #:canvas-vis-p #:origin-visible-p
   #:head #:canvas-color #:mchar-color #:render #:canvas-opac
   #:*space* #:fixed-bottom #:bottom #:fixed-height #:fixed-top
   #:inspect-br #:mm-to-px
   #:install-font #:uninstall-font #:*font* #:.installed-fonts.
   #:*fonts-hashtable* #:glyph #:glyph-bbox #:font-glyphs))

(defpackage #:smt
  (:use #:cl #:smt-engine)
  (:export
   #:barline
   #:note #:notehead #:clocked
   #:make-note #:make-notehead
   #:*font* #:.installed-fonts. #:glyph))

;;; Export glyph names
;; (export (smt::font-glyphs smt:*font*) :smt)

(defpackage #:smt-test
  (:use #:cl #:smt-engine #:it.bese.fiveam)
  )

(defpackage #:smt-docs
  (:use #:cl))
