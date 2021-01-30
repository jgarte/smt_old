


(defpackage #:xml-base
  (:use #:cl))

(defpackage #:svg
  (:use #:cl #:xml-base)
  (:export #:circle #:line #:write-svg #:g #:transform #:scale
	   #:rect #:path))
;; (defpackage #:xml-base
;;   (:use #:cl)
;;   (:export #:make-empty-element #:make-non-empty-element
;; 	   #:make-comment #:make-decl #:write-xml
;; 	   #:*xmldecl*))

;; (defpackage #:svg
;;   (:use #:cl #:xml-base)
;;   (:export #:circle #:line #:write-svg #:g #:transform #:scale
;; 	   #:rect #:path))


(defpackage #:smt-engine
  (:use #:cl) (:nicknames "NGN")
  (:export
   ;; smtobj
   #:id
   ;; Mchars
   #:make-mchar #:mchar
   ;; Forms
   #:sformp
   #:sform #:hform #:defrule #:packsvg
   #:*staff-line-thickness*
   #:left #:width #:y #:x #:height
   #:spn #:top #:right #:stacked-form #:horizontal-form
   #:vertical-form #:ruledocs #:remrule #:content #:dur
   #:hlineup #:preproc #:domain #:ruler #:canvas-vis-p #:origin-visible-p
   #:head #:canvas-color #:mchar-color #:render #:canvas-opac
   #:*staff-space* #:fixed-bottom #:bottom #:fixed-height #:fixed-top
   #:inspect-br #:mm-to-px
   ;; Fonts
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
