


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
   ;; Forms
   #:sformp
   #:sform #:hform #:defrule #:packsvg
   #:*staff-line-thickness*
   #:left #:width #:y #:x #:height
   #:spn #:top #:right #:stacked-form #:horizontal-form
   #:vertical-form #:ruledocs #:remrule #:content #:dur
   #:hlineup #:preproc #:domain #:ruler #:canvas-vis-p #:origin-visible-p
   #:head #:canvas-color #:mchar-color #:render
   #:*staff-space* #:fixed-bottom #:bottom #:fixed-height #:fixed-top
   #:make-note #:make-notehead #:inspect-br
   ;; Notes
   #:note #:notehead
   ;; Lines
   #:barline
   ;;
   #:mm-to-px
   ;; Fonts
   #:install-font #:uninstall-font #:*font* #:.installed-fonts. #:*fonts-hash-table*
   #:glyph-present-p #:glyph-bbox
   ))

(defpackage #:smt
  (:use #:cl #:ngn)
  )

(defpackage #:smt-test
  (:nicknames "TST")
  (:use #:cl "NGN" #:it.bese.fiveam)
  
  )
