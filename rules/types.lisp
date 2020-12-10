
;; (in-package #:smtngn)

;; (deftype tmpsym ()
;;   '(or note pause chord))

;; ;;; hs
;; ;; (sform cnt (tmpsym))

;; (defun tmpsym-container-p (obj)
;;   (some #'(lambda (x) (typep x 'tmpsym)) (content obj)))

;; (deftype tmpsymctr ()
;;   '(satisfies tmpsym-container-p))


;; (typep (sform :content (list (sform :content (list (make-note 3))
;; 				    )))
;;        'tmpsymctr)


