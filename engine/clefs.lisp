(in-package :smtngn)

(defclass clef (pitched-glyph) ())


(defun clefspn->glyphname (spn)
  (ecase (car spn)
    (f "clefs.F")
    (g "clefs.G")
    (c "clefs.C")))

(defun clef (spn &rest initargs &key &allow-other-keys)
  (apply #'make-instance 'clef :spn spn
			       :code (clefspn->glyphname spn)
			       initargs))



