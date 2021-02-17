(in-package #:smt-docs)

;;; Abit formatting
(eval-when (:compile-toplevel )
  (defun book (author title publisher year)
    `(:li ,(format nil "~A: " author)
	  (:i ,(format nil "~A" title))
	  ,(if (stringp publisher) (format nil ", ~A, " publisher)
	       publisher)
	  ,(format nil "~A" year))))
(register-page "Bibliography"
  (:header (:h1 "Bibliography"))
  (:ul
   #.(book "Ted Ross" "Teach Yourself The Art of Music Engraving" "Hansen Publications Ltd." 1987)
   #.(book "Herbert Chlapik" "Die Praxis des Notengraphikers" "Doblinger" 1987)
   #.(book "Kurt Stone" "Music Notation in the Twentieth Century" "W. W. Norton & Company Inc." 1980)
   #.(book "Karl Hader" "Aus der Werkstatt eines Notenstechers" "Waldheim-Eberle-Verlag" 1948)
   #.(book "Elaine Gould" "Behind Bars" "Faber Music Ltd" 2011)
   #.(book "The LilyPond development team" "Essay on Automated Music Engraving, "
	   '(:a :href "https://lilypond.org/essay.html" "https://lilypond.org/essay.html") ", 2015")
   #.(book "Mark McGrain" "Music Notation, Theory and Technique for Music Notation" "Berklee Press" 1966)
   #.(book "Jonathan Feist" "Contemporary Music Notation" "Berklee Press" 2017)
   #.(book "Leland Smith"
	   "SCORE - A Musician's Approach to Computer Music"
	   "Journal of the Audio Engineering Society"
	   1971)
   #.(book "Donald Alvin Byrd"
	   "Music Notation by Computer, "
	   '(:a :href "http://homes.sice.indiana.edu/donbyrd/Papers/DonDissScanned.pdf"
	     "http://homes.sice.indiana.edu/donbyrd/Papers/DonDissScanned.pdf")
	   ", 1984")
   ))
