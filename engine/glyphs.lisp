(in-package :smt-engine)


;;; Diese Zahlen beziehen sich auf das Origin 0,0 vom viewPort
;;; also von dort aus betrachtet! Diese Werte sind vom Firefox abgeschrieben.
;;; This indicates the original sizes.
;;; This structure is used ONLY for MTYPES! 
;; (defstruct firefox-bbox
;;   ;; XY the shape's onset,
;;   ;; These onsets r subject to scaling!!! So treat them as if w&h
;;   x y
;;   width height)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; lilyboulez ;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HAYDN ;;;;;;;;;;;;;;;;;;;;;;
(defparameter *haydn-11-paths*
  '(
    ;; Noteheads
    ("noteheads.s0" . "M173,-141c185,0,268,65,268,172c0,62,-95,110,-182,110c-175,0,-259,-57,-259,-172c0,-66,90,-110,173,-110ZM190,-109c-38.4053955078125,0,-70.4551010131836,12.711402893066406,-70.4551010131836,52.51060104370117c0,19.269397735595703,7.512977600097656,44.88869857788086,25.455101013183594,78.48939895629883c45.955596923828125,86.04440307617188,64.18569946289062,87.00050354003906,102.36500549316406,87.00050354003906c0.8677215576171875,0,1.7457275390625,-0.00049591064453125,2.6346282958984375,-0.00049591064453125c45.076507568359375,0,68.63189697265625,-15.213203430175781,68.63189697265625,-48.80400085449219c0,-21.553600311279297,-9.69818115234375,-50.67360019683838,-29.63189697265625,-88.19600105285645c-39.351409912109375,-74.60359764099121,-61.90010070800781,-81.32459831237793,-84.17640686035156,-81.32459831237793c-4.8958740234375,0,-9.778594970703125,0.32464599609375,-14.823593139648438,0.32464599609375Z")
    ("noteheads.s1" . "M0,38c0,-133,116,-194,214,-194c85,0,119,42,119,118c0,121,-93,194,-221,194c-68,0,-112,-27,-112,-118ZM251,-105c-42,0,-214,123,-214,171c0,19.84960174560547,17.277301788330078,35,37,35c48,0,214,-137,214,-172c0,-19.849403381347656,-17.277008056640625,-34,-37,-34Z")
    ("noteheads.s2" . "M0,47c0,-106,114,-204,199,-204c71,0,101,35,101,110c0,96,-104,186,-187,186c-77,0,-113,-37,-113,-92Z")
    ;; Accidentals

    ;; Clefs
    ("clefs.C" . "M0,474c-1.8592599630355835,-173.84100341796875,-2.6382598876953125,-379.875,-2.6382598876953125,-556.6399993896484c0,-203.52500915527344,1.0327098369598389,-368.24900817871094,2.6382598876953125,-400.36000061035156c0.5018680095672607,-10.0369873046875,9,-14,19,-16c18.487701416015625,-3.1693115234375,33.06919860839844,-4.385528564453125,48.01850128173828,-4.385528564453125c13.351997375488281,0,26.997398376464844,0.970184326171875,43.98149871826172,2.385528564453125c7,1,13,5,13,12c0.14644622802734375,55.3568115234375,0.2071075439453125,107.0679931640625,0.2071075439453125,156.20098876953125c0,237.2340087890625,-1.4142074584960938,414.3650131225586,-1.4142074584960938,651.5989990234375c0,49.1328125,0.0606536865234375,100.84402465820312,0.2071075439453125,156.20101928710938c0,12,-7,22,-18,24c-18.5,3.5,-33.5,5.5,-48.375003814697266,5.5s-29.625,-2,-47.625,-6.5c-9.999999821186066,-2,-8.999999814697276,-14,-8.999999814697276,-24ZM187,78c0.5,62.5,0.75,126.5,0.75,194.625s-0.25,140.375,-0.75,219.375c0,4,-6.729248046875,6,-13.468902587890625,6s-13.489593505859375,-2,-13.531097412109375,-6c-4,-384,-7,-822,-7,-987c0,-5,6.5,-7.5,13,-7.5s13,2.5,13,7.5c3,159,2,284,4,410c0,13,13,12,18,7c18,-18,63,-67,74,-135c1.091400146484375,-7.0941009521484375,7.840789794921875,-12.10369873046875,15.048095703125,-12.10369873046875c6.000091552734375,0,12.317596435546875,3.47198486328125,15.951904296875,12.10369873046875c25,60,44,126,121,126c60.125396728515625,0,72.56069946289062,-95.3800048828125,72.56069946289062,-176.92001342773438c0,-10.679290771484375,-0.21331787109375,-21.12109375,-0.56072998046875,-31.0802001953125c-3,-80,0,-185,-87,-185c-35.6376953125,0,-84.01507568359375,20.57940673828125,-84.01507568359375,36.515411376953125c0,13.32861328125,10.717681884765625,23.484588623046875,24.01507568359375,23.484588623046875c0.691070556640625,-0.024658203125,1.386383056640625,-0.03717041015625,2.08526611328125,-0.03717041015625c26.119598388671875,0,57.228973388671875,17.456817626953125,57.228973388671875,68.85049438476562c0,2.949432373046875,-0.102447509765625,6.0106201171875,-0.314178466796875,9.18670654296875c-2.644500732421875,36.141571044921875,-37.14788818359375,61.40447998046875,-73.37149047851562,61.40447998046875c-4.869476318359375,0,-9.77001953125,-0.456512451171875,-14.628509521484375,-1.404510498046875c-44.318115234375,-9.0650634765625,-71.01229858398438,-46.7288818359375,-71.01229858398438,-94.0548095703125c0,-82.63018798828125,60.14208984375,-123.94497680664062,197.01199340820312,-123.94497680664062c102.84500122070312,0,195.00802612304688,91.69668579101562,195.00802612304688,237.29098510742188c0,8.712860107421875,-0.330078125,17.61871337890625,-1.00762939453125,26.709503173828125c-10,134,-83,191.00000381469727,-188.00003051757812,191.00000381469727c-42,0,-59,-32.000003814697266,-95,-32.000003814697266c-41,0,-56,29.000003814697266,-56,70.00000333786011c0,43.00000047683716,20,74.99999666213989,54,74.99999666213989c40,0,48,-29.999996185302734,102,-29.999996185302734c147.00003051757812,0,209.00003051757812,98.99999618530273,209.00003051757812,252.99999618530273c0,123,-66,205,-233.00003051757812,209c-4.02166748046875,0.094635009765625,-8.08135986328125,0.144500732421875,-12.166900634765625,0.144500732421875c-82.26348876953125,0,-174.97499084472656,-20.2135009765625,-177.83299255371094,-102.14401245117188c-0.0743865966796875,-1.710784912109375,-0.111419677734375,-3.429840087890625,-0.111419677734375,-5.1541748046875c0,-44.200592041015625,24.335586547851562,-91.86001586914062,67.80830383300781,-91.86001586914062c0.432464599609375,0,0.866851806640625,0.004730224609375,1.303131103515625,0.01422119140625c48.253692626953125,1.048980712890625,80.06268310546875,40.139801025390625,80.06268310546875,77.86038208007812c0,35.81329345703125,-10.900390625,43.558502197265625,-30.06268310546875,53.139617919921875c-10,5,-15,12,-15,22c0,14,30,24,70,24c78,0,95.99996948242188,-76,95.99996948242188,-192c0,-125.00001525878906,-8.999969482421875,-210.00000762939453,-79.99996948242188,-210.00000762939453c-67,0,-122,79.99999237060547,-129,106.99999237060547c-2.567657470703125,10.2705078125,-10.409515380859375,17.640304565429688,-18.650299072265625,17.640304565429688c-7.80670166015625,0,-15.971527099609375,-6.6136932373046875,-20.349700927734375,-23.640304565429688c-9,-36,-32.00001525878906,-72.99999237060547,-51.00001525878906,-108.99999237060547c-2.8827972412109375,-5.285133361816406,-9.920852661132812,-9.646862030029297,-16.455703735351562,-9.646862030029297c-7.0662689208984375,0,-13.544296264648438,5.099906921386719,-13.544296264648438,19.646900177001953Z")
    ("clefs.F" . "M0,-36c0,-128.08200073242188,96,-214,251,-214c153.63900756835938,0,293,37,293,264c0,220,-158.04998779296875,325.97601318359375,-254,391c-121,82,-200.2480010986328,117.63397216796875,-339,171c-5.9390716552734375,2.28436279296875,-11.316600799560547,3.27496337890625,-15.979499816894531,3.27496337890625c-11.934799194335938,0,-19.187103271484375,-6.48974609375,-19.187103271484375,-14.3853759765625c0,-7.0484619140625,5.7794952392578125,-15.21722412109375,19.166603088378906,-20.8896484375c118,-49.999969482421875,225,-85.99996948242188,316,-199.99996948242188c91,-113.00001525878906,125,-205.9130096435547,125,-350.00000953674316c0,-135.0000057220459,-33,-224.0000057220459,-145,-224.0000057220459c-46.61500549316406,0,-77.45199584960938,12.593002319335938,-112,44c-8.092338562011719,7.3566741943359375,-11.312103271484375,13.02960205078125,-11.312103271484375,17.402206420898438c0,13.45849609375,30.502700805664062,14.597793579101562,43.312103271484375,14.597793579101562c69,0,123,64.88670349121094,123,136.0000057220459c0,75.00000190734863,-48,132.00000190734863,-139,132.00000190734863c-79.15819931030273,0,-135,-74,-135,-150ZM572,-128c0,-35,29,-64,64,-64s63,29,63,64s-28,63,-63,63s-64,-28,-64,-63ZM572,131c0,-35,29,-64,64,-64s63,29,63,64s-28,63,-63,63s-64,-28,-64,-63Z")
    ("clefs.G" . "M0,-110c0,-215,186,-392,288,-468c9.35198974609375,-6.968017578125,17.34698486328125,-12.61700439453125,18,-24c7,-122,9.399993896484375,-176.906982421875,54,-278c45,-102,107,-184,184,-228c7.23681640625,-4.1358642578125,15.1829833984375,-6.4254150390625,22.763671875,-6.4254150390625c9.79547119140625,0,18.98065185546875,3.8228759765625,25.236328125,12.4254150390625c16,22,40,80.4200439453125,40,136.00006103515625c0,170,-29.8690185546875,244.324951171875,-116,368c-39,56,-86,95.99996948242188,-142,149.99996948242188c-11.717987060546875,11.300018310546875,-20,19.72100830078125,-20,36c0,33,1,54,2,84c0.444000244140625,13.334014892578125,5.858001708984375,26,20,26c150,0,226,98.67100524902344,226,222.00000762939453c0,193,-99,274.00000762939453,-186,314.00000762939453c-13.070404052734375,6.0088348388671875,-22.102294921875,15.714096069335938,-22.102294921875,29.438980102539062c0,38.75140380859375,4.80572509765625,60.519927978515625,6.102294921875,98.56100463867188c0.2730712890625,8.009735107421875,0.410308837890625,15.915924072265625,0.410308837890625,23.711395263671875c0,163.49603271484375,-60.373199462890625,278.28900146484375,-194.4099884033203,278.28900146484375c-144,0,-222.00000190734863,-62,-222.00000190734863,-172c0,-71.16897583007812,58.65620231628418,-128,130.00000190734863,-128c56,0,108,46,108,106c0,57.1400146484375,-48,112,-94,112c-11.231704711914062,0,-17.436004638671875,5.48931884765625,-17.436004638671875,12.96759033203125c0,15.97705078125,28.319198608398438,41.03240966796875,96.43600463867188,41.03240966796875c98.06941223144531,0,159.68199157714844,-96.0338134765625,159.68199157714844,-242.822998046875c0,-8.8768310546875,-0.225311279296875,-17.939300537109375,-0.6815185546875,-27.177398681640625c-1.813995361328125,-36.7249755859375,-5,-59,-12,-94c-1.92388916015625,-9.61944580078125,-17.1929931640625,-22.059295654296875,-27.768096923828125,-22.059295654296875c-0.4183349609375,0,-0.829345703125,0.01947021484375,-1.23187255859375,0.059356689453125c-10.8939208984375,1.0791015625,-21.52630615234375,1.6046905517578125,-31.895416259765625,1.6046905517578125c-191.41899871826172,0,-293.1049977148359,-179.11500549316406,-293.1049977148359,-361.6050033569336ZM384,186c0.9072265625,11.409500122070312,12.166107177734375,20.627395629882812,23.293212890625,20.627395629882812c2.621978759765625,0,5.23663330078125,-0.5118255615234375,7.706787109375,-1.62744140625c59.948699951171875,-27.0736083984375,129.24700927734375,-107.43800354003906,129.24700927734375,-199.50900411605835c0,-3.1505000591278076,-0.08111572265625,-6.314710080623627,-0.24658203125,-9.490960121154785c-4,-80.99999761581421,-73,-147.00000524520874,-147,-147.00000524520874c-14.701171875,0,-35.025390625,1.9124603271484375,-35.025390625,16.118408203125c0,122.45599746704102,15.08819580078125,233.6709976196289,22.025390625,320.8820037841797ZM296,-374c-39,15,-228,192,-228,331c0,143.4810028076172,70.28450012207031,265.0540008544922,253.29098510742188,265.0540008544922c2.219879150390625,0,4.45635986328125,-0.01788330078125,6.70947265625,-0.0538482666015625c11.843780517578125,-0.188873291015625,23.009185791015625,-7.754302978515625,23.009185791015625,-18.465087890625c0,-115.89801025390625,-10.02728271484375,-196.81000661849976,-18.009185791015625,-318.5350036621094c-0.727081298828125,-11.083404541015625,-11.549407958984375,-19.943405151367188,-22.197601318359375,-19.943405151367188c-1.961456298828125,0,-3.916961669921875,0.300628662109375,-5.802398681640625,0.943359375c-44,15.000007629394531,-92,74.00000762939453,-92,118.00000667572021c0,46.999999046325684,11,73.00000095367432,25,114.00000095367432c1.0345916748046875,3.0300827026367188,1.4908294677734375,5.7107086181640625,1.4908294677734375,8.014106750488281c0,6.3818511962890625,-3.502227783203125,9.868034362792969,-7.909454345703125,9.868034362792969c-3.73760986328125,0,-8.126129150390625,-2.50732421875,-11.581390380859375,-7.88214111328125c-22.406997680664062,-34.855499267578125,-38,-86.00000190734863,-38,-125.00000190734863c0,-113.78000450134277,21,-218.0000057220459,119.99998474121094,-265.99999046325684c10.58251953125,-5.131622314453125,20.02142333984375,-10.35699462890625,20.02142333984375,-22.027496337890625c0,-23.17730712890625,-0.013885498046875,-39.894012451171875,-2.021392822265625,-60.972503662109375c-0.933990478515625,-9.805023193359375,-14.6669921875,-11.589996337890625,-24,-8ZM358,-634c54,-46,84,-71,143,-126c56.0989990234375,-52.2960205078125,103,-129,103,-169c0,-28.15997314453125,-4.53997802734375,-65,-33,-65c-37,0,-138.4530029296875,94.75897216796875,-175,139c-38,46,-55,135,-57,211c-0.212005615234375,8.05999755859375,12.451995849609375,15.5780029296875,19,10Z")
    ;; Dynamic
    ("f" . "M-65,26c23.195301055908203,0,40,25,40,45c0,31,-19,31,-19,43c0,8,9.9375,9,18,9c38,0,90,-231,136,-383c3,-9,-7.753997802734375,-17,-16,-17l-58,0c-5.435550689697266,0,-8.232419967651367,-7.89544677734375,-8.232419967651367,-15.717193603515625c0,-7.67694091796875,2.6942901611328125,-15.282806396484375,8.232419967651367,-15.282806396484375l66,0c16,0,23.637001037597656,-8.644012451171875,29,-20c34.003997802734375,-71.99798583984375,106,-173,186,-173c46.8179931640625,0,90,23,90,79c0,29.4110107421875,-15,61,-46,61s-49,-11,-49,-41c0,-31,20,-29,20,-41c0,-6,-8.59698486328125,-8,-15,-8c-31,0,-43,64,-56,112c-1.990997314453125,7.35101318359375,2.753997802734375,17,11,17l53,0c7.143646240234375,0,10.619110107421875,9.5687255859375,10.619110107421875,19.226806640625c0,9.840179443359375,-3.607696533203125,19.773193359375,-10.619110107421875,19.773193359375l-70,0c-11.180999755859375,0,-20.958999633789062,5.240997314453125,-24,16c-26,92,-54.95500183105469,148.02100372314453,-90,223c-43,92,-117,201,-182,201c-56,0,-81,-32,-81,-74c0,-31.575199127197266,25,-57,57,-57Z")))

;; (defparameter *haydn-11-bcr*
;;   `(("noteheads.s0" . ,(make-bcr :bottom 141 :height 282 :left 0 :right 441
;; 				 :top -141 :width 441 :x 0 :y -141))
;;     ("noteheads.s1" . ,(make-bcr :bottom 156 :height 312 :left 0
;; 				 :right 333 :top -156 :width 333 :x 0 :y -156))
;;     ("noteheads.s2" . ,(make-bcr :bottom 139 :height 296 :left 0
;; 				 :right 300 :top -157 :width 300 :x 0 :y -157))
;;     ("clefs.C" . ,(make-bcr :bottom 504.5333557128906
;;     			    :height 1007.933349609375
;; 			    :left -2.6666717529296875
;;     			    :right 669.0166778564453
;; 			    :top -503.3999938964844
;; 			    :width 671.683349609375
;; 			    :x -2.6666717529296875
;; 			    :y -503.3999938964844
;; 			    ))
;;     ("clefs.F" . ,(make-bcr :bottom 579.2833251953125
;; 			    :height 829.2833251953125
;; 			    :left -84.16667175292969
;; 			    :right 699.0166778564453
;; 			    :top -250
;; 			    :width 783.183349609375
;; 			    :x -84.16667175292969
;; 			    :y -250
;; 			    ))
;;     ("clefs.G" . ,(make-bcr :bottom 664.0167236328125
;; 			    :height 1778.4666748046875
;; 			    :left -16
;; 			    :right 632.0166625976562
;; 			    :top -1114.449951171875
;; 			    :width 648.0166625976562
;; 			    :x -16
;; 			    :y -1114.449951171875
;; 			    ))
;;     ("f" . ,(make-bcr :bottom 157
;; 		      :height 658
;; 		      :left -122
;; 		      :right 407
;; 		      :top -501
;; 		      :width 529
;; 		      :x -122
;; 		      :y -501))))

;; (loop for (x . b) in *haydn-11-bcr*
;;       always (and (= (bcr-x b) (bcr-left b))
;; 		  (= (bcr-top b) (bcr-y b))
;; 		  (= (bcr-width b) (- (bcr-right b) (bcr-left b)))
;; 		  (= (bcr-height b) (- (bcr-bottom b) (bcr-top b)))
;; 		  ))


;;;;;;;;;;;;;;;;;;;;;;;;;; BEETHOVEN ;;;;;;;;;;;;;;;;;;;;;
;;; https://common-lisp.net/project/cxml/quickstart.html
(defun resolver (pubid sysid)
  (declare (ignore pubid sysid))
  (flexi-streams:make-in-memory-input-stream nil))
;; (pathname-name "/tmp/")
;;; uninstall-font
(defun uninstall-font (font-name)
  (delete font-name .installed-fonts.)
  (remhash font-name *fonts-hash-table*))

(defun install-font (srcpath &optional (vsrg 'clefs.c) (update-current-font t))
  (setf *vertical-space-reference-glyph* vsrg)
  ;; Prepare data
  (let* ((font-name (pathname-name srcpath))
	 (font-sym (intern (string-upcase font-name)))
	 (exportpath (format nil "/tmp/~AEXPORT/" font-name))
	 (bboxpath (format nil "/tmp/~A~A" font-name (string (gensym "BBOX")))))    
    (when update-current-font (setf *font* font-sym))
    (ensure-directories-exist exportpath)
    (sb-ext:run-program
     "/usr/bin/fontforge"
     (list "-script" "/home/amir/Work/Lisp/smt/fontinstprep.ff"
    	   srcpath
	   ;; $2 Where glyphs are exported (directory)
	   (namestring exportpath)
	   ;; $3 Where bboxes are written to (file),
	   ;; this file will be created by Fontforge.
	   bboxpath)
     :output *standard-output*)
    (setf (gethash font-sym *fonts-hash-table*) (make-hash-table))
    (let ((xx
	    ;; List name ,@bbox
	    (with-open-file (bbox bboxpath)
    	      (loop for ln = (read-line bbox nil)
    		    for trimln = (string-trim '(#\Space) ln)
    		    for lst = (split-sequence:split-sequence
    			       #\Space trimln)
    		    while (and ln (not (string= (car lst) "")))
		    ;; Keep the name of the glyph as string
    		    collect (list (car lst) (mapcar #'read-from-string lst)))
    	      )))
      (loop for (str (name minx miny maxx maxy w h)) in xx
	    for d = (second (second (second (fourth (fourth (cxml:parse-file (format nil "~A~A.svg" exportpath str)
		      							     (cxml-xmls:make-xmls-builder)
									     :entity-resolver #'resolver))))))
	    do (setf (gethash (intern (string-upcase name))
			      (gethash (intern (string-upcase font-name)) *fonts-hash-table*))
		     (list name (list :x minx :left minx
				      :y miny :top miny
				      :right maxx :bottom maxy
				      :width w :height h) d))
	    )
      )
    )
  )

(defun fontht (&optional (font *font*))
  (gethash font *fonts-hash-table*))
(defun font-chars (&optional (font *font*))
  (alexandria:hash-table-keys (fontht font)))
(defun glyph-present-p (mchar-name &optional (font *font*))
  (find mchar-name (font-chars font)))
;;;;;;;;;;;;;;;;;;
;;; Font müssen alle geladen sein!
(defun mchard (glyph-name &optional (font *font*))
  (third (gethash glyph-name (fontht font))))


;; (bcr-height (second (assoc ".notdef" *bravura* :test #'string=)))
;; (defun mchar-path-d (mchar-code family)
;;   (ecase family
;;     (:haydn-11 (cdr (assoc mchar-code *haydn-11-paths* :test #'string=)))))

(defun glyph-bbox (glyph-name &optional (font *font*))
  (second (gethash glyph-name (fontht font))))


;; (defun bbx (bb) (getf  'x))
;; (defun get-bcr (mchar-code family)
;;   (ecase family
;;     (:haydn-11 (cdr (assoc mchar-code *haydn-11-bcr* :test #'string=)))))

;; (defun mchar-codes (family)
;;   "code = class.label"
;;   (mapcar #'car (ecase family (:haydn-11 *haydn-11-paths*))))

;; (defun mchar-labels (class &optional (family *font*))
;;   (let ((classlen (length class)))
;;     (mapcar
;;      #'(lambda (name) (second (split-sequence:split-sequence #\. name)))
;;      (remove-if-not #'(lambda (s) (string= class s :end2 classlen))
;; 		    (remove-if #'(lambda (s) (<= (length s) classlen)) (mchar-codes family))))))

;; (defun mchar-label->mchar-code (label class family)
;;   (assert (member label (mchar-labels class family) :test #'string=)
;; 	  (label)
;;   	  "Invalid label ~A for ~A!" label class)
;;   (format nil "~A.~A" class label))

;; (mchar-labels "noteheads" )
;; (mchar-label->mchar-code "C" "clefs" :haydn-11)



