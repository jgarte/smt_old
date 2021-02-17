(in-package :smt-docs)

(register-page "Prelude"
  (:header (:h1 "Prelude"))
  (:header (:h2 "Music Engraving"))
  (:p "Until the early 1990s, most of the large music publishing houses were involving practiced and experienced master craftsmen who did all the engraving work merely by hand. Although the developement of computer systems for writing \textit{low-budget} musical scores in the course of the last 40 years has completely outpaced this unique art and craft and made it obsolete by virtue of their comparatively low costs and higher speed of manufacturing printed music, the beauty and the quality of the hand-engraved music scores of old masters still remains unrivaled.
Historically, before computers were in widespread use in music typesetting, the main practical methods for the production of musical scores could roughly be divided into the following categories: music engraving, auto-typography, by using transfers, by using note-typewriters and by using stencils. As perhaps the most widespread music print technique and the oldest craftsmanship of it's kind, music engraving met fairly small changes over centuries only in it's fabrication of steel-stamps and precision-instruments for the engraver. Although other experiments in the field of music typesetting could find their place in the industry from time to time\footnote{For instance Johann Gottlob Immanuel Breitkopf's invention of dissasembling note-heads, stems, staff-lines, flags and other parts of music-notation into many different pieces, to mention only a prominent example out of many.}, music engraving could assert itself as the main procedure in the production of high-quality musical print.")
  ;; 
  (:header (:h2 "The Workflow of the Master Engraver"))
  (:p
   "As perhaps the most influential of the music printing techniques, music engraving was around for about 200 years until the late 20th century. The production of a single engraving plate for a music of moderate complexity level, would often take a whole working day, and basically took place in two stages: hammering of types (note-heads, stem-flags, accidentals, rests, text etc.) into the plate and subsequently \textit{engraving} all the remaining parts such as stems, ledger lines, legato bow, beams etc.. But before the master engraver could hammer the first symbol into his plate, a comprehensive work should be done, to divide the manuscript into pages and lines in an optimal way, suited best to the content and the character of the music being engraved. In the course of dividing the manuscript, the number of bars on each line\footnote{Speaking here of rather traditional western music which almost always uses bars as it's \textit{unit} of division.}, and the number of lines to be fitted into each page should be investigated, whereas the convenient page-turning spots and the overall volume of the edition had to be taken into consideration. Hence, it was important for the success of an edition, to settle the content of each page beforehand. The next stage, the division of heights, consisted of a precise calculation of the gaps between the musical systems. Although these spacings oftentimes would be quite varying, it was the artistry of the master engraver, to simulate a visual consistency and to evoke a visual harmonic illusion by means of optical adjustments. Only after this time-consuming preperations, could the five staff-lines be engraved into the plate via the so-called rastrum.")
  (:header (:h2 "Computer Music-Typesetting Systems"))
  (:p "The profession of music engraving was learned and handed on to next generations only by doing and practicing it. It's beauty and elegance resulted solely from the judgement of the engraver, his craftsmanship and his experience. Little to no sources does exist which would describe the finest case-related and the overall rules of engraving. These of course make implementing a computer program, which could compete with the work of a master engraver a non-trivial challenge!
The computer programs which have been developed for music notation fall into one of the two categories: 1- those with graphical user-interfaces and 2- those who expect input from users in a textual form. These two paradigms exhibit some substantial differences in the way they allow expressing music. Although the first paradigm (also known as \textit{What You See Is What You Get}; WYSIWYG) presents a more intuitive interface, it offers less flexibility in terms of changing the behavior of the system if needed or desirable. Organizing and managing data for bigger projects is basically not left to user's own discretion. Also non-physical storing of the resources (e.g. on the cloud) which is a vital aspect in many fields of musical research, musicology etc. when not happening in another text-based form (for instance in markup langauges and alike)
is mostly cumbersome. The other paradigm (which I call \textit{What You Read Is What You Get} or WYRIWYG, to keep up with the style of word composition of the previous paradigm) could give more control (possibly down to the kernel of the system) for adjusting or changing the behavior of the system.
With WYRIWYG comes an important aspect with which we are not confronted when only clicking on the mouse- or a midi-instrument-key for inputing
music (WYSIWYG). As the WYRIWYG paradigm bears some resemblence to a programming environment (or a markup langauge\footnote{The music notation software LilyPond for instance, having an integrated scheme interpreter as interface to it's parser/compiler toolchain, is a markup language with support for doing some programming tasks in the scheme programming language.}), it is inevitable for it's design to take the expressiveness of the language into account. As (text-based) computer systems (such as WYRIWYG music notation software) grow in complexity, the importance of the level of \textit{expressiveness} the system offers, becomes more and more relevant.
Also when it comes to particular notation fields (e.g. percussion-notation and alike), almost all notation software treat this sector as a special case which involves introducing new syntactic structures for them. While legitimate to treat some \textit{already known} special cases like percussion-notation, ancient mensural-notation, world music, extended-techniques in contemporary music etc., there could potentially be an unlimited range of individual requests and needs, where the users might want to construct and introduce their own notational concepts and systematics. The next lines of this report shall give a preliminary understanding of some of the design aspects and the philosophy of SMT.
")
  ;;
  (:header (:h2 "Why Another Score-Writer?"))
  (:p "Innovative ideas and complexity of music notation in the works of composers of the last and current centuries have posed a great challenge to digital notation systems in both technical as well as philosophical terms.
  Different composers have conceived and implemented different notation systems for their own work, partly quite distant from the established and conventional concepts we know from the Common Western Music Notation; from graphical and spatial notations of the members of the New York School or artfully rendered scores of George Crumb, attempts for transferring electronic music to paper by Stockhausen, Xenakis and Ligeti to even music not conceived for publication in form of printed material e.g. studies of Conlon Nancarrow for mechanical piano or even improvisational music to only name a few.
  Although in most cases composers, researchers and publishers snatch at the paradigmes of the Common Western Music Notation, through it's modular and rule-based design, SMT promotes re-thinking of notational concepts as well as conceiving new alternatives which might suit better to certian types of music.
  The challenge posed to notation systems basically boil down to the question whether different forms and types of music notation (including those unknown to the authors of the systems!) are expressible in the system, hence is the system extensible \textit{enough} and whether it allows for defining and accommodating new notations just as the established conventions of western music notation.
  Many music typesetting systems provide their users with some possibilities for extending the system's behavior (e.g. the ManuScript language for writing plug-ins in Sibelius or the Guile Scheme for further interaction with the LilyPond's parser-compiler toolchain), oftentimes these extensions however can't go beyond what these systems \textit{already} define and understand as music notation (namely classical western music notation), which leads to the modifications the user has to undertake in his/her notational concepts for them to fit within the scope of the system's definition of music! SMT is also an attempt for compensating some shrotcomings in this respect.
  
  As SMT is still under active development, this report strives for giving an overview of some of it's design aspects accompanied by some examples. Since this report describes a work-in-progess, the author acknowledges also that some of the aspects of the engine addressed in this report are subject to change in the future.
")
  ;; 

  )
