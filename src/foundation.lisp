

(rule
 (name data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name data))
 (add
  (print data)
  (global-node next-color))
 (del
  (global-node rule ?this-rule)))

;; For extensions, see display-rules.lisp

(rule
 (name basic-display-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name basic-display-data))
 (add
  (print basic-display-data)
  ;; (fft color green)
  ;; (fft in-node-color springgreen)
  ;; (fft out-node-color darkturquoise)
  ;; (zero color seashell)
  (aup in-node-color deeppink)
  (aup out-node-color dodgerblue)
  (aup edge-color springgreen)
  ;; (odd out-node-color palegreen)
  ;; (even out-node-color palevioletred)
  ;; (elem in-node-color  palevioletred)
  (elem out-node-color palevioletred)
  (elem edge-color  darkturquoise)
  (tree-next color dodgerblue)
  (fft-comb two-input-op)
  (fft-comb color lightskyblue)

  (fft-hb two-input-op)
  ;; (fft-hb in-node-color deeppink)
  ;; (fft-hb out-node-color dodgerblue)
  (fft-hb color turquoise)
  (fft-hb shape ellipse)

  (fft-hb-delta two-input-op)
  (fft-hb-delta in-node-color springgreen)
  (fft-hb-delta color turquoise)

  (color gv-attr)
  (shape gv-attr)
  (label gv-attr)
  (fontname gv-attr)
  (fontsize gv-attr)
  (style gv-attr)
  (width gv-attr)
  (height gv-attr)
  (fixedsize gv-attr)

  ;; (fwd-fe-rule color paleturquoise)

  (+ two-input-op))
 (del
  (global-node rule ?this-rule)))

(rule
 (name std-notes)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name std-notes))
 (add
  (print std-notes)
  (note footer "Copyright (c) 2023 Lawrence Stabile"))
 (del
  (global-node rule ?this-rule)))


(rule
 (name gen-inverse)
 (attach-to inverse)
 (root-var inverse)
 (pred
  (?a inverse ?i))
 (add
  (print gen-inverse ?a ?i)
  (?a rule
	  (rule
	   (name (add-inverse ?i))
	   (root-var ?a)
	   (pred
		(?x ?a ?y))
	   (add
		(print add-inverse ?a ?i ?x ?y)
		(?y ?i ?x))))))

(rule
 (name inverse-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name inverse-data))
 (add
  (print inverse-data)
  (member inverse is-member-of)
  (elem inverse is-elem-of))
 (del
  (?this-obj rule ?this-rule)))

(comment
(rule
 (name is-1-param)
 (attach-to is)
 (pred
  (?x is ?y ?p ?v)
  (?y rule ?r))
 (del
  (?x is ?y ?p ?v))
 (add
  (print is-1-param ?this-obj ?x ?y ?r ?p ?v)
  (?x rule ?r)
  (?x ?p ?v)
  ;; (?x from-is-1-param-rule ?r) ;; xxx We're not using these now and they add clutter
  ))

(rule
 (name is-0-param)
 (attach-to is)
 (pred
  (?x is ?y)
  (?y rule ?r))
 (del
  (?x is ?y))
 (add
  (print is-0-param ?this-obj ?x ?y ?r)
  (?x rule ?r)
  ;; (?x from-is-0-param-rule ?r) ;; xxx
))

(rule
 (name is-0-param-xrule)
 (attach-to is)
 (pred
  (?x is ?y)
  (?y xrule ?r))
 (del
  (?x is ?y))
 (add
  (print is-0-param-xrule ?this-obj ?x ?y ?r)
  (?x rule ?r)
  ;; (?x from-is-0-param-xrule-rule ?r) ;; xxx
  ))
)

(comment
 (rule
  (name is-not)
  (attach-to is-not)
  (pred
   (?x is-not ?y)	   ;; Put this...	; ; ; ;
   (?x is ?y)		   ;; ...and detect this ; ; ; ;
   (?y rule ?r)
   (?x rule ?r))
  (add
   (print is-not ?x ?y ?r))
  (del
   (?x is ?y)
   (?x is-not ?y)
   (?x rule ?r)
   (?this-obj rule ?this-rule)))
)

(comment
(rule
 (name is-not-xrule)
 (attach-to is-not)
 (pred
  (?x is-not ?y)
  (?y xrule ?r)
  (?x rule ?r))
 (add
  (print is-not-xrule ?x ?y ?r))
 (del
  (?x is-not ?y)
  (?x rule ?r)))
)


(rule
 (name xis)
 (attach-to xis)
 (pred
  (?x xis ?y)
  (?y has ?*r))
 (add
  (print xis ?x ?y ?*r)
  (?x ?*r))
 (del
  (?x xis ?y)))

(rule
 (name xis-not)
 (attach-to xis-not)
 (pred
  (?x xis-not ?y)
  (?y has ?*r))
 (add
  (print xis-not ?x ?y ?*r))
 (del
  (?x xis ?y)
  (?x xis-not ?y)
  (?x ?*r)))

(rule
 (name color-circle-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name color-circle-data))
 (add
  (print color-circle-data)
  (navajowhite next-color moccasin)
  (moccasin next-color lemonchiffon)
  (lemonchiffon next-color seashell)
  (seashell next-color aliceblue)
  (aliceblue next-color lavender)
  (lavender next-color lavenderblush)
  (lavenderblush next-color mistyrose)
  (mistyrose next-color dodgerblue)
  (dodgerblue next-color deepskyblue)
  (deepskyblue next-color skyblue)
  (skyblue next-color lightskyblue)
  (lightskyblue next-color darkturquoise)
  (darkturquoise next-color mediumturquoise)
  (mediumturquoise next-color turquoise)
  (turquoise next-color cyan)
  (cyan next-color lightseagreen)
  (lightseagreen next-color palegreen)
  (palegreen next-color springgreen)
  (springgreen next-color darkkhaki)
  (darkkhaki next-color khaki)
  (khaki next-color yellow)
  (yellow next-color gold)
  (gold next-color salmon)
  (salmon next-color deeppink)
  (deeppink next-color pink)
  (pink next-color lightpink)
  (lightpink next-color palevioletred)
  (palevioletred next-color mediumvioletred)
  (mediumvioletred next-color violetred)
  (violetred next-color magenta)
  (magenta next-color violet)
  (violet next-color blueviolet)
  (blueviolet next-color purple)
  (purple next-color AntiqueWhite1)
  (AntiqueWhite1 next-color LemonChiffon1)
  (LemonChiffon1 next-color LemonChiffon2)
  (LemonChiffon2 next-color LemonChiffon3)
  (LemonChiffon3 next-color LavenderBlush1)
  (LavenderBlush1 next-color MistyRose2)
  (MistyRose2 next-color MistyRose3)
  (MistyRose3 next-color SlateBlue1)
  (SlateBlue1 next-color blue1)
  (blue1 next-color DodgerBlue3)
  (DodgerBlue3 next-color DeepSkyBlue3)
  (DeepSkyBlue3 next-color LightSkyBlue3)
  (LightSkyBlue3 next-color LightBlue1)
  (LightBlue1 next-color PaleTurquoise1)
  (PaleTurquoise1 next-color turquoise1)
  (turquoise1 next-color SpringGreen1)
  (SpringGreen1 next-color green2)
  (green2 next-color chartreuse2)
  (chartreuse2 next-color khaki1)
  (khaki1 next-color yellow1)
  (yellow1 next-color gold2)
  (gold2 next-color RosyBrown2)
  (RosyBrown2 next-color navajowhite))
 (del
  (global-node rule ?this-rule)))

(rule
 (name color-color)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node next-color)
  (?x next-color ?y))
 (add
  (print color-color ?x)
  (?x color ?x))
 (del
  (?this-obj rule ?this-rule)))


