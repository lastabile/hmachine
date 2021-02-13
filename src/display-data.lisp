

(rule
 (name display-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name display-data))
 (add
  (print display-data)
  ;; (fft color green)
  ;; (fft in-node-color springgreen)
  ;; (fft out-node-color darkturquoise)
  (zero color violet)
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
  (style gv-attr)
  (width gv-attr)
  (fixedsize gv-attr)

  ;; (rule shape rectangle)
  ;; (rule color mistyrose)
  ;; (rule style filled)

  ;; (fwd-fe-rule color paleturquoise)

  (+ two-input-op))
 (del
  (global-node rule ?this-rule)))

;; Set color, shape, etc. for arrays and array elements

(rule
 (name array-gv-attr-rule)
 (attach-to is-elem-of)
 (pred
  (?e is-elem-of ?a))
 (add
  (print array-gv-attr-rule ?a)
  (?a shape trapezium)
  (?e shape rectangle)
  (?e color paleturquoise)))

;; Set the color, shape, and label for rules, when they are referred to by data.
	  
(rule
 (name rule-gv-attr-rule)
 (attach-to rule)
 (pred
  (?r type rule)
  (?r name ?n))
 (add
  (print rule-gv-attr-rule ?r ?n)
  (?r shape rectangle)
  (?r color mistyrose)
  (?r style filled)
  (?r label ?n)))

;; These rules set an index number for the array elements and label the nodes with that index number

(rule
 (name index-rule-zero)
 (pred
  (?x zero))
 (add
  (print index-rule-zero ?x)
  (?x index 0)
  (?x label 0)))

(rule
 (name index-rule-fnext)
 (pred
  (?x index ?i)
  (?i sigma ?j)
  (?x fnext ?y))
 (add
  (print index-rule-fnext ?x ?y ?i)
  (?y index ?j)
  (?y label ?j)))



(rule
 (name top-elems-align)
 (pred
  (?x fft-top)
  (?x fft ?y)
  (?e1 is-elem-of ?y)
  (?e2 is-elem-of ?y)
  (?e1 fnext ?e2)
  (?e1r is-elem-of ?x)
  (?e2r is-elem-of ?x)
  (?e1 ref ?e1r)
  (?e2 ref ?e2r)
  (?e1r fnext ?e2r))
 (add
  (print top-elems-align ?x ?e1 ?e2)
  (?e1 gnext ?e2)
  (?e1r gnext ?e2r)))

(rule
 (name top-ref-init)
 (pred
  (?x fft-top)
  (?e is-elem-of ?x)
  (?er ref ?e))
 (add
  (print top-ref-init ?x ?e)
  (?er top-ref ?e)))

(rule
 (name top-ref-propagate)
 (pred
  (?x top-ref ?y)
  (?z ref ?x))
 (add
  (print top-ref-propagate ?x ?y ?z)
  (?z top-ref ?y)))

(rule
 (name bot-top-ref)
 (pred
  (?e1 top-ref ?t1)
  (?e1 next)
  (?e1 is-elem-of ?a1)
  (?a1 level 0)
  (?e2 top-ref ?t2)
  (?e2 next)
  (?e2 is-elem-of ?a2)
  (?a2 level 0)
  (?e1 ?e2 fft-hb ?o))
 (add
  (print bot-top-ref ?e1 ?t1 ?e2 ?t2)
  (?e1 bot-top-ref ?t1)
  (?e2 bot-top-ref ?t2)))

(rule
 (name hnext)
 (pred
  (?e1 bot-top-ref ?r1)
  (?e2 bot-top-ref ?r2)
  (?r1 gnext ?r2))
 (add
  (print hnext ?e1 ?e2 ?r1 ?r2)
  (?e1 hnext ?e2)))

(rule
 (name bot-index)
 (pred
  (?e bot-top-ref ?r)
  (?r index ?i)
  (?e label ?l))
 (del
  (?e label ?l))
 (add
  (print bot-index ?e ?r)
  (?e index ?i)
  (?e label ?i)))
