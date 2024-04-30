
;; This never runs, as we have attached the relevant rules to constants below.

(comment
(rule
 (name fft-delta-init)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?fft-rule)
  (?p lrp-rule ?fft-rule-delta2)
  (?p lrp-rule ?fft-rule-delta3)
  (?fft-rule name fft-rule)
  (?fft-rule-delta2 name fft-rule-delta2)
  (?fft-rule-delta3 name fft-rule-delta3))
 (add
  (print fft-delta-init)
  (?fft-rule add ?x rule ?fft-rule-delta2)
  (?fft-rule add ?x rule ?fft-rule-delta3)
  )
 (del
  (?this-obj rule ?this-rule)))
)

(rule
 (name fft-rule-delta2)
 (attach-to fft)
 (attach-to color)
 (attach-to next-color)
 ;; (root-var ?x)
 (pred
  (?x fft ?y)
  (?x color ?c)
  (?c next-color ?s)
  (?s next-color ?t))
 (add
  (print fft-rule-delta2 ?this-obj ?y)
  (?y fcn-color ?s)
  (?y color ?t)
  )
 (del
  ;; (?this-obj rule ?this-rule)
  ))

(rule
 (name fft-rule-delta3)
 (attach-to color)
 (attach-to fcn-color)
 (attach-to rand)
 (attach-to rule30val)
 (pred
  (?x color ?c)
  (?y fcn-color ?c)
  (?x rand ?r)
  (?r rule30val 0))
 (add
  (print fft-rule-delta3 ?this-obj ?x ?y ?c)
  (?x delta3 ?y)
  (?x delta3-rand ?r)
  (?y inv-delta3 ?x)
  )
 (del
  ;; (?this-obj rule ?this-rule)
  ))

(rule
 (name fft-rule-delta4)		;; Propagate color and rand along weave-next
 (attach-to center-up)		;; Attching here Appears to help most with queuing
 (pred
  (?x weave-next ?y)
  (?x color ?c)
  (?x rand ?r)
  (?r center-up ?u)
  (?c next-color ?s))
 (add
  (print fft-rule-delta4 ?this-obj ?x ?y ?c ?s)
  (?y color ?s)
  (?y rand ?u)
  )
)

(rule
 (name weave-next-rule)
 (attach-to weave-next-root)
 (pred
  (?p0 weave-next-root)
  (?p0 odd ?x00)
  (?p0 even ?x01)
  (?p1 odd ?x10)
  (?p1 even ?x11)
  (?x00 weave-next ?x01)
  (?x10 weave-next ?x11)
  (?p0 weave-next ?p1)
  )
 (add
  (print weave-next-rule ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
  (?x01 weave-next ?x10)
  (?p1 weave-next-root)
  )
 (del
  (?p0 weave-next-root)))

(rule
 (disabled)
 (name weave-next-rule-paste)
 (attach-to weave-next-root)
 (pred
  (?p0 weave-next-root)
  (?p0 odd ?x00)
  (?p0 even ?x01)
  (?p1 odd ?x10)
  (?p1 even ?x11)
  (a wn b) ;; (?x00 weave-next ?x01)
  (?x10 weave-next ?x11)
  (?p0 weave-next ?p1)
  )
 (add
  (print weave-next-rule ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
  (a wn b) ;; (?x01 weave-next ?x10)
  (?p1 weave-next-root)
  )
 (del
  (?p0 weave-next-root)))
