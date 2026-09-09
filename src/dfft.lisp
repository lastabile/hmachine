
(rule
 (name fft-rule-zero)
 (root-var ?x)
 (pred
  (?x fft ?y)
  (?x level 0)
  )
 (add
  (print fft-rule-zero ?x ?y ?r)
  (?x d ?y)	;; display-connection
  ))

(rule
 (name fft-rule)
 (root-var ?x)
 (pred
  (?x fft ?y)
  (?x level ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (?nn3 new-node sn3)
  (?nn4 new-node sn4))
 (add
  (print fft-rule ?x ?y ?l ?c ?cl ?cr ?cs)
  (?x even ?nn1)
  (?x odd ?nn2)
  (?nn1 oe ?x)
  (?nn2 oe ?x)
  (?nn1 oev 0)
  (?nn2 oev 1)
  (?nn2 weave-next ?nn1)
  (?nn1 fft ?nn3)
  (?nn2 fft ?nn4)
  (?nn3 ?nn4 fft-comb ?y)
  (?nn1 level ?l1)
  (?nn2 level ?l1)
  )
 )

(rule
 (name fft-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name fft-data))
 (add
  (print fft-data)
  (fft-comb two-input-op)
  )
(del
 (global-node rule ?this-rule)))

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
 (name fft-top-rule)
 (attach-to fft-top)
 (pred
  (?x fft-top)
  (?x odd ?y)
  (?x even ?z)
  )
 (add
  (print fft-top-rule ?x ?y ?z)
  (?y weave-next ?z)
  (?x weave-next ?y)
  (?x weave-next-root)
  (?y weave-next-root)
  (?z weave-next-root)
  ))

(rule
 (name fft-color-rule)
 (local)
 (pred
  (?x weave-next ?y)
  (?x fft ?z)
  (?x color ?c)
  (?c next-color ?c1)
  (?c1 next-color ?c2)
  (?c2 next-color ?c3)
  )
 (add
  (print fft-color-rule ?x ?y ?c ?c1)
  (?y color ?c1)
  (?z color ?c2)
  (?z fcn-color ?c3)
  (?y rule ?this-rule)
  )
 (del
  (?this-obj rule ?this-rule)
  )
 )

