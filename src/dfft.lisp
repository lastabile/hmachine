
(rule
 (name dfft-rule-zero)
 (root-var ?x)
 (pred
  (?x dfft ?y)
  (?x level 0))
 (add
  (print dfft-rule-zero ?x ?y ?r)
  (?x d ?y)	;; display-connection
  ))

(rule
 (name dfft-rule)
 (root-var ?x)
 (pred
  (?x dfft ?y)
  (?x level ?l)
  (?x color ?c)
  (?c next-color ?cl)
  (?cl next-color ?cr)
  (?cr next-color ?cs)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (?nn3 new-node sn3)
  (?nn4 new-node sn4))
 (add
  (print dfft-rule ?x ?y ?l ?c ?cl ?cr ?cs)
  (?x even ?nn1)
  (?x odd ?nn2)
  (?nn1 oe ?x)
  (?nn2 oe ?x)
  (?nn1 oev 0)
  (?nn2 oev 1)
  (?nn1 dfft ?nn3)
  (?nn2 dfft ?nn4)
  (?nn3 ?nn4 dfft-comb ?y)
  (?nn1 level ?l1)
  (?nn2 level ?l1)
  (?nn1 color ?cl)
  (?nn2 color ?cr)
  (?y fcn-color ?cs)))

(rule
 (name dfft-rule-delta3)
 (pred
  (?x level 6)
  (?x even ?y)
  (?y even ?z)
  (?z level 4)
  (?z dfft ?t))
 (add
  (print dfft-rule-delta3 ?this-obj ?x ?y ?z ?t)
  (?z delta3 ?t)
  (?t inv-delta3 ?z)))

(rule
 (name dfft-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name dfft-data))
 (add
  (print dfft-data)
  (dfft-comb two-input-op))
(del
 (global-node rule ?this-rule)))