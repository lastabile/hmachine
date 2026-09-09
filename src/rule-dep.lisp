


;; Used with edge-trace-rule-graph

(rule
 (name trans-reduction)
 (distinct-vars t)
 (pred
  (?x r ?y)
  (?y r ?z)
  (?x r ?z))
 (add
  (print trans-reduction ?x ?y ?z))
 (del
  (?x r ?z)))
