

(rule
 (name tree-next-level0-rule)
 (local)
 (pred
  (?x0 l 0)
  (?x1 l 0)
  (?x0 tree-next ?x1))
 (add
  (print tree-next-level0-rule ?this-obj ?x0 ?x1)
  (?x0 next ?x1)
  )
)

(rule
 (name tree-next-rule)
 (local)
 (root-var ?p1)
 (pred
  (?x00 ul ?p0)
  (?x01 ur ?p0)
  (?x10 ul ?p1)
  (?x11 ur ?p1)
  (?x00 tree-next ?x01)
  (?x10 tree-next ?x11)
  (?p0 tree-next ?p1))
 (add
  (print tree-next-rule ?root-var ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
  (?x01 tree-next ?x10)
  )
)

(rule
 (name tree-loop-rule)
 (local)
 (pred
  (?x l 0)
  (?y l 0)
  (?yprev next ?y)		;; This makes this rule run last
  (?x zero-max-span ?y)
  )
 (add
  (print tree-loop-rule ?this-obj ?x ?y ?root-var)
  (?y next ?x)
  )
)

(rule
 (name tree-span-rule)
 (local)
 (pred
  (?xu zero-max-span ?yu)
  (?xu zero)
  (?yu max)
  (?x ul ?xu)
  (?y ur ?yu)
  (?x zero)
  (?y max)
  )
 (add
  (print tree-span-rule ?x ?xu ?y ?yu)
  (?x zero-max-span ?y)
  )
)

(rule
 (name tree-top-order-rule)
 (local)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p top)
  )
 (add
  (print tree-top-order-rule ?this-obj ?x ?y ?p)
  (?x top ?p)
  (?y top ?p)
  (?x zero)
  (?y max)
  (?x zero-max-span ?y)
  )
)

(rule
 (name tree-zero-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p zero)
  )
 (add
  (print tree-zero-rule ?this-obj ?x ?y ?p)
  (?x zero)
  )
)

(rule
 (name tree-max-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p max)
  )
 (add
  (print tree-max-rule ?this-obj ?x ?y ?p)
  (?y max)
  )
 )

(rule
 (name tree-rule)
 (local)
 (pred
  (?x l ?l)
  (?l1 sigma ?l)
  (?x top ?t)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2))
 (add
  (print tree-rule ?this-obj ?x ?nn1 ?nn2 ?l)
  (?nn1 aup ?x)
  (?nn1 ul ?x)	;; ul = up-left
  (?nn1 l ?l1)
  (?nn1 top ?t)
  (?nn2 aup ?x)
  (?nn2 ur ?x)	;; ur = up-right
  (?nn2 l ?l1)
  (?nn2 top ?t)
  (?nn1 tree-next ?nn2)
  ))

(rule
 (name tree-top-rule)
 (attach-to tree-top)
 (pred
  (tree-top top ?x levels ?l))
 (add
  (print tree-top-rule ?x ?l)
  (?x l ?l)
  (?x top)
  (?x top ?x)
  ))


;; Local Variables:
;; eval: (emacs-file-locals)
;; End:
