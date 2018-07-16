




(comment

(rule
 (name fwd-fe-rule)
 (local)
 (root-var ?n)
 (pred
  (?n sigma ?m)
  (?n even-func ?o)
  (?o sigma ?p)
  (?p sigma ?q)
  (?n fwd-fe-rule ?fwd-fe-rule)
  (?n copy-rule-rule ?copy-rule-rule)
  (?n copy-rule-rule-params-pred ?copy-rule-rule-pred)
  (?n copy-rule-rule-params-add ?copy-rule-rule-add)
  (?nn-fwd-fe-rule new-node sn1)
  (?nn-copy-rule-rule new-node sn2)
  (?nnp new-node sn3)
  (?nna new-node sn4))
 (add
  ;; (print fwd-fe-rule ?this-rule ?n ?m even-func ?q ?fwd-fe-rule ?copy-rule-rule ?nn1 ?nn2)
  (print "fwd-fe-rule" ?fwd-fe-rule)
  (print "?this-rule" ?this-rule)
  (print "?n" ?n)
  (print "?m" ?m)
  (print "even-func" even-func)
  (print "?q" ?q)
  (print "?fwd-fe-rule" ?fwd-fe-rule)
  (print "?copy-rule-rule" ?copy-rule-rule)
  (print "?nn-fwd-fe-rule" ?nn-fwd-fe-rule)
  (print "?nn-copy-rule-rule" ?nn-copy-rule-rule)
  (?m even-func ?q)

  (?fwd-fe-rule copy-rule ?nn-fwd-fe-rule)
  (?fwd-fe-rule copy-rule-rule-params-pred ?copy-rule-rule-pred)
  (?fwd-fe-rule copy-rule-rule-params-add ?copy-rule-rule-add)

  (?copy-rule-rule copy-rule ?nn-copy-rule-rule)
  (?copy-rule-rule copy-rule-rule-params-pred ?copy-rule-rule-pred)
  (?copy-rule-rule copy-rule-rule-params-add ?copy-rule-rule-add)

  (?copy-rule-rule-pred copy-rule ?nnp)
  (?copy-rule-rule-add copy-rule ?nna)

  (?nna rule ?copy-rule-rule)
  (?nnp rule ?copy-rule-rule)

  (?nn-copy-rule-rule copy-rule-rule-params-pred ?nnp)
  (?nn-copy-rule-rule copy-rule-rule-params-add ?nna)

  ;; (?copy-rule-rule-pred copy-rule-rule-params-pred ?copy-rule-rule-pred)
  ;; (?copy-rule-rule-pred copy-rule-rule-params-add ?copy-rule-rule-add)

  (?copy-rule-rule-add copy-rule-rule-params-pred ?copy-rule-rule-pred)
  (?copy-rule-rule-add copy-rule-rule-params-add ?copy-rule-rule-add)

  (?nna copy-rule-rule-params-pred ?copy-rule-rule-pred)
  (?nna copy-rule-rule-params-add ?copy-rule-rule-add)

  (?nnp copy-rule-rule-params-pred ?copy-rule-rule-pred)
  (?nnp copy-rule-rule-params-add ?copy-rule-rule-add)
  
  (?nn-fwd-fe-rule rule ?nn-copy-rule-rule)
  (?nn-fwd-fe-rule copy-rule-rule-params-pred ?nnp)
  (?nn-fwd-fe-rule copy-rule-rule-params-add ?nna)

  (?nn-copy-rule-rule rule ?copy-rule-rule)

  (?m rule ?nn-fwd-fe-rule)
  (?m fwd-fe-rule ?nn-fwd-fe-rule)
  (?m copy-rule-rule ?nn-copy-rule-rule)
  (?m copy-rule-rule-params-pred ?nnp)
  (?m copy-rule-rule-params-add ?nna)))


(rule
 (name fe-0-rule)
 (local)
 (pred
  (0 sigma 1)
  (0 local-rule-pool ?p)
  (?p lrp-rule ?fwd-fe-rule)
  (?fwd-fe-rule name fwd-fe-rule)
  (?p lrp-rule ?copy-rule-rule)
  (?copy-rule-rule name copy-rule-rule) ;; x-copy-rule-rule !!!!!!!
  (?p lrp-rule ?copy-rule-rule-pred)
  (?copy-rule-rule-pred name copy-rule-rule-pred)
  (?p lrp-rule ?copy-rule-rule-add)
  (?copy-rule-rule-add name copy-rule-rule-add))
 (add
  (print fe-0-rule 0)
  (0 even-func 2)
  (0 fwd-fe-rule ?fwd-fe-rule)
  (0 copy-rule-rule ?copy-rule-rule)
  (0 copy-rule-rule-params-pred ?copy-rule-rule-pred)
  (0 copy-rule-rule-params-add ?copy-rule-rule-add)
  (0 rule ?fwd-fe-rule)))

)


;; (comment

(rule
 (name fwd-fe-rule)
 (local)
 (root-var ?n)
 (pred
  (?n sigma ?m)
  (?n even-func ?o)
  (?o sigma ?p)
  (?p sigma ?q)
  (?n fwd-fe-rule ?fwd-fe-rule)
  (?n copy-rule-rule ?copy-rule-rule)
  (?nn-fwd-fe-rule new-node sn1)
  (?nn-copy-rule-rule new-node sn2))
 (add
  (print fwd-fe-rule ?this-rule ?n ?m even-func ?q ?fwd-fe-rule ?copy-rule-rule ?nn-fwd-fe-rule ?nn-copy-rule-rule)
#|
  (print "fwd-fe-rule" fwd-fe-rule)
  (print "?this-rule" ?this-rule)
  (print "?n" ?n)
  (print "?m" ?m)
  (print "even-func" even-func)
  (print "?q" ?q)
  (print "?fwd-fe-rule" ?fwd-fe-rule)
  (print "?copy-rule-rule" ?copy-rule-rule)
  (print "?nn-fwd-fe-rule" ?nn-fwd-fe-rule)
  (print "?nn-copy-rule-rule" ?nn-copy-rule-rule)
|#
  (?m even-func ?q)
  (?fwd-fe-rule copy-rule ?nn-fwd-fe-rule)
  (?copy-rule-rule copy-rule ?nn-copy-rule-rule)
  (?nn-fwd-fe-rule rule ?nn-copy-rule-rule)
  (?nn-copy-rule-rule rule ?copy-rule-rule)
  (?m rule ?nn-fwd-fe-rule)
  (?m fwd-fe-rule ?nn-fwd-fe-rule)
  (?m copy-rule-rule ?nn-copy-rule-rule)))

(rule
 (name fe-0-rule)
 (local)
 (pred
  (0 sigma 1)
  (0 local-rule-pool ?p)
  (?p lrp-rule ?fwd-fe-rule)
  (?fwd-fe-rule name fwd-fe-rule)
  (?p lrp-rule ?copy-rule-rule)
  (?copy-rule-rule name copy-rule-rule))
 (add
  (print fe-0-rule 0)
  (0 even-func 2)
  (0 fwd-fe-rule ?fwd-fe-rule)
  (0 copy-rule-rule ?copy-rule-rule)
  (0 rule ?fwd-fe-rule)))

;; )













(comment
(rule
 (name fwd-fe-rule)
 (local)
 (pred
  (?n sigma ?m)
  (?n even-func ?o)
  (?o sigma ?p)
  (?p sigma ?q))
 (add
  (print fwd-fe-rule ?this-rule ?m even-func ?q)
  (?m even-func ?q)))
)



(comment

(rule
 (name fwd-fe-rule-gen)
 (local)
 (pred
  (?x sigma ?y)
  (?x even-func ?z)
  (?x fe ?t)
  (?nn1 new-node sn1)
  (?x local-rule-pool ?p)
  (?p lrp-rule ?fwd-fe-rule)
  (?p lrp-rule ?copy-rule-rule)
  (?p lrp-rule ?fwd-fe-rule-gen2)
  (?fwd-fe-rule name fwd-fe-rule)
  (?copy-rule-rule name copy-rule-rule)
  (?fwd-fe-rule-gen2 name fwd-fe-rule-gen2))
 (del
  ;; (?x rule ?this-rule)
  (?x fe ?t))
 (add
  (print fwd-fe-rule-gen ?x ?nn1)

  (?fwd-fe-rule copy-rule ?nn1)
  (?fwd-fe-rule rule ?copy-rule-rule)
  (?y rule ?this-rule)
  (?y rule ?fwd-fe-rule-gen2)
  (fwd-fe-rule-gen2 ?y ?nn1)))

(rule
 (name fwd-fe-rule-gen2)
 (local)
 (pred
  (fwd-fe-rule-gen2 ?y ?nn1))
 (add
  (print fwd-fe-rule-gen2 ?y ?nn1)
  (?y rule ?nn1)))

(rule
 (name fe-0-rule)
 (local)
 (root-var 0)
 (pred
  (0 sigma 1)
  (0 local-rule-pool ?p)
  (?p lrp-rule ?fwd-fe-rule-gen)
  (?fwd-fe-rule-gen name fwd-fe-rule-gen))
 (add
  (print fe-0-rule 0)
  (0 even-func 2)
  (0 rule ?fwd-fe-rule-gen)))

#|
(rule
 (name back-fe-rule-gen)
 (root-var ?a)
 (local)
 (pred
  (?c sigma ?a)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?fe-0-rule)
  (?fe-0-rule name fe-0-rule))
 (add
  (print back-fe-rule-gen ?c)
  (?c fe ?x)
  (?c rule ?this-rule)
  (?c rule ?fe-0-rule))
 (del
  ;; (?a rule ?this-rule)
  ))
|#

(rule
 (name back-fe-rule-gen)
 (root-var ?a)
 (local)
 (pred
  (?c sigma ?a)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?fe-0-rule)
  (?fe-0-rule name fe-0-rule)
  (?p lrp-rule ?copy-rule-rule)
  (?copy-rule-rule name copy-rule-rule))
 (add
  (print back-fe-rule-gen ?c)
  (?c fe ?x)
  (?c rule ?this-rule)
  (?fe-0-rule copy-rule ?c)
  (?fe-0-rule rule ?copy-rule-rule))
 (del
  ;; (?a rule ?this-rule)
  ))



(rule
 (name back-fe-rule-gen1)
 (root-var ?a)
 (local)
 (pred
  (?c sigma ?a))
 (add
  (print back-fe-rule-gen1 ?c)
  (?c rule ?this-rule)
  (?c rule (rule
			(name fwd-fe-rule1)
			(root-var ?c)
			(pred
			 (?c sigma ?d)
			 (?c even-func ?e)
			 (?e sigma ?f)
			 (?f sigma ?g))
			(add
			 (print fwd-fe-rule1 ?d even-func ?g)
			 (?d even-func ?g)))))
 (del
  (?a rule ?this-rule)
  ))


;; Never executed -- just for display hacking

(rule
 (name display-fwd-fe-rule)
 (root-var ?m)
 (local)
 (pred
  (?n sigma ?m)
  (?n even-func ?o)
  (?o sigma ?p)
  (?p sigma ?q)
  (?n local-rule-pool ?l)
  (?l lrp-rule ?fwd-fe-rule)
  (?fwd-fe-rule name fwd-fe-rule)
  (?n rule ?back-fe-rule-gen)
  (?back-fe-rule-gen name back-fe-rule-gen))
(add
  (print fwd-fe-rule ?m even-func ?q)
  (?m even-func ?q)
  (?q rule ?fwd-fe-rule)))

(rule
 (name display-back-fe-rule-gen)
 (root-var ?a)
 (local)
 (pred
  (?c sigma ?a)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?fe-0-rule)
  (?fe-0-rule name fe-0-rule)
  (?p lrp-rule ?back-fe-rule-gen)
  (?back-fe-rule-gen name back-fe-rule-gen))
 (add
  (print back-fe-rule-gen ?c)
  (?c rule ?back-fe-rule-gen)
  (?c rule ?fe-0-rule))
 (del
  ;; (?a rule ?back-fe-rule-gen)
  ))

)