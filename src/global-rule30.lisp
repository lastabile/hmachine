(rule
 (name rule-30-rule-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule-30-data)
  (rule-30-data ?xval ?yval ?zval ?nval))
 (add
  (print rule-30-rule-gen ?xval ?yval ?zval ?nval)
  (global-rule-pool-node grp-rule
						 (rule
						  (name (rule-30-rule ?xval ?yval ?zval))
						  (root-var ?y)
						  (pred
						   (?y level ?l)
						   (?l1 sigma ?l)
						   (?x next ?y)
						   (?y next ?z)
						   (?y interior)
						   (?x rule30val ?xval)
						   (?y rule30val ?yval)
						   (?z rule30val ?zval)
						   (?nn1 new-node sn1))
						  (add
						   (print rule-30-rule ?x ?y ?z ?nn1 ?xval ?yval ?zval ?nval)
						   (?nn1 up ?y)
						   (?nn1 interior)
						   (?nn1 level ?l1)
						   (?nn1 rule30val ?nval)))))
 (del
  (global-node rule ?this-rule)))

(rule
 (name rule-30-zero-rule-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule-30-data)
  (rule-30-data 0 1 1 ?ncval)
  (rule-30-data 0 0 1 ?nnval))
 (add
  (print rule-30-zero-rule-gen 1 1 ?ncval ?nnval)
  (global-rule-pool-node grp-rule
						 (rule
						  (name (rule-30-zero-rule 1 1))
						  (root-var ?y)
						  (pred
						   (?y zero)
						   (?y level ?l)
						   (?l1 sigma ?l)
						   (?y next ?z)
						   (?y rule30val 1)
						   (?z rule30val 1)
						   (?nn1 new-node sn1)
						   (?nn2 new-node sn2))
						  (add
						   (print rule-30-zero-rule ?y ?z ?nn1 ?nn2 1 1 ?ncval ?nnval)
						   (?nn2 up ?y)
						   (?nn2 interior)
						   (?nn1 level ?l1)
						   (?nn2 level ?l1)
						   (?nn1 next ?nn2)
						   (?nn1 rule-30-next ?nn2)
						   (?nn1 zero)
						   (?nn1 rule30val ?nnval)
						   (?nn2 rule30val ?ncval)))))
 (del
  (global-node rule ?this-rule)
  ))

(rule
 (name rule-30-max-rule-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule-30-data)
  (rule-30-data ?xval 1 0 ?ncval)
  (rule-30-data 1 0 0 ?nnval))
 (add
  (print rule-30-max-rule-gen ?xval 1 ?ncval ?nnval)
  (global-rule-pool-node grp-rule
						 (rule
						  (name (rule-30-max-rule ?xval 1))
						  (root-var ?y)
						  (pred
						   (?y max)
						   (?y level ?l)
						   (?l1 sigma ?l)
						   (?x next ?y)
						   (?x rule30val ?xval)
						   (?y rule30val 1)
						   (?nn1 new-node sn1)
						   (?nn2 new-node sn2))
						  (add
						   (print rule-30-max-rule ?l ?x ?y ?nn1 ?nn2 ?xval 1 ?ncval ?nnval)
						   (?nn1 up ?y)
						   (?nn1 interior)
						   (?nn1 level ?l1)
						   (?nn2 level ?l1)
						   (?nn1 next ?nn2)
						   (?nn1 rule-30-next ?nn2)
						   (?nn2 max)
						   (?nn1 rule30val ?ncval)
						   (?nn2 rule30val ?nnval)))))
 (del
  (global-node rule ?this-rule)
  ))

(rule
 (name rule-30-next1)
 (root-var ?x1)
 (pred
  (?x1 up ?x)
  (?y1 up ?y)
  (?x interior)
  (?y max)
  (?x1 interior)
  (?y1 interior)
  (?x next ?y))
 (add
  (print rule-30-next1 ?x ?y ?x1 ?y1)
  (?x1 next ?y1)
  (?x1 rule-30-next ?y1)))

(rule
 (name rule-30-next2)
 (root-var ?x1)
 (pred
  (?x1 up ?x)
  (?y1 up ?y)
  (?x zero)
  (?y interior)
  (?x1 interior)
  (?y1 interior)
  (?x next ?y))
 (add
  (print rule-30-next2 ?x ?y ?x1 ?y1)
  (?x1 next ?y1)
  (?x1 rule-30-next ?y1)))

(rule
 (name rule-30-next3)
 (root-var ?x1)
 (pred
  (?x1 up ?x)
  (?y1 up ?y)
  (?x interior)
  (?y interior)
  (?x1 interior)
  (?y1 interior)
  (?x next ?y))
 (add
  (print rule-30-next3 ?x ?y ?x1 ?y1)
  (?x1 next ?y1)
  (?x1 rule-30-next ?y1)))

(rule
 (name rule-30-weave1)
 (pred
  (?x zero)
  (?x top ?t)
  (?x next ?xn)
  (?xn up ?xnu)
  (?xnu level ?l)
  (?y max)
  (?y top ?t)
  (?yp next ?y)
  (?yp up ?ypu)
  (?ypu level ?l))
 (add
  (print rule-30-weave1 ?x ?y ?xn ?xnu ?yp ?ypu ?l)
  (?ypu rule-30-weave-next ?x)))

(rule
 (name rule-30-weave2)
 (pred
  (?x rule-30-next ?y))
 (add
  (print rule-30-weave2 ?x ?y)
  (?x rule-30-weave-next ?y)))

;; Gettysburg Address rules

(rule
 (name rule-30-ga1)
 (pred
  (?x rule-30-weave-next ?y)
  (?x ga-word ?ga)
  (?ga next-ga-word ?gan)
  (?ga value ?w))
 (add
  (print rule-30-ga1 ?x ?y ?ga ?w)
  (?y ga-word ?gan)
  (?y label ?w)))

(rule
 (name rule-30-center)
 (pred
  (?x center)
  (?y up ?x))
 (add
  (print rule-30-center ?x ?y)
  (?y center)
  (?y center-up ?x)))

(rule
 (name rule-30-center-loop)
 (pred
  (?x center)
  (?x level 0)
  (?x top ?t))
 (add
  (print rule-30-center-loop ?x ?t)
  (?t center-up ?x)))

#|

;; Of interest when we were trying "horizontal" randomness but now we're not

(rule
 (name rule-30-loop)
 (root-var ?x)
 (pred
  (?x zero)
  (?y max)
  (?x level ?l)
  (?y level ?l))
 (add
  (print rule-30-loop ?x ?y ?l)
  (?y next ?x)
  (?x1 rule-30-next ?y1)))
|#

(rule
 (name rule-30-top)
 (root-var ?x)		;;;;;;;;;;;;;;;
 (pred
  (?x rule-30-top)
  (?x level ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (?nn3 new-node sn3))
 (add
  (print rule-30-top ?x ?nn1 ?nn2 ?nn3)
  (?nn2 up ?x)
  (?x center)
  (?x top ?x)
  (?nn1 next ?nn2)
  (?nn2 next ?nn3)
  (?nn1 rule-30-next ?nn2)
  (?nn2 rule-30-next ?nn3)
  (?nn1 level ?l1)
  (?nn2 level ?l1)
  (?nn3 level ?l1)
  (?nn1 zero)
  (?nn2 interior)
  (?nn3 max)
  (?x rule30val 1)
  (?nn1 rule30val 1)
  (?nn2 rule30val 1)
  (?nn3 rule30val 1)))

(rule
 (name rule-30-top-propagate1)
 (pred
  (?x top ?t)
  (?y up ?x)
  (?x center)
  (?y center))
 (add
  (print rule-30-top-propagate1 ?x ?y)
  (?y top ?t)))

(rule
 (name rule-30-top-propagate2)
 (pred
  (?x zero)
  (?x next ?xn)
  (?xn up ?xnu)
  (?xnu top ?t)
  (?y max)
  (?yp next ?y)
  (?yp up ?ypu)
  (?ypu top ?t))
 (add
  (print rule-30-top-propagate2 ?x ?y)
  (?x top ?t)
  (?y top ?t)))

(rule
 (name rule-30-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?rule-30-data)
  (?rule-30-data name rule-30-data))
 (add
  (print rule-30-data)
  (global-node rule-30-data)
  (rule-30-data 0 0 0 0)
  (rule-30-data 0 0 1 1)
  (rule-30-data 0 1 0 1)
  (rule-30-data 0 1 1 1)
  (rule-30-data 1 0 0 1)
  (rule-30-data 1 0 1 0)
  (rule-30-data 1 1 0 0)
  (rule-30-data 1 1 1 0))
 (del
  (global-node rule ?this-rule)))

#|
;; rule 110

(rule
 (name rule-30-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?rule-30-data)
  (?rule-30-data name rule-30-data))
 (add
  (print rule-30-data)
  (global-node rule-30-data)
  (rule-30-data 0 0 0 0)
  (rule-30-data 0 0 1 1)
  (rule-30-data 0 1 0 1)
  (rule-30-data 0 1 1 1)
  (rule-30-data 1 0 0 0)
  (rule-30-data 1 0 1 1)
  (rule-30-data 1 1 0 1)
  (rule-30-data 1 1 1 0))
 (del
  (global-node rule ?this-rule)))
|#