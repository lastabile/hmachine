#|
3/2/21

Previous version worked well using all global rules (see
global-rule30.lisp), but herein we've pruned it down to a set of
locally-propagated rules. The propagation is via "is" objects, to
inherit, and subsequently delete, rules. Using this model we get
quadratic complexity, as measured by taking runs betwen 0 and 45
levels inclusive, by fives. Then do rule stats and dig out:

egrep "RULE-30-NEXT-RULE-0-0-1    " xxx | gawk '{ print $2; }' >x1
egrep "RULE-30-MAX-RULE-0-1    " xxx | gawk '{ print $2; }'  >x2
egrep "RULE-30-ZERO-RULE-1-1    " xxx | gawk '{ print $2; }'  >x3
egrep "RULE-30-CENTER    " xxx | gawk '{ print $2; }'  >x4
egrep "MAIN" xxx | gawk '{ print $2; }'  >x5

RULE-30-CENTER shows linear growth, and the rest show quadratic
growth. This is correct as the number of nodes increases as 2n+1,
where n is the level. So we have sum (0...2N+1), where N is the max
level, which is N(N+2).

Further, with the current formulation, we test rules a nearly minimal
number of times. So the number of times for instance testing each
RULE-30-NEXT-RULE is thus approximately N(N+2). I haven't not tracked
down the small discrepancy.

|#

(rule
 (name rule-30-center-obj-rule)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?rule-30-center)
  (?p lrp-rule ?rule-30-center-loop)
  (?rule-30-center name rule-30-center)
  (?rule-30-center-loop name rule-30-center-loop))
 (add
  (print rule-30-center-obj-rule)
  (rule-30-center-obj xrule ?rule-30-center)
  (rule-30-center-obj xrule ?rule-30-center-loop))
 (del
  (global-node rule ?this-rule)))

(rule
 (name rule-30-next-rule-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule-30-data)
  (rule-30-data ?xval ?yval ?zval ?nval))
 (add
  (print rule-30-next-rule-gen ?xval ?yval ?zval ?nval)
  (rule-30-next-rule-obj xrule
						 (rule
						  (name (rule-30-next-rule ?xval ?yval ?zval))
						  (local)
						  (root-var ?y)
						  (pred
						   (?y level ?l)
						   (?l1 sigma ?l)
						   (?x rule-30-next ?y)
						   (?y rule-30-next ?z)
						   (?xu up ?x)
						   (?y next-zero ?nz)
						   (?y interior)
						   (?x rule30val ?xval)
						   (?y rule30val ?yval)
						   (?z rule30val ?zval)
						   (?nn1 new-node sn1))
						  (add
						   (print rule-30-next-rule ?this-obj newn1 ?nn1 ?x ?y ?z ?xval ?yval ?zval ?nval ?nz)

						   ;; (?y is-not rule-30-next-rule-obj)

						   (?z is rule-30-next-rule-obj)
						   (?z next-zero ?nz)
						   (?nn1 up ?y)
						   (?nn1 interior)
						   (?nn1 level ?l1)
						   (?nn1 rule30val ?nval)
						   (?xu rule-30-next ?nn1)))))
 (del
  (global-node rule ?this-rule)))

(rule
 (name rule-30-next-rule-opt)
 (attach-to rule-30-next-rule-obj)
 (root-var rule-30-next-rule-obj)
 (pred
  (rule-30-next-rule-obj xrule ?r1)
  (?r1 name ?n1)
  (rule-30-next-rule-obj xrule ?r2)
  (?r2 name ?n2))
 (add
  (print rule-30-next-rule-opt ?r1 ?n1 ?r2 ?n2)
  (?r1 del (?y rule ?r2)))
 (del
  (rule-30-next-rule-obj rule ?this-rule)))
  

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
  (rule-30-zero-rule-obj xrule
						 (rule
						  (name (rule-30-zero-rule 1 1))
						  (local)
						  (pred
						   (?y zero)
						   (?y level ?l)
						   (?l1 sigma ?l)
						   (?y rule-30-next ?z)
						   (?y rule30val 1)
						   (?z rule30val 1)
						   (?nn1 new-node sn1)
						   (?nn2 new-node sn2))
						  (add
						   (print rule-30-zero-rule ?this-obj newn1 ?nn1 newn2 ?nn2 ?y ?z 1 1 ?ncval ?nnval)
						   (?y is-not rule-30-zero-rule-obj)
						   (?z is rule-30-next-rule-obj)
						   (?z next-zero ?nn1)
						   (?nn2 up ?y)
						   (?nn2 interior)
						   (?nn1 level ?l1)
						   (?nn2 level ?l1)
						   (?nn1 rule-30-next ?nn2)
						   (?nn1 zero)
						   (?nn1 rule30val ?nnval)
						   (?nn2 rule30val ?ncval)))))
 (del
  (global-node rule ?this-rule)))

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
  (rule-30-max-rule-obj xrule
						(rule
						 (name (rule-30-max-rule ?xval 1))
						 (local)
						 (root-var ?y)
						 (pred
						  (?y max)
						  (?y level ?l)
						  (?l1 sigma ?l)
						  (?x rule-30-next ?y)
						  (?xu up ?x)
						  (?x next-zero ?nz)
						  (?x rule30val ?xval)
						  (?y rule30val 1)
						  (?nn1 new-node sn1)
						  (?nn2 new-node sn2))
						 (add
						  (print rule-30-max-rule ?this-obj newn1 ?nn1 newn2 ?nn2 ?l ?x ?y ?xval 1 ?ncval ?nnval)
						  (?y is-not rule-30-max-rule-obj)
						  (?nz is rule-30-zero-rule-obj)
						  (?nn2 is rule-30-max-rule-obj)
						  (?nn1 up ?y)
						  (?nn1 interior)
						  (?nn1 level ?l1)
						  (?nn2 level ?l1)
						  (?nn1 rule-30-next ?nn2)
						  (?nn2 max)
						  (?nn1 rule30val ?ncval)
						  (?nn2 rule30val ?nnval)
						  (?xu rule-30-next ?nn1)))))
 (del
  (global-node rule ?this-rule)))

(rule
 (name rule-30-center)
 (local)
 (pred
  (?x center)
  (?x top ?t)
  (?y up ?x))
 (add
  (print rule-30-center ?this-obj ?x ?y)
  (?y center)
  (?y is rule-30-center-obj)
  (?y center-up ?x)
  (?y top ?t))
 (del
  (?x rule ?this-rule)))

(rule
 (name rule-30-center-loop)
 (local)
 (pred
  (?x center)
  (?x level 0)
  (?x top ?t))
 (add
  (print rule-30-center-loop ?this-obj ?x ?t)
  (?t center-up ?x))
 (del
  (?x rule ?this-rule)))

(rule
 (name rule-30-top)
 ;; (root-var ?x)		;;;;;;;;;;;;;;;
 (local)
 (attach-to rule-30-top)
 (pred
  (?x rule-30-top)
  (?x level ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (?nn3 new-node sn3))
 (add
  (print rule-30-top ?this-obj ?x ?nn1 ?nn2 ?nn3)
  (?nn2 up ?x)
  (?nn2 is rule-30-center-obj)
  (?x center)
  (?x top ?x)
  (?nn1 is rule-30-zero-rule-obj)
  (?nn1 rule-30-next ?nn2)
  (?nn2 rule-30-next ?nn3)
  (?nn1 level ?l1)
  (?nn2 level ?l1)
  (?nn3 level ?l1)
  (?nn1 zero)
  (?nn2 interior)
  (?nn3 max)
  (?nn3 is rule-30-max-rule-obj)
  (?x rule30val 1)
  (?nn1 rule30val 1)
  (?nn2 rule30val 1)
  (?nn3 rule30val 1)))

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
