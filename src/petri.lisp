


















































#|

;; Initial experiments

;; Propagate "ready" and a color around a ring

(rule
 (name petri)
 (disabled)
 (no-triggered)
 (pred
  (?x ready)
  (?x next-node ?y))
 (add
  (print petri ?x ?y)
  (?y ready)
  (?y color red)
  )
 (del
  (?x ready)
  (?x color red)
  )
 )

(rule
 (name init)
 (disabled)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name init))
 (add
  (print init)
  (x0 next-node x1)
  (x1 next-node x2)
  (x2 next-node x3)
  (x3 next-node x4)
  (x4 next-node x5)
  (x5 next-node x6)
  (x6 next-node x7)
  (x7 next-node x8)
  (x8 next-node x9)
  (x9 next-node x10)
  (x10 next-node x0)
  (x0 ready)
  (x0 color red)
  )
 (del
  (global-node rule ?this-rule)
  )
 )

(rule
 (name and-gen)
 (disabled)
 (attach-to and-data)
 (pred
  (and-data ?xval ?yval ?zval))
 (add
  (print and-gen ?xval ?yval ?zval)
  (x rule (rule
		   (name andx)
		   (global)
		   (pred
			(?x value ?xval)
			(?y value ?yval)
			(?z value ?oldzval))
		   (add
			(?z value ?zval)
		   (del
			(?z value ?oldzval))))))
 (del
  (global-node rule ?this-rule)))

(rule
 (name and-data)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name and-data))
 (add
  (print and-data)
  (and-data 0 0 0)
  (and-data 0 1 0)
  (and-data 1 0 0)
  (and-data 1 1 1)
  )
 (del
  (global-node rule ?this-rule))
 )
|#


#|
;; Laboratory curiosities

;; This does an xor, using only a distinct-vars designation. It's xor
;; in the sense that it only runs if the values are unequal.

(rule
 (name xor)
 (distinct-vars t)
 (pred
  (?x value ?xval)
  (?y value ?yval))
 (add
  (print xor ?x ?y ?xval ?yval)))

;; xnor runs only if both vars are equal

(rule
 (name xnor)
 (distinct-vars t)
 (pred
  (?x value ?val)
  (?y value ?val))
 (add
  (print xnor ?x ?y ?val)))
|#












(rule
 (name xor-fcn)
 (local)
 (root-var ?z)
 (no-triggered)
 (pred
  (?x ?y xor-fcn ?z)
  (?x value ?vx)
  (?y value ?vy)
  (?z value ?vz)
  (?vx ?vy xor ?new-vz))
 (add
  ;; (print xor-fcn ?x ?y ?z ?vx ?vy ?vz ?new-vz)
  (?z value ?new-vz)
  (?z done)
  )
 (del
  (?z value ?vz)
  )
 )

(rule
 (name id-fcn)
 (local)
 (root-var ?y)
 (no-triggered)
 (pred
  (?x id-fcn ?y)
  (?x value ?vx)
  (?y value ?vy))
 (add
  ;; (print id-fcn ?x ?y ?vx ?vy)
  (?y value ?vx)
  (?y done)
  )
 (del
  (?y value ?vy)
  )
 )

(rule
 (name xor-gen)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name xor-gen))
 (add
  (print xor-gen)
  (0 0 xor 0)
  (0 1 xor 1)
  (1 0 xor 1)
  (1 1 xor 0)
  (xor-fcn two-input-op)
  (xor-fcn color pink)
  (xor two-input-op)))

(rule
 (name color-value0)
 (pred
  (?x next-out ?y)
  (?y value 0))
 (add
  (?y color blue))
 (del
  (?y color red)))

(rule
 (name color-value1)
 (pred
  (?x next-out ?y)
  (?y value 1))
 (add
  (?y color red))
 (del
  (?y color blue)))

(rule
 (name xtest)
 (disabled)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name test)
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?id-fcn)
  (?id-fcn name id-fcn)
  (?p lrp-rule ?xor-fcn)
  (?xor-fcn name xor-fcn))
 (add
  (print test)
  (x id-fcn y)
  (y id-fcn z)
  (z y xor-fcn x)
  (z rule ?id-fcn)
  (y rule ?id-fcn)
  (x rule ?xor-fcn)
  (x value 0)
  (y value 1)
  (z value 0)
  (c1 next c2)
  (c2 num 15)
  (c2 value 0)
  (c1 num 14)
  (c1 value 0)))



(rule
 (name chain)
 (root-var ?y)
 (pred
  (?n1 sigma ?n)
  (?x next ?y)
  (?y num ?n)
  (?nn1 new-node sn1))
 (add
  (print chain ?x ?y ?n1 ?n ?nn1)
  (?nn1 next ?x)
  (?x num ?n1)))

(rule
 (name shift-reg-gen-gen)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?id-fcn)
  (?id-fcn name id-fcn)
  (?p lrp-rule ?xor-fcn)
  (?xor-fcn name xor-fcn)
  (?nstages sigma 17)
  (?nstages1 sigma ?nstages)
  (?nstages2 sigma ?nstages1))
 (add
  (print shift-reg-gen-gen)
  (next rule (rule
			  (name shift-reg-gen1)
			  (pred
			   (?x next ?y)
			   (?x num ?n))
			  (add
			   (print shift-reg-gen1)
			   (?x id-fcn ?y))))
  (num rule (rule
			 (name shift-reg-gen2)
			 (pred
			  (?x num 0)
			  (?y num ?nstages2)
			  (?z num ?nstages))
			 (add
			  (print shift-reg-gen2)
			  (?y ?z xor-fcn ?x))))
  (num rule (rule
			 (name shift-reg-gen3)
			 (pred
			  (?x num 0)
			  (?y num ?nstages))
			 (add
			  (print shift-reg-gen3)
			  (?y done)
			  (?y rule ?id-fcn)
			  (?x zero)
			  (?x value 1)
			  )))
  (num rule (rule
			 (name shift-reg-gen4)
			 (pred
			  (?x num ?n)
			  (?n1 sigma ?n))
			 (add
			  (print shift-reg-gen4)
			  (?x value 0))))
  (num rule (rule
			 (name install-output)
			 (pred
			  (?x num ?nstages)
			  (?nn1 new-node sn1))
			 (add
			  (print install-output ?n)
			  (?x out)
			  (?x next-out ?nn1))))
  (global-rule-pool-node
   grp-rule (rule
			 (name control1)
			 (no-triggered)
			 (pred
			  (?x id-fcn ?y)
			  (?y id-fcn ?z)
			  (?z done)
			  (?z rule ?id-fcn))
			 (add
			  ;; (print control1 ?x ?y ?z)
			  (?y rule ?id-fcn))
			 (del
			  (?z rule ?id-fcn)
			  (?z done))))
  (global-rule-pool-node
   grp-rule (rule
			 (name control2)
			 (no-triggered)
			 (pred
			  (?x ?y xor-fcn ?z)
			  (?z id-fcn ?t)
			  (?t done)
			  (?t rule ?id-fcn))
			 (add
			  ;; (print control2 ?x ?y ?z ?t)
			  (?z rule ?xor-fcn))
			 (del
			  (?t rule ?id-fcn)
			  (?t done))))
  (global-rule-pool-node
   grp-rule (rule
			 (name control3)
			 (no-triggered)
			 (pred
			  (?x ?y xor-fcn ?z)
			  (?y num ?nstages)
			  (?t id-fcn ?y)
			  (?z done)
			  (?z rule ?xor-fcn))
			 (add
			  ;; (print control3 ?x ?y ?z ?t)
			  (?y rule ?id-fcn))
			 (del
			  (?z rule ?xor-fcn)
			  (?z done))))
  (global-rule-pool-node
   grp-rule (rule
			 (name output)
			 (no-triggered)
			 (pred
			  (?x out)
			  (?x value ?v)
			  (?x next-out ?y)
			  (?x done)
			  (?nn1 new-node sn1))
			 (add
			  (print output ?x ?y ?nn1 ?v)
			  (?nn1 value ?v)
			  (?nn1 next-out ?y)
			  (?x next-out ?nn1))
			 (del
			  (?x next-out ?y)
			  )))
  (test rule (rule
			  (name test)
			  (pred
			   (test rule ?r))
			  (add
			   (print test)
			   (c1 next c2)
			   (c2 num ?nstages)
			   (c2 value 0)
			   (c1 num ?nstages1)
			   (c1 value 0))))
))
