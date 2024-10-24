#|
3/2/21
Amended 5/16/23

Previous version worked well using all global rules (see
global-rule30.lisp), but herein we've pruned it down to a set of
locally-propagated rules. The propagation is via "is" objects, to
inherit, and subsequently delete, rules. Using this model we get
quadratic complexity, as measured by taking runs betwen 0 and 45
[5/15/23 -- up to 100 levels by fives] levels inclusive, by fives.

See dumper.lisp, class gnuplot for this code.

Saved image in rule30.gnuplot.png

RULE-30-CENTER shows linear growth, and the rest show quadratic
growth. This is correct as the number of nodes increases as 2n+1,
where n is the level. So we have sum(n=0,N)(2n+1) where N is the max
level, which is N^2+2N+1.

The number of times we test rules is optimized by the -opt rule
modifiers below. In particular a set of rules is needed to get the
values of each node, e.g., rule-30-nexrt-rule-0-0-0, where we get a
new suffix for each config we need to detect. rule-30-next-rule-opt
installs deleters in in each rule-30-nexrt-rule-n-n-n rule to rid the
object of all rules which no longer need to be tested, since one of
them has succeeded.

To run these tests, look for "Rule30 complexity test" in test.lisp.
|#

(rule
 (name rule-30-next-rule-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule-30-data)
  (rule-30-data ?xval ?yval ?zval ?nval))
 (add
  (print rule-30-next-rule-gen ?xval ?yval ?zval ?nval)
  (rule-30-next-rule-obj has rule
						 (rule
						  (name (rule-30-next-rule ?xval ?yval ?zval))
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

						   ;; (?y is-not rule-30-next-rule-obj)		;; Doesn't seem to help anything

						   (?z xis rule-30-next-rule-obj)
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
  (rule-30-next-rule-obj has rule ?r1)
  (?r1 name ?n1)
  (rule-30-next-rule-obj has rule ?r2)
  (?r2 name ?n2)
  )
 (add
  (print rule-30-next-rule-opt ?r1 ?n1 ?r2 ?n2)
  (?r1 del ?y rule ?r2)
  (?r1 add print del-rule-30-next-rule ?y ?r2))
 (del
  (rule-30-next-rule-obj rule ?this-rule)))

(rule
 (name rule-30-zero-rule-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule-30-data)
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?rule-30-zero-prune)
  (?rule-30-zero-prune name rule-30-zero-prune)
  (rule-30-data 0 1 1 ?ncval)
  (rule-30-data 0 0 1 ?nnval))
 (add
  (print rule-30-zero-rule-gen 1 1 ?ncval ?nnval)
  (rule-30-zero-rule-obj has rule
						 (rule
						  (name (rule-30-zero-rule 1 1))
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
						   (?y xis-not rule-30-zero-rule-obj)
						   (?z xis rule-30-next-rule-obj)
						   (?z next-zero ?nn1)
						   (?nn2 up ?y)
						   (?nn2 interior)
						   (?nn1 level ?l1)
						   (?nn2 level ?l1)
						   (?nn1 rule-30-next ?nn2)
						   (?nn1 zero)
						   (?nn1 rule ?rule-30-zero-prune)
						   (?nn1 rule30val ?nnval)
						   (?nn2 rule30val ?ncval)))))
 (del
  (global-node rule ?this-rule)))

(rule
 (name rule-30-zero-prune-gen)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p))
 (add
  (print rule-30-zero-prune-gen)
  (?p lrp-rule (rule
				(name rule-30-zero-prune)
				(pred
				 (?x zero))
				(add
				 (print rule-30-zero-prune ?x)
				 (?x xis-not rule-30-next-rule-obj))
				(del
				 (?this-obj-1 rule ?this-rule-1)))))
 (del
  (?this-obj rule ?this-rule)))

(comment
(rule
 (name rule-30-zero-prune)
 (local)
 (pred
  (?x zero))
 (add
  (print rule-30-zero-prune ?x)
  (?x is-not rule-30-next-rule-obj))
 (del
  (?this-obj rule ?this-rule)))
)

(rule
 (name rule-30-max-rule-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule-30-data)
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?rule-30-max-prune)
  (?rule-30-max-prune name rule-30-max-prune)
  (rule-30-data ?xval 1 0 ?ncval)
  (rule-30-data 1 0 0 ?nnval))
 (add
  (print rule-30-max-rule-gen ?xval 1 ?ncval ?nnval)
  (rule-30-max-rule-obj has rule
						(rule
						 (name (rule-30-max-rule ?xval 1))
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
						  (?y xis-not rule-30-max-rule-obj)
						  (?y xis-not rule-30-next-rule-obj)
						  (?nz xis rule-30-zero-rule-obj)
						  (?nn2 xis rule-30-max-rule-obj)
						  (?nn1 up ?y)
						  (?nn1 interior)
						  (?nn1 level ?l1)
						  (?nn2 level ?l1)
						  (?nn1 rule-30-next ?nn2)
						  (?nn2 max)
						  (?nn2 rule ?rule-30-max-prune)
						  (?nn1 rule30val ?ncval)
						  (?nn2 rule30val ?nnval)
						  (?xu rule-30-next ?nn1)))))
 (del
  (global-node rule ?this-rule)))

;; Needs work: redundant triggering!
;; Perhaps: rule leaves behind another rule that detects if deleted stuff comes back

;; 10/20/23 -- This looks very interesting! The main rule,
;; rule-30-max-prune, sometimes needs to be re-invoked, since rules
;; which need to be pruned can be added to the node again. And that
;; only happens on exec-all since the only pred is (?x max). So rather
;; than leave rule-30-max-prune there to be invoked over and over, we
;; replace it with a rule that detects when rule-30-next rules (via
;; rule-30-next-rule-obj) have been added, and reinstates
;; rule-30-max-prune, thus getting rid of the rules. Should no more
;; such rules be added, we just have the single
;; rule-30-max-prune-rules-added hanging off the last max node.
;;
;; This is a good exercise using nested rules and the "?this.."
;; indicators to refer to the rules at different levels,

(rule
 (name rule-30-max-prune)
 (local)
 (pred
  (?x max))
 (add
  (print rule-30-max-prune ?x)
  ;; (?x xis-not rule-30-next-rule-obj)
  (?x rule (rule
			(name rule-30-max-prune-rules-added)
			(pred
			 (rule-30-next-rule-obj has rule ?r)
			 (?x rule ?r))
			 (add
			  (print rule-30-max-prune-rules-added ?x ?r)
			  (?x rule ?this-rule))						;; Yes, needs to add rule-30-max-prune
			 (del
			  (?this-obj-1 rule ?this-rule-1)			;; Delete rule-30-max-prune-rules-added
			  ;; (...)									;; More dels are added by rule-30-max-prune-opt
			  ))))
 (del
  (?this-obj rule ?this-rule)							;; Delete rule-30-max-prune
  )
 )

(comment
(rule
 (name rule-30-max-prune)
 ;; (disabled)
 (local)
 (pred
  (?x max))
 (add
  (print rule-30-max-prune ?x)
  ;; (?x xis-not rule-30-next-rule-obj)
  )
 (del
  ;; (?this-obj rule ?this-rule)		;; Needs to be invoked more than once!
  )
 )
)


(rule
 (name rule-30-max-prune-opt-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?rule-30-max-prune)
  (?rule-30-max-prune name rule-30-max-prune))
 (add
  (print rule-30-max-prune-opt-gen)
  (rule-30-next-rule-obj rule (rule
							   (name rule-30-max-prune-opt)
							   (pred
								(rule-30-next-rule-obj has rule ?r))
							   (add
								(print rule-30-max-prune-opt)
								(?rule-30-max-prune del ?x rule ?r))
							   (del
								(?this-obj-1 rule ?this-rule-1)))))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name rule-30-center)
 (local)
 (root-var ?x)
 (pred
  (?x center)
  (?x top ?t)
  (?x level ?l)
  (?l1 sigma ?l)
  (?t local-rule-pool ?p)
  (?y up ?x))
 (add
  (print rule-30-center ?this-obj ?root-var ?x ?y)
  (?y center)
  (?y local-rule-pool ?p)
  ;; (?y xis rule-30-center-obj)
  (?y center-up ?x)
  (?y top ?t)
  ;; (?x xis-not rule-30-center-obj)
  )
 (del
  ;; (?x rule ?this-rule)		;; Handled by the mod rule below
  )
 )

(rule
 (name rule-30-center-loop)
 (local)
 (pred
  (?x center)
  (?x level 0)
  (?x top ?t))
 (add
  (print rule-30-center-loop ?this-obj ?x ?t)
  (?t center-up ?x)
  ;; (?x xis-not rule-30-center-obj)
  )
 (del
  (?x rule ?this-rule)))

(rule
 (name rule-30-center-mod)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?rule-30-center)
  (?p lrp-rule ?rule-30-center-loop)
  (?rule-30-center name rule-30-center)
  (?rule-30-center-loop name rule-30-center-loop))
 (add
  (print rule-30-center-mod)
  (?rule-30-center add ?y rule ?rule-30-center)
  (?rule-30-center add ?y rule ?rule-30-center-loop)
  (?rule-30-center del ?x rule ?rule-30-center-loop)
  (?rule-30-center del ?x rule ?rule-30-center)
  (?rule-30-center add ?y rule-order ?rule-30-center ?rule-30-center-loop)
  )
 (del
  (global-node rule ?this-rule)))

;; 9/5/23 Regarding is/is-not rules 
;; This is disabled for now until we figure out how to make is/is-not
;; more effective. "is" seems ok: std queuing handles it fairly
;; efficiently, though an "exec" model is probably better. However
;; "is-not" suffers from the queuing delay, in that needed deletions
;; don't happen right away, thus cauing more rule testing than
;; desired. So we need to get somwe kind of ":exec" model to work,
;; though for now experiments in that realm havn't worked right and
;; are currently disabled.
;; 
;; We'll try this using "-opt" or "-mod" modification, as we did with cas.

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
  (rule-30-center-obj has rule ?rule-30-center)
  (rule-30-center-obj has rule ?rule-30-center-loop)
  ;; (rule-30-center-obj has rule-order ?rule-30-center ?rule-30-center-loop)
  )
 (del
  (global-node rule ?this-rule)))

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
  (?x xis rule-30-center-obj)
  (?x center)
  (?x top ?x)
  (?nn1 xis rule-30-zero-rule-obj)
  (?nn1 rule-30-next ?nn2)
  (?nn2 rule-30-next ?nn3)
  (?nn1 level ?l1)
  (?nn2 level ?l1)
  (?nn3 level ?l1)
  (?nn1 zero)
  (?nn2 interior)
  (?nn3 max)
  (?nn3 xis rule-30-max-rule-obj)
  (?x rule30val 1)
  (?nn1 rule30val 1)
  (?nn2 rule30val 1)
  (?nn3 rule30val 1)
  )
 )

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
