
;; This never runs, as we have attached the relevant rules to constants below.

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
  (?y color ?t))
 (del
  (?this-obj rule ?this-rule)))

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
  (?y inv-delta3 ?x))
 (del
  ;; (?this-obj rule ?this-rule)
  ))

(rule
 (name fft-rule-delta4)		;; Propagate color and rand along weave-next
 (attach-to weave-next)
 (attach-to color)
 (attach-to rand)
 (attach-to center-up)
 (attach-to next-color)
 (pred
  (?x weave-next ?y)
  (?x color ?c)
  (?x rand ?r)
  (?r center-up ?u)
  (?c next-color ?s))
 (add
  (print fft-rule-delta4 ?this-obj ?x ?y ?c ?s)
  (?y color ?s)
  (?y rand ?u)))

(rule
 (name fft-rule-delta5)
 (attach-to d)
 (attach-to casz-ref)
 (pred
  (?x d ?y)
  (?x casz-ref ?z))
 (add
  (print fft-rule-delta5 ?this-obj ?x ?y ?z)
  (?x d-casz ?z)))

(rule
 (name fft-rule-delta6)
 (attach-to d-casz)
 (attach-to casns-ref)
 (pred
  (?x d-casz ?y)
  (?y casns-ref ?z))
 (add
  (print fft-rule-delta6 ?this-obj ?x ?y ?z)
  (?x d-casz-casns-ref ?z)  ;; These two are eqv. e shortcut for display
  (?x e ?z)))				;; 

(comment

;; 8/23/23 -- Commented-out as there is a dup of these in fft.lisp
 
;; Rule optimizer (for a specific rule). Adds rule propagators.
;; Experimental -- note this one adds more rules to a given node than needed.

(rule
 (name fft-rule-opt-rule-names)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name fft-rule-opt-rule-names))
 (add
  (print fft-rule-opt-rule-names)
  (fft-rule-opt-rule-names-data copy-array-struct-new)
  (fft-rule-opt-rule-names-data even-new-rule-propagate)
  (fft-rule-opt-rule-names-data odd-new-rule-propagate)
  (fft-rule-opt-rule-names-data fft-rule-zero)
  (fft-rule-opt-rule-names-data fft-comb-rule-zero))
 (del
  (global-node rule ?this-rule)))

(rule
 (name fft-rule-opt)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool local-rule-pool-node)
  (local-rule-pool-node lrp-rule ?fft-rule)
  (?fft-rule name fft-rule)
  (local-rule-pool-node lrp-rule ?r)
  (?r name ?n)
  (fft-rule-opt-rule-names-data ?n))
 (add
  (print fft-rule-opt ?fft-rule)
  (local-rule-pool-node lrp-rule ?fft-rule)
  (?fft-rule add print fft-opt-run ?fft-rule)
  (?fft-rule add ?nn1 rule ?r)
  (?fft-rule add ?nn2 rule ?r)
  (?fft-rule add ?x rule ?r)
  (?fft-rule add ?y rule ?r)
  (?fft-rule add ?nn1 rule ?fft-rule)
  (?fft-rule add ?nn2 rule ?fft-rule)
  (?fft-rule opt-done))
 (del
  (?this-obj rule ?this-rule)))

)

(comment		;; rem-add-main
(rule
 (name fft-rule-opt-display)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool local-rule-pool-node)
  (local-rule-pool-node lrp-rule ?fft-rule)
  (?fft-rule name fft-rule)
  (?fft-rule opt-done)
  (?fft-rule add ?a))
 (add
  (print fft-rule-opt-display ?fft-rule ?a)
  (?fft-rule add-main ?a))
 (del
  (?this-obj rule ?this-rule)))
)
