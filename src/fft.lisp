
(rule
 (name ev-init-gen)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?od-next)
  (?od-next name od-next))
 (add
  (print ev-init-gen)
  (local-rule-pool-node lrp-rule
						(rule
						 (name ev-init)
						 (pred
						  ;; (?a elem ?e0)
						  (?e0 is-elem-of ?a)
						  (?e0 zero))
						 (add
						  (print ev-init ?e0)
						  (?e0 ev)
						  (?e0 rule ?od-next)
						  (?e0 local-rule-pool ?p))
						 (del
						  (?this-obj-1 rule ?this-rule-1)))))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name od-next)
 (local)
 (pred
  ;; (?a elem ?e0)			;; !!!!!!!!!!!!! 9/19/20 -- in removing this the system still works, and the expansion length shrinks by a lot, but still not constant across trials
  ;; (?a elem ?e1)
  (?e0 is-elem-of ?a)
  (?e1 is-elem-of ?a)
  (?e0 next ?e1)
  (?e0 ev))
 (add
  (print od-next ?this-obj ?this-rule ?root-var ?e0 ?e1 ?a)
  (?e1 od))
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name ev-next)
 (local)
 (pred
  ;; (?a elem ?e0)				;; !!!!!!!!!!!!!!!!!!!! -- ditto 
  ;; (?a elem ?e1)
  (?e0 is-elem-of ?a)
  (?e1 is-elem-of ?a)
  (?e0 next ?e1)
  (?e0 od))
 (add
  (print ev-next ?e1 ?root-var)
  (?e1 ev))
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name ev-od-opt)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?od-next)
  (?od-next name od-next)
  (?p lrp-rule ?ev-next)
  (?ev-next name ev-next)
  (?p lrp-rule ?ev-init)
  (?ev-init name ev-init)
  (?p lrp-rule ?even-new)
  (?even-new name even-new)
  )
 (add
  (print ev-od-opt)
  (?od-next add ?e1 rule ?ev-next)
  (?ev-next add ?e1 rule ?od-next)
  (?od-next del ?e1 rule ?ev-init)
  (?ev-next del ?e1 rule ?ev-init)
  (?od-next add ?e1 rule-order ?ev-next ?ev-init)
  (?ev-next add ?e1 rule-order ?od-next ?ev-init)
  (?ev-next add ?e1 rule ?even-new)
  (?ev-init add ?e0 rule ?even-new)
  )
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name ev-od-obj-rule)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?ev-init)
  (?ev-init name ev-init))
 (add
  (print ev-od-obj-rule)
  (ev-od-obj has rule ?ev-init)
  )
 (del
  (global-node rule ?this-rule)))

;; An array is a node denoted in a rule as ?a with a set of nodes
;; denoted by property elem related by next

(rule
 (name even-new)
 (local)
 (pred
  (?a even ?a1)
  (?e0 is-elem-of ?a)
  (?e0 ev)
  (?e0 value ?v)
  (?nn1 new-node sn1)
  (?e0 local-rule-pool ?p)
  (?p lrp-rule ?even-zero)
  (?p lrp-rule ?odd-next)
  (?p lrp-rule ?even-next)
  (?p lrp-rule ?odd-new)
  (?even-zero name even-zero)
  (?odd-next name odd-next)
  (?even-next name even-next)
  (?odd-new name odd-new)
  )
 (add
  (print even-new ?this-obj ?a ?e0 ?nn1)
  (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
  (?a1 elem ?nn1)
  (?nn1 value ?v)

  (?nn1 en-ref ?e0)
  (?nn1 oe-ref ?e0)
  (?nn1 ref ?e0)

  (?nn1 local-rule-pool ?p)

  (?nn1 xis ev-od-obj)
  (?nn1 rule ?even-zero)
  (?nn1 rule ?odd-next)
  (?nn1 rule ?even-next)
  (?nn1 rule ?odd-new)
  ;;;; (?nn1 rule ?this-rule)

  (?nn1 likes level-zero)		;; Assure level-zero is queued
  )
 (del
  (?this-obj rule ?this-rule)
  (?this-obj rule ?odd-new)
  ))

(rule
 (name even-next)
 (local)
 (root-var ?ae0)
 (pred
  (?a even ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae1 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?ae1 ref ?e1)
  (?e0 is-elem-of ?a)
  (?e0.5 is-elem-of ?a)
  (?e1 is-elem-of ?a)
  (?e0 next ?e0.5)
  (?e0.5 next ?e1))
 (add
  (print even-next ?a ?a1 ?ae0 ?ae1 ?e0 ?e1)
  (?ae0 next ?ae1))
 (del
  (?this-obj rule ?this-rule) ;; This can stay in if root-var is specified. 
  ))

(rule
 (name odd-new)
 (local)
 (pred
  (?a odd ?a1)
  (?e0 is-elem-of ?a)
  (?e0 od)
  (?e0 value ?v)
  (?nn1 new-node sn1)
  (?e0 local-rule-pool ?p)
  (?p lrp-rule ?odd-zero)
  (?p lrp-rule ?odd-next)
  (?p lrp-rule ?even-next)
  (?p lrp-rule ?even-new)

  (?odd-zero name odd-zero)
  (?odd-next name odd-next)
  (?even-next name even-next)
  (?even-new name even-new)
  )
 (add
  (print odd-new ?a ?e0 ?nn1)
  (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
  (?a1 elem ?nn1)
  (?nn1 value ?v)

  (?nn1 local-rule-pool ?p)

  (?nn1 on-ref ?e0)
  (?nn1 oe-ref ?e0)
  (?nn1 ref ?e0)

  (?nn1 xis ev-od-obj)
  (?nn1 rule ?odd-zero)
  (?nn1 rule ?odd-next)
  (?nn1 rule ?even-next)
  ;;;; (?nn1 rule ?even-new)
  (?nn1 rule ?this-rule)

  (?nn1 likes level-zero)		;; Assure level-zero is queued
  )
 (del
  (?this-obj rule ?this-rule)
  (?this-obj rule ?even-new)
  ))

(rule
 (name odd-next)
 (local)
 (root-var ?ae0)
 (pred
  (?a odd ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae1 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?ae1 ref ?e1)
  (?e0 is-elem-of ?a)
  (?e0.5 is-elem-of ?a)
  (?e1 is-elem-of ?a)
  (?e0 next ?e0.5)
  (?e0.5 next ?e1))
 (add
  (print odd-next ?a ?a1 ?ae0 ?ae1 ?e0 ?e1)
  (?ae0 next ?ae1))
 (del
  (?this-obj rule ?this-rule) ;; This can stay in if root-var is specified. 
  ))

(rule
 (name even-zero)
 (local)
 (pred
  (?a even ?a1)
  (?e0 is-elem-of ?a)
  (?ae0 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?e0 zero)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?cas-new)
  (?cas-new name cas-new)
  )
 (add
  (print even-zero ?this-obj ?a ?a1 ?ae0 ?e0)
  (?ae0 zero)
  (?ae0 rule ?cas-new)
  )
 (del
  (?this-obj rule ?this-rule)		;; Leaving in these dels looks ok
  ))

(rule
 (name odd-zero)
 (local)
 (pred
  (?a odd ?a1)
  (?ae0 is-elem-of ?a1)

  ;; These two preds, while not essential to match, help narrow it down
  ;; much better in subst-match, avoiding much depth scanning
  (?e0 is-elem-of ?a)  
  (?e1 is-elem-of ?a)
  
  (?ae0 ref ?e1)
  (?e0 next ?e1)
  (?e0 zero)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?cas-new)
  (?cas-new name cas-new)
  )
 (add
  (print odd-zero ?this-obj ?root-var ?a ?a1 ?ae0 ?e0)
  (?ae0 zero)
  (?ae0 rule ?cas-new)
  )
 (del
  (?this-obj rule ?this-rule)		;; Leaving in these dels looks ok
  ))


(comment ;; These don't seem to be needed anymore
(rule
 (name odd-new-rule-propagate)
 (local)
 (pred
  (?a odd ?a1)
  (?e0 is-elem-of ?a)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?odd-new)
  (?odd-new name odd-new))
 (add
  (print odd-new-rule-propagate ?a ?e0)
  (?e0 rule ?odd-new))
 (del
  ;; (?this-obj rule ?this-rule)		;; Tried putting this del in and it fails
  ))

(rule
 (name even-new-rule-propagate)
 (local)
 (pred
  (?a even ?a1)
  (?e0 is-elem-of ?a)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?even-new)
  (?even-new name even-new))
 (add
  (print even-new-rule-propagate ?a ?e0)
  (?e0 rule ?even-new)))
)

;; Begin copy-array-struct section
;; cas == copy-array-struct

(rule
 (name cas-new)
 (local)
 (root-var ?e0)
 (pred
  (?a copy-array-struct ?a1)
  (?e0 is-elem-of ?a)
  (?e1 is-elem-of ?a)
  (?e0 next ?e1)
  (?nn1 new-node sn1))
 (add
  (print cas-new ?this-obj ?root-var ?a ?a1 ?e0 ?e1 ?nn1)
  (?nn1 is-elem-of ?a1)
  (?a1 elem ?nn1)
  (?nn1 ref ?e0)
  )
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name cas-zero)
 (local)
 (root-var ?e0)
 (pred
  (?a copy-array-struct ?a1)
  (?e0 is-elem-of ?a)
  (?ae0 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?e0 zero))
 (add
  (print cas-zero ?this-obj ?root-var ?a ?a1 ?e0 ?ae0)
  (?ae0 zero)
  (?ae0 cas-zero)
  (?a1 casz-ref1 ?ae0)
  (?a casz-ref ?e0)
  )
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name cas-next)
 (local)
 (root-var ?ae0)
 (pred
  (?a copy-array-struct ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae1 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?ae1 ref ?e1)
  (?e0 is-elem-of ?a)
  (?e1 is-elem-of ?a)
  (?e2 is-elem-of ?a)
  (?e0 next ?e1)
  (?e1 next ?e2)
  )
 (add
  (print cas-next ?this-obj ?root-var ?a ?a1 ?ae0 ?ae1 ?e0 ?e1)
  (?ae0 next ?ae1)
  )
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name cas-rule-mod)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?cas-zero)
  (?cas-zero name cas-zero)
  (?p lrp-rule ?cas-new)
  (?cas-new name cas-new)
  (?p lrp-rule ?cas-next)
  (?cas-next name cas-next)
  (?p lrp-rule ?fft-comb-rule-zero)
  (?fft-comb-rule-zero name fft-comb-rule-zero)
  )
 (add
  (print cas-rule-mod)
  (?cas-new add ?nn1 rule ?cas-next)
  
  (?cas-new add ?e1 rule ?cas-new)
  (?cas-new del ?e0 rule ?cas-new)

  (?cas-next add ?e1 rule ?cas-zero)
  ;; (?cas-next del ?e0 rule ?cas-zero)	;; 7/29/24 Leaving this in makes an incomplete set of fft butterflies at n=6 and beyond! See doc.txt.
  (?cas-next add ?e1 rule-order ?cas-next ?cas-zero)

  (?cas-zero add ?ae0 rule ?fft-comb-rule-zero)

  ;; (?cas-next del ?ae1 rule ?cas-next)		;; Can't del the rule from here yet! Removed 1/9/24 when did "sequential" tree rules
  ;; (?cas-next del ?e1 rule ?cas-zero)
  )
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name tree-elem-rule-mod)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-elem-rule)						;; Note cross-module ref
  (?tree-elem-rule name tree-elem-rule)
  (?p lrp-rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name tree-elem-zero-rule)
  (?p lrp-rule ?even-new)
  (?even-new name even-new)
  (?p lrp-rule ?odd-new)
  (?odd-new name odd-new)
  (?p lrp-rule ?cas-new)
  (?cas-new name cas-new)
  )
 (add
  (print tree-elem-rule-mod)
  (?tree-elem-rule add ?x rule ?even-new)
  (?tree-elem-rule add ?x rule ?odd-new)
  (?tree-elem-zero-rule add ?x rule ?cas-new)
  )
 (del
  (?this-obj rule ?this-rule)))

;; End copy-array-struct section

;; We used to need a a root-var here, but not bailing from root-var loop fixed it.

(rule
 (name fft-comb-rule-next)
 (local)
 (pred
  (?x0 ?x1 fft-comb ?y)
  (?ef0 is-elem-of ?x0)
  (?ef1 is-elem-of ?x0)
  (?eg0 is-elem-of ?x1)
  (?eg1 is-elem-of ?x1)
  (?ef0 next ?ef1)
  (?eg0 next ?eg1)
  (?ey0 is-elem-of ?y)
  (?ey1 is-elem-of ?y)
  (?ey0 next ?ey1)
  (?ef0 ?eg0 fft-hb ?ey0))
 (add
  (print fft-comb-rule-next ?this-obj ?root-var ?x0 ?x1 ?y ?ef0 ?ef1 ?eg0 ?eg1 ?ey0 ?ey1)

  ;; (?ef0 rule ?this-rule)			;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;; (?eg0 rule ?this-rule)			;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;; (?ey0 rule ?this-rule)			;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  (?ef1 rule ?this-rule)
  (?eg1 rule ?this-rule)
  ;; (?ey1 rule ?this-rule)			;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  (?ef1 ?eg1 fft-hb ?ey1)

  ;; For display
  (?y comb-hb ?ey1)
  )
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name fft-comb-rule-zero)
 (local)
 ;; (root-var ?y)
 (root-var ?e1)
 (pred
  (?x0 ?x1 fft-comb ?y)
  (?e0 is-elem-of ?x0)
  (?e1 is-elem-of ?x1)
  (?ey is-elem-of ?y)
  (?e0 zero)
  (?e1 zero)
  (?ey zero)
  (?x0 local-rule-pool ?p)
  (?p lrp-rule ?fft-comb-rule-next)
  (?fft-comb-rule-next name fft-comb-rule-next)
  )
 (add
  (print fft-comb-rule-zero  ?this-obj ?x0 ?x1 ?y ?e0 ?e1 ?ey)
  ;; (?e0 rule ?fft-comb-rule-next)			;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  (?e1 rule ?fft-comb-rule-next)
  ;; (?ey rule ?fft-comb-rule-next)			;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  (?e0 ?e1 fft-hb ?ey)

  ;; For display

  (?y comb-hb ?ey)
  (?y comb-hb-z ?ey)
  
  )
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name fft-rule-level0)
 (local)
 (pred
  (?x fft ?y)
  (?x level 0)
  )
 (add
  (print fft-rule-level0 ?x ?y)
  (?x copy-array-struct ?y)
  (?y type array)
  (?y level 0)
  (?x level-zero)
  ;; (?y level-zero)
  (?x d ?y)	;; display-connection
  )
 (del
  (?this-obj rule ?this-rule)))

;; This is efficient since like "is" ("xis") it breaks the connection
;; that queued it once done. So even though it's a rule attached to a
;; constant we don't keep testing

(rule
 (name level-zero-rule)
 (attach-to level-zero)
 (pred
  (?x level-zero)
  (?y is-elem-of ?x)
  (?y rule ?even-new)
  (?even-new name even-new)
  (?y rule ?odd-new)
  (?odd-new name odd-new)
  (?x rule ?fft-rule)
  (?fft-rule name fft-rule)
  )
 (add
  (print level-zero-rule ?x ?y)
  (?x junk ?y))		;; Force an add for testing !!!!!!!!!!!
 (del
  (?y rule ?odd-new)
  (?y rule ?even-new)
  (?x rule ?fft-rule)
  (?x level-zero)))

;; This rule is global and bare-bones, with no rule-passing
;; and related optimizations. It's modified explicitly by
;; the optimizer in fft-delta.lisp.

(rule
 (name fft-rule)
 (local)
 (pred
  (?x fft ?y)
  (?x level ?l)
  (?l1 sigma ?l)
  (?x local-rule-pool ?p)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (?nn3 new-node sn3)
  (?nn4 new-node sn4)
  (?p lrp-rule ?fft-comb-rule-zero)
  (?fft-comb-rule-zero name fft-comb-rule-zero)
  (?p lrp-rule ?fft-rule-level0)
  (?fft-rule-level0 name fft-rule-level0)
  (?p lrp-rule ?fft-rule)
  (?fft-rule name fft-rule)
  )
 (add
  (print fft-rule ?x ?y ?l)
  (?x even ?nn1)
  (?x odd ?nn2)
  (?nn1 type array)
  (?nn2 weave-next ?nn1)
  (?nn2 type array)

  (?nn1 oe ?x)
  (?nn2 oe ?x)
  (?nn1 oev 0)
  (?nn2 oev 1)
  (?x copy-array-struct ?y)
  (?y type array)
  (?nn1 fft ?nn3)
  (?nn2 fft ?nn4)
  (?nn3 ?nn4 fft-comb ?y)
  #|
  ;;;;;; (?y rule ?fft-comb-rule-zero)		;; ok to remove this ?????
  ;;;; (?nn3 rule ?fft-comb-rule-zero)
  ;;;; (?nn4 rule ?fft-comb-rule-zero)
  |#
  (?nn1 rule ?fft-rule-level0)
  (?nn2 rule ?fft-rule-level0)
  (?nn1 rule-order ?fft-rule ?fft-rule-level0)
  (?nn2 rule-order ?fft-rule ?fft-rule-level0)
  (?nn1 rule ?fft-rule)
  (?nn2 rule ?fft-rule)
  
  ;;;;;; (?x rule ?fft-rule-level0)
  ;;;;;; (?y rule ?fft-rule-level0)
   
  (?y level ?l)
  (?nn1 level ?l1)
  (?nn2 level ?l1)

  (?nn1 local-rule-pool ?p)
  (?nn2 local-rule-pool ?p)
  (?nn3 local-rule-pool ?p)
  (?nn4 local-rule-pool ?p)

  ;; (queue ?nn1 ?nn2 ?nn3 ?nn4 ?y)
  ;; (exec ?nn1 ?nn2 ?nn3 ?nn4 ?y)
  )
 (del
  (?this-obj rule ?this-rule)
  (?this-obj rule ?fft-rule-level0)
  ))

(rule
 (name fft-top-rule)
 (attach-to fft-top)
 (pred
  (?x fft-top)
  (?x odd ?y)
  (?x even ?z)
  (?x rand ?r)
  (?r level ?level)
  (?x l ?l))
 (add
  (print fft-top-rule ?x ?y ?z)
  (note title "FFT Butterflies with Rule-30 Random Deltas")
  (note fft 2^ ?l points "\\n" rule-30 ?level levels)
  (?y weave-next ?z)
  (?x weave-next ?y)
  (?x weave-next-root)
  (?y weave-next-root)
  (?z weave-next-root)
  ))

;; Rule optimizer (for a specific rule). Adds rule propagators.
;; Experimental -- note this one adds more rules to a given node than needed.
;;
;; Note these two are disabled -- moved around rhe rule propagation and this is not currently used.

(rule
 (name fft-rule-opt-rule-names)
 (disabled)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name fft-rule-opt-rule-names))
 (add
  (print fft-rule-opt-rule-names)

  ;; (fft-rule-opt-rule-names-data copy-array-struct-new)

  ;;;; (fft-rule-opt-rule-names-data even-new-rule-propagate)
  ;;;; (fft-rule-opt-rule-names-data odd-new-rule-propagate)
  
  ;;;;;; (fft-rule-opt-rule-names-data fft-rule-level0)
  ;;;; (fft-rule-opt-rule-names-data fft-comb-rule-zero)
  )
 (del
  (global-node rule ?this-rule)))

(rule
 (name fft-rule-opt)
 (disabled)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool local-rule-pool-node)
  (local-rule-pool-node lrp-rule ?fft-rule)
  (?fft-rule name fft-rule)
  (local-rule-pool-node lrp-rule ?r)
  (?r name ?n)
  (fft-rule-opt-rule-names-data ?n)
  )
 (add
  (print fft-rule-opt ?fft-rule ?r ?n)
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

(rule
 (name fft-rule-opt-2-rule-names)
 (disabled)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name fft-rule-opt-rule-names))
 (add
  (print fft-rule-opt-2-rule-names)
  ;; (fft-rule-opt-2-rule-names-data copy-array-struct-new)
  ;; (fft-rule-opt-2-rule-names-data even-new-rule-propagate)
  ;; (fft-rule-opt-2-rule-names-data odd-new-rule-propagate)
  ;; (fft-rule-opt-2-rule-names-data fft-rule-level0)
  ;; (fft-rule-opt-2-rule-names-data fft-comb-rule-zero)
  )
 (del
  (global-node rule ?this-rule)))

(rule
 (name fft-rule-opt-2)
 (disabled)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node local-rule-pool local-rule-pool-node)
  (local-rule-pool-node lrp-rule ?fft-rule)
  (?fft-rule name fft-rule)
  (local-rule-pool-node lrp-rule ?r)
  (?r name ?n)
  (fft-rule-opt-2-rule-names-data ?n)
  )
 (add
  (print fft-rule-opt-2 ?fft-rule ?r ?n)
  (local-rule-pool-node lrp-rule ?fft-rule)
  (?fft-rule add print fft-opt-run ?fft-rule)
  (?fft-rule add ?nn1 rule ?r)
  (?fft-rule add ?nn2 rule ?r)
  ;; (?fft-rule add ?x rule ?r)
  ;; (?fft-rule add ?y rule ?r)
  (?fft-rule add ?nn1 rule ?fft-rule)
  (?fft-rule add ?nn2 rule ?fft-rule)
  (?fft-rule opt-done))
 (del
  (?this-obj rule ?this-rule)))


;; These "clean" versions are just for display purposes

(comment

(rule
 (name clean-fft-rule)
 (local)
 (pred
  (?x fft ?y)
  (?x level ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (?nn3 new-node sn3)
  (?nn4 new-node sn4))
 (add
  (?x even ?nn1)
  (?x odd ?nn2)
  (?x copy-array-struct ?y)
  (?nn1 fft ?nn3)
  (?nn2 fft ?nn4)
  (?nn3 ?nn4 fft-comb ?y)
  (?nn1 level ?l1)
  (?nn2 level ?l1)))

(rule
 (name clean-fft-comb-rule-zero)
 (local)
 (pred
  (?x0 ?x1 fft-comb ?y)
  (?e0 is-elem-of ?x0)
  (?e1 is-elem-of ?x1)
  (?ey is-elem-of ?y)
  (?e0 zero)
  (?e1 zero)
  (?ey zero))
 (add
  (?e0 ?e1 fft-hb ?ey)))

(rule
 (name clean-fft-comb-rule-next)
 (local)
 (pred
  (?x0 ?x1 fft-comb ?y)
  (?ef0 is-elem-of ?x0)
  (?ef1 is-elem-of ?x0)
  (?eg0 is-elem-of ?x1)
  (?eg1 is-elem-of ?x1)
  (?ef0 next ?ef1)
  (?eg0 next ?eg1)
  (?ey0 is-elem-of ?y)
  (?ey1 is-elem-of ?y)
  (?ey0 next ?ey1)
  (?ef0 ?eg0 fft-hb ?ey0))
 (add
  (?ef1 ?eg1 fft-hb ?ey1)))

)
