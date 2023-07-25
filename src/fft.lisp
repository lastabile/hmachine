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
						  (?a elem ?e0)
						  (?e0 is-elem-of ?a)
						  (?e0 zero))
						 (add
						  (print ev-init ?e0)
						  (?e0 ev)
						  (?e0 rule ?od-next)
						  (?e0 local-rule-pool ?p))
						 (del
						  (?this-obj rule ?this-rule)))))
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
  (?ev-next name ev-next))
 (add
  (print ev-od-opt)
  (?od-next add ?e1 rule ?ev-next)
  (?ev-next add ?e1 rule ?od-next))
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
  (ev-od-obj xrule ?ev-init)
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
  (?p lrp-rule ?odd-zero)
  (?p lrp-rule ?odd-next)
  (?p lrp-rule ?even-next)
  (?p lrp-rule ?copy-array-struct-new)
  (?even-zero name even-zero)
  (?odd-zero name odd-zero)
  (?odd-next name odd-next)
  (?even-next name even-next)
  (?copy-array-struct-new name copy-array-struct-new))
 (add
  (print even-new ?this-obj ?a ?e0 ?nn1)
  (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
  (?a1 elem ?nn1)
  (?nn1 value ?v)

  (?nn1 en-ref ?e0)
  (?nn1 oe-ref ?e0)
  (?nn1 ref ?e0)

  (?nn1 local-rule-pool ?p)

  (?nn1 is ev-od-obj)
  (?nn1 rule ?even-zero)
  (?nn1 rule ?odd-zero)
  (?nn1 rule ?odd-next)
  (?nn1 rule ?even-next)
  (?nn1 rule ?copy-array-struct-new)
  )
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name even-next)
 (local)
 ;; (attach-to even is-elem-of ref next)
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
  (?this-obj rule ?this-rule) ;; Leaving in these dels looks ok
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
  (?p lrp-rule ?even-zero)
  (?p lrp-rule ?odd-zero)
  (?p lrp-rule ?odd-next)
  (?p lrp-rule ?even-next)
  (?p lrp-rule ?copy-array-struct-new)
  (?even-zero name even-zero)
  (?odd-zero name odd-zero)
  (?odd-next name odd-next)
  (?even-next name even-next)
  (?copy-array-struct-new name copy-array-struct-new))
 (add
  (print odd-new ?a ?e0 ?nn1)
  (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
  (?a1 elem ?nn1)
  (?nn1 value ?v)

  (?nn1 local-rule-pool ?p)

  (?nn1 on-ref ?e0)
  (?nn1 oe-ref ?e0)
  (?nn1 ref ?e0)

  (?nn1 is ev-od-obj)
  (?nn1 rule ?even-zero)
  (?nn1 rule ?odd-zero)
  (?nn1 rule ?odd-next)
  (?nn1 rule ?even-next)
  (?nn1 rule ?copy-array-struct-new)
  )
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name odd-next)
 (local)
 ;; (attach-to odd is-elem-of ref next)
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
  (?this-obj rule ?this-rule)		;; Leaving in these dels looks ok
  ))


(rule
 (name even-zero)
 (local)
 (pred
  (?a even ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?e0 zero))
 (add
  (print even-zero ?this-obj ?a ?a1 ?ae0 ?e0)
  (?ae0 zero))
 (del
  (?this-obj rule ?this-rule)		;; Leaving in these dels looks ok
  ))

(rule
 (name odd-zero)
 (local)
 (pred
  (?a odd ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae0 ref ?e1)
  (?e0 next ?e1)
  (?e0 zero))
 (add
  (print odd-zero ?this-obj ?a ?a1 ?ae0 ?e0)
  (?ae0 zero))
 (del
  (?this-obj rule ?this-rule)		;; Leaving in these dels looks ok
  ))

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

(rule
 (name even-tree-max)
 (attach-to even)
 (attach-to odd)
 (pred
  (?x even ?y)
  (?x odd ?z)
  (?z weave-next ?y)
  (?x oe-max))
 (add
  (print even-tree-max ?this-obj ?x ?y)
  (?y oe-max)))

(rule
 (name odd-tree-zero)
 (attach-to even)
 (attach-to odd)
 (pred
  (?x odd ?y)
  (?x even ?z)
  (?y weave-next ?z)
  (?x oe-zero))
 (add
  (print odd-tree-zero ?this-obj ?x ?y)
  (?y oe-zero)))


;; This looks to be redundant, but not sure why.
;; However for some reason things are faster with it in, so I'll leave it for now

;; (comment
(rule
 (name odd-even-weave)
 (attach-to even)
 (attach-to odd)
 (pred
  (?p0 odd ?x00)
  (?p0 even ?x01)
  (?p1 odd ?x10)
  (?p1 even ?x11)
  (?x00 oe-zero)
  (?x11 oe-max)
  (?p0 oe-zero)
  (?p1 oe-max)
  (?p0 level ?l)
  (?p1 level ?l)
  (?x00 weave-next ?x01)
  (?x10 weave-next ?x11))
 (add
  (print odd-even-weave ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
  (?p1 weave-next ?x00)))
;; )

(rule
 (name weave-next-rule)
 (attach-to weave-next)
 (attach-to odd)
 (attach-to even)
 (pred
  (?p0 odd ?x00)
  (?p0 even ?x01)
  (?p1 odd ?x10)
  (?p1 even ?x11)
  (?x00 weave-next ?x01)
  (?x10 weave-next ?x11)
  (?p0 weave-next ?p1))
 (add
  (print weave-next-rule ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
  (?x01 weave-next ?x10)))


(rule
 (name install-copy-array-struct-next)
 (attach-to local-rule-pool-node)
 (pred 
  (local-rule-pool-node lrp-rule ?r)
  (?r add ?x is-elem-of ?y)
  (?r name ?n)
  (local-rule-pool-node lrp-rule ?copy-array-struct-next)
  (?copy-array-struct-next name copy-array-struct-next))
 (add
  (print install-copy-array-struct-next ?r ?x ?n)
  (?r add ?x rule ?copy-array-struct-next))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name copy-array-struct-next)
 (local)
 (pred
  (?a copy-array-struct ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae1 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?ae1 ref ?e1)
  (?e0 is-elem-of ?a)
  (?e1 is-elem-of ?a)
  (?e0 next ?e1))
 (add
  (print copy-array-struct-next ?this-obj ?root-var ?a ?a1 ?ae0 ?ae1 ?e0 ?e1)
  (?ae0 next ?ae1))
 (del
  (?this-obj rule ?this-rule)
  ))


(rule
 (name copy-array-struct-zero)
 (local)
 (pred
  (?a copy-array-struct ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae0 ref ?e0)
  (?e0 zero))
 (add
  (print copy-array-struct-zero ?this-obj ?a ?a1 ?ae0 ?e0)
  (?ae0 zero)
  (?a1 casz-ref1 ?ae0)
  (?a casz-ref ?e0))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name copy-array-struct-not-zero)
 (local)
 (pred
  (?a copy-array-struct ?a1)
  (?ae0 is-elem-of ?a1)
  (?ae0 ref ?e0))
 (not					;; Using not is convenient, but can probably do the same using tree-next, i.e., detect non-zero
  (?e0 zero))
 (add
  (print copy-array-struct-not-zero ?this-obj ?a ?a1 ?ae0 ?e0))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name opt-copy-array-struct-zero-and-not-zero)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?copy-array-struct-zero)
  (?copy-array-struct-zero name copy-array-struct-zero)
  (?p lrp-rule ?copy-array-struct-not-zero)
  (?copy-array-struct-not-zero name copy-array-struct-not-zero))
 (add
  (print opt-copy-array-struct-zero-and-not-zero)
  (?copy-array-struct-zero del ?ae0 rule ?copy-array-struct-not-zero)
  (?copy-array-struct-not-zero del ?ae0 rule ?copy-array-struct-zero))
 (del
  (?this-obj rule ?this-rule)))

(comment
(rule
 (name copy-array-struct-new-gen)
 (attach-to copy-array-struct)
 (pred
  (?a copy-array-struct ?a1)
  (?a local-rule-pool ?p)
  (?p lrp-rule ?copy-array-struct-zero)
  (?copy-array-struct-zero name copy-array-struct-zero)
  (?p lrp-rule ?copy-array-struct-not-zero)
  (?copy-array-struct-not-zero name copy-array-struct-not-zero)
  (?p lrp-rule ?copy-array-struct-next)
  (?copy-array-struct-next name copy-array-struct-next)
  )
 (add
  (print copy-array-struct-new-gen ?a ?a1)
  (?a rule
	  (rule
	   (name (copy-array-struct-new ?a))
	   (pred
		(?e0 is-elem-of ?a)
		(?a level ?l)
		(?nn1 new-node sn1))
	   (add
		(print copy-array-struct-new ?this-obj ?this-rule ?this-obj-1 ?this-rule-1 ?a ?a1 ?e0 ?nn1)
		(?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
		(?a1 elem ?nn1)
		(?nn1 ref ?e0)
		(?a1 level ?l)

		(?nn1 local-rule-pool ?p)

		(?nn1 casn-ref ?e0)

		(?nn1 rule ?copy-array-struct-zero)
		(?nn1 rule ?copy-array-struct-not-zero)
		(?e0 rule ?copy-array-struct-next)
		)
	   (del
		;; (?this-obj-1 rule ?this-rule-1) ;; !!!!!!
		)))))
)

(rule
 (name copy-array-struct-new-gen)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?copy-array-struct-zero)
  (?copy-array-struct-zero name copy-array-struct-zero)
  (?p lrp-rule ?copy-array-struct-not-zero)
  (?copy-array-struct-not-zero name copy-array-struct-not-zero)
  (?p lrp-rule ?copy-array-struct-next)
  (?copy-array-struct-next name copy-array-struct-next)
  )
 (add
  (print copy-array-struct-new-gen)
  (local-rule-pool-node lrp-rule
						(rule
						 (name copy-array-struct-new)
						 ;; (root-var ?e0)	;; This is desired -- should be auto 
						 (pred
						  (?e0 is-elem-of ?a)
						  (?a copy-array-struct ?a1)
						  (?a level ?l)
						  (?nn1 new-node sn1))
						 (add
						  (print copy-array-struct-new ?this-obj ?this-rule ?this-obj-1 ?this-rule-1 ?a ?a1 ?e0 ?nn1)
						  (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
						  (?a1 elem ?nn1)
						  (?nn1 ref ?e0)
						  (?a1 level ?l)
						  (?nn1 local-rule-pool ?p)
						  (?nn1 casn-ref ?e0)
						  (?nn1 rule ?copy-array-struct-zero)
						  (?nn1 rule ?copy-array-struct-not-zero)
						  (?e0 rule ?copy-array-struct-next)
						  )
						 (del
						  (?this-obj-1 rule ?this-rule-1)))))
 (del
  (?this-obj rule ?this-rule)))

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
  (?ef1 ?eg1 fft-hb ?ey1))
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name fft-comb-rule-zero)
 (local)
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
  (?e0 ?e1 fft-hb ?ey))
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name fft-rule-zero)
 (local)
 (pred
  (?x fft ?y)
  (?x level 0)
  )
 (add
  (print fft-rule-zero ?x ?y ?r)
  (?x copy-array-struct ?y)
  (?y level 0)
  (?x d ?y)	;; display-connection
  )
 (del
  (?this-obj rule ?this-rule)
  ))

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

  )
 (add
  (print fft-rule ?x ?y ?l)
  (?x even ?nn1)
  (?x odd ?nn2)
  (?nn2 weave-next ?nn1)
  (?nn1 oe ?x)
  (?nn2 oe ?x)
  (?nn1 oev 0)
  (?nn2 oev 1)
  (?x copy-array-struct ?y)
  (?nn1 fft ?nn3)
  (?nn2 fft ?nn4)
  (?nn3 ?nn4 fft-comb ?y)
  (?y rule ?fft-comb-rule-zero)
  (?nn3 rule ?fft-comb-rule-zero)
  (?nn4 rule ?fft-comb-rule-zero)
  (?y level ?l)
  (?nn1 level ?l1)
  (?nn2 level ?l1)

  (?nn1 local-rule-pool ?p)
  (?nn2 local-rule-pool ?p)
  (?nn3 local-rule-pool ?p)
  (?nn4 local-rule-pool ?p)
)
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name fft-top-rule)
 (attach-to fft-top)
 (pred

  (?x fft-top)		;; An experiment in symbol-free matching, i.e., looking for the three
					;; commented-out edges rather than fft-top, for max locality. Works, but slow.
  ;; (?x ?n1)
  ;; (?n1 ?n2)
  ;; (?n2 ?n3)
			
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
  (?y oe-zero)
  (?z oe-max)))


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
