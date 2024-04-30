
;; 1/9/24
;;
;; Added this tree-next-level0-zero-rule rule and modified others to get a purely sequential rule triggering of the
;; "next" relation. This includes tree-loop-rule, so when that runs we know the tree is "done."
;;
;; This is in service of finding good sequential models. It's not the best thing for parallelism and illustrates the
;; trade-offs. Note the rule efficiency goes down just a little.

(rule
 (name tree-next-level0-zero-rule)
 (local)
 (pred
  (?x0 l 0)
  (?x0 zero)
  (?x1 l 0)
  (?x0 tree-next ?x1))
 (add
  (print tree-next-level0-zero-rule ?this-obj ?x0 ?x1)
  (?x0 next ?x1)
  )
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name tree-next-level0-rule)
 (local)
 (pred
  (?x0 l 0)
  (?x1 l 0)
  (?x2 l 0)
  (?x0 next ?x1)
  (?x1 tree-next ?x2))
 (add
  (print tree-next-level0-rule ?this-obj ?x0 ?x1 ?x2)
  (?x1 next ?x2)
  )
 (del
  (?this-obj rule ?this-rule)))

;; This is the original tree-next-level0-rule before the sequential change noted above. Note we just depend on tree-next
;; not prev nexts.

(comment
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
 (del
  (?this-obj rule ?this-rule)))
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
  (?x01 x-tree-next ?x10)		;; for diag
  (?this-obj xis-not treeobj)
  (?this-obj xis-not treeobj-2)
  ;; (queue ?x10)
  )
 (del
  (?this-obj rule ?this-rule)))		;; !!!!!!!!!!!!!

#|
;; Needs rethink!

(rule
 (name tree-next-rule-mod)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-next-rule)
  (?tree-next-rule name tree-next-rule))
 (add
  (print tree-next-rule-mod)
  (?tree-next-rule del ?p0 rule ?tree-next-rule)))
|#


;; This rule was modified to conform to the sequential model noted in the header to this file. One clause was added,
;; which requires a prev next node.

(rule
 (name tree-loop-rule)
 (local)
 ;; (root-var ?x) ;; Fails with explicit root-var
 (pred
  (?x top ?t)
  (?y top ?t)
  (?x zero)
  (?x l 0)
  (?y max)
  (?y l 0)
  (?y0 next ?y) 						;; Added to conform to sequential model
  )
 (add
  (print tree-loop-rule ?this-obj ?x ?y ?root-var)
  (?y next ?x)
  ;; (queue)
  )
 (del
  (?this-obj rule ?this-rule)
  ))

(rule
 (name tree-top-order-rule)
 (local)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p top)
  ;; (?p p)
  (?p local-rule-pool ?rp)
  (?rp lrp-rule ?tree-zero-rule)
  (?tree-zero-rule name tree-zero-rule)
  (?rp lrp-rule ?tree-max-rule)
  (?tree-max-rule name tree-max-rule)
  ;; (?rp lrp-rule ?tree-top-propagate-rule)
  ;; (?tree-top-propagate-rule name tree-top-propagate-rule)
  (?rp lrp-rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?rp lrp-rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name tree-elem-zero-rule)
  (?rp lrp-rule ?tree-next-level0-zero-rule)
  (?tree-next-level0-zero-rule name tree-next-level0-zero-rule))
 (add
  (print tree-top-order-rule ?this-obj ?x ?y ?p)
  (?x top ?p)
  (?y top ?p)
  (?x zero)
  (?y max)
  (?x rule ?tree-zero-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)
  (?x rule ?tree-next-level0-zero-rule)
  ;; (?x rule ?tree-top-propagate-rule)
  ;; (?y rule ?tree-top-propagate-rule)
  (?y rule ?tree-max-rule)
  (?y rule ?tree-loop-rule)
  ;; (queue ?x ?y)
  )
 (del
  (?this-obj rule ?this-rule)
  (?p rule ?tree-loop-rule)
  (?p rule ?tree-elem-zero-rule)))

(rule
 (name tree-top-propagate-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?p top ?t)
  (?t local-rule-pool ?rp))
 (add
  (print tree-top-propagate-rule ?this-obj ?x ?p)
  (?x top ?t)
  (?x rule ?this-rule)
  (?x local-rule-pool ?rp))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name tree-elem-rule)
 (local)
 (pred
  (?x top ?t)
  (?x l 0)
  (?v new-node sn1)
  (?x local-rule-pool ?p))
 (add
  (print tree-elem-rule ?this-obj ?t ?x)
  (?t elem ?x)
  (?x value ?v)
  ;; (?x xis-not treeobj-2)
  (?x xis-not treeobj)
  )
 (del
  (?this-obj rule ?this-rule))
 )

(rule
 (name tree-elem-zero-rule)
 (local)
 (root-var ?x)
 (pred
  (?x top ?t)
  (?x l 0)
  (?x zero))
 (add
  (print tree-elem-zero-rule ?this-obj ?t ?x)
  (?x xis ev-od-obj))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name tree-zero-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p zero)
  (?p rule ?tree-zero-rule)
  (?tree-zero-rule name tree-zero-rule)
  (?p rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?p rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name tree-elem-zero-rule)
  (?p rule ?tree-next-level0-zero-rule)
  (?tree-next-level0-zero-rule name tree-next-level0-zero-rule))
 (add
  (print tree-zero-rule ?this-obj ?x ?y ?p)
  (?x zero)
  (?x rule ?tree-zero-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)
  (?x rule ?tree-next-level0-zero-rule))
 (del
  (?p rule ?tree-zero-rule)
  (?p rule ?tree-loop-rule)
  (?p rule ?tree-elem-zero-rule)))

(rule
 (name tree-max-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p max)
  (?p rule ?tree-max-rule)
  (?tree-max-rule name tree-max-rule)
  (?p rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  )
 (add
  (print tree-max-rule ?this-obj ?x ?y ?p)
  (?y max)
  (?y rule ?tree-max-rule)
  (?y rule ?tree-loop-rule)  
  )
 (del
  (?p rule ?tree-max-rule)
  (?p rule ?tree-loop-rule)
  )
 )

;; tree-rule and tree-leaf-rule are modified by tree-rule-opt to
;; install rule propagation and deletion. By doing so we cut rules off
;; as soon as they fire, and thus the tree builds with complexity 2^?l,
;; which is best, and with a mininum constant of proportionality wrt
;; the number of tests made to a node.
;;
;; Looks like tree-leaf-rule is optimized to run only when it will
;; succeed due to the ordering of matching of tree-rule versus
;; tree-leaf-rule. tree-rule matches first, and deletes
;; tree-leaf-rule before it can be tested.
;;
;; This is accidental, but illustrates the issues in finding good
;; sequential forms for something which is inherently parallel like
;; this. Specification of sequential behavior is not easy in H.

(rule
 (name tree-rule)
 (local)
 (pred
  (?x l ?l)
  (?l1 sigma ?l)
  (?x top ?t)
  (?t local-rule-pool ?rp)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2))
 (add
  (print tree-rule ?this-obj ?x ?nn1 ?nn2 ?l)
  (?nn1 aup ?x)
  (?nn1 ul ?x)	;; ul = up-left
  (?nn1 l ?l1)
  (?nn1 local-rule-pool ?rp)
  (?nn1 top ?t)
  (?nn1 xis treeobj)
  (?nn1 xis treeobj-2)
  (?nn2 aup ?x)
  (?nn2 ur ?x)	;; ur = up-right
  (?nn2 l ?l1)
  (?nn2 local-rule-pool ?rp)
  (?nn2 top ?t)
  (?nn2 xis treeobj)
  (?nn2 xis treeobj-2)
  (?nn1 tree-next ?nn2)
  (exec ?nn1 ?nn2)
  ))

(rule
 (name tree-leaf-rule)
 (local)
 (pred
  (?x l 0))
 (add
  (print tree-leaf-rule ?x)))

;; 8/28/23 This model has been successful here, as the combo of tree-rule and tree-elem-rule, with deletion and rule
;; ordering, as below, causes tree-elem-rule to have 100% efficiency. Few rules do.
;;
;; This technique is worth exploring in depth, i.e., where some "interior" rule passes down, but also deletes, a "leaf"
;; rule.  High efficiency is promoted using rule-order, which always deletes the leaf rule before it can be tested. Thus
;; the leaf rule never runs until it is at a leaf.
;;
;; There is a doc entry for this. See 8/19/23.

(rule
 (name tree-rule-opt)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-rule)
  (?tree-rule name tree-rule)
  (?p lrp-rule ?tree-leaf-rule)
  (?tree-leaf-rule name tree-leaf-rule)
  (?p lrp-rule ?tree-elem-rule)
  (?tree-elem-rule name tree-elem-rule))
 (add
  (print tree-rule-opt)

  (?tree-rule add ?nn1 rule ?tree-rule)
  (?tree-rule add ?nn2 rule ?tree-rule)
  (?tree-rule add ?nn1 rule ?tree-leaf-rule)
  (?tree-rule add ?nn2 rule ?tree-leaf-rule)
  (?tree-rule add ?nn1 rule ?tree-elem-rule)
  (?tree-rule add ?nn2 rule ?tree-elem-rule)

  (?tree-rule add ?nn1 rule-order ?tree-rule ?tree-elem-rule)
  (?tree-rule add ?nn2 rule-order ?tree-rule ?tree-elem-rule)
  
  (?tree-rule del ?x rule ?tree-rule)
  (?tree-rule del ?x rule ?tree-leaf-rule)
  (?tree-rule del ?x rule ?tree-elem-rule)
  
  (?tree-leaf-rule del ?x rule ?tree-rule)
  (?tree-leaf-rule del ?x rule ?tree-leaf-rule)
  )
 (del
  (global-node rule ?this-rule)))

(rule
 (name tree-top-rule)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-rule)
  (?tree-rule name tree-rule)
  (?p lrp-rule ?tree-leaf-rule)
  (?tree-leaf-rule name tree-leaf-rule)
  (?p lrp-rule ?tree-top-order-rule)
  (?tree-top-order-rule name tree-top-order-rule)
  (?p lrp-rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?p lrp-rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name  tree-elem-zero-rule)
  (tree-rule ?x ?l))			;; Tie the data to the node tree-rule since it's an easy way to get all the pred edges connected, as required by H
 (add
  (print tree-top-rule ?x ?l)
  (?x rule ?tree-rule)
  (?x rule ?tree-leaf-rule)
  (?x xis treeobj)
  (?x xis treeobj-2)
  (?x l ?l)
  (?x top)
  (?x top ?x)
  (?x type array)
  (?x rule ?tree-top-order-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)))

(rule
 (name treeobj-rule)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-next-rule)
  (?tree-next-rule name tree-next-rule)
  ;; (?p lrp-rule ?tree-elem-rule)
  ;; (?tree-elem-rule name tree-elem-rule)
  ;; (?p lrp-rule ?tree-next-level0-rule)
  ;; (?tree-next-level0-rule name tree-next-level0-rule)
  )
 (add
  (print treeobj-rule)
  ;; (treeobj has rule ?tree-next-level0-rule)
  (treeobj has rule ?tree-next-rule)
  ;; (treeobj has rule ?tree-elem-rule)
  ;; (treeobj has rule-order ?tree-next-rule ?tree-next-level0-rule ?tree-elem-rule)
  )
 (del
  (global-node rule ?this-rule)))

(rule
 (name treeobj-rule-2)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-next-level0-rule)
  (?tree-next-level0-rule name tree-next-level0-rule)
  )
 (add
  (print treeobj-rule-2)
  (treeobj-2 has rule ?tree-next-level0-rule)
  )
 (del
  (global-node rule ?this-rule)))

;; Local Variables:
;; eval: (emacs-file-locals)
;; End:
