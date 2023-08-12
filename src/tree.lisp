
(rule
 (name tree-next-zero-rule)
 (local)
 (pred
  (?x0 l 0)
  (?x1 l 0)
  (?x0 tree-next ?x1))
 (add
  (print tree-next-zero-rule ?this-obj ?x0 ?x1)
  (?x0 next ?x1)
  ;; (queue ?x1)
  )
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name tree-next-rule)
 (local)
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
  (?this-obj is-not treeobj)
  ;; (queue ?x10)
  )
 (del
  (?this-obj rule ?this-rule)))		;; !!!!!!!!!!!!!

(rule
 (name tree-loop-rule)
 (local)
 (root-var ?x)
 (pred
  (?x top ?t)
  (?y top ?t)
  (?x zero)
  (?x l 0)
  (?y max)
  (?y l 0))
 (add
  (print tree-loop-rule ?this-obj ?x ?y ?root-var)
  (?y next ?x)
  ;; (queue)
  )
 (del
  (?this obj rule ?this-rule)
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
  (?rp lrp-rule ?tree-top-propagate-rule)
  (?tree-top-propagate-rule name tree-top-propagate-rule)
  (?rp lrp-rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?rp lrp-rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name tree-elem-zero-rule))
 (add
  (print tree-top-order-rule ?this-obj ?x ?y ?p)
  (?x top ?p)
  (?y top ?p)
  (?x zero)
  (?y max)
  (?x rule ?tree-zero-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)
  (?x rule ?tree-top-propagate-rule)
  (?y rule ?tree-top-propagate-rule)
  (?y rule ?tree-max-rule)
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
  ;; (?x is-not treeobj)		;; Commented-out because it appears to work (at least for small trees)
								;; however failures are possible due to this rule not 
								;; detecting a complete array-elem object
  )
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
  (?x is ev-od-obj))
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
  (?tree-elem-zero-rule name tree-elem-zero-rule))
 (add
  (print tree-zero-rule ?this-obj ?x ?y ?p)
  (?x zero)
  (?x rule ?tree-zero-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule))
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
  )
 (add
  (print tree-max-rule ?this-obj ?x ?y ?p)
  (?y max)
  (?y rule ?tree-max-rule)
  )
 (del
  (?p rule ?tree-max-rule))
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
  (?nn1 new-node sn1)
  (?nn2 new-node sn2))
 (add
  (print tree-rule ?this-obj ?x ?nn1 ?nn2 ?l)
  (?nn1 aup ?x)
  (?nn1 ul ?x)	;; ul = up-left
  (?nn1 l ?l1)
  (?nn1 is treeobj)
  (?nn2 aup ?x)
  (?nn2 ur ?x)	;; ur = up-right
  (?nn2 l ?l1)
  (?nn2 is treeobj)
  (?nn1 tree-next ?nn2)
;;  (exec ?nn1 ?nn2)
))

(rule
 (name tree-leaf-rule)
 (local)
 (pred
  (?x l 0))
 (add
  (print tree-leaf-rule ?x)))

(rule
 (name tree-rule-opt)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-rule)
  (?tree-rule name tree-rule)
  (?p lrp-rule ?tree-leaf-rule)
  (?tree-leaf-rule name tree-leaf-rule))
 (add
  (print tree-rule-opt)
  (?tree-rule add ?nn1 rule ?tree-rule)
  (?tree-rule add ?nn2 rule ?tree-rule)
  (?tree-rule add ?nn1 rule ?tree-leaf-rule)
  (?tree-rule add ?nn2 rule ?tree-leaf-rule)
  (?tree-rule del ?x rule ?tree-rule)
  (?tree-rule del ?x rule ?tree-leaf-rule)
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
  (?x is treeobj)
  (?x l ?l)
  (?x top)
  (?x type array)
  (?x rule ?tree-top-order-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)))

(rule
 (name treeobj-rule)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-next-zero-rule)
  (?p lrp-rule ?tree-next-rule)
  (?p lrp-rule ?tree-elem-rule)
  ;; (?p lrp-rule ?tree-zero-rule)
  ;; (?p lrp-rule ?tree-max-rule)
  (?tree-next-zero-rule name tree-next-zero-rule)
  (?tree-next-rule name tree-next-rule)
  (?tree-elem-rule name tree-elem-rule)
  ;; (?tree-zero-rule name tree-zero-rule)
  ;; (?tree-max-rule name tree-max-rule)
  )
 (add
  (print treeobj-rule)
  (treeobj xrule ?tree-next-zero-rule)
  (treeobj xrule ?tree-next-rule)
  (treeobj xrule ?tree-elem-rule)
  ;; (treeobj xrule ?tree-zero-rule)
  ;; (treeobj xrule ?tree-max-rule)
  )
 (del
  (global-node rule ?this-rule)))

(comment

;; The old way, replaced by the chunk starting at tree-rule above.

(rule
 (name tree-rule)
 (root-var ?x)
 (local)
 (pred
  (?x l ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2))
 (add
  (print tree-rule ?this-obj ?x ?nn1 ?nn2 ?l)
  (?nn1 aup ?x)
  (?x adn ?nn1)
  ;; (?nn1 p)
  (?nn1 l ?l1)
  (?nn1 is treeobj)
  (?nn2 aup ?x)
  (?x adn ?nn2)
  ;; (?nn2 p)
  (?nn2 l ?l1)
  (?nn2 is treeobj)
  (?nn1 tree-next ?nn2))
 (del
  (?x rule ?this-rule)))

(rule
 (name treeobj-rule)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-next-zero-rule)
  (?p lrp-rule ?tree-next-rule)
  (?p lrp-rule ?tree-elem-rule)
  (?p lrp-rule ?tree-zero-rule)
  (?p lrp-rule ?tree-max-rule)
  (?p lrp-rule ?tree-rule)
  (?tree-next-zero-rule name tree-next-zero-rule)
  (?tree-next-rule name tree-next-rule)
  (?tree-elem-rule name tree-elem-rule)
  (?tree-zero-rule name tree-zero-rule)
  (?tree-max-rule name tree-max-rule)
  (?tree-rule name tree-rule))
 (add
  (print treeobj-rule)
  (treeobj xrule ?tree-next-zero-rule)
  (treeobj xrule ?tree-next-rule)
  (treeobj xrule ?tree-elem-rule)
  (treeobj xrule ?tree-zero-rule)
  (treeobj xrule ?tree-max-rule)
  (treeobj xrule ?tree-rule))
 (del
  (global-node rule ?this-rule)))

(rule
 (name treetopobj-rule)
 (attach-to global-node)
 (pred
  (global-node local-rule-pool ?p)
  (?p lrp-rule ?tree-top-rule)
  (?tree-top-rule name tree-top-rule))
 (add
  (print treetopobj-rule)
  (treetopobj rule ?tree-top-rule))
 (del
  (global-node rule ?this-rule)))

(rule
 (name tree-top-rule)
 (local)
 (pred
  (?x local-rule-pool ?p)
  (?p lrp-rule ?tree-top-order-rule)
  (?tree-top-order-rule name tree-top-order-rule)
  (?p lrp-rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?p lrp-rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name  tree-elem-zero-rule))
 (add
  (print tree-top-rule ?this-obj ?x)
  ;; (?x p)
  (?x top)
  (?x is treeobj)
  (?x rule ?tree-top-order-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)
  )
 (del
  (?x rule ?this-rule)))

)
