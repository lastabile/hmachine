
;;; 11/1/17 Commented-out remaining queueing operations (retaining old
;;; commenting-out) since I'm taking queue operations out of the rule
;;; language, but would like retain this file and the rule-passing.

(rule
 (name tree-next-zero-rule)
 (local)
 (pred
  (?x0 l 0)
  (?x1 l 0)
  (?x0 tree-next ?x1))
 (add
  (print tree-next-zero-rule ?this-obj ?x0 ?x1)
  ;; (dont-queue)
  (?x0 next ?x1)))

(rule
 (name tree-next-rule)
 (local)
 (root-var ?x00)
 (pred
  (?x00 aup ?p0)
  (?x01 aup ?p0)
  (?x10 aup ?p1)
  (?x11 aup ?p1)
  (?p0 adn ?x00)
  (?p0 adn ?x01)
  (?p1 adn ?x10)
  (?p1 adn ?x11)
  (?x00 tree-next ?x01)
  (?x10 tree-next ?x11)
  (?p0 tree-next ?p1))
 (add
  (print tree-next-rule ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
  ;; (dont-queue)
  (?x01 tree-next ?x10)))

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
  ;; ;; (dont-queue)
  (?y next ?x))
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
  (?p p)
  (?p local-rule-pool ?rp)
  (?rp lrp-rule ?tree-top-propagate-rule)
  (?tree-top-propagate-rule name tree-top-propagate-rule)
  (?p lrp-rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?p lrp-rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name tree-elem-zero-rule))
 (add
  (print tree-top-order-rule ?this-obj ?x ?y ?p)
  ;; (queue ?x)
  ;; ;; (dont-queue)
  (?x top ?p)
  (?y top ?p)
  (?x zero)
  (?y max)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)
  (?x rule ?tree-top-propagate-rule)
  (?y rule ?tree-top-propagate-rule))
 (del
  (?this-obj rule ?this-rule)
  (?p rule ?tree-loop-rule)
  (?p rule ?tree-elem-zero-rule)))

#|
(rule
 (name tree-top-propagate-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p top ?t)
  (?p p))
 (add
  (print tree-top-propagate-rule ?this-obj ?x ?y ?p)
  ;; (dont-queue)
  (?x top ?t)
  (?y top ?t)
  (?x rule ?this-rule)
  (?y rule ?this-rule))
 (del
  (?this-obj rule ?this-rule)))
|#

(rule
 (name tree-top-propagate-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?p top ?t)
  (?p p))
 (add
  (print tree-top-propagate-rule ?this-obj ?x ?p)
  ;; (dont-queue)
  (?x top ?t)
  (?x rule ?this-rule))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name tree-elem-rule)
 (local)
 (pred
  (?x top ?t)
  (?x l 0)
  (?v new-node sn1)
  (?x local-rule-pool ?p)
  (?p lrp-rule ?tree-elem-rule-prune)
  (?tree-elem-rule-prune name tree-elem-rule-prune))
 (add
  (print tree-elem ?this-obj ?t ?x)
  ;; (dont-queue)
  (?t elem ?x)
  (?x value ?v)
  (?x rule ?tree-elem-rule-prune)
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
  ;; ;; (dont-queue)
  (?x xis ev-od-obj))
 (del
  (?this-obj rule ?this-rule)))

(rule
 (name tree-elem-rule-prune)
 (local)
 (pred
  (?x top ?t)
  (?x l 0)
  (?x rule ?tree-rule)
  (?tree-rule name tree-rule))
 (add
  (print tree-elem-rule-prune ?this-obj ?x)
  ;; ;;(dont-queue)
  )
 (del
  (?x rule ?tree-rule)
  (?x rule ?this-rule)))

#|
(rule
 (name tree-elem-rule-prune)
 (local)
 (pred
  (?x top ?t)
  (?x l 0)
  (?x from-is-rule ?r))
 (add
  (print tree-elem-rule-prune ?x))
 (del
  (?x rule ?r)
  (?x from-is-rule ?r)
  (?x rule ?this-rule)))
|#

(rule
 (name tree-zero-rule)
 (local)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p zero)
  (?p rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?p rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name tree-elem-zero-rule))
 (add
  (print tree-zero-rule ?this-obj ?x ?y ?p)
  ;; (dont-queue)
  (?x zero)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule))
 (del
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
  (?p max))
 (add
  (print tree-max-rule ?this-obj ?x ?y ?p)
  ;; (dont-queue)
  (?y max)))

(rule
 (name tree-rule)
 (root-var ?x)
 (local)
 (pred
  (?x p)
  (?x l ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2))
 (add
  ;; ;; (dont-queue ?x)
  ;; (dont-queue ?l)
  ;; (dont-queue ?l1)
  ;; (queue ?nn1)
  ;; (queue ?nn2)
  ;; ;; (dont-queue)
  (print tree-rule ?this-obj ?x ?nn1 ?nn2 ?l)
  (?nn1 aup ?x)
  (?x adn ?nn1)
  (?nn1 p)
  (?nn1 l ?l1)
  (?nn1 xis treeobj)
  (?nn2 aup ?x)
  (?x adn ?nn2)
  (?nn2 p)
  (?nn2 l ?l1)
  (?nn2 xis treeobj)
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
  ;; ;; (dont-queue)
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
  ;; ;; (dont-queue)
  (treetopobj rule ?tree-top-rule))
 (del
  (global-node rule ?this-rule)))

(rule
 (name tree-top-rule)
 (local)
 (pred
  (?x local-rule-pool ?p)
  (?p lrp-rule ?tree-top-rule)
  (?tree-top-rule name tree-top-rule)
  (?p lrp-rule ?tree-top-order-rule)
  (?tree-top-order-rule name tree-top-order-rule)
  (?p lrp-rule ?tree-loop-rule)
  (?tree-loop-rule name tree-loop-rule)
  (?p lrp-rule ?tree-elem-zero-rule)
  (?tree-elem-zero-rule name  tree-elem-zero-rule))
 (add
  (print tree-top-rule ?this-obj ?x)
  ;; (queue ?x)
  ;; ;; (dont-queue)
  (?x p)
  (?x top)
  (?x xis treeobj)
  (?x rule ?tree-top-order-rule)
  (?x rule ?tree-loop-rule)
  (?x rule ?tree-elem-zero-rule)
  )
 (del
  (?x rule ?this-rule)))
