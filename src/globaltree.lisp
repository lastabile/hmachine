(rule
 (name tree-next-zero-rule)
 (pred
  (?x0 l 0)
  (?x1 l 0)
  (?x0 tree-next ?x1))
 (add
  (print tree-next-zero-rule ?this-obj ?x0 ?x1)
  (?x0 next ?x1)))

(rule
 (name tree-next-rule)
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
  (?x01 tree-next ?x10)))

(rule
 (name tree-loop-rule)
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
  (?y next ?x)))

(rule
 (name tree-top-propagate-rule)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?p top ?t))
 (add
  (print tree-top-propagate-rule ?this-obj ?x ?p)
  (?x top ?t)))

(rule
 (name tree-elem-rule)
 (pred
  (?t top)
  (?x top ?t)
  (?x l 0)
  (?v new-node sn1))
 (add
  (print tree-elem-rule ?this-obj ?t ?x ?v)
  (?t elem ?x)
  (?x value ?v)))

(rule
 (name tree-elem-zero-rule)
 (root-var ?x)
 (pred
  (?x top ?t)
  (?x l 0)
  (?x zero))
 (add
  (print tree-elem-zero-rule ?this-obj ?t ?x)
  (?x xis ev-od-obj)))

(rule
 (name tree-zero-rule)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p zero))
 (add
  (print tree-zero-rule ?this-obj ?x ?y ?p)
  (?x zero)))

(rule
 (name tree-max-rule)
 (root-var ?p)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p max))
 (add
  (print tree-max-rule ?this-obj ?x ?y ?p)
  (?y max)))

(rule
 (name tree-rule)
 (root-var ?x)
 (pred
  (?x l ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2))
 (add
  (print tree-rule ?this-obj ?x ?nn1 ?nn2 ?l)
  (?nn1 aup ?x)
  (?x adn ?nn1)
  (?nn1 l ?l1)
  (?nn2 aup ?x)
  (?x adn ?nn2)
  (?nn2 l ?l1)
  (?nn1 tree-next ?nn2)))

(rule
 (name tree-top-rule)
 (pred
  (?x is treetopobj levels ?l))
 (add
  (print tree-top-rule ?this-obj ?x)
  (?x top)
  (?x l ?l)))

(rule
 (name tree-top-order-rule)
 (pred
  (?x aup ?p)
  (?y aup ?p)
  (?x tree-next ?y)
  (?p top))
 (add
  (print tree-top-order-rule ?this-obj ?x ?y ?p)
  (?x top ?p)
  (?y top ?p)
  (?x zero)
  (?y max)))

;; If this is activated, be sure to comment-out tree-top-order-rule and tree-top-rule

(comment

(rule
 (name tree-top-rule)
 (pred
  (?x is treetopobj levels ?l)
  (?x local-rule-pool ?p)
  (?p lrp-rule ?tr)
  (?tr name tree-rule)
  (?p lrp-rule ?cr)
  (?cr name copy-rule-rule)
  (?nntrc new-node sn1))
 (add
  (print tree-top-rule ?this-obj ?this-rule ?this-rule-name ?x ?l ?p ?tr ?cr ?nntrc)
  (?x top)
  (?x l ?l)
  (?tr copy-rule ?nntrc)
  (?nntrc rule ?cr)
  (?x rule ?nntrc)
  ;; (?x tree-rule-copy-node ?tnn1)  ;; !!!!!!!
  (?nntrc add (?nn1 zero))
  (?nntrc add (?nn2 max))
  (?nntrc add (?nn1 top ?x))
  (?nntrc add (?nn2 top ?x))
  (?nntrc add (print tree-rule-copy ?x ?l ?nn1 ?nn2))
  (?nntrc del (?x rule ?nntrc))))

)

(comment
 (rule
  (name tree-top-rule1)
  (pred
   (?x is treetopobj levels ?l)
   (?x local-rule-pool ?p)
   (?p lrp-rule ?r)
   (?r name tree-rule)
   (?tnn1 new-node sn1))
  (add
   (print tree-top-rule1 ?this-obj ?x ?l ?p ?r ?tnn1)
   (?x top)
   (?x l ?l)
   (?r copy-rule ?tnn1)
   (?x tree-rule-copy-node ?tnn1)
   (?x rule (rule
			 (name tree-top-rule2)
			 (pred
			  (?x tree-rule-copy-node ?c)
			  (?c rule ?cr)
			  ;; (?an1 new-node sn1)
			  ;; (?an2 new-node sn2)
			  ;; (?an3 new-node sn3)
			  ;; (?an4 new-node sn4)
			  )
			 (add
			  (print tree-top-rule2 ?this-obj ?x ?c ?cr ?nn1 ?nn2)
			  (plain edge ?cr)
			  ;; (?cr add ?an1)
			  ;; (?an1 elem0 ?nn1)
			  ;; (?an1 elem1 zero)
			  ;; (?cr add ?an2)
			  ;; (?an2 elem0 ?nn2)
			  ;; (?an2 elem1 max)
			  ;; (?cr add ?an3)
			  ;; (?an3 elem0 ?nn1)
			  ;; (?an3 elem1 top)
			  ;; (?an3 elem2 ?x)
			  ;; (?cr add ?an4)
			  ;; (?an4 elem0 ?nn2)
			  ;; (?an4 elem1 top)
			  ;; (?an4 elem2 ?x)
			  (?cr add (?nn1 zero))
			  (?cr add (?nn2 max))
			  (?cr add (?nn1 top ?x))
			  (?cr add (?nn2 top ?x))
			  (?x rule ?cr)
			  ))))))


#|
(x is treetopobj 5)

(x is treetopobj levels 5)

|#
