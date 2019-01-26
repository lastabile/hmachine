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
 ;; (root-var ?x)			;; !!!!!!!!!!!!!!!!!!
 ;; (local)					;; !!!!!!!!!!!!!! Need to flip this to local for the tree-copy method of tree-top-rule
 (pred
  (?x l ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (?x local-rule-pool ?p)
  (?p lrp-rule ?tr)
  (?tr name tree-rule))
 (add
  (print tree-rule ?this-rule ?this-rule-name ?this-obj ?x ?nn1 ?nn2 ?l)
  (?nn1 aup ?x)
  (?x adn ?nn1)
  (?nn1 l ?l1)
  (?nn2 aup ?x)
  (?x adn ?nn2)
  (?nn2 l ?l1)
  (?nn1 tree-next ?nn2)
  (?nn1 rule ?tr)
  (?nn2 rule ?tr)))

;; (comment

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

;; )

;; If this is activated, be sure to comment-out tree-top-order-rule and tree-top-rule, and set tree-rule to local

(comment

(rule
 (name tree-top-rule)
 (pred
  (?xx is treetopobj levels ?ll)
  (?xx local-rule-pool ?p)
  (?p lrp-rule ?tr)
  (?tr name tree-rule)
  (?p lrp-rule ?cr)
  (?cr name copy-rule-rule)
  (?nntrc new-node sn1))
 (add
  (print tree-top-rule ?this-obj ?this-rule ?this-rule-name ?xx ?ll ?p ?tr ?cr ?nntrc)
  (?tr copy-rule ?nntrc)
  (?nntrc rule ?cr)
  (?nntrc name tree-rule-copy)
  (?nntrc pred (?x abc))			;; Need to add something to the
									;; pred or the rule will not
									;; trigger, since its env is the
									;; same as that of tree-rule,
									;; which most likely already ran
									;; on the top obj.  ALso need to
									;; add the edge predicatd upon,
									;; which is next.
  (?xx abc)
  (?nntrc add (?x rule ?nntrc))
  (?nntrc add (?x top))
  (?nntrc add (?x l ?ll))
  (?nntrc add (?nn1 zero))
  (?nntrc add (?nn2 max))
  (?nntrc add (?nn1 top ?x))
  (?nntrc add (?nn2 top ?x))
  (?nntrc add (print tree-rule-copy ?x ?l ?nn1 ?nn2))
  (clausify ?nntrc)
  (spec-rule ?nntrc ?x ?xx)
  (?nntrc rule (rule						;; Doing the name delete as a rule assures we do it at the right time, i.e., after it's been created
				(pred
				 (?nntrc name ?nntrc))
				(add
				 (?nntrc abc)				;; Dummy addition to trigger printing
				 (print del name ?nntrc ?this-rule))
				(del
				 (?nntrc name ?nntrc)
				 (?nntrc rule ?this-rule))))))

)

(rule
 (name clausify-pred)
 (pred
  (clausify ?r)
  (?r type rule)
  (?r pred ?p))
 (add
  (print clausify-pred ?r ?p)
  (?r clause ?p)))

(rule
 (name clausify-add)
 (pred
  (clausify ?r)
  (?r type rule)
  (?r add ?p))
 (add
  (print clausify-add ?r ?p)
  (?r clause ?p)))

(rule
 (name clausify-del)
 (pred
  (clausify ?r)
  (?r type rule)
  (?r del ?p))
 (add
  (print clausify-del ?r ?p)
  (?r clause ?p)))

(rule
 (name spec-rule1)
 (pred
  (spec-rule ?r ?var ?obj)
  (?r type rule)
  (?r name ?rule-name))
 (add
  (print spec-rule1 ?r ?var ?obj)
  (?r var ?var)
  (?r local)
  (spec-rule2 ?r ?rule-name ?var ?obj)))

(rule
 (name spec-rule2)
 (pred
  (spec-rule2 ?r ?rule-name ?var ?obj)
  (?r type rule)
  (?r clause ?c)
  (?c _elem ?n ?var)
  (?r var ?var))
 (add
  (print spec-rule2 ?r ?rule-name ?c ?n ?var ?obj)
  (?obj rule ?r)
  (?c _elem ?n ?obj)
  ;; (?obj abc)
  )
 (del
  (?c _elem ?n ?var)))



#|

(rule
 (name xxx)
 (pred 
  (x l ?l)
  (?l1 sigma ?l)
  (?nn1 new-node sn1)
  (?nn2 new-node sn2)
  (x abc))
 (add
  (?nn2 aup x)
  (?nn1 aup x)
  (?nn1 tree-next ?nn2)
  (x adn ?nn2)
  (x adn ?nn1)
  (?nn1 l ?l1)
  (print tree-rule ?this-obj x ?nn1 ?nn2 ?l)
  (?nn2 l ?l1)
  (x rule n1921)
  (x top)
  (x l 3)
  (?nn1 zero)
  (?nn2 max)
  (?nn1 top x)
  (?nn2 top x)
  (print tree-rule-copy x ?l ?nn1 ?nn2))
 (del
  (x rule n1921)))


;; Something to try. Can activate this to do local stuff without editing

(rule
 (name add-local)
 (attach-to global-node)
 (pred
  (?r type rule)
  (?r name {set of names}))
 (add
  (?r local)))

|#