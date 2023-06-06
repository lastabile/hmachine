

;; Set color, shape, etc. for arrays and array elements

(rule
 (name array-gv-attr-rule)
 (attach-to is-elem-of)
 (pred
  (?e is-elem-of ?a))
 (add
  (print array-gv-attr-rule ?a)
  (?a shape trapezium)
  (?e shape rectangle)
  (?e color paleturquoise)))

;; fnext ("flat" next, does not loop like next does)
;;
;; With the not clause, we remove the need to assert then retract,
;; which is to some degree definitionally superior, but has ordering/consistency
;; issues.

(rule
 (name fnext-rule)
 (attach-to next)
 (pred
  (?x next ?y))
 (not							;; If we remove this not, then can use rule below, which
								;; deletes the undesired edge. Raises interesting temporal-logical questions
  (?y zero))
 (add
  (print fnext-rule ?x ?y)
  (?x fnext ?y)))

(comment 
(rule
 (name fnext-del-rule)
 (pred
  (?x fnext ?y)
  (?y zero))
 (add
  (print fnext-del-rule ?x ?y)
  (?y fnext-del-run))
 (del
  (?x fnext ?y)))
)

;; These rules set an index number (the "index" attribute) for the array
;; elements and label the nodes with that index number

(rule
 (name index-rule-zero)
 (attach-to zero)
 (pred
  (?x zero))
 (add
  (print index-rule-zero ?x)
  (?x index 0)
  (?x label 0)))

(rule
 (name index-rule-fnext)
 (local)
 (attach-to index)
 (pred
  (?x index ?i)
  (?i sigma ?j)
  (?x fnext ?y))
 (add
  (print index-rule-fnext ?x ?y ?i ?j)
  (?y index ?j)
  (?y label ?j)))

(rule
 (name top-elems-align)
 (attach-to is-elem-of)
 (pred
  (?x fft-top)
  (?x fft ?y)
  (?e1 is-elem-of ?y)
  (?e2 is-elem-of ?y)
  (?e1 fnext ?e2)
  (?e1r is-elem-of ?x)
  (?e2r is-elem-of ?x)
  (?e1 ref ?e1r)
  (?e2 ref ?e2r)
  (?e1r fnext ?e2r))
 (add
  (print top-elems-align ?x ?e1 ?e2)
  (?e1 gnext ?e2)
  (?e1r gnext ?e2r)))

(rule
 (name top-ref-init)
 (attach-to is-elem-of)
 (pred
  (?x fft-top)
  (?e is-elem-of ?x)
  (?er ref ?e))
 (add
  (print top-ref-init ?x ?e)
  (?er top-ref ?e)))

(rule
 (name top-ref-propagate)
 (attach-to top-ref)
 (pred
  (?x top-ref ?y)
  (?z ref ?x))
 (add
  (print top-ref-propagate ?x ?y ?z)
  (?z top-ref ?y)))

(rule
 (name bot-top-ref)
 (attach-to top-ref)
 (attach-to is-elem-of)
 (pred
  (?e1 top-ref ?t1)
  (?e1 next)
  (?e1 is-elem-of ?a1)
  (?a1 level 0)
  (?e2 top-ref ?t2)
  (?e2 next)
  (?e2 is-elem-of ?a2)
  (?a2 level 0)
  (?e1 ?e2 fft-hb ?o))
 (add
  (print bot-top-ref ?e1 ?t1 ?e2 ?t2)
  (?e1 bot-top-ref ?t1)
  (?e2 bot-top-ref ?t2)))

(rule
 (name hnext)
 (attach-to bot-top-ref)
 (pred
  (?e1 bot-top-ref ?r1)
  (?e2 bot-top-ref ?r2)
  (?r1 gnext ?r2))
 (add
  (print hnext ?e1 ?e2 ?r1 ?r2)
  (?e1 hnext ?e2)))

(rule
 (name bot-index)
 (attach-to bot-top-ref) 
 (pred
  (?e bot-top-ref ?r)
  (?r index ?i)
  (?e label ?l))
 (del
  (?e label ?l))
 (add
  (print bot-index ?e ?r)
  (?e index ?i)
  (?e label ?i)))
