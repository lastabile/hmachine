




































(rule
 (name fwd-fe-rule)
 (local)
 (root-var ?n)
 (pred
  (?n sigma ?m)
  (?n even-func ?o)
  (?o sigma ?p)
  (?p sigma ?q)
  (?n fwd-fe-rule ?fwd-fe-rule)
  (?n copy-rule-rule ?copy-rule-rule)
  (?n local-rule-pool ?lrp)
  (?nn-fwd-fe-rule new-node sn1)
  (?nn-copy-rule-rule new-node sn2))
 (add
  (print fwd-fe-rule ?this-rule ?n ?m even-func ?q ?fwd-fe-rule ?copy-rule-rule ?nn-fwd-fe-rule ?nn-copy-rule-rule)
#|
  (print "fwd-fe-rule" fwd-fe-rule)
  (print "?this-rule" ?this-rule)
  (print "?n" ?n)
  (print "?m" ?m)
  (print "even-func" even-func)
  (print "?q" ?q)
  (print "?fwd-fe-rule" ?fwd-fe-rule)
  (print "?copy-rule-rule" ?copy-rule-rule)
  (print "?nn-fwd-fe-rule" ?nn-fwd-fe-rule)
  (print "?nn-copy-rule-rule" ?nn-copy-rule-rule)
|#
  (?m even-func ?q)
  (?m local-rule-pool ?lrp)
  (?fwd-fe-rule copy-rule ?nn-fwd-fe-rule)
  (?fwd-fe-rule local-rule-pool ?lrp)
  (?copy-rule-rule copy-rule ?nn-copy-rule-rule)
  (?copy-rule-rule local-rule-pool ?lrp)
  (?nn-fwd-fe-rule rule ?nn-copy-rule-rule)
  (?nn-copy-rule-rule rule ?copy-rule-rule)
  (?m rule ?nn-fwd-fe-rule)
  (?m fwd-fe-rule ?nn-fwd-fe-rule)
  (?m copy-rule-rule ?nn-copy-rule-rule)
  #|
  (?n rule (rule
			(name del-fwd-fe-rule)
			(local)
			(root-var ?n)
			(pred
			 (?n rule ?fwd-fe-rule)
			 (?nn1 new-node sn1))						;; Test !!!!!!
			(add
			 (print del-fwd-fe-rule ?n ?fwd-fe-rule)
			 (?nn1 hi there))							;; Test !!!!!!
			(del
			 (?n rule ?fwd-fe-rule)
			 (?n rule ?this-rule))))
  |#
))

(rule
 (name fe-0-rule)
 (local)
 (pred
  (0 sigma 1)
  (0 local-rule-pool ?p)
  (?p lrp-rule ?fwd-fe-rule)
  (?fwd-fe-rule name fwd-fe-rule)
  (?p lrp-rule ?copy-rule-rule)
  (?copy-rule-rule name copy-rule-rule))
 (add
  (print fe-0-rule 0)
  (0 even-func 2)
  (0 fwd-fe-rule ?fwd-fe-rule)
  (0 copy-rule-rule ?copy-rule-rule)
  (0 rule ?fwd-fe-rule))
 (del
  ;; (?this-obj rule ?this-rule) ;; This is efficient and all that but let's keep everything in place for dispplay
  ))
