

(rule
 (name fe-0-rule)
 (local)
 (root-var 0)
 (pred
  (0 sigma 1))
 (add
  (print fe-0-rule 0)
  (0 even-func 2)))

(rule
 (name fwd-fe-rule)
 (root-var ?m)
 (local)
 (pred
  (?n sigma ?m)
  (?m sigma ?o)
  (?n even-func ?p)
  (?p sigma ?q)
  (?q sigma ?r))
(add
  (print fwd-fe-rule ?m even-func ?r)
  (queue ?o)
  (?m even-func ?r)))
  

(rule
 (name back-fe-rule)
 (root-var ?m)
 (local)
 (pred
  (?n sigma ?m)
  (?m rule ?fe-0-rule)
  (?fe-0-rule name fe-0-rule)
  (?m rule ?back-fe-rule)
  (?back-fe-rule name back-fe-rule)
  (?m rule ?fwd-fe-rule)
  (?fwd-fe-rule name fwd-fe-rule))
 (add
  (print back-fe-rule ?n)
  (?n rule ?back-fe-rule)
  (?n rule ?fwd-fe-rule)
  (?n rule ?fe-0-rule)))

(rule
 (name fe-rule-gen)
 (root-var ?n)
 (local)
 (pred
  (?n local-rule-pool ?p)
  (?p lrp-rule ?fe-0-rule)
  (?fe-0-rule name fe-0-rule)
  (?p lrp-rule ?back-fe-rule)
  (?back-fe-rule name back-fe-rule)
  (?p lrp-rule ?fwd-fe-rule)
  (?fwd-fe-rule name fwd-fe-rule))
 (add
  (print fe-rule-gen ?n)
  (?n rule ?back-fe-rule)
  (?n rule ?fe-0-rule)
  (?n rule ?fwd-fe-rule)))