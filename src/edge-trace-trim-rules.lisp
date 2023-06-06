










(rule
 (name edge-trace-trim-gen)
 (attach-to edge-trace-trim-info)
 (pred
  (edge-trace-trim-info ?attr ?rule-name ?rule-var))
 (add
  (print edge-trace-trim-gen ?attr ?rule-name ?rule-var)
  (?attr rule
		 (rule
		  (name (edge-trace-trim ?attr ?rule-name ?rule-var))
		  (pred
		   (?x ?attr ?y)
		   (?rule-var name ?rule-name))
		  (add
		   (print edge-trace-trim ?x ?attr ?y))
		  (del
		   (?x ?attr ?y))))))

(rule
 (name edge-trace-trim-info)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name edge-trace-trim-info))
 (add
  (print edge-trace-trim-info)
  (edge-trace-trim-info a is-0-param-xrule ?x)
  (edge-trace-trim-info p is-0-param-xrule ?y)
  (edge-trace-trim-info an is-0-param-xrule ?x)
  (edge-trace-trim-info pn is-0-param-xrule ?y)
  (edge-trace-trim-info pe is-0-param-xrule ?y)
  (edge-trace-trim-info pr is-0-param-xrule ?y)
#|
  (edge-trace-trim-info a rule-30-top ?x)
  (edge-trace-trim-info p rule-30-top ?y)
  (edge-trace-trim-info an rule-30-top ?x)
  (edge-trace-trim-info pn rule-30-top ?y)
|#
))
