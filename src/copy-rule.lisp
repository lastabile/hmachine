


















(comment

(rule
 (name copy-rule-rule-gen)
 (attach-to rule-clause-type)
 (pred
  (rule-clause-type ?ct))
 (add
  (print copy-rule-rule-gen ?ct)
  (local-rule-pool-node lrp-rule
						(rule
						 (name copy-rule-rule)
						 (local)
						 (root-var ?y)
						 (pred
						  (?r copy-rule ?y)
						  (?r name ?name)
						  (?r root-var ?x-root-var)	;; Note a rule will have an explicit root-var equal to :undefined added by define-rule if none was given
						  (?r std-var-level ?l)
						  (?r ?ct ?*rest))
						 (add
						  (print copy-rule-rule ?root-var ?this-obj ?this-rule ?name ?r ?ct ?*rest)
						  (?y type rule)
						  (?y name ?name)
						  (?y root-var ?x-root-var)
						  (?y std-var-level ?l)
						  (?y copied-from ?r)
						  (?y ?ct ?*rest))))))
(rule
 (name rule-clause-type)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name rule-clause-type))
 (add
  (print rule-clause-type)
  (rule-clause-type pred)
  (rule-clause-type del)
  (rule-clause-type add)
  (rule-clause-type not)))

)



(rule
 (name copy-rule-rule)
 (local)
 (root-var ?y)
 (pred
  (?r copy-rule ?y)
  (?r type ?t)
  (?r name ?name)
  (?r root-var ?x-root-var)
  (?r std-var-level ?l)
  (?r pred ?*rest-pred)
  (?r add ?*rest-add)
  (?r del ?*rest-del)
  (?r not ?*rest-not)
  )
 (add
  (print copy-rule-rule ?root-var ?this-obj ?this-rule ?name ?r ?y ?t rest-pred= ?*rest-pred rest-add= ?*rest-add rest-del= ?*rest-del rest-not= ?*rest-not)
  (?y type ?t)
  (?y name ?name)
  (?y root-var ?x-root-var)
  (?y std-var-level ?l)
  (?y copied-from ?r)
  (?y pred ?*rest-pred)
  (?y add ?*rest-add)
  (?y del ?*rest-del)
  (?y not ?*rest-not)
  ))
