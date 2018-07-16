
(rule
 (name copy-rule-rule)
 (local)
 (pred
  (?r copy-rule ?y)
  (?r name ?name)
  (?r root-var ?x-root-var)	;; Note a rule will have an explicit root-var equal to nil added by define-rule if none was given
  (?r std-var-level ?l)
  (?r local-rule-pool ?p)
  (?p lrp-rule ?rp)
  (?rp name copy-rule-rule-pred)
  (?p lrp-rule ?ra)
  (?ra name copy-rule-rule-add)
  (?p lrp-rule ?rpe)
  (?rpe name copy-rule-rule-pred-elem)
  (?p lrp-rule ?rae)
  (?rae name copy-rule-rule-add-elem))
 (add
  (print copy-rule-rule ?this-obj ?this-rule ?name ?r ?y ?l ?rp ?ra ?rpe ?rae)
  (?y type rule)
  (?y name ?y)
  (?y root-var ?x-root-var)
  (?y std-var-level ?l)
  (?y copied-from ?r)
  (?y copy-rule-rule-add-rule ?r)
  (?y copy-rule-rule-pred-rule ?r)
  (?y rule ?rp)
  (?y rule ?ra)
  (?y rule ?rpe)
  (?y rule ?rae)))

(rule
 (name copy-rule-rule-pred)
 (local)
 (pred
  (?new-r copy-rule-rule-pred-rule ?r)
  (?r pred ?p)
  (?nnp new-node sn1))
 (add
  (print copy-rule-rule-pred ?r ?new-r ?p ?nnp)
  (?new-r pred ?nnp)
  (?new-r copy-rule-rule-pred-elem ?p ?nnp)))

(rule
 (name copy-rule-rule-pred-elem)
 (local)
 (pred
  (?new-r copy-rule-rule-pred-elem ?p ?nnp)
  (?p _elem ?i ?pe))
 (add
  (print copy-rule-rule-pred-elem ?p ?i ?pe ?nnp)
  (?nnp _elem ?i ?pe)))

(rule
 (name copy-rule-rule-add)
 (local)
 (pred
  (?new-r copy-rule-rule-add-rule ?r)
  (?r add ?a)
  (?nna new-node sn1))
 (add
  (print copy-rule-rule-add ?r ?new-r ?a ?nna)
  (?new-r add ?nna)
  (?new-r copy-rule-rule-add-elem ?a ?nna)))

(rule
 (name copy-rule-rule-add-elem)
 (local)
 (pred
  (?new-r copy-rule-rule-add-elem ?a ?nna)
  (?a _elem ?i ?ae))
 (add
  (print copy-rule-rule-add-elem ?a ?i ?ae ?nna)
  (?nna _elem ?i ?ae)))










(rule
 (name x-copy-rule-rule)
 (local)
 (pred
  (?r copy-rule ?y)
  (?r name ?name)
  (?r root-var ?x-root-var)
  (?r std-var-level ?l)
  (?r copy-rule-rule-params-pred ?rp)
  (?r copy-rule-rule-params-add ?ra))
 (add
  (print copy-rule-rule ?name ?r ?y ?l)
  (?y type rule)
  (?y name ?y)
  (?y root-var ?x-root-var)
  (?y std-var-level ?l)
  (?y copied-from ?r)
  (?y copy-rule-rule-add-rule ?r)
  (?y copy-rule-rule-pred-rule ?r)
  (?y rule ?rp)
  (?y rule ?ra)))




(comment

(rule
 (name copy-rule-rule-pred)
 (local)
 (pred
  (?new-r copy-rule-rule-pred-rule ?r)
  (?r pred ?p)
  (?p elem0 ?pe0)
  (?p elem1 ?pe1)
  (?p elem2 ?pe2)
  (?nnp new-node sn1))
 (add
  (print copy-rule-rule-pred ?r ?p ?pe0 ?pe1 ?pe2 ?nnp)
  (?new-r pred ?nnp)
  (?nnp elem0 ?pe0)
  (?nnp elem1 ?pe1)
  (?nnp elem2 ?pe2)))

(rule
 (name copy-rule-rule-add)
 (local)
 (pred
  (?new-r copy-rule-rule-add-rule ?r)
  (?r add ?a)
  (?a elem0 ?ae0)
  (?a elem1 ?ae1)
  (?a elem2 ?ae2)
  (?nna new-node sn1))
 (add
  (print copy-rule-rule-add ?r ?a ?ae0 ?ae1 ?ae2 ?nna)
  (?new-r add ?nna)
  (?nna elem0 ?ae0)
  (?nna elem1 ?ae1)
  (?nna elem2 ?ae2)))

)

;; Original nested copy-rule-rule

(comment
 (rule
  (name copy-rule-rule)
  (pred
   (?r copy-rule ?y)
   (?r name ?name)
   (?r root-var ?x-root-var) ;; !!! Note a rule must have an explicit root-var to be copied
   (?r std-var-level ?l)
   (?nn1 new-node sn1))
  (add
   (print copy-rule-rule ?r ?y ?r ?name ?x-root-var ?nn1)
   (?nn1 type rule)
   (?nn1 name ?nn1)
   (?nn1 root-var ?x-root-var)
   (?nn1 std-var-level ?l)
   (?r rule (rule
			 (name copy-rule-rule-pred)
			 (pred
			  (?r pred ?p)
			  (?nn2 new-node sn2))
			 (add
			  (print copy-rule-rule-pred ?r ?p ?nn2)
			  (?r rule (rule
						(name copy-rule-rule-pred-2)
						(pred
						 (?r name ?name)
						 (?r pred ?p) ;; connected!
						 (?p elem0 ?pe0)
						 (?p elem1 ?pe1)
						 (?p elem2 ?pe2))
						(add
						 (print copy-rule-rule-pred-2 ?r ?p ?nn1 ?nn2 ?pe0 ?pe1 ?pe2)
						 (?y rule ?nn1)
						 (?nn1 pred ?nn2)
						 (?nn2 elem0 ?pe0)
						 (?nn2 elem1 ?pe1)
						 (?nn2 elem2 ?pe2)))))))
   (?r rule (rule
			 (name copy-rule-rule-add)
			 (pred
			  (?r add ?a)
			  (?nn3 new-node sn3))
			 (add
			  (print copy-rule-rule-add ?r ?a ?nn3)
			  (?r rule (rule
						(name copy-rule-rule-add-2)
						(pred
						 (?r name ?name)
						 (?r add ?a) ;; connected!
						 (?a elem0 ?ae0)
						 (?a elem1 ?ae1)
						 (?a elem2 ?ae2))
						(add
						 (print copy-rule-rule-add-2 ?r ?a ?nn1 ?nn3 ?ae0 ?ae1 ?ae2)
						 (?y rule ?nn1)
						 (?nn1 add ?nn3)
						 (?nn3 elem0 ?ae0)
						 (?nn3 elem1 ?ae1)
						 (?nn3 elem2 ?ae2))))))))))

;; Non-deterministic version of copy rule which uses no nesting. This is being worked.

(comment
 (rule
  (name copy-rule-rule)
  (pred
   (?r copy-rule ?y)
   (?r name ?name)
   (?r root-var ?x-root-var)
   (?r std-var-level ?l)
   (?nnr new-node sn1)
   (?nnp new-node sn2)
   (?nna new-node sn3)
   (?r pred ?p)
   (?r add ?a)
   (?p elem0 ?pe0)
   (?p elem1 ?pe1)
   (?p elem2 ?pe2)
   (?a elem0 ?ae0)
   (?a elem1 ?ae1)
   (?a elem2 ?ae2))
  (add
   (print copy-rule-rule ?name ?r ?y ?p ?pe0 ?pe1 ?pe2 ?a ?ae0 ?ae1 ?ae2 ?nnr ?nnp ?nna)
   (?y rule ?nnr)
   (?nnr type rule)
   (?nnr name ?nnr)
   (?nnr root-var ?x-root-var)
   (?nnr std-var-level ?l)
   (?nnr pred ?nnp)
   (?nnp elem0 ?pe0)
   (?nnp elem1 ?pe1)
   (?nnp elem2 ?pe2)
   (?nnr add ?nna)
   (?nna elem0 ?ae0)
   (?nna elem1 ?ae1)
   (?nna elem2 ?ae2))))

