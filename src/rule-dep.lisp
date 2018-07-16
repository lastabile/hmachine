
;; Global rules for exploring transitity and related calcs on rule
;; instantiated/success graphs.
;;
;; Below, i == instantiates
;;		  v == variable
;;        s == successful-rule-is

(rule
 (name trans-inst1)
 (root-var ?y)
 (pred
  (?x i ?y)
  (?y s ?z))
 (add
  (print trans-inst1 ?x ?y ?z)
  (?x ti ?z)))

(rule
 (name trans-inst2)
 (disabled)
 (pred
  (?r1 v ?v)
  (?v i ?o)
  (?o s ?r2)
  (success ?sseq ?r2 ?o)
  (instantiated ?iseq ?r1 ?v ?o))
 (add
  (print trans-inst2 ?r1 ?v ?o ?r2 ?iseq ?sseq)
  (?r1 tr ?r2)))

(rule
 (name trans-inst3)
 (disabled)
 (pred
  (?r1 v ?v)
  (?v i ?o)
  (?o s ?r2)
  (success ?sseq ?r2 ?o)
  (instantiated ?iseq ?r1 ?v ?o))
 (add
  (print trans-inst3 ?r1 ?v ?o ?r2 ?iseq ?sseq)
  (eval trans-diff ?v ?r2 ?iseq ?sseq)))
