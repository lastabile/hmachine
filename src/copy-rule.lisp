























































;; After many versions, the addition of rest vars, the new rule
;; format, and the adoption of conventions on minimal rule content has
;; made copy-rule be as simple as it should be. The copy is done as a
;; single rule triggering, which avoids race conditions possible when
;; copy is spread across rules. Note that the H-Machine works well
;; with partial rules, but not having to deal with it in this basic
;; case is more efficient.
;;
;; Note that copy-rule can copy itself, and does so in the fe-rule-test

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
  )
 (del
  ;; (?this-obj rule ?this-rule)	;; fe-rule-test is more efficient with this in (similar dels in fe.lisp), but we leave it like this for display
  )
 )

