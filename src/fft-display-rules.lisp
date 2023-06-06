

;; Set the color, shape, and label for rules, when they are referred to by data.

 (rule
  (name rule-gv-attr-rule)
  (attach-to rule)
  (pred
   (?r type rule)
   (?r name ?n))
  (add
   (print rule-gv-attr-rule ?r ?n)
   (?r shape rectangle)
   (?r color mistyrose)
   (?r style filled)
   (?r label ?n)))


