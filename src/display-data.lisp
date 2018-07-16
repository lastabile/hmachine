

(rule
 (name display-data)
 (attach-to global-node)
 (root-var global-node)
 (pred
  (global-node rule ?r)
  (?r name display-data))
 (add
  (print display-data)
  ;; (fft color green)
  ;; (fft in-node-color springgreen)
  ;; (fft out-node-color darkturquoise)
  (zero color violet)
  (aup in-node-color deeppink)
  (aup out-node-color dodgerblue)
  (aup edge-color springgreen)
  ;; (odd out-node-color palegreen)
  ;; (even out-node-color palevioletred)
  ;; (elem in-node-color  palevioletred)
  (elem out-node-color palevioletred)
  (elem edge-color  darkturquoise)
  (tree-next color dodgerblue)
  (fft-comb two-input-op)
  (fft-comb color lightskyblue)
  (fft-hb two-input-op)
  (fft-hb in-node-color deeppink)
  ;; (fft-hb out-node-color dodgerblue)
  (fft-hb color pink)
  (fft-hb-delta two-input-op)
  (fft-hb-delta in-node-color springgreen)
  (fft-hb-delta color turquoise)

  (rule shape rectangle)
  (rule color mistyrose)
  (rule style filled)

  ;; (fwd-fe-rule color paleturquoise)

  (+ two-input-op))
 (del
  (global-node rule ?this-rule)))
