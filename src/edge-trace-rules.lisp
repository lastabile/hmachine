


















































;;
;; These rules just apply to graphs created by the edge-trace-graph method of objgraph
;;









;; Example of creation and dump of an edge-trace graph:
;;
;; (let ((n 4))		;; Testing max failed gv at 75 but ok at 60
;;   (setq g (make-rule-30-test))
;;   (time (! (g run) n)))
;; 
;; (let ()
;;   (setq x (! (g edge-trace-graph) :rules '(
;; 										   rule-30-next-rule-1-1-1   
;; 										   rule-30-next-rule-1-1-0   
;; 										   rule-30-next-rule-1-0-0   
;; 										   rule-30-next-rule-0-0-1   
;; 										   rule-30-center            
;; 										   rule-30-zero-rule-1-1     
;; 										   rule-30-next-rule-0-1-1   
;; 										   rule-30-next-rule-0-0-0   
;; 										   rule-30-next-rule-1-0-1   
;; 										   rule-30-next-rule-0-1-0   
;; 										   rule-30-max-rule-1-1      
;; 										   rule-30-max-rule-0-1
;; 										   rule-30-top
;; 										   init
;; 										   )
;; 			 :nodes '(level sigma rule-30-next rule-30-top)))
;;   (! (x read-rule-file) "edge-trace-rules.lisp")
;;   (! (x execute-global-all-objs-loop)))
;; 
;; (let ((d (make-dumper))) (! (d set-graph) x)(! (d dump-gv-edges) "rule-30-edge-trace.gv" :rules nil :attrs '(a p r e)))
;;
;; et == edge-trace
;;

(rule
 (name edge-trace-info-rule)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name edge-trace-info-rule))
 (add
  (print edge-trace-info-rule)
  (am edge-color red)))

(rule
 (name rule-30-next-color-rule)
 (local)
 (attach-to rule-30-next-color-rule-info)
 (pred
  (?r type et-rule)
  (?r name ?n)
  (?r generic-done)
  (?r shape ?cur-shape)
  (?r color ?cur-color)
  (?r style ?cur-style)
  (?r label ?cur-label)
  (rule-30-next-color-rule-info ?n ?new-shape ?new-color ?new-style))
 (del
    (?r shape ?cur-shape)
	(?r color ?cur-color)
	(?r style ?cur-style)
	(?r label ?cur-label)
	(?r generic-done))
 (add
  (print rule-30-next-color-rule ?r ?n  ?cur-shape ?cur-color ?cur-style ?new-shape ?new-color ?new-style)
  (?r shape ?new-shape)
  (?r color ?new-color)
  (?r style ?new-style)
  (?r label ?n)
  ;; Uncomment these four to get the graphic of colored squares, as saved in xxx.
  ;; Also comment-out label and shape above.
  ;; (?r shape rectangle)
  ;; (?r label "")
  ;; (?r width 15)
  ;; (?r height 15)
  ))

;; Note no label -- node name as edge text does it here

(rule
 (name edge-trace-graph-edge-rule)
 (local)
 (attach-to et-edge)
 (pred
  (?e type et-edge))
 (add
  (print edge-trace-graph-edge-rule ?e)
  (?e shape rectangle)
  (?e color paleturquoise)
  (?e style filled)
  ;; (?e height 1.0)
  ;; (?e fontsize 50.0)
  ;; Uncomment these three to get the graphic of colored squares, as saved in xxx.
  ;; (?e label "")
  ;; (?e width 0)
  ;; (?e height 0)
  ))

(rule
 (name edge-trace-graph-obj-rule)
 (local)
 (attach-to et-obj)
 (pred
  (?e type et-obj))
 (add
  (print edge-trace-graph-obj-rule ?e)
  (?e shape rectangle)
  (?e color lightpink)
  (?e style filled)))

(rule
 (name edge-trace-graph-pred-node-rule)
 (local)
 (attach-to pred-node)
 (pred
  (?e type pred-node))
 (add
  (print edge-trace-graph-obj-rule ?e)
  (?e shape ellipse)
  (?e color plum)
  (?e style filled)))

(rule
 (name edge-trace-graph-add-node-rule)
 (local)
 (attach-to add-node)
 (pred
  (?e type add-node))
 (add
  (print edge-trace-graph-obj-rule ?e)
  (?e shape ellipse)
  (?e color peachpuff)
  (?e style filled)))

(rule
 (name rule-30-next-color-rule-info)
 (attach-to global-node)
 (pred
  (global-node rule ?r)
  (?r name rule-30-next-color-rule-info))
 (add
  (print rule-30-next-color-rule-info)
  (rule-30-next-color-rule-info rule-30-next-rule-1-1-1 ellipse mistyrose filled)
  (rule-30-next-color-rule-info rule-30-next-rule-1-1-0 ellipse turquoise filled)
  (rule-30-next-color-rule-info rule-30-next-rule-1-0-0 ellipse lightseagreen filled)
  (rule-30-next-color-rule-info rule-30-next-rule-0-0-1 ellipse khaki filled)
  (rule-30-next-color-rule-info rule-30-next-rule-0-1-1 ellipse salmon filled)
  (rule-30-next-color-rule-info rule-30-next-rule-0-0-0 ellipse deepskyblue3 filled)
  (rule-30-next-color-rule-info rule-30-next-rule-1-0-1 ellipse deeppink filled)
  (rule-30-next-color-rule-info rule-30-next-rule-0-1-0 ellipse violet filled)))

(rule
 (name rule-30-next-color-rule-info-gen)
 (attach-to et-rule)
 (pred
  (?r type et-rule)
  (?r name ?n))
 (add
  (print rule-30-next-color-rule-info-gen ?r ?n)
  (?r generic-done)
  (?r shape ellipse)
  (?r color seashell)
  (?r style filled)
  (?r label ?n)
  ;; (?r height 1.0)
  ;; (?r fontsize 50.0)
  ))

  




