

(let ((n 3))
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-the-graph))
  ;; (! (g add-natural-number-edges) 50)
  (! (g define-rule) `(rule
					   (name init)
					   (attach-to global-node)
					   (pred
						(global-node rule ?r)
						(?r name init))
					   (add
						(print init)
						(r level ,(* 1 n))
						(r rule-30-top)
						(x is treetopobj levels ,n)
						(x fft-top)
						(x fft xfft)
						(x level ,n)
						(x color navajowhite)
						(x rand r)
						(x rule ,(! (g query) '((?x name fft-rule)) '?x))

						(x local-rule-pool local-rule-pool-node)
						(x global-rule-pool global-rule-pool-node)
						(r local-rule-pool local-rule-pool-node)
						(r global-rule-pool global-rule-pool-node))
					   (del
						(global-node rule ?this-rule))))
  ;; (! (g trace-rule) 'fft-comb-rule-next-sing-delta)
  (timer 'main
		 (lambda ()
		   (! (g execute-global-all-objs-loop))
		   )))



  (defun del-rules ()
	(dolist (rn '(copy-rule-rule-add copy-rule-rule copy-rule-rule-pred copy-rule-rule-add-2 copy-rule-rule-pred-2
									 fe-0-rule room-rule add-rule display-fwd-fe-rule fwd-fe-rule-gen back-fe-rule-gen1 clean-fft-rule clean-fft-comb-rule-next
									 clean-fft-comb-rule-zero
									 ))
	  (let ((r (first (! (g define-rule) `(rule (pred (?r type rule)(?r name ,rn)) (del (local-rule-pool-node lrp-rule ?r)(global-rule-pool-node grp-rule ?r)))
						 :local t :add-to-local-rule-pool nil))))
		(! (g match-and-execute-rule) r rn))))


;; dfft is just the dataflow part, no butterflies. That's quite
;; simple and we don't need the tree, odd-even, or rule30.

(let ((n 6))
  (setq g (make-the-graph))
  (! (g read-rule-file) "dfft.lisp")
  (! (g def-obj-edges) `((x dfft xfft)
						 (x level ,n)
						 (x color navajowhite)))
  (timer 'main
	(lambda ()
	  (! (g execute-global-all-objs-loop))
	  )))



(let ()
  (setq g (make-the-graph))
  (let ()
	(setq switch (! (g query) '((?s type switch)) '?s))
	(print (list switch on off)))
  (! (g clear-queue))
  (! (g add) switch 'event 'on)
  (! (g execute-queue))
  (! (g add) switch 'event 'off)
  (! (g execute-queue))
  (! (g add) switch 'event 'on)
  (! (g execute-queue)))

(let ()
  (setq g (make-the-graph))
    (let ()
	(setq room (! (g query) '((?s type room)) '?s))
	(print (list room)))
  (! (g clear-queue))
  (! (g add) room 'event 'occupied)
  (! (g execute-queue)))


;; fe-rule-test

;; Use the usual g var

(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g  (make-the-graph))
  (! (g add) 4 'rule (! (g query) '((?x name back-fe-rule-gen)) '?x))
  (timer 'main
	(lambda ()
	  (! (g execute-queue) :rule-mode :local-only))))

;; Defines new var f

(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq f (make-the-graph))
  (! (f execute-obj) 'global-node :cont (lambda (m s e p) nil))
  (! (f add) 4 'rule (! (f query) '((?x name back-fe-rule-gen)) '?x))
  (timer 'main
	(lambda ()
	  (! (f execute-queue) :rule-mode :local-only))))

;; Using the no-copy form of the fe rules on g

(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g  (make-base-graph))
  (! (g add-natural-number-edges) 20)
  (! (g read-rule-file) "fe-no-copy.lisp")
  (! (g add) 4 'rule (! (g query) '((?x name fe-rule-gen)) '?x))
  (timer 'main
	(lambda ()
	  (! (g execute-queue) :rule-mode :local-only))))


(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g  (make-base-graph))
  (! (g add-natural-number-edges) 20)
  (! (g read-rule-file) "fe.lisp")
  (! (g read-rule-file) "copy-rule.lisp")
  (! (g read-rule-file) "display-data.lisp")
  (! (g add) 0 'rule (! (g query) '((?x name fe-0-rule)) '?x))
  (timer 'main
	(lambda ()
	  (! (g execute-obj) 'global-node :cont (lambda (m s e p) nil))
	  (! (g execute-queue) :rule-mode :local-only))))


(let ()
  (setq perf-stats-list nil)
  (dotimes (i 10)
	(let ()
	  (print (list '*********** (+ i 1)))
	  (clear-counters)
	  (clear-perf-stats)
	  (setq g  (make-base-graph))
	  (! (g add-natural-number-edges) (* (+ i 1) 5))
	  (! (g read-rule-file) "fe.lisp")
	  (! (g read-rule-file) "copy-rule.lisp")
	  (! (g read-rule-file) "display-data.lisp")
	  (! (g add) 0 'rule (! (g query) '((?x name fe-0-rule)) '?x))
	  (timer 'main
		(lambda ()
		  (! (g execute-obj) 'global-node :cont (lambda (m s e p) nil))
		  (! (g execute-queue) :rule-mode :local-only)))
	  (perf-stats)
	  (setq perf-stats-list (append perf-stats-list (list (get-perf-hash)))))))






;; 3/5/17: Looks like we did max 200 rule 30 levels -- see tree.jpg

(let ()
  (setq g (make-the-graph))
  (! (g add-natural-number-edges) 200)
  (! (g def-obj-edges) '((x level 200)(x rule-30-top)))
  (! (g execute-global-loop)))

(let ((n 100))
  (setq g (make-the-graph))
  (! (g add-natural-number-edges) (+ n 10))
  (! (g def-obj-edges) `((r level ,n)(r rule-30-top)))
  (timer 'main
		 (lambda ()
		   (! (g execute-global-all-objs-loop))
		   )))

(let ()
  (setq f (make-the-graph))
  (! (f add-node) 'nn))


(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "xfft.gv" :attrs '(fft fft-hb fft-comb odd even copy-array-struct zero)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "xfft.gv" :attrs '( fft-hb fft-hb-delta fft-comb odd even zero d center-up next-color rule30val)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "xfft-32-rule30-100.gv" :omit-unmatched-rules nil :attrs '( fft-hb fft-hb-delta fft-comb odd even zero d #| center-up |# next-color rule30val rule-30-next up #| delta3 delta3-rand |#)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "rule30.gv" :attrs '(rule-30-next up center-up rule30val)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "xtree.gv" :attrs 
											   '(aup next tree-next zero max ev od)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "fe.gv" :attrs 
											   '(sigma even-func fe)))		   

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "xruledep.gv" :rules nil :attrs '(xrule-dep xxadd xxpred)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "x4.gv" :attrs '(fft fft-hb fft-comb odd even copy-array-struct zero) :rules '(add-parent ev-init od-next ev-next ev-od-obj-rule is tree-next-zero-rule tree-next-rule tree-loop-rule tree-top-order-rule tree-top-propagate-rule tree-elem-rule tree-zero-rule tree-max-rule tree-rule treeobj-rule even-next odd-next even-zero odd-zero self-cycle odd-new even-new copy-array-struct-next copy-array-struct-next-sing copy-array-struct-zero copy-array-struct-new fft-comb-rule-next-sing fft-comb-rule-next fft-comb-rule-zero fft-rule-zero fft-rule)))

;; 4/28 Used this to create the complte fft-rule figure, fft-8-just-fft-rules.gv
(let ((d (make-dumper :omit-unmatched-rules t :emit-labels t :emit-legend nil))) (! (d set-graph) g)(! (d dump-gv-edges) "xfft.gv" :rules t :attrs '( fft-comb fft-hb  odd even  d d-casz) :omitted-rules '(color-circle-data print-gc6 print-gc1 print-gc2 print-gc3 print-gc4 print-gc5 nil rule-30-rule-gen rule-30-zero-rule-gen rule-30-max-rule-gen rule-30-next1 rule-30-next2 rule-30-next3 rule-30-center rule-30-center-loop rule-30-loop rule-30-top rule-30-top-propagate rule-30-data add-rule color-color print-gc-rule display-data data switch-room-obj-rule odd-even-weave weave-next-rule fft-delta-init even-tree-max std-notes inverse-data gen-inverse fft-rule-opt fft-rule-opt-display fft-rule) :omitted-attrs '(color in-node-color two-input-op)))


(let ((d (make-dumper))) 
  (! (d set-graph) g)
  (! (d dump-gv-edges) "xfft.gv" 
	 :attrs '(fft-hb fft-hb-delta fft-comb odd even zero d)
	 :rules '(clean-fft-rule clean-fft-comb-rule-next clean-fft-comb-rule-zero odd-next)))



;;
;; Ladybug and Butterfly graph
;;
;; Use the following graphviz command. Note omitted -n2 from neato
;; "c:\Program Files (x86)\Graphviz2.38\bin\dot.exe" xfft.gv | "c:\Program Files (x86)\Graphviz2.38\bin\gvpack.exe" -m0  | "c:\Program Files (x86)\Graphviz2.38\bin\neato.exe" -s  -Tsvg | sed -e "s/<svg.*$/\<svg/" > xfft.svg
;;
(let ((d (make-dumper)))
  (! (d set-graph) g)
  (! (d dump-gv-edges) "xfft.gv" :omit-unmatched-rules nil :rules nil :emit-labels t :attrs
	 '(
	   fft-hb
	   fft-comb
	   odd 
	   even
	   zero
       d-casz
	   d
	   center-up
;;     next-color
	   rule30val
;;	   rule-30-next
	   up
;;	   delta3
;;	   delta3-rand
	   next
       casz-ref
       casz-ref1
;;     copy-array-struct
	   )))








(let () (setq g (make-the-graph)) (! (g init-the-graph)) (! (g execute-obj) 'global-rule-node) (! (g clear-queue)))

(let () (setq switch (! (g query) '((?s type switch)) '?s))(setq on (! (g query) '((?s name on)) '?s))(setq off (! (g query) '((?s name off)) '?s))(print (list switch on off)))

(! (g del) switch 'state off)(! (g del) switch 'state on)(! (g clear-queue))(! (g add) switch 'state on)(! (g execute-queue))


(defun f ()
  (let ((result nil)
		(rl (mapcar (lambda (x)
					  (let ((r (env-lookup '?x (first x))))
						(list (! (g hget) r 'name)
							  (! (g get-rule-consts) r))))
					(! (g query) '((?x type rule))))))
	(let ((nl (! (g get-all-nodes))))
	  (dolist (n nl)
		(when (! (g has-rules) n)
		  (dolist (r rl)
			(let ((rn (first r))
				  (rc (second r))
				  (nn (! (g nodes) (! (g get-edges) n))))
			  (let ((i (intersect rc nn)))
				(when t ;; i
				  (setq result (cons (list rn n i)result))
				  ;; (print result)
				  )))))))
	result))

(let ((info (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n))))
  (mapcar (lambda (x)
			(list x (! (g get-rule-consts) (first x))))
		  info))

(let ((infolist (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n)))
	  (tt (! (g get-env-triggered-table))))
  (mapcar (lambda (info)
			(let ((r (first info))
				  (n (second info)))
			  (list info (! (tt lookup-last-matched-edges) r))))
		  infolist))


(dolist (x (mapcar (lambda (x) (let ((r (env-lookup '?x (first x)))) (list (! (g hget) r 'name) (intersect (! (g get-rule-consts) r)(! (g nodes) (! (g get-edges) 'N1313)))  ))) (! (g query) '((?x type rule))))) (print x))

(dolist (x (sort (mapcan (lambda (l) 
						   (let ((x (env-lookup '?x l)))
							 (when (! (g hget) x 'new-edges)
							   (list (list (! (g hget) x 'name)
										   (or (! (g hget) x 'tested) 0)
										   (or (! (g hget) x 'matched) 0)
										   (or (! (g hget) x 'new-edges) 0)
										   (or (! (g hget) x 'not-new-edges) 0)
										   (or (! (g hget) x 'failed) 0)
										   (or (! (g hget) x 'filter) 0))))))
						 (! (g query) '((?x type rule))))
				 (lambda (x y) (> (second x) (second y))))) (print x))

"c:\Program Files (x86)\clisp-2.49/base/lisp.exe" -B "c:\Program Files (x86)\clisp-2.49" -M "c:\Program Files (x86)\clisp-2.49/base/lispinit.mem" -N "c:\Program Files (x86)\clisp-2.49\locale"

;; Note can pass "array" specs to gvpack but it's better to pack them in

"c:\Program Files (x86)\Graphviz2.38\bin\dot.exe" xfft.gv | "c:\Program Files (x86)\Graphviz2.38\bin\gvpack.exe" -m0  | "c:\Program Files (x86)\Graphviz2.38\bin\neato.exe" -s -n2 -Tsvg | sed -e "s/<svg.*$/\<svg/" > xfft.svg

egrep "defm|defc|defl" *.lisp


egrep "MAIN|ALL-MATCHES|BIPARTITE-BREADTH-RULE-WALK-SEQ|CROSS-AUX2" xxx

egrep "MAIN" xxx | gawk '{ print $2 }' >m1
egrep "ALL-MATCHES" xxx | gawk '{ print $4 }' >m2
egrep "CROSS-AUX2" xxx | gawk '{ print $4 }' >m3
egrep "EXPAND-RULE-OBJ-EDGES " xxx | gawk '{ print $4 }' >m4
egrep "EXPAND-EDGES " xxx | gawk '{ print $4 }' >m5

egrep "ALL-MATCHES" xxx | gawk '{ print $2 }' >A-ALL-MATCHES
egrep "CROSS-AUX2" xxx | gawk '{ print $2 }' >A-CROSS-AUX2
egrep "EXPAND-RULE-OBJ-EDGES " xxx | gawk '{ print $2 }' >A-EXPAND-RULE-OBJ-EDGES
egrep "EXPAND-EDGES " xxx | gawk '{ print $2 }' >A-EXPAND-EDGES
egrep "VAR-MATCH-FILTER-EDGES" xxx | gawk '{ print $2 }' >A-VAR-MATCH-FILTER-EDGES

egrep "ALL-MATCHES" xxx | gawk '{ print $3 }' >C-ALL-MATCHES
egrep "CROSS-AUX2" xxx | gawk '{ print $3 }' >C-CROSS-AUX2
egrep "EXPAND-RULE-OBJ-EDGES " xxx | gawk '{ print $3 }' >C-EXPAND-RULE-OBJ-EDGES
egrep "EXPAND-EDGES " xxx | gawk '{ print $3 }' >C-EXPAND-EDGES
egrep "MATCH-ONE-EDGE" xxx | gawk '{ print $3 }' >C-MATCH-ONE-EDGE
egrep "M-AND-E-RULES-TESTED" xxx | gawk '{ print $3 }' >C-M-AND-E-RULES-TESTED
egrep "M-AND-E-RULES-MATCHED" xxx | gawk '{ print $3 }' >C-M-AND-E-RULES-MATCHED


plot "m1" with lines,  "m2" with lines,  "m3" with lines, "m4" with lines, "m5" with lines

plot "ALL-MATCHES" with lines,  "CROSS-AUX2" with lines,  "EXPAND-RULE-OBJ-EDGES" with lines, "EXPAND-EDGES" with lines


plot "C-ALL-MATCHES" with lines, \
	"C-CROSS-AUX2" with lines, \
	"C-EXPAND-RULE-OBJ-EDGES" with lines, \
	"C-EXPAND-EDGES" with lines,\
	"C-MATCH-ONE-EDGE" using :($1/100) with lines, \
	"C-M-AND-E-RULES-TESTED" with lines, \
	"C-M-AND-E-RULES-MATCHED" with lines, \
	"A-ALL-MATCHES" using :($1*1e7) with lines, \
	"A-EXPAND-RULE-OBJ-EDGES" using :($1*1e7) with lines, \
	"A-EXPAND-EDGES" using :($1*1e7) with lines, \
	"A-VAR-MATCH-FILTER-EDGES" using :($1*1e7) with lines

egrep "defc|defm" *.lisp


(let ((r nil))
  (maphash (lambda (k v) 
			 (setq r (cons
					  (list (! (g hget) (failed-key-rule-node k) 'name) (failed-key-obj-node k) (failed-entry-count v))
					  r)))
		   (first (! ((! (g get-failed)) as-list))))
  (dolist (x (sort r (lambda (x y) (string< (symbol-name (first x)) (symbol-name (first y))))))
	(print x))
  nil)

(timer 'test
  (lambda ()
	(dotimes (i 1000)
	  (! (g add-edge) `(test ,i)))
	nil))

(with-open-file (s "xxx" :direction :output) (! (g dump-edges) :dump-fcn (lambda (e) (format s "~a~%" e))))



(mapcar (lambda (x)
		  (let ((r x))
			(let ((w (! (g bipartite-breadth-rule-walk-seq) r nil)))
			  (let ((c (cross-aux (reverse w))))
				(list (! (g hget) r 'name) w c)))))
		(mapcar (lambda (x) (let ((r (second (first x)))) r)) (! (g query) '((?x type rule)(?x name ?n)))))

(mapcar (lambda (x)
		  (let ((r x))
			(let ((w (! (g bipartite-depth-rule-walk) r nil (lambda (x) (print x)))))
			  (list (! (g hget) r 'name) w))))
		(mapcar (lambda (x) (let ((r (second (first x)))) r)) (! (g query) '((?x type rule)(?x name ?n)))))


(let ((l nil)
	  (p nil))
  (defun g () l)
  (defun f (x)
	(print l)
	(if (null x)
		(let ()
		  (setq l (append l (list p)))
		  (setq p nil))
		(when (is-edge x)
		  (setq p (append (list x) p))))
	(lambda (x) (f x))))


(! (g bipartite-depth-rule-walk) 'N899 '?ae0 #'f)





(mapcar (lambda (x)
		  (let ((r x))
			(let ((w (! (g bipartite-breadth-rule-walk-seq) r '?x0)))
			  (let ((c (cross-aux (reverse w))))
				(list (! (g hget) r 'name) w c)))))
		(mapcar (lambda (x) (let ((r (second (first x)))) r)) (! (g query) '((?x type rule)(?x name fft-comb-rule-next-sing)))))



(let ((rule-edges
	   (mapcar (lambda (r) (list (env-lookup '?n r) (! ((! (g get-rule-components) (env-lookup '?x r)) preds))))
			   (! (g query) '((?x type rule)(?x name ?n))))))
  rule-edges)


(let ((rule-edges
	   (mapcar (lambda (r) (list (env-lookup '?n r) (! ((! (g get-rule-components) (env-lookup '?x r)) preds))))
			   (! (g query) '((?x type rule)(?x name ?n))))))
  (dolist (x (sort (mapcar (lambda (x) 
					   (let ((o (! (g overlap-info) (second x))))
						 (list (first x) o (sxhash o))))
					 rule-edges)
				   (lambda (x y) (< (third x) (third y)))))
	(print x))
  nil)




(let ((rule-nodes
	   (mapcar (lambda (r) (list (env-lookup '?n r) (env-lookup '?x r)))
			   (! (g query) '((?x type rule)(?x name ?n))))))
  (mapcar (lambda (rn)
			(let ((subsetlist (! ((! (g make-rule-graph) (second rn)) get-subsetlist))))
			  (list (first rn) subsetlist)))
		  rule-nodes))



(let ((rule-nodes
	   (mapcar (lambda (r) (list (env-lookup '?n r) (env-lookup '?x r)))
			   (! (g query) '((?x type rule)(?x name ?n))))))
  (mapcar (lambda (rn)
			(let ((rg (! (g make-rule-graph) (second rn))))
			  (! (rg build-chains))
			  (! (rg subsume-chains))
			  (let ((chains (! ((! (rg get-chains)) as-list))))
				(list (first rn) (second rn) chains))))
		  rule-nodes))


(let ((x '(x fft xfft)))
  (dotimes (i 10000) 
	(when (and (not (member (first x) '(failure success)))
			   (not (member (second x) '(tested failed not-new-edges))))
	  (print x))
	(setq x (! (g get-next-edge) x))))

(let ()
  (setq s (! (g all-subsets)))
  (dolist (x (sort (mapcar (lambda (x) (list (first x) (length (second x)))) s) (lambda (x y) (> (second x) (second y)))))
	(print x)))

(let ()
  (setq s (! (g all-subsets)))
  (let ((c 0))
	(dolist (x s)
	  (when (= (length (second x)) 0)
		(setq c (+ c 1))))
	c))

(let () (list (length (! (g all-subqets))) (length (! (g get-all-edges)))))



(let ()
  (setq s (! (g get-all-nodes)))
  (let ((nrules 0))
	(dolist (x (sort (mapcar (lambda (x) 
							   (setq nrules (+ (length (! (g hget-all) x 'rule)) nrules))
							   (list x
									 (length (! (g get-edges) x))
									 (length (! (g hget-all) x 'rule))
									 ))
							 s)
					 (lambda (x y) (> (third x) (third y)))))
	  (print x))
	(print (list 'nrules nrules))))


;; Sorted rule-stat, showing sequence of failure/success for each node,rule pair

(let ()
  (setq s (! (g get-rule-status-graph)))
  (defr
	(defl g< (x y)
	  (string< (format nil "~a" x) (format nil "~a" y)))
	(let ((envs (! (s query) `((rule-stat ?type ?rule ?node ?seq-no)))))
	  (let ((edge-info (mapcar (lambda (env)
								 (let ((type (env-lookup '?type env))
									   (rule (env-lookup '?rule env))
									   (node (env-lookup '?node env))
									   (seq-no (env-lookup '?seq-no env)))
								   `(,(! (g hget) rule 'name) (rule-stat ,type ,rule ,node ,seq-no))))
							   envs)))
		(let ((seq-sorted-edge-info (stable-sort edge-info (lambda (x y) (< (nth 4 (second x)) (nth 4 (second y)))))))
		  (let ((rule-sorted-edge-info (stable-sort seq-sorted-edge-info (lambda (x y) (string< (first x) (first y))))))
			(let ((node-sorted-edge-info (stable-sort rule-sorted-edge-info (lambda (x y) (g< (nth 3 (second x)) (nth 3 (second y)))))))
			  (dolist (se node-sorted-edge-info)
				;; (dolist (se rule-sorted-edge-info)
				(print se))))))
	  nil)))

#|
(let ()
  (setq s (! (g get-rule-status-graph)))
  (let ((nodes (! (g get-all-nodes))))
	(dolist (node nodes)
	  (let ((rules (! (g hget-all) node 'rule)))
		(dolist (rule rules)
		  (let ((edges (! (s get-edges-from-subqet) `(rule-stat ,rule ,node))))
			(let ((sorted-edges (sort edges (lambda (x y) (< (nth 4 x) (nth 4 y))))))
			  (print (list node (! (g hget) rule 'name)))
			  (dolist (se sorted-edges)
				(print se)))))))
	nil))
|#

#|
(let ((nodes (! (g get-all-nodes))))
  (dolist (node nodes)
	(let ((rules (! (g hget-all) node 'rule)))
	  (dolist (rule rules)
		(let ((envs (! (g query) `((rule-stat ?t ,rule ,node ?seq-no)))))
		  (let ((edges (mapcar (lambda (env)
								 (let ((type (env-lookup '?t env))
									   (seq-no (env-lookup '?seq-no env)))
								   `(rule-stat ,type ,rule ,node ,seq-no)))
							   envs)))
			(let ((sorted-edges (sort edges (lambda (x y) (< (nth 4 x) (nth 4 y))))))
			  (print (list node (! (g hget) rule 'name)))
			  (dolist (s sorted-edges)
				(print s))))))))
  nil)
|#

(setq g (make-graph))
(! (g add-edge) '(1 2 3))
(! (g add-edge) '(1 2 3 4))
(! (g add-edge) '(1 2 3 4 5))
(! (g get-all-edges))
(! (g get-edges) 1)
(! (g get-edges-from-subset) '(1 2))
(! (g get-edges-from-subset) '(1))
(! (g subsets) '(1 2))
(! (g subsets) '(1 2 3))
(! (g supersets) '(1 2))


(let ((rule-edges-info-list
	   (mapcar (lambda (r) (list (env-lookup '?n r) (! ((! (g get-rule-components) (env-lookup '?x r)) preds))))
			   (! (g query) '((?x type rule)(?x name ?n)
							  ;; (?x name fft-rule-opt)
							  )))))
  (dolist (rule-edges-info rule-edges-info-list)
	(let ((rule-name (first rule-edges-info))
		  (rule-edges (second rule-edges-info)))
	  (print rule-name)
	  ;; (print (! (g xcross-intersect-rule-edges) rule-edges))
	  (print (mapcar (lambda (xinfo)
					   (let ((edges (first xinfo))
							 (edgeslist (second xinfo)))
						 (! (g f) edges edgeslist)))
					 (! (g xcross-intersect-rule-edges) rule-edges)))
	  nil)))

(let ((rule-edges-info-list
	   (mapcar (lambda (r) (list (env-lookup '?n r) (! ((! (g get-rule-components) (env-lookup '?x r)) preds))))
			   (! (g query) '((?x type rule)(?x name ?n)
							  ;; (?x name fft-rule-opt)
							  )))))
  (let ((n-pred-edges 0))
	(dolist (rule-edges-info rule-edges-info-list)
	  (let ((rule-name (first rule-edges-info))
			(rule-edges (second rule-edges-info)))
		(setq n-pred-edges (+ n-pred-edges (length rule-edges)))
		(print rule-name)
		(print (! (g xc-filter) rule-edges))
		nil))
	(print n-pred-edges))
  nil)
  
(let ((rule-edges-info-list
	   (mapcar (lambda (r) (list (env-lookup '?n r) (! ((! (g get-rule-components) (env-lookup '?x r)) preds))))
			   '(
				  ((?X N0) (?N PRINT-GC1) (T T)) 
				  ((?X N3) (?N PRINT-GC2) (T T)) 
				  ((?X N7) (?N PRINT-GC3) (T T)) 
				  ((?X N10) (?N PRINT-GC4) (T T)) 
				  ((?X N13) (?N PRINT-GC5) (T T)) 
				  ((?X N16) (?N PRINT-GC6) (T T)) 
				  ((?X N19) (?N PRINT-GC-RULE) (T T)) 
				  ((?X N43) (?N DATA) (T T)) 
				  ((?X N90) (?N ADD-PARENT) (T T)) 
				  ((?X N1174) (?N EV-INIT) (T T)) 
				  ((?X N1180) (?N OD-NEXT) (T T)) 
				  ((?X N1189) (?N EV-NEXT) (T T)) 
				  ((?X N133) (?N EV-OD-OBJ-RULE) (T T)) 
				  ;; ((?X N140) (?N COPY-RULE-RULE) (T T)) 
				  ;; ((?X NN0) (?N COPY-RULE-RULE-PRED) (T T)) 
				  ;; ((?X NN4) (?N COPY-RULE-RULE-PRED-2) (T T)) 
				  ;; ((?X NN81) (?N COPY-RULE-RULE-ADD) (T T)) 
				  ;; ((?X NN85) (?N COPY-RULE-RULE-ADD-2) (T T)) 
				  ((?X N610) (?N FWD-FE-RULE) (T T)) 
				  ((?X N617) (?N FWD-FE-RULE-GEN) (T T)) 
				  ((?X N632) (?N FE-0-RULE) (T T)) 
				  ((?X N640) (?N BACK-FE-RULE-GEN) (T T)) 
				  ((?X N650) (?N BACK-FE-RULE-GEN1) (T T)) 
				  ((?X NN162) (?N FWD-FE-RULE1) (T T)) 
				  ((?X N686) (?N ADD-RULE) (T T)) 
				  ((?X NN169) (?N ADD-AUX-RULE) (T T)) 
				  ((?X N720) (?N ADDX-RULE) (T T)) 
				  ((?X N727) (?N SWITCH-RULE) (T T)) 
				  ((?X N740) (?N ROOM-RULE) (T T)) 
				  ((?X N752) (?N SWITCH-ROOM-OBJ-RULE) (T T)) 
				  ((?X N762) (?N IS) (T T)) 
				  ((?X N770) (?N XIS) (T T)) 
				  ((?X N777) (?N EVEN-NEXT) (T T)) 
				  ((?X N790) (?N ODD-NEXT) (T T)) 
				  ((?X N1167) (?N EVEN-ZERO) (T T)) 
				  ((?X N1159) (?N ODD-ZERO) (T T)) 
				  ((?X N818) (?N SELF-CYCLE) (T T)) 
				  ((?X N825) (?N ODD-NEW-RULE-PROPAGATE) (T T)) 
				  ((?X N833) (?N EVEN-NEW-RULE-PROPAGATE) (T T)) 
				  ((?X N841) (?N ODD-NEW) (T T)) 
				  ((?X N870) (?N EVEN-NEW) (T T)) 
				  ((?X N899) (?N COPY-ARRAY-STRUCT-NEXT) (T T)) 
				  ((?X N910) (?N COPY-ARRAY-STRUCT-NEXT-SING) (T T)) 
				  ((?X N918) (?N COPY-ARRAY-STRUCT-ZERO) (T T)) 
				  ((?X N925) (?N COPY-ARRAY-STRUCT-NEW) (T T)) 
				  ((?X N943) (?N FFT-COMB-RULE-NEXT-SING) (T T)) 
				  ((?X N961) (?N FFT-COMB-RULE-NEXT) (T T)) 
				  ((?X N988) (?N FFT-COMB-RULE-ZERO) (T T)) 
				  ((?X N1009) (?N FFT-RULE-ZERO) (T T)) 
				  ((?X N1030) (?N FFT-RULE) (T T)) 
				  ((?X N1065) (?N DISPLAY-DATA) (T T)) 
				  ((?X N1089) (?N COLOR-CIRCLE-DATA) (T T)) 
				  ((?X N1150) (?N COLOR-TREE) (T T)) 
				  ((?X N1198) (?N TREE-NEXT-ZERO-RULE) (T T)) 
				  ((?X N1204) (?N TREE-NEXT-RULE) (T T)) 
				  ((?X N1218) (?N TREE-LOOP-RULE) (T T)) 
				  ((?X N1227) (?N TREE-TOP-ORDER-RULE) (T T)) 
				  ((?X N1238) (?N TREE-TOP-PROPAGATE-RULE) (T T)) 
				  ((?X N1244) (?N TREE-ELEM-RULE) (T T)) 
				  ((?X N1254) (?N TREE-ELEM-ZERO-RULE) (T T)) 
				  ((?X N1259) (?N TREE-ELEM-DONE-RULE) (T T)) 
				  ((?X N1269) (?N TREE-TOP-DONE-RULE) (T T)) 
				  ((?X N1279) (?N TREE-ZERO-RULE) (T T)) 
				  ((?X N1286) (?N TREE-MAX-RULE) (T T)) 
				  ((?X N1293) (?N TREE-RULE) (T T)) 
				  ((?X N1309) (?N TREE-TOP-RULE) (T T)) 
				  ((?X N1314) (?N EXEC-TREE-RULE1) (T T)) 
				  ((?X N1324) (?N EXEC-TREE-RULE2) (T T)) 
				  ((?X N1342) (?N EXEC-TREE-RULE3) (T T)) 
				  ((?X N1353) (?N EXEC-TREE-RULE-FAILURE) (T T)) 
				  ((?X N1371) (?N EXEC-TREE-ZERO-MAX-RULE1) (T T)) 
				  ((?X N1393) (?N EXEC-TREE-ZERO-MAX-RULE2) (T T)) 
				  ((?X N1410) (?N EXEC-EV-INIT-RULE1) (T T)) 
				  ((?X N1425) (?N EXEC-EV-INIT-RULE2) (T T)) 
				  ((?X N1437) (?N EXEC-EV-INIT-RULE3) (T T)) 
				  ((?X N1449) (?N EXEC-TREE-ELEM-DONE-FAILURE) (T T)) 
				  ((?X N1458) (?N EXEC-TREE-ELEM-RULE) (T T)) 
				  ((?X N1468) (?N EXEC-TREE-ELEM-DONE-RULE) (T T)) 
				  ((?X N1476) (?N EXEC-OD-NEXT-RULE1) (T T)) 
				  ((?X N1485) (?N EXEC-OD-NEXT-RULE2) (T T)) 
				  ((?X N1495) (?N EXEC-EV-NEXT-RULE2) (T T)) 
				  ((?X N1505) (?N EXEC-TREE-NEXT-FAILURE) (T T)) 
				  ((?X N1510) (?N RULE-30-RULE-GEN) (T T)) 
				  ((?X N1580) (?N RULE-30-ZERO-RULE-GEN) (T T)) 
				  ((?X N1662) (?N RULE-30-MAX-RULE-GEN) (T T)) 
				  ((?X N1745) (?N RULE-30-NEXT1) (T T)) 
				  ((?X N1755) (?N RULE-30-NEXT2) (T T)) 
				  ((?X N1765) (?N RULE-30-NEXT3) (T T)) 
				  ((?X N1775) (?N RULE-30-CENTER) (T T)) 
				  ((?X N1781) (?N RULE-30-CENTER-LOOP) (T T)) 
				  ((?X N1787) (?N RULE-30-LOOP) (T T)) 
				  ((?X N1794) (?N RULE-30-TOP) (T T)) 
				  ((?X N1817) (?N RULE-30-TOP-PROPAGATE) (T T)) 
				  ((?X N1824) (?N RULE-30-DATA) (T T)) 
				  ((?X N1838) (?N FFT-DELTA-INIT) (T T)) 
				  ((?X N1864) (?N FFT-COMB-RULE-NEXT-SING-DELTA) (T T)) 
				  ((?X N1886) (?N FFT-RULE-DELTA1) (T T)) 
				  ((?X N1899) (?N FFT-RULE-DELTA2) (T T)) 
				  ((?X N1910) (?N FFT-RULE-OPT) (T T)) 
				  ((?X N2340) (?N ADD-AUX-RULE) (T T))
				 ))))
  (dolist (rule-edges-info rule-edges-info-list)
	(let ((rule-name (first rule-edges-info))
		  (rule-edges (second rule-edges-info)))
	  (print rule-name)
	  (print (! (g h) rule-edges)))))


(let ()
  (let ((l (mapcar
			(lambda (x)
			  (list (first x) (length (second x))))
			(! (g get-all-subsets)))))
	(let ((l (sort l (lambda (x y) (> (second x) (second y))))))
	  (dolist (x l)
		(print x))))
  nil)



(defun rule-stats ()
  (defr
	(defl hget (x a) (! (g hget) x a))
	(let ((m 0))
	  (let ((info (mapcan (lambda (l) 
							(let ((x (env-lookup '?x l)))
							  (when t ;; (hget x 'new-edges)
								(setq m (max m (length (symbol-name (hget x 'name)))))
								(list (list (hget x 'name)
											(or (hget x 'tested) 0)
											(or (hget x 'matched) 0)
											(or (hget x 'new-edges) 0)
											(or (hget x 'not-new-edges) 0)
											(or (hget x 'failed) 0))))))
						  (! (g query) '((?x type rule))))))
		(let ((info (sort info (lambda (x y) (> (nth 3 x) (nth 3 y))))))
		  (let ((s 10)
				(m (+ m 2)))
			(format t "~%name~vttested~vtmatched~vtnew-e~vtnot-new-e~vtfailed~%"
					(+ m (* s 0)) (+ m (* s 1)) (+ m (* s 2)) (+ m (* s 3)) (+ m (* s 4)) (+ m (* s 5)))
			(dolist (x info)
			  (format t "~%~a~vt~a~vt~a~vt~a~vt~a~vt~a~vt~a"
					  (nth 0 x) (+ m (* s 0))
					  (nth 1 x) (+ m (* s 1))
					  (nth 2 x) (+ m (* s 2))
					  (nth 3 x) (+ m (* s 3))
					  (nth 4 x) (+ m (* s 4))
					  (nth 5 x) (+ m (* s 5))
					  (nth 6 x))))))))
  nil)


(sort (mapcan (lambda (n) 
				(let ((e0 (! (g get-edges) n 0))
					  (e2 (! (g get-edges) n 2)))
				(when (and (not (null e0)) (not (null e2)))
				  (let ((l0 (length e0))
						(l2 (length e2)))
				  (list (list n l2 l0))))))
			  (! (g get-all-nodes)))
	  (lambda (x y) (< (second x) (second y))))


(let ()
  (load "hoss.lisp")
  (load "top-obj.lisp")
  (load "hossexample.lisp")
  (setq p (make-pencils-and-pens))
  (! (p add-item) 'pen-bic-42)
  (! (p add-item) 'pencil-mechanical-57)
  (! (p add-item) 'pencil-std-no-2-99)
  (! (p add-item) 'pen-pilot-1)
  nil)

(! (p total))

(! (p all-items))



(let ((rns (mapcar (lambda (x) (first x)) (! (g query) '((?r type rule)(?r name ?n)) '(?n)))))
  (let ((l nil))
	(dolist (rn rns)
	  (setq l (cons (list rn (! (g rule-dep-walkback) rn)) l)))
	l))


;; abcl build

(let ()
  (let ((*suppress-compiler-warnings* t))
	(compile-file "hoss.lisp")
	(load "hoss.abcl")
	(compile-file "top-obj.lisp")
	(load "top-obj.abcl")
	(compile-file "h.lisp")
	(load "h.abcl")
	nil))

(let ()
  (load "hoss.abcl")
  (load "top-obj.abcl")
  (load "h.abcl")
  nil)


(! (g define-rule)
   '(rule
	 (name test1)
	 (pred
	  (?x sigma ?y))
	 (add
	  (print test1)
	  (?x rule (rule
				(name test2)
				(pred
				 (?y sigma ?z))
				(add
				 (print test2)
				 (?y rule (rule
						   (name test3)
						   (pred
							(?z sigma ?t))
						   (add
							(print test3))))))))))

(defun symbol-< (x y)
  (string< (symbol-name x) (symbol-name y)))

(defun filter (l)
  (mapcan (lambda (x)
			(when (< (second (second x)) (second (first x)))
			  (list x)))
		  l))

(defun ftimes (x y)
  (if (and (< x 1e-18)
		   (< y 1e-18))
	  0
	  (* x y)))

(defun exp-prod (x y)
  (ftimes (- 1 (exp (- (if (> x 70) 70 x)))) y))

(let ()
  (let ((rule-info-list (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n))))
	(dolist (rule-info rule-info-list)
	  (let ((rule (first rule-info))
			(rule-name (second rule-info)))
		(let ((h (make-hash-table :test #'equal)))
		  (let ((qlist (filter (! (g query) `((?r1 type rule)(?r1 name ,rule-name)(?r2 type rule)(?r2 name ?n2)(instantiated ?s1 ?r1 ?v ?x)(success ?s2 ?r2 ?x))))))
			(dolist (q qlist)
			  (let ((s-seq (second (first q)))
					(i-seq (second (second q)))
					(rn2 (second (fifth q)))
					(v (second (seventh q)))
					(o (second (eighth q))))
				(let ((k1 (list rule-name v rn2 o)))
				  (setf (gethash k1 h) (- s-seq i-seq)))
				(let ((k2 (list rule-name v)))
				  (setf (gethash k2 h) (+ (or (gethash k2 h) 0) 1)))))
			(print rule-name)
			(let ((nh (make-hash-table :test #'equal)))
			  (maphash (lambda (k v)
						 (when (= (length k) 4)
						   (let ((k2 (list (first k) (second k))))
							 (let ((rule-count (gethash k2 h)))
							   (let ((k3 (list (first k) (second k) (third k))))
								 (setf (gethash k3 nh) (exp-prod v (or (gethash k3 nh) 1))))))))
					   h)
			  (print (hash-table-to-list nh)))
			;; (print (hash-table-to-list h))
			(print (sort 
					(sort
					 (sort 
					  (sort
					   qlist
					   (lambda (x y) (< (second (first x)) (second (first y)))))
					  (lambda (x y) (< (second (second x)) (second (second y)))))
					 (lambda (x y) (symbol-< (second (eighth x)) (second (eighth y)))))
					(lambda (x y) (symbol-< (second (seventh x)) (second (seventh y))))))
			))))))



(let ()
  (! (g read-rule-file) "rule-dep.lisp")
  (let ((rules (mapcar (lambda (n) (first (first (! (g query) `((?r type rule)(?r name ,n)) '(?r)))))
					   '(trans-inst1 #| trans-inst2 trans-inst3 |#))))
	(clear-perf-stats)
	(timer 'ti
	  (lambda ()
		(! (g execute-all-objs) :only-these-rules rules)))))

(! (g rule-dep-graph) :rules '(
init
;; data
;; std-notes
;; add-parent
;; gen-inverse
;; inverse-data
;; ev-init
;; od-next
;; ev-next
;; ev-od-obj-rule
;; copy-rule-rule
;; copy-rule-rule-pred
;; copy-rule-rule-pred-2
;; copy-rule-rule-add
;; copy-rule-rule-add-2
;; add-rule
;; add-aux-rule
;; addx-rule
;; switch-rule
;; room-rule
;; switch-room-obj-rule
;; is
;; xis
;; is-not
;; even-next
;; odd-next
;; even-zero
;; odd-zero
;; self-cycle
;; odd-new-rule-propagate
;; even-new-rule-propagate
;; odd-new
;; even-new
;; even-tree-max
;; odd-tree-zero
;; odd-even-weave
;; weave-next-rule
;; copy-array-struct-next
;; copy-array-struct-next-sing
;; copy-array-struct-zero
;; copy-array-struct-new
;; fft-comb-rule-next-sing
;; fft-comb-rule-next
;; fft-comb-rule-zero
;; fft-rule-zero
;; fft-rule
;; fft-top-rule
;; clean-fft-rule
;; clean-fft-comb-rule-zero
;; clean-fft-comb-rule-next
;; display-data
;; color-circle-data
;; color-color
;; tree-next-zero-rule
;; tree-next-rule
;; tree-loop-rule
tree-top-order-rule
tree-top-propagate-rule
;; tree-elem-rule
;; tree-elem-zero-rule
;; tree-zero-rule
;; tree-max-rule
tree-rule
tree-top-rule
;; rule-30-rule-gen
;; rule-30-zero-rule-gen
;; rule-30-max-rule-gen
;; rule-30-next1
;; rule-30-next2
;; rule-30-next3
;; rule-30-center
;; rule-30-center-loop
;; rule-30-loop
;; rule-30-top
;; rule-30-top-propagate
;; rule-30-data
;; rule-30-data
;; fft-delta-init
;; fft-rule-delta2
;; fft-rule-delta3
;; nfft-rule-delta4
;; fft-rule-delta5
;; fft-rule-opt
;; fft-rule-opt-rule-names
;; fft-rule-opt
;; fft-rule-opt-display

))

(! (g rule-dep-graph) :rules '(
init
data
std-notes
;; add-parent
gen-inverse
inverse-data
ev-init
od-next
ev-next
ev-od-obj-rule
;; copy-rule-rule
;; copy-rule-rule-pred
;; copy-rule-rule-pred-2
;; copy-rule-rule-add
;; copy-rule-rule-add-2
;; add-rule
;; add-aux-rule
;; addx-rule
;; switch-rule
;; room-rule
;; switch-room-obj-rule
is
xis
is-not
even-next
odd-next
even-zero
odd-zero
self-cycle
odd-new-rule-propagate
even-new-rule-propagate
odd-new
even-new
even-tree-max
odd-tree-zero
odd-even-weave
weave-next-rule
copy-array-struct-next
copy-array-struct-next-sing
copy-array-struct-zero
copy-array-struct-new
fft-comb-rule-next-sing
fft-comb-rule-next
fft-comb-rule-zero
fft-rule-zero
fft-rule
fft-top-rule
;; clean-fft-rule
;; clean-fft-comb-rule-zero
;; clean-fft-comb-rule-next
display-data
color-circle-data
color-color
tree-next-zero-rule
tree-next-rule
tree-loop-rule
tree-top-order-rule
tree-top-propagate-rule
tree-elem-rule
tree-elem-zero-rule
tree-zero-rule
tree-max-rule
tree-rule
tree-top-rule
rule-30-rule-gen
rule-30-zero-rule-gen
rule-30-max-rule-gen
rule-30-next1
rule-30-next2
rule-30-next3
rule-30-center
rule-30-center-loop
rule-30-loop
rule-30-top
rule-30-top-propagate
rule-30-data
rule-30-data
fft-delta-init
fft-rule-delta2
fft-rule-delta3
fft-rule-delta4
fft-rule-delta5
fft-rule-opt
fft-rule-opt-rule-names
fft-rule-opt
fft-rule-opt-display

))

(timer 'main (lambda () (! (g execute-rule-dep) 'init :rule-dep-rule-exceptions '(
add-parent
copy-rule-rule
copy-rule-rule-pred
copy-rule-rule-pred-2
copy-rule-rule-add
copy-rule-rule-add-2
add-rule
add-aux-rule
addx-rule
switch-rule
room-rule
switch-room-obj-rule
clean-fft-rule
clean-fft-comb-rule-zero
clean-fft-comb-rule-next
fwd-fe-rule1
back-fe-rule-gen
back-fe-rule-gen1
fwd-fe-rule
fe-0-rule
display-fwd-fe-rule
fwd-fe-rule-gen
display-back-fe-rule-gen
is
))))



(timer 'main (lambda () (! (g execute-rule-dep) 'init :rule-dep-rules '(init tree-top-order-rule tree-top-propagate-rule tree-rule tree-top-rule))))



(let ((e (! ((! (g get-edge-to-rule)) as-list))))
  (dolist (x (sort (mapcar (lambda (x) 
							 (let ((h (make-hash-table)))
							   (dolist (y (second x))
								 (setf (gethash (second y) h) (+ (or (gethash (second y) h) 0) 1)))
							   (list (first x) (apply #'max (hash-table-value-to-list h)) (hash-table-value-to-list h))))
						   e)
				   (lambda (x y) (> (second x) (second y)))))
	(print x))
  nil)




;; Shows max number of reused edges per-rule

(dolist (x (sort (let ((e (! ((! (g get-edge-to-rule)) as-list))))
				   (let ((rule-to-max (make-hash-table)))
					 (dolist (x e)
					   (let ((h (make-hash-table)))
						 (dolist (y (second x))
						   (setf (gethash (second y) h) (+ (or (gethash (second y) h) 0) 1)))
						 (maphash (lambda (k v)
									(let ((rule-name k)
										  (edge-max v))
									  (setf (gethash rule-name rule-to-max) (max (or (gethash rule-name rule-to-max) 0) edge-max))))
								  h)))
					 (hash-table-to-list rule-to-max))) (lambda (x y) (> (second x)(second y)))))
  (print x))







(let ((p (! (g get-pred-node-to-new-edges)))) 
  (let ((e (mapcar (lambda (x)
					 (list 
					  (! (g hget) (! (g hget) (first x) 'xpred) 'name)
					  (! (g hget) (first x) 'as-string)
					  (length (second x))
					  ))
				   (! (p as-list)))))
	(let ((s (sort e (lambda (x y) (string< (symbol-name (first x)) (symbol-name (first y)))))))
	  (dolist (x s)
		(print x)))))


(let ((d (make-dumper :emit-legend nil))) (! (d set-graph) g)(! (d dump-gv-edges) "x.gv" :rules nil :attrs nil :string-attrs-only t))

(let ((d (make-dumper :emit-legend nil))) (! (d set-graph) g)(! (d dump-gv-edges) "x.gv" :rules nil :attrs '(a p r t)))



;;
;; This is exact replay
;;
(let ()
  (clear-counters)
  (setq g1 (make-the-graph))
  (mapcar (lambda (x)
			(! (g1 match-and-execute-rule) (second x) (third x)))
		  (sort (! (g query) '((success ?s ?r ?o)) '(?s ?r ?o))
				(lambda (x y) (< (or (first x) 0) (or (first y) 0))))))


(let ((n 1))
  (clear-perf-stats)
  (setq g1 (make-the-graph))
  (! (g1 set-disable-root-vars) t)
  (! (g1 define-rule) `(rule
						(name init)
						(attach-to global-node)
						(pred
						 (global-node rule ?r)
						 (?r name init))
						(add
						 (print init)
						 (r level ,(* 1 n))
						 (r rule-30-top)
						 (x l ,n)
						 (x is treetopobj)
						 (x fft-top)
						 (x fft xfft)
						 (x level ,n)
						 (x color navajowhite)
						 (x rand r)
						 (x rule ,(! (g query) '((?x name fft-rule)) '?x))

						 (x local-rule-pool local-rule-pool-node)
						 (x global-rule-pool global-rule-pool-node)
						 (r local-rule-pool local-rule-pool-node)
						 (r global-rule-pool global-rule-pool-node))
						(del
						 (global-node rule ?this-rule))))
  (let ((nodes nil))
	(mapcar (lambda (x)
			  (let ((name (second x)))
				(print (list 'exec name nodes))
				(! (g1 execute-rule-on-objs)
				   (first (first (! (g1 query) `((?r name ,name)) '(?r))))
				   nodes
				   :cont
				   (lambda (m s n)
					 (when n
					   (setq nodes (hunion nodes n)))
					 (print s)))))
			(sort (! (g query) '((success ?s ?r ?o)(?r name ?n)) '(?s ?n ?o))
				  (lambda (x y) (< (or (first x) 0) (or (first y) 0)))))))

(let ((n 1))
  (clear-perf-stats)
  (setq g1 (make-the-graph))
  (! (g1 set-disable-root-vars) t)
  (! (g1 define-rule) `(rule
						(name init)
						(attach-to global-node)
						(pred
						 (global-node rule ?r)
						 (?r name init))
						(add
						 (print init)
						 (r level ,(* 1 n))
						 (r rule-30-top)
						 (x l ,n)
						 (x is treetopobj)
						 (x fft-top)
						 (x fft xfft)
						 (x level ,n)
						 (x color navajowhite)
						 (x rand r)
						 (x rule ,(! (g query) '((?x name fft-rule)) '?x))

						 (x local-rule-pool local-rule-pool-node)
						 (x global-rule-pool global-rule-pool-node)
						 (r local-rule-pool local-rule-pool-node)
						 (r global-rule-pool global-rule-pool-node))
						(del
						 (global-node rule ?this-rule))))
  (let ((nodes nil))
	(mapcar (lambda (x)
			  (let ((name (second x)))
				(print (list 'exec name nodes))
				(! (g1 execute-rule-on-objs)
				   (first (first (! (g1 query) `((?r name ,name)) '(?r))))
				   nodes
				   :cont
				   (lambda (m s n)
					 (when n
					   (setq nodes (hunion nodes n)))
					 (print s)))))
			(sort (! (g query) '((success ?s ?r ?o)(?r name ?n)) '(?s ?n ?o))
				  (lambda (x y) (< (or (first x) 0) (or (first y) 0)))))))

(let ((n 5))
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-the-graph))
  ;; (! (g add-natural-number-edges) 105)
  (! (g define-rule) `(rule
					   (name init)
					   (attach-to global-node)
					   (pred
						(global-node rule ?r)
						(?r name init))
					   (add
						(print init)
						(r level ,(* 1 n))
						(r rule-30-top)
						(x is treetopobj levels ,n)
						(x fft-top)
						(x fft xfft)
						(x level ,n)
						(x color navajowhite)
						(x rand r)
						(x rule ,(! (g query) '((?x name fft-rule)) '?x))

						(x local-rule-pool local-rule-pool-node)
						(x global-rule-pool global-rule-pool-node)
						(r local-rule-pool local-rule-pool-node)
						(r global-rule-pool global-rule-pool-node))
					   (del
						(global-node rule ?this-rule))))
  (timer 'main (lambda () (! (g execute-rule-dep) 'init :rule-dep-rule-exceptions
							 '(
							   add-parent
							   copy-rule-rule
							   copy-rule-rule-pred
							   copy-rule-rule-pred-elem
							   copy-rule-rule-add
							   copy-rule-rule-add-elem
							   x-copy-rule-rule
							   add-rule
							   add-aux-rule
							   addx-rule
							   switch-rule
							   room-rule
							   switch-room-obj-rule
							   clean-fft-rule
							   clean-fft-comb-rule-zero
							   clean-fft-comb-rule-next
							   fwd-fe-rule1
							   back-fe-rule-gen
							   back-fe-rule-gen1
							   fwd-fe-rule
							   fe-0-rule
							   display-fwd-fe-rule
							   fwd-fe-rule-gen
							   display-back-fe-rule-gen
							   )))))


(let ()
  (setq dims nil)
  (dotimes (i 6)
	(let ((i (+ i 1)))
	  (print (list 'n i))
	  (let ((n i))
		(clear-counters)
		(clear-perf-stats)
		(setq g (make-the-graph))
		(! (g define-rule) `(rule
							 (name init)
							 (attach-to global-node)
							 (pred
							  (global-node rule ?r)
							  (?r name init))
							 (add
							  (print init)
							  (r level ,(* 1 n))
							  (r rule-30-top)
							  (x l ,n)
							  (x is treetopobj)
							  (x fft-top)
							  (x fft xfft)
							  (x level ,n)
							  (x color navajowhite)
							  (x rand r)
							  (x rule ,(! (g query) '((?x name fft-rule)) '?x))

							  (x local-rule-pool local-rule-pool-node)
							  (x global-rule-pool global-rule-pool-node)
							  (r local-rule-pool local-rule-pool-node)
							  (r global-rule-pool global-rule-pool-node))
							 (del
							  (global-node rule ?this-rule))))
		(timer 'main (lambda () (! (g execute-rule-dep) 'init :rule-dep-rule-exceptions
								   '(
									 add-parent
									 copy-rule-rule
									 copy-rule-rule-pred
									 copy-rule-rule-pred-2
									 copy-rule-rule-add
									 copy-rule-rule-add-2
									 add-rule
									 add-aux-rule
									 addx-rule
									 switch-rule
									 room-rule
									 switch-room-obj-rule
									 clean-fft-rule
									 clean-fft-comb-rule-zero
									 clean-fft-comb-rule-next
									 fwd-fe-rule1
									 back-fe-rule-gen
									 back-fe-rule-gen1
									 fwd-fe-rule
									 fe-0-rule
									 display-fwd-fe-rule
									 fwd-fe-rule-gen
									 display-back-fe-rule-gen
									 is
									 ))))
		(setq dims (cons (! (g dimensions)) dims))
		))))



(defun f (dims-list)
  (let ((h (make-hash-table)))
	(dolist (dims dims-list)
	  (dolist (dim dims)
		(let ((name (first dim))
			  (dimval (second dim)))
		  (setf (gethash name h) (cons dimval (gethash name h))))))
	h))


(let ()
  (clear-counters)
  (setq g1 (make-the-graph))
  (setq g2 (make-the-graph))
  (setq g3 (make-the-graph))
  (! (g1 def-obj-edges) `((x1 l 1)(x1 is treetopobj)))
  (! (g2 def-obj-edges) `((x2 l 2)(x2 is treetopobj)))
  (! (g3 def-obj-edges) `((x3 l 3)(x3 is treetopobj)))
  (timer 'main
		 (lambda ()
		   (! (g1 execute-global-all-objs-loop))
		   (! (g2 execute-global-all-objs-loop))
		   (! (g3 execute-global-all-objs-loop))
		   )))

(dotimes (i 1000) (print (! (g path) (nth (random (length (! (g get-all-nodes)))) (! (g get-all-nodes)))(nth (random (length (! (g get-all-nodes)))) (! (g get-all-nodes))))))

(dotimes (i 1000) (print (! (g path) (nth (random (length (! (g get-all-nodes)))) (! (g get-all-nodes)))(nth (random (length (! (g get-all-nodes)))) (! (g get-all-nodes))) :excl-set '(elem0 elem1 elem2 elem3 elem4 elem5 elem6 elem7 elem8 elem9
							  elem10 elem11 elem12 elem13 elem14 elem15 elem16 elem17 elem18 elem19
							  elem pred add del rule add-main
							  instantiated success name tested new-edges not-new-edges
							  global-rule-pool-node global-rule-pool local-rule-pool-node local-rule-pool))))

(dolist (x '(N2188 FROM-IS-RULE N714 RULE N701)) (print (! (g path) 'n2189 x)))





(! (g depth) (! (g query) `((?r type rule)(?r name init)) '?r)
	 (lambda (rule-node level)
	   (print (! (g hget) rule-node 'name))
	   (mapunion (lambda (x) (! (g hget-inverse-all) x 'pred))
				 (mapunion (lambda (x) (! (g hget-all) x 'rule-dep))
						   (! (g hget-all) rule-node 'add)))))


(! (g depth) 'init (lambda (node) (mapcar (lambda (x) (first x)) (! (g query) `((rd 1 ,node "" "" ?n2)) '(?n2)))) (lambda (node) (print node)))


(let ((rule-node (! (g query) `((?r type rule)(?r name init)) '?r)))
  (! (g breadth) rule-node 
	 (lambda (rule-node)
	   (print (! (g hget) rule-node 'name))
	   (mapunion (lambda (x) (! (g hget-inverse-all) x 'pred))
				 (mapunion (lambda (x) (! (g hget-all) x 'rule-dep))
						   (! (g hget-all) rule-node 'add))))))

(let ((rule-node (! (g query) `((?r type rule)(?r name init)) '?r)))
  (let ((l 0))
	(! (g new-depth) rule-node 
	   (lambda (rule-node level)
		 (dotimes (i level) (format t "     "))
		 (format t "~a~%" (! (g hget) rule-node 'name))
		 (let ((cs (mapunion (lambda (x) (! (g hget-inverse-all) x 'pred))
							 (mapunion (lambda (x) (! (g hget-all) x 'rule-dep))
									   (! (g hget-all) rule-node 'add)))))
		   cs)))))

(let ((rule-node (! (g query) `((?r type rule)(?r name init)) '?r)))
  (let ((ni (make-node-info :node rule-node :type :rule)))
	(! (g breadth) ni
	   (lambda (ni)
		 (let ((node (node-info-node ni))
			   (node-type (node-info-type ni)))
		   (cond
			((eq node-type :rule)
			 (mapcar (lambda (x) (make-node-info :node x :type :add))
					 (! (g hget-all) node 'add)))
			((eq node-type :add)
			 (mapcar (lambda (x) (make-node-info :node x :type :pred))
					 (! (g hget-all) node 'rule-dep)))
			((eq node-type :pred)
			 (mapcar (lambda (x) (make-node-info :node x :type :rule))
					 (! (g hget-inverse-all) node 'pred)))
			(t (print 'error))))))))




(let ((rule-node (! (g query) '((?r type rule)(?r name fft-rule)) '?r)))
  (let ((pred-edges (! ((! (g get-rule-components) rule-node) preds))))
	(mapcar (lambda (env)
			  (! (g matched-edges) pred-edges env))
			(! (g all-matches-on-edges) rule-node (! (g get-all-edges))))))




;; sed -e "s/<svg.*$/\<svg/" x.svg >y.svg

(let ((gv-files (directory "c:/users/lstabile/sugarsyncedstuff/h/graphics/*.gv")))
  (let ((svg-dir "c:/users/lstabile/sugarsyncedstuff/h/pub/gallery/"))
	(let ((pub-dir "c:/users/lstabile/sugarsyncedstuff/h/pub/"))
	  (let ((h-dir "c:/users/lstabile/sugarsyncedstuff/h/"))
		(let ((h-lisp-files '(
							  "h.lisp"
							  "hoss.lisp"
							  "fft-delta.lisp"
							  "globaltree.lisp"
							  "rule30.lisp"
							  "fe.lisp"
							  "fe-no-copy.lisp"
							  "ht.lisp"
							  "hashtab.lisp"
							  "doc"
							  "hmachine.pdf"
							  )))
		  (let ((other-files '(
							   "c:/users/lstabile/sugarsyncedstuff/fft.lisp"
							   "c:/users/lstabile/sugarsyncedstuff/complexity.pdf"
							   )))

			(defun copy-file (src-path dst-path)
			  (let ((cmd (format nil "cp -f ~a ~a" src-path dst-path)))
				(system cmd)))
		  
			(defun gen-svgs (&key doit)
			  (dolist (gv-file gv-files)
				(let ((gv-file (namestring gv-file)))
				  (let ((svg-file (format nil "~a~a.svg" svg-dir (pathname-name (string-right-trim ".gv" gv-file)))))
					(let ((dot-cmd "\"c:\\Program Files (x86)\\Graphviz2.38\\bin\\dot.exe\"")
						  (gvpack-cmd "\"c:\\Program Files (x86)\\Graphviz2.38\\bin\\gvpack.exe\"")
						  (neato-cmd "\"c:\\Program Files (x86)\\Graphviz2.38\\bin\\neato.exe\"")
						  (sed-cmd "c:\\cygwin\\bin\\sed"))
					  (let ((cmd (format nil "~a ~a | ~a -m0 | ~a -s -n2 -Tsvg -o | ~a -e \"s/<svg.*$/\\<svg/\" >~a" 
										 dot-cmd gv-file gvpack-cmd neato-cmd sed-cmd svg-file)))
						(let ((cmd-file "gen-svgs-temp.bat"))
						  (with-open-file (s cmd-file :direction :output)
							(format s "~a~%" cmd))
						  (format t "~a~%" cmd)
						  (when doit
							(system cmd-file))
						  ))))))
			  nil)

		(defun make-pub (&key doit)
		  (gen-svgs :doit doit)
		  (dolist (f h-lisp-files)
			(let ((src-path (format nil "~a~a" h-dir f)))
			  (let ((dst-path (format nil "~a~a" pub-dir f)))
				(format t "copy ~a ~a~%" src-path dst-path)
				(when doit
				  (copy-file src-path dst-path)))))
		  (dolist (src-path other-files)
			(let ((name (pathname-name src-path)))
			  (let ((type (pathname-type src-path)))
				(let ((dst-path (format nil "~a~a~a~a" pub-dir name  (and type ".") (or type ""))))
				  (format t "copy ~a ~a~%" src-path dst-path)
				  (when doit
					(copy-file src-path dst-path))))))
		  nil)))))))


At last, I have a follow-on to my Chaos paper which you so kindly read
and reviewed. The paper is attached. Note I have not written a
conclusion section yet, although I have notes on that in the doc file
in the code package referenced in the paper. I'd like to get some
feedback before completing it, and it's already fairly long.

There are several graphics in the paper and in the code files (see
gallery/*.svg) which can provide a good initial scan through
this. Anyway I hope you find it interesting and I'd be grateful for
any comments you might have.

Thanks,

 - Larry


* Henry
* Steve L
* Jonathon B
* Joe W
* Bob M

Jack D
* Al O
Gerry S

















(let ((n 3))
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-objgraph)))


(! (g define-rule)
   '(rule
	 (name x)
	 (attach-to x)
	 (pred
	  (?x abc ?z)
	  (?nn1 new-node sn1))
	 (add
	  (print x ?x ?z ?nn1)
	  (?x1 1)
	  (?x2 add (?x3 2))
	  (?x4 rule (rule
				(name y)
				(pred
				 (?y1 3))
				(add
				 (print y ?y1)
				 (?y2 4)
				 (?y3 add (?y4 5))))))))

(! (g define-rule)
   '(rule
	 (name x)
	 (attach-to x)
	 (pred
	  (?x 0))
	 (add
	  (print x ?x)
	  (?x1 1)
	  (?x2 add (?x3 2))
	  (?x4 rule (rule
				(name y)
				(pred
				 (?y1 3))
				(add
				 (print y ?y1)
				 (?y2 4)
				 (?y3 add nn42)
				 (nn42 elem0 ?y4)
				 (nn42 elem1 5)))))))

(! (g define-rule)
   '(rule
	 (name x)
	 (attach-to x)
	 (pred
	  (?x 0)
	  (?nn1 new-node sn1))
	 (add
	  (print x ?x ?nn1)
	  (?x1 1)
	  (x rule (rule
				(name y)
				(pred
				 (?y1 3)
				 (?nn2 new-node sn2))
				(add
				 (print y ?y1 ?nn1 ?nn2)
				 (?y1 4)
				 (?y1 add ?nn2)
				 (?nn2 elem0 4)
				 (?nn2 elem1 5)
				 (?nn2 elem2 ?nn42)
				 (x rule (rule
						  (name z)
						  (pred
						   (?y1 4)
						   (?nn3 new-node sn3))
						  (add
						   (print z ?y1 ?nn3)
						   (?y1 5)
						   (?nn3 6))))))))))

(! (g def-obj-edges) '((x abc 0)(x abc 1)))

(! (g def-obj-edge) '(y1 3))

(! (g execute-obj) 'x :cont (lambda (m s e p) (list m s e p)))

(! ((! (g get-rule-components) (! (g query) '((?r type rule)(?r name x)) '?r)) adds))

(! ((! (g get-rule-components) (! (g query) '((?r type rule)(?r name y)) '?r)) adds))



(! (g match-and-execute-rule) (! (g query) '((?r type rule)(?r name y)) '?r) 'y1 :cont (lambda (m s e p) (list m s e p)))


























(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-the-graph))
  (let ((r (! (g query) '((?r type rule)(?r name fwd-fe-rule)) '?r)))
	(let ((c (! (g query) '((?r type rule)(?r name copy-rule-rule)) '?r)))
	  (! (g def-obj-edges) `((,r copy-rule r1)(,r rule ,c)))
	  (! (g execute-obj) r :cont (lambda (m s e p) (list m s e p))))))


(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-the-graph))
  (let ((r (! (g query) '((?r type rule)(?r name copy-rule-rule)) '?r)))
	(let ((c (! (g query) '((?r type rule)(?r name copy-rule-rule)) '?r)))
	  (! (g def-obj-edges) `((,r copy-rule r1)(,r rule ,c)))
	  (! (g execute-obj) r :cont (lambda (m s e p) (list m s e p))))))

















(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-the-graph))
  (let ((r (! (g query) '((?r type rule)(?r name fwd-fe-rule)) '?r)))
	(let ((c (! (g query) '((?r type rule)(?r name copy-rule-rule)) '?r)))
	  (! (g def-obj-edges) `((,r copy-rule r1)(,r rule ,c)))
	  (! (g execute-obj) r :cont (lambda (m s e p) (list m s e p))))))


(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g  (make-base-graph))
  (! (g add-natural-number-edges) 20)
  (! (g read-rule-file) "fe.lisp")
  (! (g read-rule-file) "copy-rule.lisp")
  (! (g read-rule-file) "display-data.lisp")
  (let ((r (! (g query) '((?r type rule)(?r name fwd-fe-rule)) '?r)))
	(let ((c (! (g query) '((?r type rule)(?r name copy-rule-rule)) '?r)))
	  (! (g def-obj-edges) `((,r copy-rule r1)(,r rule ,c)))
	  (! (g execute-obj) r :cont (lambda (m s e p) (list m s e p))))))

















(! (g define-rule)
  '(rule
	(name test1)
	(pred
	 (?x sigma ?y)
	 (?nn1 new-node sn1))
	(add
	 (print test1 ?x ?y ?nn1)
	 (?x a 1)
	 (?y b 2)
	 (?nn1 c ?x ?y)
	 (?nn1 test2))))

(! (g define-rule)
  '(rule
	(name test2)
	(pred
	 (?z test2))
	(add
	 (print test2 ?z)
	 (stuff ?z))))
