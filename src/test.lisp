;; grep list-to-edge-elem-node *.lisp | egrep -v '\.[0-9]'

;; Prototype for the "canonical" fft test loop. Note should clean up
;; the others in this file.  As of the veriosn checked in 8/11/20,
;; this supports tree.lisp (local rules); commented-out (x is
;; treetopobj levels ,n) is needed for globaltree.lisp

(let ((n 3))
  (clear-counters)
  (clear-perf-stats)

  (setq g (make-foundation))  

  (! (g read-rule-file) "fft.lisp")
  ;; (! (g read-rule-file) "fft-const-rules.lisp")
  
  (! (g read-rule-file) "tree.lisp")  ;;; 8/11/20 -- Fixed issues with this and it should hold as the default now
  ;; (read-rule-file "globaltree.lisp")
  (! (g read-rule-file) "rule30.lisp")
  (! (g read-rule-file) "fft-delta.lisp")

  ;; (! (g read-rule-file) "display-rules.lisp")
  ;; (! (g read-rule-file) "fft-display-rules.lisp")
  
  ;; (! (g add-natural-number-edges) 50)		;; Need this if expand rule-30 to 50 (or beyond)

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

						;; (x is treetopobj levels ,n)		;; Supports global-tree.lisp

						;; (x is treetopobj l ,n)				;; Supports tree.lisp
						(tree-rule x ,n)

						(x fft-top)		;; An experiment in symbol-free matching, for max locality. Works, but slow. See fft-top-rule
						;; (x n1)
						;; (n1 n2)
						;; (n2 n3)

						(x fft xfft)
						(x level ,n)
						(x color navajowhite)
						(x rand r)
						(x rule ,(! (g query) '((?x name fft-rule)) '?x))

						(x local-rule-pool local-rule-pool-node)
						(r local-rule-pool local-rule-pool-node)
						)
					   (del
						(global-node rule ?this-rule))))

  ;; (! (g trace-rule) 'od-next)
  ;; (! (g break-rule) 'od-next 'del-consequent-edges)
  ;; (! (g break-rule) 'od-next 'del-edge)
  ;; (! (g break-rule) 'od-next 'add-consequent-edges)
  ;; (! (g break-rule) 'od-next 'match-and-execute-rule)
  (time
   (timer 'main
	 (lambda ()
	   (! (g execute-global-all-objs-loop))
	   ))))

;; Performance loop, making a log fftperf we can scan
;;
;; I've only let it run to 2^8, since higher looked like it would take
;; several hours. 2^8 took about 7700 seconds.

(let ()
  (defr
	(defl f (n)
	  (let ((n n))
		(clear-counters)
		(clear-perf-stats)
		(setq g (make-foundation))  
		(! (g read-rule-file) "fft.lisp")
		(! (g read-rule-file) "tree.lisp")
		(! (g read-rule-file) "rule30.lisp")
		(! (g read-rule-file) "fft-delta.lisp")
		;; (! (g read-rule-file) "display-rules.lisp")
		;; (! (g read-rule-file) "fft-display-rules.lisp")
		;; (! (g add-natural-number-edges) 50)		;; Need this if expand rule-30 to 50 (or beyond)
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

							  ;; (x is treetopobj levels ,n)		;; Supports global-tree.lisp

							  ;; (x is treetopobj l ,n)				;; Supports tree.lisp
							  (tree-rule x ,n)

							  (x fft-top) ;; An experiment in symbol-free matching, for max locality. Works, but slow. See fft-top-rule
							  ;; (x n1)
							  ;; (n1 n2)
							  ;; (n2 n3)

							  (x fft xfft)
							  (x level ,n)
							  (x color navajowhite)
							  (x rand r)
							  (x rule ,(! (g query) '((?x name fft-rule)) '?x))

							  (x local-rule-pool local-rule-pool-node)
							  (xfft local-rule-pool local-rule-pool-node)
							  (r local-rule-pool local-rule-pool-node))
							 (del
							  (global-node rule ?this-rule))))

		;; (! (g trace-rule) 'the-other-copy-array-struct-next)
		(time
		 (timer 'main
		   (lambda ()
			 (! (g execute-global-all-objs-loop))
			 )))))
	(with-open-file (s "fftperf" :direction :output)
	  (let ((std *standard-output*))
		(let ((*standard-output* s))
		  (let ((n 9))
			(time
			 (dotimes (i n)
			   (print (list '************* i) std)
			   (f i)
			   (perf-stats)
			   (! (g rule-stats))
			   (room t)))))))))
(let ((n 3))
  (clear-counters)
  (clear-perf-stats)

  (setq g (make-foundation))  
  (! (g read-rule-file) "fft-bare-bones-no-sing.lisp")
  (! (g read-rule-file) "tree.lisp")

  ;; (! (g add-natural-number-edges) 50)

  (! (g define-rule) `(rule
					   (name init)
					   (attach-to global-node)
					   (pred
						(global-node rule ?r)
						(?r name init))
					   (add
						(print init)
						(tree-rule x ,n)
						(x fft-top)
						(x fft xfft)
						(x level ,n)
						(x rule ,(! (g query) '((?x name fft-rule)) '?x))
						(x local-rule-pool local-rule-pool-node)
						(xfft local-rule-pool local-rule-pool-node)
						)
					   (del
						(global-node rule ?this-rule))))

  ;; (! (g trace-rule) 'the-other-copy-array-struct-next)
  (time
   (timer 'main
	 (lambda ()
	   (! (g execute-global-all-objs-loop))
	   ))))



;; For fundamential testing, here we split out only the fft rule runs (i.e., no rule30 or random stuff).

(let ((n 3))
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-foundation))  
  (! (g read-rule-file) "fft.lisp")
  (! (g read-rule-file) "tree.lisp")  ;;; 8/11/20 -- Fixed issues with this and it should hold as the default now
  ;; (! (g read-rule-file) "display-rules.lisp")
  ;; (! (g read-rule-file) "fft-display-rules.lisp")
  (! (g define-rule) `(rule
					   (name init)
					   (attach-to global-node)
					   (pred
						(global-node rule ?r)
						(?r name init))
					   (add
						(print init)
						(tree-rule x ,n)
						(x fft-top)
						(x fft xfft)
						(x level ,n)
						(x rule ,(! (g query) '((?x name fft-rule)) '?x))
						(x local-rule-pool local-rule-pool-node)
						)
					   (del
						(global-node rule ?this-rule))))
  (time
   (timer 'main
	 (lambda ()
	   (! (g execute-global-all-objs-loop))
	   ))))

(let ((d (make-dumper)))
  (! (d set-graph) g)
  (! (d dump-gv-edges) "xxfft.gv" :rules t :attrs
	 '(
	   fft-hb
	   fft-comb
	   odd 
	   even
;;	   zero
;;     d-casz
	   d
;;	   center-up
       next-color
	   rule30val
	   rule-30-next
	   up
	   delta3
	   delta3-rand
;;	   next
;;       casz-ref
;;       casz-ref1
;;     copy-array-struct
	   ))
  (! (d gv-to-svg) "xxfft"))
 
(let ((d (make-dumper)))
  (! (d set-graph) g)
  (! (d dump-gv-edges) "y1.gv" :rules nil :attrs-fcn
	 (lambda (edge)
	   (or (intersect edge '(center-up delta3-rand))
		   (and (eq (second edge) 'rule30val)
				(! (g superqets) (list (first edge) 'center-up)))))))

(with-open-file (s "xxx" :direction :output)
  (let ((et (! ((! (g get-edge-to-trace)) as-list))))
	(dolist (e et)
	  (let ((edge (first e)))
		(dolist (r (second e))
		  (when (eq (first r) 'add)
			(print-object (list edge r) s)
			(terpri s)))))
	nil))

;; Standard full trace file

(with-open-file (s "xxxet" :direction :output)
  (format s "********** edge-to-trace~%")
  (let ((et (! ((! (g get-edge-to-trace)) as-list))))
	(dolist (e et)
	  (let ((edge (first e)))
		(dolist (r (second e))
		  (print-object (list edge r) s)
		  (terpri s))))
	nil)
  (format s "********** edge-to-pred~%")
  (let ((et (! ((! (g get-edge-to-pred)) as-list))))
	(dolist (e et)
	  (let ((edge (first e)))
		(dolist (r (second e))
		  (print-object (list edge r) s)
		  (terpri s))))
	nil)
  (format s "********** edge-to-add~%")
  (let ((et (! ((! (g get-edge-to-add)) as-list))))
	(dolist (e et)
	  (let ((edge (first e)))
		(dolist (r (second e))
		  (print-object (list edge r) s)
		  (terpri s))))
	nil))

;; Pred trace

(with-open-file (s "xxxep" :direction :output)
  (let ((et (! ((! (g get-edge-to-pred)) as-list))))
	(dolist (e et)
	  (let ((edge (first e)))
		(dolist (r (second e))
		  (print-object (list edge r) s)
		  (terpri s))))
	nil))

(with-open-file (s "xxx" :direction :output)
  (let ((*print-pretty* nil))
	(let ((et (! ((! (g get-edge-to-trace)) as-list))))
	  (let ((flatlist nil))
		(dolist (e et)
		  (let ((edge (first e)))
			(dolist (r (second e))
			  (setq flatlist (cons (list edge r) flatlist)))))
		(let ((flatlist (sort flatlist 
							  (lambda (x y)
								(< (if (eq (first (second x)) 'tested) (third (second x)) (second (second x)))
								   (if (eq (first (second y)) 'tested) (third (second y)) (second (second y))))))))
		  (dolist (x flatlist)
			(print x s))))
	  nil)))

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
	(setq room (! (g query) '((?s type room)) '?s))
	(print (list room)))
  (! (g clear-queue))
  (! (g add) room 'event 'occupied)
  (! (g execute-queue)))


;; fe-rule-test

;; See copy-rule.lisp:
;;	comment on non-det version
;;  Also much stuff commented-out -- need at least doc on this

;; nat = 185 is max levels of perf before dot craps out

;; Uses the no-copy form of the fe rules on g

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

(defr
  ;; One run of the copying form of fe-rule-test
  (defl f (nat)
	(let ((*print-pretty* nil))
	  (clear-counters)
	  (clear-perf-stats)
	  (setq g (make-foundation)) ;; Can alternatively say base-graph here, but foundation provides display colors, etc.
	  (! (g add-natural-number-edges) nat)
	  (! (g read-rule-file) "fe.lisp")
	  (! (g read-rule-file) "copy-rule.lisp")
	  (! (g addraw) 0 'rule (! (g query) '((?x name fe-0-rule)) '?x))
	  (! (g add-edge) '(0 local-rule-pool local-rule-pool-node))
	  ;; (! (g trace-rule) 'copy-rule-rule)
	  ;; (! (g trace-rule) 'fe-0-rule)
	  (timer 'main
		(lambda ()
		  (! (g execute-obj) 'global-node :cont (lambda (m s e) nil))
		  (! (g execute-obj) 0 :cont (lambda (m s e) nil))
		  (! (g execute-queue) :rule-mode :local-only)

		  ;; (! (g execute-global-all-objs-loop)) ;; Temp! until we get queuing work right.
		  ))))

  ($nocomment 
   (f 20))								;; The basic single run

  ;; Perf runs
  ;; Nat to 200 
  ($comment								;; Perf runs
   (with-open-file (s "feperf" :direction :output)
	 (let ((std *standard-output*))
	   (let ((*standard-output* s))
		 (let ((n 20))
		   (time
			(dotimes (i n)
			  (let ((nat (* (+ i 1) 5)))
				(print (list '************* (+ i 1) nat) std)
				(f nat))
			  (perf-stats)
			  (! (g rule-stats))
			  (room t))))))))

  ($comment
   (let ()
	 (setq gp (make-gnuplot))
	 (! (gp plot-log) (make-fe-perf-stats-info))))

  ;; Large single run, to transcript file 
  ;; Nat 150 looks like the max that dot will handle. 
  ($comment
   (with-open-file (s "feout" :direction :output)
	 (let ((std *standard-output*))
	   (let ((*standard-output* s))
		 (let ((nat 150))
			(print (list '************* nat) std)
			(time (f nat)))))))
)

(let ((d (make-dumper)))
  (! (d set-graph) g)
  (! (d dump-gv-edges) "fe.gv"
	 :omitted-attrs '(for-rule next-color
							   copy-rule-rule-pred copy-rule-rule-add copy-rule-rule-pred-elem copy-rule-rule-add-elem)
	 :rules nil
	 :attrs '(sigma even-func fe copied-from rule copy-rule))
  
  (! (d gv-to-svg) "fe"))

;; End fe-rule-test

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

(let ((d (make-dumper)))
  (! (d set-graph) g)
  (! (d dump-gv-edges) "rule30.gv" :attrs '(rule-30-next up center-up rule30val))
  (! (d gv-to-svg) "rule30"))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "xtree.gv" :attrs 
											   '(aup next tree-next zero max ev od)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "fe.gv" :attrs 
											   '(sigma even-func fe copied-from  next-color)))		   

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "x.gv" :attrs 
											   '(#| sigma even-func |# fe copied-from  next-color rule)
											   :omitted-rules '(color-circle-data)
											   :omitted-attrs '(copy-rule-rule copy-rule-rule-pred copy-rule-rule-pred-elem copy-rule-rule-add copy-rule-rule-add-elem)))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "x4.gv" :attrs '(fft fft-hb fft-comb odd even copy-array-struct zero) :rules '(add-parent ev-init od-next ev-next ev-od-obj-rule is tree-next-zero-rule tree-next-rule tree-loop-rule tree-top-order-rule tree-top-propagate-rule tree-elem-rule tree-zero-rule tree-max-rule tree-rule treeobj-rule even-next odd-next even-zero odd-zero self-cycle odd-new even-new copy-array-struct-next copy-array-struct-next-sing copy-array-struct-zero copy-array-struct-new fft-comb-rule-next-sing fft-comb-rule-next fft-comb-rule-zero fft-rule-zero fft-rule)))

;; 4/28 Used this to create the complete fft-rule figure, fft-8-just-fft-rules.gv
(let ((d (make-dumper :omit-unmatched-rules t :emit-labels t :emit-legend nil))) (! (d set-graph) g)(! (d dump-gv-edges) "xfft.gv" :rules t :attrs '( fft-comb fft-hb  odd even  d d-casz) :omitted-rules '(color-circle-data print-gc6 print-gc1 print-gc2 print-gc3 print-gc4 print-gc5 nil rule-30-rule-gen rule-30-zero-rule-gen rule-30-max-rule-gen rule-30-next1 rule-30-next2 rule-30-next3 rule-30-center rule-30-center-loop rule-30-loop rule-30-top rule-30-top-propagate rule-30-data add-rule color-color print-gc-rule display-data data switch-room-obj-rule odd-even-weave weave-next-rule fft-delta-init even-tree-max std-notes inverse-data gen-inverse fft-rule-opt fft-rule-opt-display fft-rule) :omitted-attrs '(color in-node-color two-input-op)))


;; First, run a std fft sa above. Then do this to get nice graphics.

(let ()
  (! (g read-rule-file) "fft-for-show.lisp")
  (dolist (x (! (g get-edges) 'note)) (! (g rem-edge) x))
  (! (g add-edge) '(note title "16-point FFT: Dataflow, Butterflies, and Rules"))
  (! (g add-edge) '(note body "Lawrence Stabile 2022"))
  (! (g add-edge) '(note footer ""))
  (let ((d (make-dumper))) 
	(! (d set-graph) g)
	(! (d dump-gv-edges) "xfft.gv" 
	   :attrs '(fft-hb fft-hb-delta fft-comb odd even d) :omit-unmatched-rules nil
	   :omitted-attrs '(attach-to rule color shape two-input-op in-node-color)
	   :rules '(
				init
				tree-elem-rule 
				od-next 
				ev-init 
				tree-zero-rule 
				copy-array-struct-next 
				copy-array-struct-new
				odd-zero 
				tree-max-rule 
				tree-top-order-rule 
				tree-loop-rule 
				tree-next-rule 
				fft-comb-rule-zero 
				copy-array-struct-zero 
				tree-next-zero-rule 
				fft-rule 
				fft-rule-zero 
				odd-next 
				even-next 
				tree-top-rule 
				fft-comb-rule-next 
				even-zero 
				ev-next 
				tree-rule 
				odd-new 
				even-new 
				))))

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



(let ((d (make-dumper)))
  (let ((notes (! (g get-edges) 'note)))
	(dolist (note notes)
	  (when (or (eq (first note) 'note)
				(and (eq (first note) 'set)
					 (eq (second note) 'note)))
		(! (g rem-edge) note)))
	(! (g add-edge) `(set note title fontsize ,(! (g query) '((r level ?x)) '?x)))
	(! (g add-edge) '(note title "FFT Butterflies with Rule-30 Random Deltas\\\nLawrence Stabile, 2020\\\nSee nerdlynotions.org, H-Machine Series"))
	(! (d set-graph) g)
	(! (d dump-gv-edges) "xfft.gv" :omit-unmatched-rules nil :rules nil :emit-labels nil :attrs
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
		 ))))

;;
;; For the graphviz gallery 
;;

;; This version generates an fft top-down/bottom-up connected diagram, without excess

(let ((d (make-dumper)))
  (let ((notes (! (g get-edges) 'note)))
	(dolist (note notes)
	  (when (or (eq (first note) 'note)
				(and (eq (first note) 'set)
					 (eq (second note) 'note)))
		(! (g rem-edge) note)))
	(! (g add-edge) `(set note title fontsize ,(! (g query) '((r level ?x)) '?x)))
	(! (g add-edge) '(note title "FFT Butterflies with Rule-30 Random Deltas\\\nLawrence Stabile, 2020\\\nSee nerdlynotions.org, H-Machine Series"))
	(! (d set-graph) g)
	(! (d dump-gv-edges) "xfft.gv" :omit-unmatched-rules nil :rules nil :emit-labels t :emit-legend nil 
	   :omitted-attrs
	   '(in-node-color color shape two-input-op local-rule-pool rule is-two-input-op)
	   :attrs
	   `(
		    zero ;; ga-word value rule-30-weave-next   ;;	top level max  zero is-elem-of 
		 ;; is-two-input-op
		 fft-hb
		 fft-comb
		 odd 
		 even
		 ;; zero
		 ;;	d-casz
		 ;;	d
		 ;; center-up
		 ;; next-color
		 rule30val
		 ;; rule-30-next
		  up
		 ;;	delta3
		 ;;	delta3-rand
		 ;; next
		 ;;	casz-ref
		 ;;	casz-ref1
		 ;;	casn-ref
		 ;;	casns
		 e
		 ;; copy-array-struct
		 ))))





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





(let ((d (make-dumper :emit-legend nil))) (! (d set-graph) g)(! (d dump-gv-edges) "x.gv" :rules nil :attrs nil :string-attrs-only t))

(let ((d (make-dumper :emit-legend nil))) (! (d set-graph) g)(! (d dump-gv-edges) "x.gv" :rules nil :attrs '(a p r t)))




(let ()
  (setq dims nil)
  (dotimes (i 6)
	(let ((i (+ i 1)))
	  (print (list 'n i))
	  (let ((n i))
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
							  (r local-rule-pool local-rule-pool-node))
							 (del
							  (global-node rule ?this-rule))))
		;; (! (g trace-rule) 'fft-comb-rule-next-sing-delta)
		(timer 'main
		  (lambda ()
			(! (g execute-global-all-objs-loop))))
		(setq dims (cons (! (g dimensions)) dims))))))

(defun f (dims-list)
  (let ((h (make-hash-table)))
	(dolist (dims dims-list)
	  (dolist (dim dims)
		(let ((name (first dim))
			  (dimval (second dim)))
		  (setf (gethash name h) (cons dimval (gethash name h))))))
	h))

		(setq dims (cons (! (g dimensions)) dims))))))

;; Dumping number on dimensions (degrees) of nodes 

(let ((lmax 0))
  (dolist (n (! (g get-all-nodes)))
	(let ((l (length (! (g get-edges) n))))
	  (when (> l lmax)
		(setq lmax l))))
  (let ()
	(setq a (make-array (+ lmax 1):initial-element 0))
	(dolist (n (! (g get-all-nodes)))
	  (let ((l (length (! (g get-edges) n))))
		(setf (aref a l) (+ (aref a l) 1))))
	(with-open-file (s "xxx" :direction :output)
	  (dotimes (i (length a))
		(format s "~a~%" (aref a i))))))

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
							  local-rule-pool-node local-rule-pool))))

(dolist (x '(N2188 FROM-IS-RULE N714 RULE N701)) (print (! (g path) 'n2189 x)))







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
  (! (g read-rule-file) "display-rules.lisp")
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


(let ((x (! ((! (g  get-superqet-map)) as-list))))
  (let ((s (mapcad (lambda (x)
					 (when (= (length (first x)) 2) 
					   (list (first x) (length (! (g get-edges-from-subqet) (first x))))))
				   x)))
	(let ((h (make-hash-table)))
	  (dolist (e s)
		(let ((n (second e)))
		  (setf (gethash n h) (+ (or (gethash n h) 0) 1))))
	  (let ((l (hash-table-to-list h)))
		(let ((m (sort l (lambda (x y) (> (second x) (second y))))))
		  (dolist (x m)
			(print x))))))
  nil)




(dotimes (i 5)
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
						  (r local-rule-pool local-rule-pool-node))
						 (del
						  (global-node rule ?this-rule))))
	;; (! (g trace-rule) 'fft-comb-rule-next-sing-delta)
	(timer 'main
	  (lambda ()
		(! (g execute-global-all-objs-loop))
		))) 
  (perf-stats)
  (print (list 'gc (gc)))
  (print (list 'gc (gc))))


;; all-var-mod
;;
;; 5/18/23 Disabled support for this, but left in tag, as it's not
;; practical right now. See write-up in doc.txt
;;
;; 9/27/20 This section tests all-vars rule preds. Though all-vars
;; preds have been disallowed form the beginning, I made changes to
;; permit it (see "all-var-mod" comment), so that we could try rules
;; which did not rely on specific global constants, thus allowing for
;; lower edge-expansion sizes when matching rules.

(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-the-graph))
)

(! (g define-rule)
  '(rule
	(name xxx)
	(pred
	 (?x abc ?y))
	(add
	 (print xxx ?x ?y)
	 (def)))
)


(! (g define-rule)
  '(rule
	(name yyy)
	(pred
	 (?x ?n1)
	 (?n1 ?n2)
	 (?n2 ?n3)
	 (?n3 ?y))
	(add
	 (print yyy ?x ?y)
	 (ghi)))
)

(dolist (x '((x1 abc y1)(x2 n1)(n1 n2)(n2 n3)(n3 y2)))
  (! (g add-obj-edge) x))

(! (g trace-rule) 'xxx)

(! (g trace-rule) 'yyy)

(! (g execute-obj) 'x2)

(! (g execute-obj) 'x1)

;; end all-var-mod




(setq x (! (g make-rule-graph) 'N691))
(! (x all-qets))
(setq y (! (x make-qet-graph)))
(! (y get-all-edges))
(let ((d (make-dumper))) (! (d set-graph) y)(! (d dump-gv-edges) "xxx.gv" :attrs '(up)))


(setq y (! (g make-qet-graph)))
(let ((d (make-dumper))) (! (d set-graph) y)(! (d dump-gv-edges) "yyy.gv" :attrs '(up)))



(let ()
  (setq x1 (! (g make-rule-graph) 'N691))
  (setq x2 (! (x1 make-qet-graph)))
  (dolist (e (! (x2 get-all-edges)))
	(print (list 'e e))
	(when (and (= (length e) 1)
			   (intersect '(fft-comb is-elem-of zero local-rule-pool lrp-rule name fft-comb-rule-next fft-comb-rule-next-sing
									 local-rule-pool-node global-node fft-comb-rule-zero)
						  e))
	  (print (list 'd e))
	  (! (x2 rem-edge) e)))
  (setq x3 (! (x2 make-hasse-graph))))


(let ()
  (setq x1 
		(let ((x (make-objgraph)))
		  (dolist (e (! (g get-all-edges)))
			(when (intersect '(elem is-elem-of) e)
			  (when (not (intersect '(_elem) e))
				(! (x add-edge) e))))
		  x))
  (setq x2 (! (x1 make-qet-graph)))
  (setq x3
		(let ((x (make-objgraph)))
		  (dolist (e (! (x2 get-all-edges)))
			  (when (not (intersect '(elem is-elem-of) e))
				(! (x add-edge) e)))
		  x))
  (setq x4 (! (x3 make-qet-graph)))
  (setq x5 (! (x4 make-hasse-graph) :levels-to-omit '(0))))

(let ()
  (setq x1 
		(let ((x (make-objgraph)))
		  (dolist (e (! (g get-all-edges)))
			(when (intersect '(elem is-elem-of) e)
			  (when (not (intersect '(_elem) e))
				(! (x add-edge) e))))
		  x))
  (setq x2 (! (x1 make-qet-graph)))
  (setq x3
		(let ((x (make-objgraph)))
		  (dolist (e (! (x2 get-all-edges)))
			  (when (not (intersect '(elem is-elem-of) e))
				(! (x add-edge) e)))
		  x))
  (setq x4 (! (x3 make-qet-graph)))
  (setq x5 (! (x4 make-hasse-graph) :levels-to-omit '(0))))

(setq x
	  (let ((x (! (g graph-filter) (lambda (e) (intersect '(elem is-elem-of) e)))))
		(let ((x (! (x graph-filter) (lambda (e) (not (intersect '(_elem) e))))))
		  (let ((x (! (x make-qet-graph))))
			(let ((x (! (x graph-filter) (lambda (e) (not (intersect '(elem is-elem-of) e))))))
			  (let ((x (! (x make-qet-graph))))
				(let ((x (! (x make-hasse-graph))))
				  x)))))))

(setq x
	  (let ((x (! (g graph-filter) (lambda (e) (intersect '(oe-ref next is-elem-of) e)))))
		(let ((x (! (x graph-filter) (lambda (e) (not (intersect '(_elem) e))))))
		  (let ((x (! (x graph-filter) (lambda (e) (intersect e
															  '(N2199 N2184 N2029 N2207  
																N2182 N2194 N1998 N2196  
																N2194 N2182 N1999 N2204  
																N2190 N2181 N2014 N2200  
																N2180 N2186 N1983 N2188  
																N2181 N2190 N2013 N2192  
																N2184 N2199 N2028 N2201  
																N2186 N2180 N1984 N2195))))))
			(let ((x (! (x make-simplicial-complex-graph))))
			  x)))))

(setq x
	  (let ((x (! (g make-rule-graph) 'N388)))
		(let ((x (! (x graph-filter) (lambda (e) (intersect '(oe-ref next is-elem-of) e)))))
		  (let ((x (! (x graph-filter) (lambda (e) (not (intersect '(_elem) e))))))
			(let ((x (! (x make-simplicial-complex-graph))))
			  x)))))





;; 3/6/21
;;
;; Rule30 complexity test, building up levels by fives. Save the resulting perf
;; stats in a file like xxx and then run gnuplot graphs as described
;; in rule30.lisp
;;
;; 10/24/22 Modified to use the rule-30-test class in h.lisp

(with-open-file (s "rule30perf" :direction :output)
  (let ((std *standard-output*))
	(let ((*standard-output* s))
	  (let ((n 10))
		(time
		 (dotimes (i n)
		   (setq g (make-rule-30-test))
		   (print (list '************* i (* 5 i)) std)
		   (! (g run) (* 5 i))
		   (perf-stats)
		   (! (g rule-stats))
		   (room t)
		   (print (sort (mapcar (lambda (e) (if (not (memq 'print e)) (length e) 0)) (! (g get-all-edges))) (lambda (x y) (> x y))))))))))

(with-open-file (s "rule30test" :direction :output)
  (let ((std *standard-output*))
	(let ((*standard-output* s))
	  (setq g (make-rule-30-test))
	  (time (! (g run) 200)))))

(let ((n 5))	;; 3
  (setq g (make-rule-30-test))
  (time (! (g run) n)))

(ca-to-svg "y.svg" 101 30 :colorized t)

(let ((d (make-dumper)))
  (! (d set-graph) g)
  (! (d dump-gv-edges) "rule30.gv" :rules nil :separate-number-nodes t :attrs '(rule-30-next up center rule30val xcoord level pos neg zero max))
  (! (d gv-to-svg) "rule30"))

(let ()
  (let ((n 3))
	(setq g (make-rule-30-test))
	(time (with-redirected-stdout "y1"
								  (lambda (std)
									(! (g run) n)))))
  (setq x (! (g edge-trace-graph) 
			 :make-new-graph t
			 :rules-fcn (lambda (r) (memq r '(
											  rule-30-next-rule-1-1-1   
											  rule-30-next-rule-1-1-0   
											  rule-30-next-rule-1-0-0   
											  rule-30-next-rule-0-0-1   
											  rule-30-next-rule-0-1-1   
											  rule-30-next-rule-0-0-0   
											  rule-30-next-rule-1-0-1   
											  rule-30-next-rule-0-1-0   
											  ;; rule-30-zero-rule-1-1     
											  ;; rule-30-center            
											  ;; rule-30-max-rule-1-1      
											  ;; rule-30-max-rule-0-1
											  ;; rule-30-top
											  ;; init
											  ;; is-0-param-xrule          
											  ;; is-not-xrule              
											  ;; gen-inverse               
											  ;; init                      
											  ;; add-inverse-is-elem-of    
											  ;; add-inverse-is-member-of  
											  ;; rule-30-zero-rule-gen     
											  ;; rule-30-max-rule-gen      
											  ;; rule-30-next-rule-gen     
											  ;; data                      
											  ;; basic-display-data        
											  ;; color-circle-data         
											  ;; inverse-data              
											  ;; is-0-param                
											  ;; rule-30-center-obj-rule   
											  ;; rule-30-data              
											  ;; rule-30-next-rule-opt     
											  ;; is-1-param                
											  ;; std-notes                 
											  ;; color-color             
											  )))
			 ;; :nodes '(level sigma rule-30-next rule-30-top)
			 ))
  (! (x read-rule-file) "edge-trace-trim-rules.lisp")
  (time (with-redirected-stdout "y2"
								(lambda (std)
								  (! (x execute-global-all-objs-loop)))))
  (! (x read-rule-file) "edge-trace-rules.lisp")
  ;;  (! (x trace-rule) 'edge-trace-add-trim)
  (time (with-redirected-stdout "y3"
								(lambda (std)
								  (! (x execute-global-all-objs-loop)))))
  (let ((d (make-dumper)))
	(! (d set-graph) x)
	(! (d dump-gv-edges) "y.gv" :rules nil :emit-legend nil :attrs '(p a pe pr #| d r an pn q e rn |#))
	(! (d gv-to-svg) "y"))
  )


#|
rule-30-zero-rule-gen     
rule-30-max-rule-gen      
rule-30-next-rule-gen     
rule-30-next-rule-opt     
rule-30-next-rule-1-1-1   
rule-30-next-rule-1-1-0   
rule-30-next-rule-1-0-0   
rule-30-next-rule-0-0-1   
rule-30-next-rule-0-1-1   
rule-30-next-rule-0-0-0   
rule-30-next-rule-1-0-1   
rule-30-next-rule-0-1-0   
rule-30-zero-rule-1-1     
rule-30-center            
rule-30-max-rule-1-1      
rule-30-max-rule-0-1
rule-30-top
init
is-0-param-xrule          
is-not-xrule              
gen-inverse               
init                      
add-inverse-is-elem-of    
add-inverse-is-member-of  
rule-30-zero-rule-gen     
rule-30-max-rule-gen      
rule-30-next-rule-gen     
data                      
basic-display-data        
color-circle-data         
inverse-data              
is-0-param                
rule-30-center-obj-rule   
rule-30-data              
rule-30-next-rule-opt     
is-1-param                
std-notes                 
color-color             
|#

(let ()
  (let ((n 5)) ;; 5 ;; 10
	(setq g (make-rule-30-test))
	(time (with-redirected-stdout "y1"
								  (lambda (std)
									(! (g run) n)))))
  (setq x (! (g edge-trace-graph) 
			 :make-new-graph t
			 :rules-fcn (lambda (r) (memq r '(
											  rule-30-zero-rule-gen     
											  rule-30-max-rule-gen      
											  rule-30-next-rule-gen     
											  rule-30-next-rule-opt     

											  rule-30-zero-rule-1-1     
											  rule-30-center            
											  rule-30-max-rule-1-1      
											  rule-30-max-rule-0-1
											  )))
			 :except-rules-fcn (lambda (r) (memq r '(
													 #|
													 color-color
													 rule-30-next-rule-1-1-1   
													 rule-30-next-rule-1-1-0   
													 rule-30-next-rule-1-0-0   
													 rule-30-next-rule-0-0-1   
													 rule-30-next-rule-0-1-1   
													 rule-30-next-rule-0-0-0   
													 rule-30-next-rule-1-0-1   
													 rule-30-next-rule-0-1-0   
													 |#
													 )))
			 ;; :nodes '(level sigma rule-30-next rule-30-top)
			 :trim-dangling-adds-and-preds nil
			 ))
  (! (x read-rule-file) "edge-trace-trim-rules.lisp")
  (time (with-redirected-stdout "y2"
								(lambda (std)
								  (! (x execute-global-all-objs-loop)))))
  (! (x read-rule-file) "edge-trace-rules.lisp")
  ;;  (! (x trace-rule) 'edge-trace-add-trim)
  (time (with-redirected-stdout "y3"
								(lambda (std)
								  (! (x execute-global-all-objs-loop)))))
  (let ((d (make-dumper)))
	(! (d set-graph) x)
	(! (d dump-gv-edges) "y.gv"
	   :rules nil 
	   :emit-legend nil
	   :gv-graph-props "rankdir=LR;"  ;; "ranksep=5.0;"
	   :attrs '(d ae ar pe pr am #| p d r an pn q e rn |#))
	(! (d gv-to-svg) "y" :edit-svg t))
  )


(let ()
  (let ((n 10))
	(setq g (make-rule-30-test))
	(time (with-redirected-stdout "y1"
								  (lambda (std)
									(! (g run) n)))))
  (setq x (! (g edge-trace-rule-graph) 
			 ))
  (let ((d (make-dumper)))
	(! (d set-graph) x)
	(! (d dump-gv-edges) "x.gv"
	   :rules nil 
	   :emit-legend nil
	   :attrs '(r))
	(! (d gv-to-svg) "x" :edit-svg t))
  )



(let ()
  (let ((n 3))
	(setq g (make-fft-bare-bones-no-sing-test n))
	(time (! (g run))))
  (let ()
	(setq x (! (g edge-trace-graph)
			   :make-new-graph t
			   #|						;
			   :rules-fcn (lambda (r) 
			   (memq r '(
			   the-other-copy-array-struct-next
			   fft-comb-rule-zero
			   fft-comb-rule-next
			   even-next
			   odd-next
			   even-new
			   odd-new
			   )))
			   |#
			   #|
			   :nodes-fcn (lambda (edge)
			   (block b
			   (dolist (node edge)
			   (when (memq node '(fft odd even fft-hb fft-comb copy-array-struct))
			   (return-from b t)))))
			   |#
			 :except-nodes-fcn (lambda (edge)
								 (block b
								   (dolist (node edge)
									 (when (memq node '(color _elem rule add pred add-main name next-color local-rule-pool))
									   (return-from b t)))))))
  
  (! (x read-rule-file) "edge-trace-trim-rules.lisp")
  (! (x execute-global-all-objs-loop))
  (! (x read-rule-file) "edge-trace-rules.lisp")
  (! (x execute-global-all-objs-loop))
  )
  (let ((d (make-dumper))) (! (d set-graph) x)(! (d dump-gv-edges) "y1.gv" :rules nil :attrs '(a p)))
)


;; "Generic" edge trace graph run, assumes g set

(let ()
  (time
   (setq x (! (g edge-trace-graph) 
			  :make-new-graph t
			  :rules-fcn (lambda (r) (memq r '(
											   ;; add-inverse-is-elem-of 
											   ;; add-inverse-is-member-of 
											   rule-30-max-rule-0-1 
											   rule-30-max-rule-1-1 
											   rule-30-zero-rule-1-1 
											   rule-30-zero-prune 
											   ;; ev-init 
											   init 
											   fft-rule-opt 
											   fft-rule-opt-rule-names 
											   ;; fft-rule-delta6
											   ;; fft-rule-delta5
											   ;; fft-rule-delta4
											   ;; fft-rule-delta3
											   ;; fft-rule-delta2
											   ;; fft-delta-init
											   ;; rule-30-data 
											   ;; rule-30-top 
											   ;; rule-30-center-loop 
											   ;; rule-30-center 
											   ;; rule-30-max-prune 
											   ;; rule-30-max-rule-gen 
											   ;; rule-30-zero-prune-gen 
											   ;; rule-30-zero-rule-gen 
											   ;; rule-30-next-rule-opt 
											   ;; rule-30-next-rule-gen 
											   ;; rule-30-center-obj-rule 
											   ;; treeobj-rule 
											   tree-top-rule 
											   tree-rule-opt 
											   ;; tree-leaf-rule 
											   tree-rule 
											   ;; tree-max-rule 
											   ;; tree-zero-rule 
											   ;; tree-elem-zero-rule 
											   ;; tree-elem-rule 
											   ;; tree-top-propagate-rule 
											   ;; tree-top-order-rule 
											   ;; tree-loop-rule 
											   tree-next-rule 
											   ;; tree-next-zero-rule 
											   fft-top-rule 
											   fft-rule 
											   fft-rule-zero 
											   fft-comb-rule-zero 
											   fft-comb-rule-next 
											   copy-array-struct-new-gen 
											   opt-copy-array-struct-zero-and-not-zero 
											   copy-array-struct-not-zero 
											   copy-array-struct-zero 
											   copy-array-struct-next 
											   install-copy-array-struct-next 
											   ;; weave-next-rule
											   ;; odd-even-weave
											   ;; odd-tree-zero
											   ;; even-tree-max 
											   ;; even-new-rule-propagate 
											   ;; odd-new-rule-propagate 
											   ;; odd-zero 
											   ;; even-zero 
											   ;; odd-next 
											   ;; odd-new 
											   ;; even-next 
											   ;; even-new 
											   ;; ev-od-obj-rule
											   ev-od-opt 
											   ev-next 
											   od-next 
											   ev-init-gen 
											   inverse-data 
											   gen-inverse 
											   ;; basic-display-data
											   ))
						  ;; t			;
						   )
			  :except-rules-fcn (lambda (r) (memq r '(
													  #| ;
													  |#
													 )))
			 :nodes-fcn (lambda (e) (intersect e '(
												   ;; fft-hb
												   ;; fft-comb
												   ;; odd
												   ;; even
												   fft
												   ))
						  t
						  )
			 :except-nodes-fcn (lambda (e) (intersect e '(lrp-rule is-element-of local-rule-pool-node local-rule-pool)))
			 :trim-dangling-adds-and-preds nil
			 )))
  (! (x read-rule-file) "edge-trace-trim-rules.lisp")
  (time (with-redirected-stdout "y2"
								(lambda (std)
								  (! (x execute-global-all-objs-loop)))))
  (! (x read-rule-file) "edge-trace-rules.lisp")
  ;;  (! (x trace-rule) 'edge-trace-add-trim)
  (time (with-redirected-stdout "y3"
								(lambda (std)
								  (! (x execute-global-all-objs-loop)))))
  (let ((d (make-dumper)))
	(! (d set-graph) x)
	(! (d dump-gv-edges) "z.gv"
	   :rules nil 
	   :emit-legend nil
	   :gv-graph-props "rankdir=LR;"
	   :attrs '(ae ar pe pr am d #| p d r an pn q e rn |#))
	(time (! (d gv-to-svg) "z" :edit-svg nil)))
  )

(let ((d (make-dumper))) (! (d set-graph) x)(! (d dump-gv-edges) "y1.gv" :rules nil :attrs '(a p r e)))
(let ((d (make-dumper))) (! (d set-graph) x)(! (d dump-gv-edges) "y.gv" :rules nil :attrs '(a p r e)))

;; 9/20/22 Basic test on hoss wrt inhertited ctor args

(defc x nil (a b &key c)
  (let ()
	(defm f ()
	  (list a b c))))
#'make-x
(defc y x (d e f &key (g 42) h)
  (let ()
	(defm g ()
	  (list a b c d e f g  h))))
#'make-y
(defc z y (i j)
  (let ()
	(defm h ()
	  (list a b c d e f g h i j))))
#'make-z
(setq x (make-z 1 2 3 4 5 6 7 :g 8 :h 9 :c 10))
(! (x f))
(! (x g))
(! (x h))


;; 10/17/22 Basic test of hoss with nested defms
;;
;; (! (x f) 10) => 13
;; (! (x g) 10) => 13

(defc x nil nil
  (let ((a 10))
	(defm f (x)
	  (if (= x 0)
		  1
		  (! (self g) x)))
	(let ((b 2))
	  (defm g (x)
		(+ x b (! (self f) 0))))))



;; 11/15/22 Running basic-tests.lisp, which contains a class with the original switch/room/add tests.
;; See basic-tests.lisp for more info.

(let ()
  (load "basic-tests.lisp")
  (setq g (make-basic-test))
  (! (g run)))


;; Tree tests

(let ((n 3))
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-foundation))
  (! (g read-rule-file) "tree.lisp")
  (! (g define-rule) `(rule
					   (name init)
					   (attach-to global-node)
					   (pred
						(global-node rule ?r)
						(?r name init))
					   (add
						(print init)
						(tree-rule x ,n)
						(x local-rule-pool local-rule-pool-node))
					   (del
						(global-node rule ?this-rule))))
  (time
   (timer 'main
	 (lambda ()
	   (! (g execute-global-all-objs-loop))
	   ))))

(let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "y1.gv" :rules nil :attrs '(aup next tree-next zero max)))

;; string-nodes

(let ((rule-chain-indices nil))
  (defr
	(defl xprint (x) nil)
	(dolist (x (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n)))
	  (let ((rule (first x)))
		(let ((name (second x)))
		  (let ((r (! (g make-rule-graph) rule)))
			(mapc (lambda (x) (! (g rem-edge) (list rule 'rg x))) (! (g hget-all) rule 'rg))
			(! (g add-edge) (list rule 'rg r))
			(let ((root-vars (mapcad (lambda (x) (when (is-var-name x) x)) (! (r get-root-vars)))))
			  (let ((root-var (first root-vars)))
				(let ((conn (! (r rule-is-connected-via-vars))))
				  (let ((rule-covered (set-equal (! (r get-all-edges)) (! (r get-edges-from-chains)))))
					(let ((rule-det (= (or (gethash name (! (g get-match-count-hash))) 0) 1)))
					  (print (list '************* 'rule rule name root-var conn rule-covered rule-det))
					  (when root-var
						(print (list 'bipseq (! (g bipartite-breadth-rule-walk-seq) r root-var))))
					  (xprint (list 'rule-chains (! ((! (r get-chains)) as-list))))
					  (xprint (list 'rule-chain-indices (! ((! (r get-chains)) inputs))))
					  (setq rule-chain-indices (hunion rule-chain-indices (mapcar (lambda (x) (rest x)) (! ((! (r get-chains)) inputs)))))
					  (! (r bipartite-breadth-rule-walk)
					  ;; (! (r bipartite-depth-rule-walk)
						 root-var
						 :result-fcn (lambda (bnode level)
									   (when (or (is-var-name bnode)
												 (is-edge bnode))
										 (print (list bnode (/ (- level 1) 2)))))))))))))))
	(xprint (list 'all-rule-chain-indices rule-chain-indices (length rule-chain-indices)))
	(let ((data-chain-indices (mapunion (lambda (x) (list (rest x))) (! ((! (g get-chains)) inputs)))))
	  (xprint (list 'all-data-chain-indices data-chain-indices (length data-chain-indices)))))
  nil)

;; tree-next-rule det

(let ((rn (! (g query) '((?r type rule)(?r name tree-next-rule)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'N107 '?x00 :verbose t)
))

(! (r new-add-chains) nil)
(dolist (x (! ((! (r get-chains)) as-list))) (print x))
(dolist (root (! (r get-root-vars))) (print (list root (! (r subst-match) g 'n1837 root))))

;; fft-rule non-det

(let ((rn (! (g query) '((?r type rule)(?r name fft-rule)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'x '?x :verbose t)
))

;; even-next non-det

(let ((rn (! (g query) '((?r type rule)(?r name even-next)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'x '?a :verbose nil)))

;; tree-top-rule -- not var-connected?

(let ((rn (! (g query) '((?r type rule)(?r name tree-top-rule)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'global-node nil :verbose nil)))


;;	fft-rule-delta2 -- need var as root

(let ((rn (! (g query) '((?r type rule)(?r name fft-rule-delta2)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'fft 'fft :verbose nil)))

;;	is-0-param-xrule -- need var as root

(let ((rn (! (g query) '((?r type rule)(?r name is-0-param-xrule)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'is 'is :verbose nil)))

;;	odd-even-weave -- need var as root

(let ((rn (! (g query) '((?r type rule)(?r name odd-even-weave)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'even 'even :verbose nil)))

;;	fft-comb-rule-zero

(let ((rn (! (g query) '((?r type rule)(?r name fft-comb-rule-zero)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'fft-comb 'fft-comb :verbose nil)))

;;	tree-top-order-rule
;;
;; This will test the presence of vars in edges, since it picks up some for some reason. Not working yet.
;;

(let ((rn (! (g query) '((?r type rule)(?r name tree-top-order-rule)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(clear-perf-stats)
	(! (r subst-match) g 'x '?p :verbose nil)))

(defun subst-match-test (rule-name obj-node root-var &key verbose)
  (let ((r (! (g query) `((?r type rule)(?r name ,rule-name)) '?r)))
	(let ((rg (! (g make-rule-graph) r)))
	  (setq rg rg)
	  (clear-perf-stats)
	  ;; (print (! (g all-matches) r obj-node))
	  (print (! (rg subst-match) g obj-node root-var :verbose verbose))
	  nil)))

;; even-new

(subst-match-test 'even-new 'x '?a :verbose t)

(let ((rn (! (g query) '((?r type rule)(?r name even-new)) '?r)))
  (let ((rg (! (g make-rule-graph) rn)))
	(setq r rg)
	(mapunion (lambda (q) (when (>= (length q) 2) (! (g get-edges-from-subqet) q))) 
			  (mapcar (lambda (q) (filter-vars q)) (! (r all-subqets))))))


;; fft-comb-rule-next non-det

(defr
  (defl while (thunk)
	(when (funcall thunk)
	  (while thunk)))
  (let ((q (! (r get-conts))))
	(while (lambda ()
			 (let ((cont (! (q pop-head))))
			   (when cont
				 (print (funcall cont)))
			   (not (null cont)))))))



(let ((edges 
	   '(
		 (m1 a m2)
		 (m2 b 42)
		 (m1 a m3)
		 (m3 b 42)
		 (m1 a m4)
		 (m4 b 42)
		 (m1 a m5)
		 (m5 b 42)
		 (m1 a m6)
		 (m6 b 42)

		 (m2 a-1 m1)
		 (m3 a-1 m1)
		 (m4 a-1 m1)
		 (m5 a-1 m1)
		 (m6 a-1 m1)

		 )
	   ))
  (setq g (make-objgraph))
  (dolist (e edges)
	(! (g add-edge) e))
  (! (g define-rule)
	'(rule
	  (name r1)
	  (pred 
	   (?x a ?y)
	   (?y b 42))
	  (add
	   (print r1 ?x ?y)
	   (done ?x ?y))))
  (! (g define-rule)
	'(rule
	  (name r2)
	  (pred 
	   (?x a-1 ?y))
	  (add
	   (print r2 ?x ?y)
	   (done ?x ?y))))
  (! (g execute-global-all-objs-loop))
  (let ((rn (! (g query) '((?r type rule)(?r name r2)) '?r)))
	(let ((rg (! (g make-rule-graph) rn)))
	  (setq r rg)
	  (clear-perf-stats)
	  (! (r subst-match) g 'm1 '?y :verbose nil))))

(let ((edges 
	   '(
		 (m1 a ?xxx)
		 (m1 b 42)
		 )
	   ))
  (setq g (make-objgraph))
  (dolist (e edges)
	(! (g add-edge) e))
  (! (g define-rule)
	'(rule
	  (name r1)
	  (pred 
	   (?x a ?y)
	   (?x b 42)
	   (?nn1 new-node sn1))
	  (add
	   (print r1 ?x ?y ?nn1)
	   (done ?x ?y))))
  (! (g execute-global-all-objs-loop))
  (let ((rn (! (g query) '((?r type rule)(?r name r1)) '?r)))
	(let ((rg (! (g make-rule-graph) rn)))
	  (setq r rg)
	  (clear-perf-stats)
	  (! (r subst-match) g 'm1 '?x :verbose nil))))

(let ()
  (! (g define-rule)
	 '(rule
	  (name rule-30-next-rule-opt)
	  (attach-to rule-30-next-rule-obj)
	  (root-var rule-30-next-rule-obj)
	  (pred
	   (rule-30-next-rule-obj xrule ?r1)
	   ;; (?r1 name ?n1)
	   (rule-30-next-rule-obj xrule ?r2)
	   ;; (?r2 name ?n2)
	   )
	  (add
	   (print rule-30-next-rule-opt ?r1 ?n1 ?r2 ?n2)
	   (?r1 del (?y rule ?r2)))))
  (let ((rn (! (g query) '((?r type rule)(?r name rule-30-next-rule-opt)) '?r)))
	(let ((rg (! (g make-rule-graph) rn)))
	  (setq r rg)
	  (clear-perf-stats)
	  (! (r subst-match) g 'rule-30-next-rule-obj 'rule-30-next-rule-obj :verbose t))))
  
;; end string-nodes?


(let ((nodes (! (g get-all-nodes))))
  (dolist (x (sort (mapcar (lambda (node)
							 (let ((qets (list (list node))))
							   (dotimes (i 1)
								 (setq qets (mapcar (lambda (qet) (! (g superqets) qet)) qets)))
							   (list node (length qets) (first qets) (second qets) (third qets))))
						   nodes)
				   (lambda (x y) (< (second x) (second y)))))
	(print x)))


(dolist (x (sort (mapcar (lambda (n) (list n (length (! (g superqets) (list n))) (first (! (g superqets) (list n))) (second (! (g superqets) (list n))))) (! (g get-all-nodes))) (lambda (x y) (< (second x) (second y))))) (print x))




(dolist (x (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n)))
  (let ((rule (first x)))
	(let ((name (second x)))
	  (let ((r (! (g make-rule-graph) rule)))
		(! (g add-edge) (list rule 'rg r))
		;; print rule node, rule name, and largest edge size
		(print (list '************* 'rule rule name (apply #'max (cons 0 (mapcar (lambda (x) (length x)) (! (r get-all-edges)))))))
		(format t "~%")
		(let ((chains (! ((! (r get-chains)) as-list))))
		  (dolist (chain chains)
			(let ((key (first chain)))
			  (format t "~a~%" key)
			  (let ((edge-pairs (second chain)))
				(dolist (edge-pair edge-pairs)
				  (format t "         ~a~%" edge-pair))))))
		(print (! (r get-all-edges)))
		(print (! (r get-edges-from-chains)))
		(print (set-equal (! (r get-all-edges))
						  (! (r get-edges-from-chains))))))))

(length (! (g get-all-edges)))
(length (! ((! (g get-subqet-map)) inputs)))
(length (! ((! (g get-subqet-map)) results)))
(length (! ((! (g get-superqet-map)) inputs)))
(length (! ((! (g get-superqet-map)) results)))


(! (g get-edges) 'x)
(! (g x-add-chain) '(N1837 IS-ELEM-OF X))
(! ((! (g get-chains)) as-list))

(with-open-file (s "xxx" :direction :output) (dolist (x (! ((! (g get-chains)) as-list))) (print x s)) nil)
(length (! (g get-all-edges)))
(length (! (g get-all-nodes)))
(length (! ((! (g get-chains)) as-list)))







(let ((nloops 15))
  (with-open-file (u "chain-info" :direction :output)
	(dotimes (i nloops)
	  (let ((filename (format nil "chain-dump-~a" i)))
		(with-open-file (s filename :direction :output)
		  (print (list '************* i))
		  (let ((n i))
			(clear-counters)
			(clear-perf-stats)
			(setq g (make-foundation))
			(! (g read-rule-file) "simple-tree.lisp")
			(! (g define-rule) `(rule
								 (name init)
								 (attach-to global-node)
								 (pred
								  (global-node rule ?r)
								  (?r name init))
								 (add
								  (print init)
								  (tree-rule x ,n))
								 (del
								  (global-node rule ?this-rule))))
			(time
			 (timer 'main
			   (lambda ()
				 (! (g execute-global-all-objs-loop)))))
			(dolist (x (! ((! (g get-chains)) as-list))) (print x s))
			(let ((e (length (! (g get-all-edges)))))
			  (let ((n (length (! (g get-all-nodes)))))
				(let ((c (length (! ((! (g get-chains)) as-list)))))
				  (format u "~a ~a ~a ~a ~a ~a~%" 
						  i
						  e n c
						  (float (/ e c))
						  (float (/ n c))))))
			(perf-stats)))))))



(setq r (! (g hget) 'N662 'rg))

(! (g query) '((?x zero)(?x tree-next ?y)))

(! (r x-expand-rule-obj-edges) g 'N1841 '?p)

plot "xxx" using 1:($3/10) with lines, '' using 1:4 with lines, '' using 1:($6/10) with points

(! (g define-rule)
	  '(rule
		(name xxx)
		(pred 
		 (?x elem ?y))
		(add
		 (print xxx ?x)
		 (?x rule
			 (rule
			  (name yyy)
			  (pred
			   (?x ?a ?z))
			  (add
			   (print yyy ?x ?z)))))))

;; nodes in pos zero and 1, get matching qets

(let ((nodes (! (g get-all-nodes))))
  (let ((r nil))
	(dolist (node nodes)
	  (let ((keep nil))
		(let ((edges (! (g get-edges) node 1)))
		  (dolist (edge edges)
			(when (= (length edge) 3)
			  (setq keep t))))
		(when keep
		  (setq r (cons node r)))))
	r))



(w "xxx" (lambda (s)
		   (setq x (! (g all-subqets)))
		   (let ((l (mapcad (lambda (q)
							  (when (= (length q) 3)
								(list q (length (! (g get-edges-from-subqet) q)))))
							x)))
			 (let ((l (sort l (lambda (x y) (> (second x) (second y))))))
			   (dolist (x l)
				 (print x s))))))


;; new-rule-format

(let ((edges 
	   '(
		 (x a y)
		 (y b 42)
		 (y c 57)
		 (r b 99)
		 )
	   ))
  (setq g (make-objgraph))
  (dolist (e edges)
	(! (g add-edge) e))

  (! (g define-rule)
	'(rule
	  (name r1)
	  (root-var ?x)
	  (pred 
	   (?x a ?y)
	   (?y b 42)
	   (?r b 99)
	   )
	  (add
	   (print r1 ?x ?y ?r)
	   (done ?x ?y)
#|
													  (?y rule ?r)
													  (?r type rule)
													  (?r name r3)
													  (?r pred ?z c ?t)
													  (?r add print r3 ?z ?t)
													  (?r add ?z d 86)
													  |#
	   (?y rule 
		   (rule
			(name r2)
			(pred
			 (?z c ?t))
			(add
			 (print r2 ?z ?t)
			 (?z d 86))))
	   )))
  ;; (! (g trace-rule) 'r1)
  (! (g execute-global-all-objs-loop)))

(let ((rules (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n))))
  (mapc (lambda (x)
		  (let ((r (first x)))
			(let ((n (second x)))
			  (let ((res (! (g get-obj-edges) r)))
				(print (list r n (mapcar (lambda (re) (length re)) res)))))))
		rules))

(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq g (make-objgraph))  
  (! (g read-rule-file) "new-copy-rule.lisp")
  (let ((rules (! (g query) '((?r type rule)(?r name copy-rule-rule)) '(?r))))
	(mapc (lambda (rule) (! (g add-edge) `(y rule ,(first rule)))) rules))
  (let ((from-rule (! (g query) '((?r type rule)(?r name rule-clause-type)) '?r)))
	(! (g add-edge) `(,from-rule copy-rule y)))
  (! (g execute-global-all-objs-loop)))



(let ((edges 
	   '(
		 (x a y)
		 (y b z)
		 (z c u)
		 )
	   ))
  (setq g (make-objgraph))
  (dolist (e edges)
	(! (g add-edge) e))

  (! (g define-rule)
	'(rule
	  (name r1)
	  (root-var ?x)
	  (pred 
	   (?x a ?y))
	  (add
	   (print r1 ?x ?y)
	   (?y rule 
		   (rule
			(name r2)
			(root-var ?y)
			(pred
			 (?y b ?z))
			(add
			 (print r2 ?y ?z)
			 (?z rule
				 (rule
				  (name r3)
				  (root-var ?z)
				  (pred
				   (?z c ?u))
				  (add
				   (print r3 ?z ?u))))))))))
  ;; (! (g trace-rule) 'r1)
  (! (g execute-global-all-objs-loop)))


(let ((edges 
	   '(
		 (x a y 1 2 3)
		 (y b z)
		 (z c u)
		 )
	   ))
  (setq g (make-objgraph))
  (dolist (e edges)
	(! (g add-edge) e))

  (! (g define-rule)
	'(rule
	  (name r1)
	  (root-var ?x)
	  (pred 
	   (?x a ?y ?*rest))
	  (add
	   (print r1 ?x ?y ?*rest))))

  ;; (! (g trace-rule) 'r1)
  (! (g execute-global-all-objs-loop)))

(let ((edges 
	   '(
		 )
	   ))
  (setq g (make-objgraph))

  (! (g read-rule-file) "new-copy-rule.lisp")

  (dolist (e edges)
	(! (g add-edge) e))

  (! (g define-rule)
	'(rule
	  (name r1)
	  (local)
	  (root-var ?x)
	  (pred 
	   (?x a ?y)
	   (?x ?y f ?z)
	   (?z b c ?*rest))
	  (add
	   (print r1 ?x ?y)
	   (?x xrule (rule
				   (name r3)
				   (pred
					(?t d ?u)
					(?t std-var-level ?l))
				   (add
					(?t e ?u)))))
	  (del
	   ;; (?x a 42)
		)
))

  (! (g define-rule)
	 '(rule
	   (name r2)
	   (attach-to global-node)
	   (pred
		(global-node local-rule-pool ?p)
		(?p lrp-rule ?r)
		(?r name r1)
		(?p lrp-rule ?copy-rule-rule)
		(?copy-rule-rule name copy-rule-rule))
	   (add
		(print r2 ?r ?copy-rule-rule)
		(r rule ?copy-rule-rule)
		(?r copy-rule r))))

  ;; (! (g trace-rule) 'r1)
  ;; (! (g trace-rule) 'copy-rule-rule)
  (! (g execute-global-all-objs-loop)))

;; end new-rule-format

;; better-root-var

(dolist (x (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n)))
  (let ((rule (first x)))
	(let ((name (second x)))
	  (let ((r (! (g make-rule-graph) rule)))
		(print (list rule name (! (r get-vars)) (length (! (r get-vars)))))))))

(let ((ris (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n))))
  (dolist (ri ris)
	(let ((r (first ri)))
	  (let ((n (second ri)))
		(print (list r n (! (g hget-all) r 'used-root-var)))))))

;; end better-root-var

#|
													  (a f d)
													  (a d b)
													  (b d c)
													  (d c e)
													  (c e a)
													  (f e a)
													  |#

(let ()
  (let ((edges 
		 '(

		   (a n f)
		   (f n d)
		   (d n a)

		   ;; (a n d)
		   (b n d)
		   (b n a)

		   ;; (b n d)
		   (d n c)
		   (c n b)

		   ;; (d n c)
		   (c n e)
		   (e n d)

		   ;; (c n e)
		   (e n a)
		   (a n c)

		   (f n e)
		   ;; (e n a)
		   ;; (a n f)

		   )))
	(setq g (make-objgraph))
	(dolist (e edges)
	  (! (g add-edge) e)))
  (let ((d (make-dumper)))
	(! (d set-graph) g)
	(! (d dump-gv-edges) "x.gv" :rules nil :emit-legend nil :attrs
	   '(
		 a b c d e f n
		 ))
	(! (d gv-to-svg) "x")))


(defr
  (defl div (x y) (if (= y 0) 0 (/ x y)))
  (let ((u (mapcar (lambda (z) 
					 (let ((clauses (! ((! (g get-rule-components) (first z)) as-list))))
					   (let ((rule-edges (mapappend (lambda (y) y) clauses)))
						 (let ((rule-edges (mapcad (lambda (x) (when (not (memq 'print x)) x)) rule-edges)))
						   (list (second z) (float (div (apply #'+ (mapcar (lambda (x) (length x)) rule-edges))
														(length rule-edges))))))))
				   (! (g query) '((?r type rule)(?r name ?n)) '(?r ?n)))))
	(dolist (x (sort u (lambda (x y) (> (second x) (second y)))))
	  (print x))))


(let ()
  (let ((edges 
		 '(
		   (a b c d)
		   (d e f g)
		   )))
	(setq g (make-objgraph))
	(dolist (e edges)
	  (! (g add-edge) e)))
  (let ((d (make-dumper)))
	(! (d set-graph) g)
	(! (d dump-gv-edges) "z.gv" :rules nil :emit-legend nil :attrs
	   '(
		 a b c d e f g
		 ))
	(! (d gv-to-svg) "z")))
