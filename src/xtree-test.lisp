




























($comment
 
(let ()
  (hcompile "xtree-test")
  (load "xtree-test.fas"))

(let ()
  (print "*** x ***")
  (! (x print-nuclist-data))
  (print "*** x1 ***")
  (! (x1 print-nuclist-data))
  (! (x crossover) x1)
  (print "*** x ***")
  (! (x print-nuclist-data))
  (print "*** x1 ***")
  (! (x1 print-nuclist-data))
  )




)




;; Select an element from l assuming a size of n, where n can be
;; greater than the length of l. In that case assume l is padded with
;; nils. This allows us to control the probability of selecting an
;; element, ie length(l)/n.

(defun rand-select (l n)
  (let ((i (random n)))
	(nth i l)))

;; I'll use some of the terms in genetics because that's typically
;; what GAs do. Not sure how well it fits. So a gene is sequence of
;; nucleotides.

;; Make one with a list of rules which will for the rule-var
;; sequence. These are also the rules which need to be distributed

(defc gene nil (ruleset graph)
  (let ((g graph))
	(let ((nuclist nil))
	  (dolist (rule ruleset)
		(let ((rc (! (g get-rule-components) rule :no-cache t)))
		  (dolist (var (! (rc all-vars)))
			(let ((rule-to-add (rand-select ruleset (* 2 (length ruleset)))))
			  (setq nuclist (append nuclist (list (make-rule-var-add-nuc rule var rule-to-add ruleset))))))))
	  (defm get-nuclist ()
		nuclist)
	  (defm set-nuclist (new-nuclist)
		(setq nuclist new-nuclist))
	  (defm get-nuclist-data () ;; Debug only
		(mapcar (lambda (n) (! (n as-list))) nuclist))
	  (defm print-nuclist-data () ;; Debug only
		(dolist (n (get-nuclist-data))
		  (print n)))
	  (defm mutate ()
		(dolist (nuc nuclist)
		  (! (nuc mutate))))
	  (defm crossover (other-gene)
		;; The two genes should be of the same length, but in case not,
		;; use the smallest length to calc the crossover point.
		(let ((len (min (length nuclist) (length (! (other-gene get-nuclist))))))
		  (let ((crosspoint (random len)))
			(print (list 'las99 crosspoint))
			(let ((other-nuclist (! (other-gene get-nuclist))))
			  (let ((other-new-nuclist nil))
				(let ((new-nuclist nil))
				  (dotimes (i crosspoint)
					(print (list 'las101 i))
					(setq new-nuclist (append new-nuclist (list (nth i nuclist))))
					(setq other-new-nuclist (append other-new-nuclist (list (nth i other-nuclist)))))
				  (dotimes (i (- len crosspoint))
					(let ((i (+ i crosspoint)))
					  (print (list 'las102 i))
					  (setq new-nuclist (append new-nuclist (list (nth i other-nuclist))))
					  (setq other-new-nuclist (append other-new-nuclist (list (nth i nuclist))))))
				  (set-nuclist new-nuclist)
				  (! (other-gene set-nuclist) other-new-nuclist)))))))
	  ;; By clear here, we just mean remove all adds of rules
	  (defm clear-ruleset ()
		(dolist (rule ruleset)
		  (let ((rule-edges (! (g get-obj-edges) rule)))
			(dolist (clause rule-edges)
			  (when (and (eq (second clause) 'add) (eq (fourth clause) 'rule))
				(! (g rem-edge) clause))))))
	  ;; This is the "genotype to phenotype" function: place the rules
	  ;; as spec'ed by the gene into the rules in ruleset, as "add" edges.
	  (defm distribute ()
		(clear-ruleset)
		(dolist (nuc nuclist)
		  (mlet (((to-rule to-var rule-to-add) (! (nuc as-list))))
			(when rule-to-add
			  (print (list 'las42 `(,to-rule add ,to-var rule ,rule-to-add)))
			  (! (g add-edge) `(,to-rule add ,to-var rule ,rule-to-add))))))
	  nil)
	))


(defc nucleotide nil nil
  (let ()
	(defm dummy ())))	;; Need some minimal stuff for the class to be valid, else subclass croaks.

(defc rule-var-add-nuc nucleotide (to-rule to-var rule-to-add ruleset)
  (let ((ruleset-size (length ruleset)))
	(let ((mutate-prob-space-size (* 2 ruleset-size))) ;; 100
	  (defm mutate ()
		(let ((new-rule (rand-select ruleset mutate-prob-space-size)))
		  (when new-rule
			(setq rule-to-add new-rule))))
	  (defm as-list ()
		(list to-rule to-var rule-to-add)))))



(defc xtree-test base-graph nil
  (let ()
	(defm init ()
	  (base-graph-init)
	  )
	(defm run (n &key (rule-mode :local-global))
	  (add-natural-number-edges n)
	  (read-rule-file "xtree.lisp")
	  (print 'phase-one)
	  (timer 'main
		(lambda ()
		  (execute-global-all-objs-loop)))
	  (define-rule `(rule
					 (name init)
					 (attach-to global-node)
					 (pred
					  (global-node rule ?r)
					  (?r name init))
					 (add
					  (print init)
					  (tree-top top x levels ,n)
					  (queue x)
					  )
					 (del
					  (global-node rule ?this-rule))))
	  (! ((get-edge-to-trace) init-trace) self)
	  (print 'phase-two)
	  (timer 'main
		(lambda ()
		  (execute-global-all-objs-loop))))))










;; Local Variables:
;; eval: (emacs-file-locals)
;; End:


