
(defun f (n m)  ;; pop-size num-generations
  (defr
    (defl check-crossover (gene1 gene2)
      (when (and (= (random 10) 0)
                 gene1 gene2
                 (not (! (gene1 get-inh-mutate))) (not (! (gene2 get-inh-mutate))))
        (print (list 'las57 gene1 gene2))
        (! (gene1 crossover) gene2)))
    (defl sort-fcn (gk1 gk2)  ;; gk is a (gene, kpi-list) pair
      (defr
        (defl access (gk)
          (let ((k (second gk)))
            (let ((progress (first k)))
              (let ((eff (first (second k))))
                (list progress eff)))))
        (mlet (((p1 e1) (access gk1)))
          (mlet (((p2 e2) (access gk2)))
            (cond
             ((and (= p1 1.0)
                   (not (= p2 1.0)))
              t)
             ((and (= p1 1.0)
                   (= p2 1.0))
              (> e1 e2))
             (t
              (> p1 p2)))))))
    (let ()
      (clear-counters)
      (clear-perf-stats)
      (clear-class-stats)
      (setq rulenames '(tree-loop-rule tree-max-rule tree-next-level0-rule tree-next-rule tree-rule tree-span-rule
                                       tree-top-order-rule tree-top-rule tree-zero-rule))
      (let ((pop-size n))
        (let ((pop nil))
          (timer 'xtree-test
            (lambda ()
              (let ((proto-org (make-xtree-org)))
                (let ((proto-g (! (proto-org get-g))))
                  (timer 'make-genes
                    (lambda ()
                      (dotimes (i pop-size)
                        (let ((x (make-gene  rulenames proto-g)))
                          (push x pop)))))))
              (dotimes (i m)
                (print (list 'generation i))
                (let ((survivors nil))
                  (let ((org (make-xtree-org)))
                    (let ((prev-gene nil))
                      (dolist (gene pop)
                        (! (org init-g))
                        (check-crossover gene prev-gene)
                        (setq prev-gene gene)
                        (! (gene mutate))
                        (! (gene distribute) (! (org get-g)))
                        (! (org run))
                        (setq g (! (org get-g)))
                        (let ((k (! (org get-stored-kpis))))
                          ;; (print (list 'result-kpis k))         ;; LAS
                          (when (= (first k) 1.0)
                            (let ((s (list gene k)))
                              (push s survivors)))))))
                  (setq survivors (sort survivors #'sort-fcn))
                  ;; (setq survivors (subseq survivors 0 (round (/ (length survivors) 3))))
                  (print (list 'n-survivors (length survivors)))
                  (dolist (s survivors)
                    (! ((first s) inh-mutate) nil)
                    (print (second s)))
                  (when (= i (- m 1))
                    (mlet (((gene kpis) (first survivors)))
                      (when gene
                        (setq best-gene gene)
                        (print (list (! (gene get-nuclist-data)) kpis)))))
                  (setq pop nil)
                  (when survivors
                    (let ((lens (length survivors)))
                      (let ((did-inh-mutate nil))
                        (dotimes (i pop-size)
                          (let ((gene (first (nth (mod i (ceiling (/ lens 2))) survivors))))
                            (let ((clone (! (gene replicate))))
                              (when (not did-inh-mutate)
                                (! (clone inh-mutate) t)
                                (setq did-inh-mutate t))
                              (push clone pop)))))))))
              
              (let ()
                (setq best-org (make-xtree-org :new-n 6))
                (! (best-gene distribute) (! (best-org get-g)))
                (time (! (best-org run)))
                (setq best-g (! (best-org get-g))))
              
              )))))))
              
($comment
 
(let ()
  (hcompile "xtree-test")
  (load "xtree-test.fas"))

(f 10)

) ;; $comment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Via the rulenames and the passed graph we will get the rule-var sequence. These are also the rules which need to be
;; distributed.  The initial graph is just to extract rule info. A gene only stores by name, so we're not dependent on a
;; particular graph. Hence for operations like distribute we pass in an active graph to operate upon.

(defc gene nil (rulenames graph-for-rules)
  (let ((g graph-for-rules)
        (inh-mutate nil))
    (let ((nuclist nil))
      (dotimes (i 3) ;; 5 ;; LAS
        (dolist (rulename rulenames)
          (let ((rule (! (g hget-inverse) rulename 'name)))
            (let ((rc (! (g get-rule-components) rule :no-cache t)))
              (dolist (var (! (rc all-vars)))
                (let ((rulename-to-add (rand-select rulenames (* 1 (length rulenames)))))
                  (let ((rulename-to-del nil #| (rand-select rulenames (* 10 (length rulenames))) |# ))     ;; LAS
                    (let ((nuc (make-rule-var-nuc rulename var rulename-to-add rulename-to-del rulenames)))
                      (setq nuclist (append nuclist (list nuc)))))))))))
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
        (when (not inh-mutate)
          (dolist (nuc nuclist)
            (! (nuc mutate)))))
      (defm inh-mutate (x)
        (setq inh-mutate x))
      (defm get-inh-mutate ()
        inh-mutate)
      (defm crossover (other-gene)
        ;; The two genes should be of the same length, but in case not,
        ;; use the smallest length to calc the crossover point.
        (let ((len (min (length nuclist) (length (! (other-gene get-nuclist))))))
          (let ((crosspoint (random len)))
            (let ((other-nuclist (! (other-gene get-nuclist))))
              (let ((other-new-nuclist nil))
                (let ((new-nuclist nil))
                  (dotimes (i crosspoint)
                    (setq new-nuclist (append new-nuclist (list (nth i nuclist))))
                    (setq other-new-nuclist (append other-new-nuclist (list (nth i other-nuclist)))))
                  (dotimes (i (- len crosspoint))
                    (let ((i (+ i crosspoint)))
                      (setq new-nuclist (append new-nuclist (list (nth i other-nuclist))))
                      (setq other-new-nuclist (append other-new-nuclist (list (nth i nuclist))))))
                  (set-nuclist new-nuclist)
                  (! (other-gene set-nuclist) other-new-nuclist)))))))
      (defm replicate ()
        ;; Note replication does not carry over inh-mutate
        (let ((new-gene (make-gene nil nil)))
          (! (new-gene set-nuclist)
             (mapcar (lambda (nuc) (! (nuc replicate))) nuclist))
          new-gene))
      ;; By clear here, we just mean remove all adds of rules
      (defm clear-ruleset (g)
        (let ((rules (! (g hget-all-list) rulenames '((inv name)))))
          (dolist (rule rules)
            (let ((rule-edges (! (g get-obj-edges) rule)))
              (dolist (clause rule-edges)
                (when (and (eq (second clause) 'add) (eq (fourth clause) 'rule))
                  (! (g rem-edge) clause)))))))
      ;; This is the "genotype to phenotype" function: place the rules
      ;; as spec'ed by the gene into the rules in ruleset, as "add" or "del" edges.
      (defm distribute (g)
        (clear-ruleset g)
        (! (g clear-rule-components-cache))
        (dolist (nuc nuclist)
          (mlet (((to-rulename to-var rulename-to-add rulename-to-del) (! (nuc as-list))))
            (when rulename-to-add
              (let ((rule-to-add (! (g hget-inverse) rulename-to-add 'name)))
                (let ((to-rule (! (g hget-inverse) to-rulename 'name)))
                  (! (g add-edge) `(,to-rule add ,to-var rule ,rule-to-add)))))
            (when rulename-to-del
              (let ((rule-to-del (! (g hget-inverse) rulename-to-del 'name)))
                (let ((to-rule (! (g hget-inverse) to-rulename 'name)))
                  (! (g add-edge) `(,to-rule del ,to-var rule ,rule-to-del))))))))
      )))

(defc nucleotide nil nil
  (let ()
    (defm dummy ())))   ;; Need some minimal stuff for the class to be valid, else subclass croaks.

(defc rule-var-nuc nucleotide (to-rulename to-var rulename-to-add rulename-to-del rulenames)
  (let ((nrules (length rulenames)))
    (let ((mutate-prob-space-size (* 2 nrules))) 
      (defm mutate ()
        (timer 'nuc-mutate
          (lambda ()
            (let ((new-rulename-to-add (rand-select rulenames (* 5 mutate-prob-space-size))))
              (when new-rulename-to-add
                (setq rulename-to-add new-rulename-to-add))
              (let ((new-rulename-to-del (rand-select rulenames (* 5 mutate-prob-space-size))))
                (when new-rulename-to-del
                  (setq rulename-to-del new-rulename-to-del)))))))
      (defm replicate ()
        (make-rule-var-nuc to-rulename to-var rulename-to-add rulename-to-del rulenames))
      (defm as-list ()
        (list to-rulename to-var rulename-to-add rulename-to-del)))))

(defc org nil nil
  (let ()
    (defm dummy ())))

(defc xtree-org org (&key new-n)
  (let ((n (or new-n 4))  ;; 3
        (kpis nil)
        (g nil)
        (n-goal-edges 72.0)) ;; 33.0
    (defm get-g ()      ;; Debug
      g)
    (defm init ()
      (init-g))
    (defm init-g ()
      (clear-counters)
      (clear-perf-stats)
      (setq g (make-base-graph))
      (! (g add-natural-number-edges) n)
      (! (g read-rule-file) "xtree.lisp")
      (! (g add-edge) `(tree-top top x levels ,n))
      (dolist (rule (! (g query) '((?r type rule)) '(?r))) ;; All rules get a "del ?this-obj rule ?this-rule"
        (! (g add-edge) `(,rule del ?this-obj rule ?this-rule))))
    (defm progress ()
      (let ((l (length (union (! (g query) '((?x1 next ?x2)) :edges)
                              (union (! (g query) '((?x1 tree-next ?x2)) :edges)
                                     (! (g query) '((?x1 aup ?x2)) :edges))))))
        (/ l n-goal-edges)))
    (defm get-stored-kpis ()
      kpis)
    (defm run ()
      (! (g clear-rule-components-cache))
      (with-redirected-stdout (and t "treeout")
                              (lambda (s)
                                (timer 'main
                                  (lambda ()
                                    (! (g execute-global-all-objs-loop))))))
      (setq kpis (list (progress) (get-kpis))))))

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
;; eval: (setq-local indent-tabs-mode nil)
;; End:


