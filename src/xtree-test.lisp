
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

(let ()
  (clear-counters)
  (clear-perf-stats)
  (clear-class-stats)
  (setq rulenames '(tree-loop-rule tree-max-rule tree-next-level0-rule tree-next-rule tree-rule tree-span-rule
                                   tree-top-order-rule tree-top-rule tree-zero-rule))
  (let ((pop-size 10))
    (let ((pop (make-array pop-size)))
      (timer 'xtree-test
        (lambda ()
          (timer 'make-orgs
            (lambda ()
              (dotimes (i pop-size)
                (let ((x (make-org rulenames)))
                  (setf (aref pop i) x)))))
          (dotimes (i pop-size)
            (let ((x (aref pop i)))
              (timer 'init-g (lambda () (! (x init-g))))
              (timer 'mutate (lambda () (! (x mutate))))
              (timer 'distribute (lambda () (! (x distribute))))
              (timer 'run (lambda () (! (x run))))
              (print (! ((aref pop i) get-stored-kpis)))))
          (defr
            (defl access (x)
              (first (second (! (x get-stored-kpis)))))
            ($comment
             (print "sorted:")
             (let ((s (sort pop (lambda (x y) (> (access x) (access y))))))
               (dotimes (i pop-size)
                 (print (! ((aref s i) get-stored-kpis))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xtree test -- prep for GA experiment

(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq rule-names '(tree-loop-rule tree-max-rule tree-next-level0-rule tree-next-rule tree-rule tree-span-rule
                     tree-top-order-rule tree-top-rule tree-zero-rule))
  (let ((n 3))
    (with-redirected-stdout (and t "treeout")
      (lambda (s)
        (setq g (make-xtree-test))
        (! ((! (g get-edge-to-trace)) init-trace) g)
        ;; (! (g trace-rule) 'tree-loop-rule)
        (time (! (g run) n)))))
  (let ((n 3))
    (with-redirected-stdout (and t "treeout1")
      (lambda (s)
        (setq g1 (make-xtree-test))
        (! ((! (g1 get-edge-to-trace)) init-trace) g1)
        ;; (! (g1 trace-rule) 'tree-loop-rule)
        (time (! (g1 run) n)))))
  (setq r (! (g hget-all-list) rule-names '((inv name))))
  (setq x (make-gene r g))
  (setq r1 (! (g1 hget-all-list) rule-names '((inv name))))
  (setq x1 (make-gene r1 g1))
  )

(let ()
  (clear-counters)
  (clear-perf-stats)
  (setq rulenames '(tree-loop-rule tree-max-rule tree-next-level0-rule tree-next-rule tree-rule tree-span-rule
                                   tree-top-order-rule tree-top-rule tree-zero-rule))
  (let ((n 3))
    (setq g (make-base-graph))
    (! (g add-natural-number-edges) n)
    (! (g read-rule-file) "xtree.lisp")
    (! (g add-edge) `(tree-top top x levels ,n))
    (let ((rys (! (g query) '((?r type rule)(?r name ?n)(?r add ?y is treeobj)) '(?r ?y))))
      (dolist (ry rys)
        (mlet (((r y) ry))
          (! (g rem-edge) `(,r add ,y is treeobj)))))
  (setq r (! (g hget-all-list) rule-names '((inv name))))
  (setq x (make-gene r g))
  (! (x distribute))
  (! (g clear-rule-components-cache))
  (let () ;; time
   (with-redirected-stdout (or nil "treeout")
                           (lambda (s)
                             (timer 'main
                               (lambda ()
                                 (! (g execute-global-all-objs-loop)))))))
  ;; (perf-stats)
  (mlet (((eff red fail) (get-kpis)))
    (let ((l (length (union (! (g query) '((?x1 next ?x2)) :edges)
                            (union (! (g query) '((?x1 tree-next ?x2)) :edges)
                                   (! (g query) '((?x1 aup ?x2)) :edges))))))
      (let ((p (/ l 33.0)))
        ;; (format t "~a~20t~a~40t~a~60t~a~80t~a~%" p eff red fail (/ (* p eff) (* red fail)))
        (when (= p 1)
          (format t "~a~20t~a~40t~a~%" p eff (* p eff))))))))

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
  (let ((g graph-for-rules))
    (let ((nuclist nil))
      (dotimes (i 5)
        (dolist (rulename rulenames)
          (let ((rule (! (g hget-inverse) rulename 'name)))
            (let ((rc (! (g get-rule-components) rule :no-cache t)))
              (dolist (var (! (rc all-vars)))
                (let ((rulename-to-add (rand-select rulenames (* 1 (length rulenames)))))
                  (let ((nuc (make-rule-var-add-nuc rulename var rulename-to-add rulenames)))
                    (setq nuclist (append nuclist (list nuc))))))))))
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
        nil)
      ;; By clear here, we just mean remove all adds of rules
      (defm clear-ruleset (g)
        (let ((rules (! (g hget-all-list) rulenames '((inv name)))))
          (dolist (rule rules)
            (let ((rule-edges (! (g get-obj-edges) rule)))
              (dolist (clause rule-edges)
                (when (and (eq (second clause) 'add) (eq (fourth clause) 'rule))
                  (! (g rem-edge) clause)))))))
      ;; This is the "genotype to phenotype" function: place the rules
      ;; as spec'ed by the gene into the rules in ruleset, as "add" edges.
      (defm distribute (g)
        (clear-ruleset g)
        (dolist (nuc nuclist)
          (mlet (((to-rulename to-var rulename-to-add) (! (nuc as-list))))
            (when rulename-to-add
              (let ((rule-to-add (! (g hget-inverse) rulename-to-add 'name)))
                (let ((to-rule (! (g hget-inverse) to-rulename 'name)))
                  (! (g add-edge) `(,to-rule add ,to-var rule ,rule-to-add))))))))
      )))

(defc nucleotide nil nil
  (let ()
    (defm dummy ())))   ;; Need some minimal stuff for the class to be valid, else subclass croaks.

(defc rule-var-add-nuc nucleotide (to-rulename to-var rulename-to-add rulenames)
  (let ((nrules (length rulenames)))
    (let ((mutate-prob-space-size (* 2 nrules))) 
      (defm mutate ()
        (timer 'nuc-mutate
          (lambda ()
            (let ((new-rulename (rand-select rulenames mutate-prob-space-size)))
              (when new-rulename
                (setq rulename-to-add new-rulename))))))
      (defm replicate ()
        (make-rule-var-add-nuc to-rulename to-var rulename-to-add rulenames))
      (defm as-list ()
        (list to-rulename to-var rulename-to-add)))))

(defc new-org nil (gene)
  (let ((n 3)
        (kpis nil)
        (g nil)
        (n-goal-edges 33.0))
    (defm get-g ()      ;; Debug
      g)
    (defm distribute ()
      (! (gene distribute) g)
      (! (g clear-rule-components-cache)))
    (defm progress ()
      (let ((l (length (union (! (g query) '((?x1 next ?x2)) :edges)
                              (union (! (g query) '((?x1 tree-next ?x2)) :edges)
                                     (! (g query) '((?x1 aup ?x2)) :edges))))))
        (/ l n-goal-edges)))
    (defm get-stored-kpis ()
      kpis)
    (defm run ()
      (clear-counters)
      (clear-perf-stats)
      (setq g (make-base-graph))
      (! (g add-natural-number-edges) n)
      (! (g read-rule-file) "xtree.lisp")
      (! (g add-edge) `(tree-top top x levels ,n))
      (! (g clear-rule-components-cache))
      (with-redirected-stdout (and nil "treeout")
                              (lambda (s)
                                (timer 'main
                                  (lambda ()
                                    (! (g execute-global-all-objs-loop))))))
      (setq kpis (list (progress) (get-kpis))))))

(defc org nil (rulenames)
  (let ((n 3)
        (kpis nil)
        (g nil)
        (gene nil)
        (n-goal-edges 33.0))
    (defm init ()
      (init-g)
      (setq gene (make-gene rulenames g)))
    (defm init-g ()
      ;; (clear-counters)
      (setq g (make-base-graph))
      (! (g add-natural-number-edges) n)
      (! (g read-rule-file) "xtree.lisp")
          (! (g add-edge) `(tree-top top x levels ,n)))
    (defm get-g ()      ;; Debug
      g)
    (defm get-gene ()   ;; Debug
      gene)
    (defm distribute ()
      (! (gene distribute) g)
      (! (g clear-rule-components-cache)))
    (defm mutate ()
      (! (gene mutate)))
    (defm progress ()
      (let ((l (length (union (! (g query) '((?x1 next ?x2)) :edges)
                              (union (! (g query) '((?x1 tree-next ?x2)) :edges)
                                     (! (g query) '((?x1 aup ?x2)) :edges))))))
        (/ l n-goal-edges)))
    (defm get-stored-kpis ()
      kpis)
    (defm run ()
      (! (g clear-rule-components-cache))
      ;; (clear-perf-stats)
      (with-redirected-stdout (and nil "treeout")
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


