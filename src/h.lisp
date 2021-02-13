
(defc graph nil nil
  (let ((size 63))   ;; 1021 !!!!!!!!!!!!!!!!!!!!!!!
	(let (
		  (nodelist (make-sur-map :input-size size :res-size 17))
		  (edgelist (make-hash-table :test #'equal :size size))
		  (nodeposlist (make-hash-table :test #'equal :size size))

		  (subqet-map (make-sur-map :res-size 17))
		  (superqet-map (make-sur-map :res-size 17))

		  (chains (make-sur-map :res-size 17))

		  (qu (make-qet-utils))
		  )

	  (defm add-edge (edge)
		(let ((e (gethash edge edgelist)))
		  (when (null e)
			(setq e edge)
			(setf (gethash edge edgelist) edge)
			(let ((i 0))
			  (dolist (n e)
				(add-node n i e)
				(setq i (+ i 1))))
			(add-subqets e))
			;; (! (self add-chain) e)		;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		  e))

	  (defm edge-exists (edge)
		(and (gethash edge edgelist) t))

	  (defm add-node (node pos edge)
		(let ()
		  (! (nodelist insert) node edge)
		  (let ((pos-hash (gethash node nodeposlist)))
			(when (null pos-hash)
			  (setq pos-hash (make-hash-table :test #'equal))
			  (setf (gethash node nodeposlist) pos-hash))
			(let ((pos-edge-hash (gethash pos pos-hash)))
			  (when (null pos-edge-hash)
				(setq pos-edge-hash (make-hash-table :test #'equal))
				(setf (gethash pos pos-hash) pos-edge-hash))
			  (setf (gethash edge pos-edge-hash) edge)))
		  node))

	  (defm get-edges (node &optional pos)
		(defr
		  (defl g< (x y)
			(string< (format nil "~a" x) (format nil "~a" y)))
		  (if pos
			  (if nil ;; !!!!!!!!!!!!!!!!!!!
				  (get-edges-from-subqet-pos (list node) pos) ;; This slow way is for experiment
				  (let ((pos-hash (gethash node nodeposlist)))
					(when pos-hash
					  (let ((edgehash (gethash pos pos-hash)))
						(when edgehash
						  (let ((r nil))
							(maphash (lambda (k v)
									   (setq r (cons v r)))
									 edgehash)
							r))))))
			  (let ((r1 (! (nodelist lookup) node)))
				;; (let ((r2 (sort r1 #'g<)))
				;; r2))
				r1)
			  )))


	  ;; This is used with the cross-intersect evaluation model, which
	  ;; is slow but has nice theory. 
	  ;;
	  ;; Scans up the tree of superqets of subqet and returns all such
	  ;; superqets which are also edges

	  (defm get-edges-from-subqet (subqet)
		(timer 'get-edges-from-subqet
		  (lambda ()
			(let ((map (make-hash-table :test #'equal :size 256)))
			  (defr
				(defl g (qet)
				  (if (edge-exists qet)
					  (setf (gethash qet map) qet)
					  (let ((sup (superqets qet)))
						(dolist (qet sup)
						  (g qet)))))
				(g subqet))
			  (let ((edges (hash-table-value-to-list map)))
				edges)))))

	  ;; Does same scan as get-edges-from-subqet but just returns size
	  ;; of resulting hash table
	  ;;
	  ;; Note this can be optimized by keeping a running count when
	  ;; inserting qets. Used in scan-and-subst
	  
	  (defm count-edges-from-subqet (subqet)
		(timer 'count-edges-from-subqet
		  (lambda ()
			(let ((map (make-hash-table :test #'equal :size 256)))
			  (defr
				(defl g (qet)
				  (if (edge-exists qet)
					  (setf (gethash qet map) qet)
					  (let ((sup (superqets qet)))
						(dolist (qet sup)
						  (g qet)))))
				(g subqet))
			  (hash-table-count map)))))

	  ;; subqet is assumed singleton for now
	  ;;
	  ;; This is a way to get edges with a node at a given pos, without
	  ;; extra tables, i.e., we use the subqet lattice. Very slow, and
	  ;; probably has a pathology.

	  (defm get-edges-from-subqet-pos (subqet pos)
		(timer 'get-edges-from-subqet-pos
		  (lambda ()
			(let ((map (make-hash-table :test #'equal :size 256)))
			  (defr
				(defl g (qet)
				  (when (and (edge-exists qet)
							 (equal (first subqet) (nth pos qet)))
					(setf (gethash qet map) qet))
				  (let ((sup (superqets qet)))
					(if (null sup)
						nil
						(dolist (qet sup)
						  (when (or (>= pos (length qet))
									(equal (first subqet) (nth pos qet)))
							(g qet))))))
				(g subqet))
			  (let ((edges (hash-table-value-to-list map)))
				edges)))))

	  (defm rem-edge (edge)
		(when (edge-exists edge)
		  (rem-subqets edge)
		  (remhash edge edgelist)
		  (let ((pos 0))
			(dolist (node edge)
			  (! (nodelist remove-res) node edge)
			  (let ((pos-hash (gethash node nodeposlist)))
				(when pos-hash
				  (let ((edge-hash (gethash pos pos-hash)))
					(when edge-hash
					  (remhash edge edge-hash)))))
			  (setq pos (+ pos 1)))))
		nil)

	  (defm rem-edges (node) ;; Remove all edges containing node
		(let ((edges (get-edges node)))
		  (dolist (edge edges)
			(rem-edge edge))
		  nil))

	  (defm get-nodepos-dist ()
		(let ((maxpos nil))
		  (maphash (lambda (node poshash)
					 (maphash (lambda (pos edgehash)
								(if (null maxpos)
									(setq maxpos pos)
									(setq maxpos (max pos maxpos))))
							  poshash))
				   nodeposlist)
		  (let ((l nil)
				(tot (make-array (+ maxpos 1) :initial-element 0)))
			(maphash (lambda (node poshash)
					   (let ((a (make-array (+ maxpos 1) :initial-element 0)))
						 (maphash (lambda (pos edgehash)
									(setf (aref a pos) (hash-table-count edgehash)))
								  poshash)
						 (dotimes (i (+ maxpos 1))
						   (setf (aref tot i) (+ (aref tot i) (aref a i))))
						 (setq l (cons (list node a) l))))
					 nodeposlist)
			(append l (list tot)))))

	  ;; Returns hash table: node -> poslist

	(defm nodepos (edges)
	  (let ((node-table (make-hash-table :test #'equal)))
		(dolist (edge edges)
		  (let ((i 0))
			(dolist (node edge)
			  (setf (gethash node node-table) (dedup-list (cons i (gethash node node-table))))
			  (setq i (+ i 1)))))
		node-table))

	(defm nodes (edges)
	  (let ((node-table (make-hash-table :test #'equal)))
		(dolist (edge edges)
		  (dolist (node edge)
			(setf (gethash node node-table) node)))
		(let ((r nil))
		  (maphash (lambda (k v)
					 (setq r (cons v r)))
				   node-table)
		  r)))

	;; ********** Subqet Methods ********** 
	;;
	;; These subqet methods access the lattice structure. Within a
	;; graph a qet is distiguished from an edge.  Edges are qets,
	;; but not all qets are edges.

	;; Public:

	(defm subqets (qet)
	  (! (subqet-map lookup) qet))

	(defm superqets (qet)
	  (! (superqet-map lookup) qet))

	(defm superqet (qet)
	  (! (superqet-map lookup-one) qet))

	;; I don't normally use optional args like this, but I wanted to
	;; retain compat with some earlier uses (which are probably dead
	;; anyway).
	;;
	;; With no arg, returns all the subqets, else just the ones below the passed qet

	(defm all-subqets (&optional qet)
	  (if (null qet)
		  (hunion (! (superqet-map inputs))
				  (! (superqet-map results)))
		  (let ((r nil))
			(breadth qet
					 (lambda (qet level)
					   (setq r (cons qet r))
					   (subqets qet)))
			r)))

	(defm qet-exists (qet)
	  (or (edge-exists qet)
		  (and (superqets qet) t)))

	;; Get all qets (including edges)

	(defm all-qets ()
	  (hunion (all-subqets) (get-all-edges)))

	;; predicate-fcn == (lambda (edge) ...) => Boolean. 
	;; Returns a new graph with the object graph filter via thei predicate fcn

	(defm graph-filter (predicate-fcn)
	  (let ((r (make-objgraph)))
		(dolist (e (get-all-edges))
		  (when (funcall predicate-fcn e)
			(! (r add-edge) e)))
		r))

	(defm make-qet-graph ()		;; All qets as edges
	  (let ((g (make-objgraph)))
		(dolist (qet (all-qets))
		  (! (g add-edge) qet))
		g))

	;; For graphical purposes only at this point. Build a "Hasse
	;; Diagram" of the ASC formed by the edges of the graph and all
	;; its subqets.

	;; String nodes to dump, with an "up" relation

	(defm make-hasse-graph (&key (levels-to-omit '(0)))	;; list of levels to omit, with zero as the bottom "T" level (omit by default)
	  (let ((gd (make-objgraph)))
		(defr
		  (defl get-singleton-qets ()
			(let ((qets (all-qets)))
			  (let ((r nil))
				(dolist (qet qets)
				  (when (= (length qet) 1)
					(setq r (cons qet r))))
				r)))
		  (defl get-qet-string (qet)
			(format nil "~a" qet))
		  (defl walk (qet level)
			(if (null qet)
				nil
				(let ((superqets (if (eq qet t) (get-singleton-qets) (superqets qet))))
				  (dolist (superqet superqets)
					(when (not (member level levels-to-omit))
					  (! (gd add-edge) `(,(get-qet-string qet) up ,(get-qet-string superqet)))))
				  (dolist (superqet superqets)
					(walk superqet (+ level 1))))))
		  (let ()
			(walk t 0)
			gd))))

	(defm make-simplicial-complex-graph (&key (nodes-to-subst-fcn (lambda (n) (not (or (is-new-obj-node n)
																					   (is-var-name n))))))
	  (let ((r (make-objgraph)))
		(let ((i 0))
		  (dolist (e (get-all-edges))
			(let ((e (mapcar 
					  (lambda (n)
						(if (funcall nodes-to-subst-fcn n)
							(let ()
							  (setq i (+ i 1))
							  (symcat n i))
							n))
					  e)))
			  (when (= (length e) 3)
				(! (r add-edge) `(,(first e) r ,(second e)))
				(! (r add-edge) `(,(second e) r ,(third e)))
				(! (r add-edge) `(,(third e) r ,(first e)))))))
		r))

	;; Private:

	(defm get-subqet-map ()			;; Debug only
	  subqet-map)

	(defm get-superqet-map ()		;; Debug only
	  superqet-map)

	(defm add-subqets (edge)
	  (defr
		(defl add-layer (qets sub-qets len)
		  (when (not (null sub-qets))
			(when (not (null qets))
			  (dolist (qet qets)
				(dolist (sub-qet sub-qets)
				  (when (! (qu is-subqet) sub-qet qet)
					(add-subqet sub-qet qet)))))
			(add-layer sub-qets (! (qu subqets-length-n) edge len) (- len 1))))
		(add-layer nil (list edge) (- (length edge) 1)) ;; 2 !!!!!!!
		nil))

	(defm add-subqet (sub super) ;; Add mapping in both directions
	  (! (subqet-map insert) super sub)
	  (! (superqet-map insert) sub super)
	  nil)

	(defm rem-subqet (qet)
	  (dolist (s (subqets qet))
		(! (superqet-map remove-res) s qet))
	  (dolist (s (superqets qet))
		(! (subqet-map remove-res) s qet))
	  (! (subqet-map remove) qet)
	  (! (superqet-map remove) qet)
	  nil)

	(defm rem-subqets (edge)
	  (defr
		(defl rem-qet (qet)
		  (when (not (null qet))
			(let ((subqets (subqets qet)))
			  (rem-subqet qet)
			  (dolist (subqet subqets)
				(when (= (length (superqets subqet)) 0)
				  (rem-qet subqet))))
			nil))
		(rem-qet edge)
		nil))

	;; ********** End Subqet Methods ********** 

	(defm add-chain (edge)
	  (timer 'graph-add-chain
		(lambda ()
		  (when (= (length edge) 3)
			(let ((n0 (first edge))
				  (n2 (third edge)))
			  (let ((in-edges (get-edges n0 2))
					(out-edges (get-edges n2 0)))
				(when in-edges
				  (dolist (in-edge in-edges)
					(let ((key (list (second in-edge) (second edge))))
					  (let ((chain (list in-edge edge)))
						(! (chains insert) key chain)
						(! (chains insert) (list (first in-edge)) chain)
						(! (chains insert) (list (third edge)) chain)))))
				(when out-edges
				  (dolist (out-edge out-edges)
					(let ((chain (list edge out-edge)))
					  (let ((key (list (second edge) (second out-edge))))
						(! (chains insert) key chain)
						(! (chains insert) (list (first edge)) chain)
						(! (chains insert) (list (third out-edge)) chain))
					  (let ((key (list (second edge) (second out-edge) (third out-edge))))
						(! (chains insert) key chain)
						(! (chains insert) (list (first edge)) chain)
						(! (chains insert) (list (third out-edge)) chain))))))
			  (let ((two-chains (mapcan (lambda (chain)
										  (when (and (= (length chain) 2)
													 (equal n2 (first (first chain))))
											(list chain)))
										(! (chains lookup) n2))))
				(defr
				  (defl add-three-chain (e1 e2 e3)
					(let ((chain (list e1 e2 e3)))
					  (let ((n0 (first e1))
							(n1 (second e1))
							(n2 (second e2))
							(n3 (second e3))
							(n4 (third e3)))
						(let ((key (list n1 n2 n3)))
						  (! (chains insert) key chain)
						  (! (chains insert) (list n0) chain)
						  (! (chains insert) (list n4) chain))
						(let ((key (list n1 n2 n3 n4)))
						  (! (chains insert) key chain))
						(let ((key (list n0 n3 n4)))
						  (! (chains insert) key chain)))))
				  (dolist (two-chain two-chains)
					(let ((e1 edge)
						  (e2 (first two-chain))
						  (e3 (second two-chain)))
					  (add-three-chain e1 e2 e3)
					  ;; (add-three-chain e2 e3 e1)
					  ))))))
		  nil)))



	(defm test-code ()
	
	  ;; We have added two-chains; now scan them to add
	  ;; edge to get three-chains, if match

	  (let ((new-chains (! (new-chains results))))
		(dolist (chain new-chains)
		  (when (equal (third edge) (first (first chain)))
			(let ((e1 edge)
				  (e2 (first chain))
				  (e3 (second chain)))
			  (let ((n1 (second e1))
					(n2 (second e2))
					(n3 (second e3))
					(n4 (third e3)))
				(let ((chain (list e1 e2 e3)))
				  (let ((key (list n1 n2 n3)))
					(! (chains insert) key chain))
				  (let ((key (list n1 n2 n3 n4)))
					(! (chains insert) key chain))))))
		  (when (equal (third (second chain)) (first edge))
			(let ((e1 (first chain))
				  (e2 (second chain))
				  (e3 edge))
			  (let ((n1 (second e1))
					(n2 (second e2))
					(n3 (second e3))
					(n4 (third e3)))
				(let ((chain (list e1 e2 e3)))
				  (let ((key (list n1 n2 n3)))
					(! (chains insert) key chain))
				  (let ((key (list n1 n2 n3 n4)))
					(! (chains insert) key chain)))))))))



	(defm get-chains ()
	  chains)

	;; The edges in the graph already, along with all their subqets,
	;; form an abstract simplicial complex (ASC). From these qets,
	;; form a pseudo-topo space by taking the pairwise union of all
	;; these edges and qets, continuing until no new qets are added.
	;;
	;; It's pseudo-topo in that we only form a union if the
	;; intersection of the qets in question is non-empty, and we
	;; remove vars from the resulting qet to be inserted.
	;;
	;; Note that we continue the convention that edges are qets, but not all qets are edges

	(defm build-pseudo-topo-qets ()
	  (defr
		(defl name (q)
		  (format nil "~a" q))
		(defl qunion (q1 q2)
		  (let ((q1 (sort q1 (lambda (x y) (string< (name x) (name y))))))
			(let ((q2 (sort q2 (lambda (x y) (string< (name x) (name y))))))
			  (hunion q1 q2))))
		(let ((qets (hunion (all-subqets) (get-all-edges))))
		  (dolist (q1 qets)
			(dolist (q2 qets)
			  (when (not (eq q1 q2))
				(when (not (null (intersect q1 q2)))
				  (let ((q (filter-vars (hunion q1 q2))))
					(add-subqets q)))))))))




	(defm dump-edges (&key (sort t) (dump-fcn #'print))
	  (if (not sort)
		  (maphash (lambda (k v)
					 (let ((edge k))
					   (funcall dump-fcn edge)))
				   edgelist)
		  (let ((edges nil))
			(maphash (lambda (k v)
					   (let ((edge k))
						 (setq edges (cons edge edges))))
					 edgelist)
			(let ((first-nodes nil))
			  (mapcar (lambda (edge)
						(let ((first-node (first edge)))
						  (when (not (member first-node first-nodes :test #'equal))
							(setq first-nodes (cons first-node first-nodes)))))
					  edges)
			  (mapcar (lambda (first-node)
						(let ((first-node-edges (get-edges first-node)))
						  (mapcar (lambda (edge)
									(when (equal (first edge) first-node)
									  (funcall dump-fcn edge)))
								  first-node-edges)))
							  
					  first-nodes))))
	  nil)

	(defm dump-nodes (&key (dump-fcn #'print))
	  (maphash (lambda (k v)
				 (funcall dump-fcn v))
			   nodelist)
	  nil)

	;; For overlap testing -- see test.lisp

	(defm overlap-info (edges)
	  (let ((h (make-hash-table :test #'equal)))
		(dolist (e1 edges)
		  (dolist (e2 edges)
			(when (not (eq e1 e2))
			  (let ((i1 0))
				(dolist (n1 e1)
				  (let ((i2 0))
					(dolist (n2 e2)
					  (when (equal n1 n2)
						(let ((k (list i1 i2)))
						  (setf (gethash k h) k)))
					  (setq i2 (+ i2 1))))
				  (setq i1 (+ i1 1)))))))
		(hash-table-value-to-list h)))

	;; End overlap testing 

	(defm load-graph-file (file)
	  (with-open-file (s file :direction :input)
		(loop
		 (let ((e (read s nil nil)))
		   (if (null e)
			   (return nil)
			   (add-edge e))))
		nil))

	(defm get-all-nodes ()
	  (! (nodelist inputs)))

	(defm get-all-edges ()
	  (let ((edge-hash edgelist))
		(let ((r nil))
		  (maphash (lambda (k v)
					 (let ((edge k))
					   (setq r (cons edge r))))
				   edge-hash)
		  r)))

	;; excl-set = exclusion set (hitting set?) -- any edge which
	;; overlaps with excl-set is not used.

	(defm path (n1 n2 &key excl-set shortest trace)
	  (let ((visit-hash (make-hash-table :test #'equal))
			(cur-bnode-chain nil))
		(defr
		  (defl filter-edges (edges)
			(if (null excl-set)
				edges
				(let ((r nil))
				  (dolist (edge edges)
					(when (not (intersect edge excl-set))
					  (setq r (append r (list edge)))))
				  r)))
		  (defl get-children (bnode)
			(let ((r (cond 
					  ((is-node bnode)
					   (filter-edges (get-edges bnode)))
					  ((is-edge bnode)
					   bnode)
					  (t
					   (print 'error)
					   nil))))
			  ;; (print (list 'g bnode r))
			  r))
		  (defl b2 ()
			(block b
			  (defr
				;; bni = bnode-info = (bnode chain)
				(defl b1 (bnis)
				  ;; (print bnis)
				  (when bnis
					(b1 (mapunion (lambda (bni)
									(let ((bnode (first bni))
										  (bnode-chain (second bni)))
									  	(if (equal bnode n2)
											(let ()
											  (when trace
												(print (list 'bnc bnode-chain)))
											  (when (or (null cur-bnode-chain)
														(< (length bnode-chain) (length cur-bnode-chain)))
												(setq cur-bnode-chain bnode-chain)
												(when (not shortest)
												  (return-from b nil))
												nil))
											(when (not (gethash bnode visit-hash))
											  (setf (gethash bnode visit-hash) bnode)
											  (mapcar (lambda (bnode1) (list bnode1 (cons bnode bnode-chain)))
													  (get-children bnode))))))
								  bnis))))
				(b1 (list (list n1 nil))))))
		  (let ()
			(b2)
			(reverse (cons n2 cur-bnode-chain))))))

	;; Generalized depth-first search. Start with node, children-fcn
	;; gets list of child nodes and level. It also does whatever node
	;; processing is required. In this manner, if the node processing
	;; so dictates, the function can return nil, thus pruning the
	;; search.
	;;
	;; Returns list of all cycle heads
	;;
	;; (children-fcn node level)
	;;
	;; Example:
	;;
	;;     (! (g depth) 'init (lambda (node level) (print node) (mapcar (lambda (x) (first x)) (! (g query) `((rd 1 ,node "" "" ?n2)) '(?n2)))))

	(defm depth (node children-fcn)
	  (let ((visit-hash (make-hash-table :test #'equalp))
			(cycle-head-node-hash (make-hash-table :test #'equalp)))
		(defr
		  (defl d (node level)
			(if (or (null node)
					(gethash node visit-hash))
				(when node
				  (let ((visited-node-level (gethash node visit-hash)))
					(when (< visited-node-level level)
					  (setf (gethash node cycle-head-node-hash) node)))
				  nil)
				(let ((children (funcall children-fcn node level)))
				  (setf (gethash node visit-hash) level)
				  (dolist (child children)
					(d child (+ level 1))))))
		  (d node 0)
		  (hash-table-value-to-list cycle-head-node-hash))))

	(defm breadth (node children-fcn)
	  (let ((visit-hash (make-hash-table :test #'equalp))
			(cycle-head-node-hash (make-hash-table :test #'equalp)))
		(defr
		  (defl get-children (node level)
			(if (gethash node visit-hash)
				(let ((visited-node-level (gethash node visit-hash)))
				  (when (< visited-node-level level)
					(setf (gethash node cycle-head-node-hash) node))
				  nil)
				(let ()
				  (setf (gethash node visit-hash) level)
				  (let ((r (funcall children-fcn node level)))
					r))))
		  (defl b (nodes level)
			(when nodes
			  (b (mapunion (lambda (node) (get-children node level))
							nodes)
				 (+ level 1))))
		  (b (list node) 0)
		  (hash-table-value-to-list cycle-head-node-hash))))

	)))

(defc xgraph graph nil
  (let ((a 1))
	(defm m1 (x) (+ a x))
	(defm m2 ())
	(defm get-all-nodes ()
	  "Ha ha!!!")))

(defc rule-components nil nil
  (let ((pred-list nil)
		(pred-node-list nil)
		(del-list nil)
		(add-list nil)
		(add-node-list nil)
		(not-list nil)
		(add-main-list nil))								;; Always a subset of the adds. Just for display, so not scanned in all-nodes
	(defm set-components (pred pred-node del add add-node not add-main)
	  (setq pred-list pred)
	  (setq pred-node-list pred-node)
	  (setq del-list del)
	  (setq add-list add)
	  (setq add-node-list add-node)
	  (setq not-list not)
	  (setq add-main-list add-main))
	(defm all-nodes ()
	  (let ((h (make-hash-table :test #'equal)))
		(dolist (l (append pred-list del-list add-list not-list))
		  (dolist (n l)
			(setf (gethash n h) n)))
		(let ((r nil))
		  (maphash (lambda (k v)
					 (setq r (cons v r)))
				   h)
		  r)))
	(defm as-list ()	;; For debug only
	  (list pred-list del-list add-list not-list add-main-list))
	(defm preds ()
	  pred-list)
	(defm pred-nodes ()
	  pred-node-list)
	(defm dels ()
	  del-list)
	(defm adds ()
	  add-list)
	(defm add-nodes ()
	  add-node-list)
	(defm nots ()
	  not-list)
	(defm add-mains ()
	  add-main-list)))

(defstruct queue-entry
  (prev nil)		;; toward tail
  (next nil)		;; toward head
  (value nil))

(defc queue nil nil
  (let ((qhash  (make-hash-table :test #'equal :size 509))
		(head nil)
		(tail nil))

	;; public:

	(defm push-tail (x)
	  (let ((entry (make-queue-entry :value x)))
		(setf (gethash x qhash) (+ (or (gethash x qhash) 0) 1))
		(when tail
		  (setf (queue-entry-prev tail) entry)
		  (setf (queue-entry-next entry) tail))
		(setq tail entry)
		(unless head
		  (setq head entry)))
	  nil)

	(defm pop-tail ()
	  (when tail
		(let ((r (queue-entry-value tail)))
		  (let ((c (gethash r qhash)))
			(setq c (- c 1))
			(if (= c 0)
				(remhash r qhash)
				(setf (gethash r qhash) c))
			(remove-one tail))
		  r)))

	(defm push-head (x)
	  (let ((entry (make-queue-entry :value x)))
		(setf (gethash x qhash) (+ (or (gethash x qhash) 0) 1))
		(when head
		  (setf (queue-entry-next head) entry)
		  (setf (queue-entry-prev entry) head))
		(setq head entry)
		(unless tail
		  (setq tail entry)))
	  nil)

	(defm pop-head ()
	  (when head
		(let ((r (queue-entry-value head)))
		  (let ((c (gethash r qhash)))
			(setq c (- c 1))
			(if (= c 0)
				(remhash r qhash)
				(setf (gethash r qhash) c))
			(remove-one head))
		  r)))

	;; Removes from the queue all entries "equal" to the given entry

	(defm remove (x)
	  (let ((entry-hash (gethash x qhash)))
		(when entry-hash
		  (maphash (lambda (k v)
					 (let ((entry v))
					   (remove-one entry)))
				   entry-hash)
		  (setf (gethash x qhash) nil))
		nil))

	(defm as-list ()
	  (let ((e tail)
			(r nil))
		(block b
		  (loop 
		   (if (null e)
			   (return-from b r)
			   (let ()
				 (setq r (append r (list (queue-entry-value e))))
				 (setq e (queue-entry-next e))))))
		r))

	(defm is-queued (x)
	  (let ((c (gethash x qhash)))
		(and c (> c 0) t)))

	;; private:

	(defm remove-one (entry)
	  (when entry
		(let ((prev (queue-entry-prev entry))
			  (next (queue-entry-next entry)))
		  (when prev
			(setf (queue-entry-next prev) next))
		  (when next
			(setf (queue-entry-prev next) prev))
		  (when (eq head entry)
			(setq head prev))
		  (when (eq tail entry)
			(setq tail next)))
		nil))))

;; Since nodes are universally shared among graphs, we need global seq
;; counters to assure no dups. An alternate method is to export the
;; counter functions and make an objgraph derived from
;; another. However that seems overly complex so we'll use this model
;; unless something pushes things the other way.

(let ()
  (defvar objnodeseq 0)
  (defvar newpoolnodeseq 0)
  (defun clear-counters ()
	(setq objnodeseq 0)
	(setq newpoolnodeseq 0)))

(defc std-vars nil ()
  (let ((base-vars '(?this-obj ?this-rule ?this-rule-name ?root-var)))
	(let ((var-cache base-vars))
	  (defr
		(defl cat (var-base level)
		  (if (= level 0) var-base (intern (format nil "~a-~a" (symbol-name var-base) level))))
		(defm var-base-to-var (x level)
		  (if (memq x base-vars)
			  (let ((r (cat x level)))
				(setq var-cache (union var-cache (list r)))
				r)
			  x))
		(defm base-vars ()
		  base-vars)
		(defm var-cache ()
		  var-cache)))))

(defc objgraph graph nil
  (let ((obj-queue (make-queue))
		(global-node 'global-node)
		(global-rule-pool 'global-rule-pool-node)
		(local-rule-pool 'local-rule-pool-node)
		(hget-key-rest1 (list nil nil))
		(hget-sup-rest1 (list nil nil))
		(do-obj-var nil)
		(disable-root-vars nil)
		(pred-node-to-new-edges (make-sur-map))
		(rule-node-to-new-edges (make-sur-map))
		(edge-to-pred (make-sur-map))					;; Nop by using  make-dummy-sur-map; Active using make-sur-map
		(edge-to-trace (make-sur-map))					;; Nop by using  make-dummy-sur-map; Active using make-sur-map
		(dims-list nil)
		(edge-elem-node-to-list-cache (make-sur-map))
		(local-pool-add-node nil)
		(global-pool-add-node nil)
		(std-vars (make-std-vars))
		(seqno 0)
		(rule-stats nil))
	(let ((hget-key-rest2 (rest hget-key-rest1))
		  (hget-sup-rest2 (rest hget-sup-rest1))
		  (elem-attrs '(_elem))
		  (env-triggered-table (make-env-triggered std-vars))
		  )

	  (defm init ()
		(! (env-triggered-table set-graph) self)
		(setq rule-stats (make-rule-stats self))
		(addraw global-node 'local-rule-pool local-rule-pool)
		(addraw global-node 'global-rule-pool-ref global-rule-pool)
		nil)

	  (defm get-pred-node-to-new-edges ()
		pred-node-to-new-edges)

 	  (defm get-edge-to-pred ()
 		edge-to-pred)

	  (defm get-edge-to-trace ()
		edge-to-trace)

	  (defm get-rule-node-to-new-edges ()
		rule-node-to-new-edges)

	  (defm get-dims-list ()
		dims-list)

	  (defm insert-dims ()
		(setq dims-list (cons (dimensions) dims-list)))

	  (defm get-elem-attrs ()
		elem-attrs)

	  (defm set-do-obj-var (v)
		(setq do-obj-var v))

	  (defm set-disable-root-vars (v)
		(setq disable-root-vars v))

	  (defm std-vars ()		;; For debug only
		std-vars)

	  ;; new-pool is 1 or greater. 1 means a normal new-pool node, NNx;
	  ;; greater than 1 means chain together that many. When it's time to
	  ;; allocate a real node, if the NN has any in the chain, get that
	  ;; instead of a real new NN.

	  (defm new-obj-node (&key (new-pool 0))
		(defr
		  (defl new-pool-node (new-pool)
			(if (= new-pool 0)
				nil
				(let ((node (new-obj-node-aux t)))
				  (add-edge (list node))
				  (let ((next (new-pool-node (- new-pool 1))))
					(when next
					  ;; (print (list 'n node 'next-new-node next))
					  (add-edge (list node 'next-new-node next)))
					node))))
		  (defl new-obj-node-aux (new-pool-p)
			(let ((name-root (if new-pool-p "NN" "N"))
				  (seq (if new-pool-p newpoolnodeseq objnodeseq)))
			  (let ((node (intern (concatenate 'string name-root (format nil "~a" seq)))))
				(if new-pool-p
					(setq newpoolnodeseq (+ newpoolnodeseq 1))
					(setq objnodeseq (+ objnodeseq 1)))
				node)))
		  (if (= new-pool 0)
			  (let ((node (new-obj-node-aux nil)))
				node)
			  (let ((node (new-pool-node new-pool)))
				node))))

	  (defm get-obj-edges (node)
		(let ((edges (get-edges node)))
		  (let ((r nil))
			(dolist (edge edges)
			  (when (equal node (first edge))
				(setq r (cons edge r))))
			r)))

	  ;; Add a triplet edge of the form (node attr value)

	  (defm addraw (n a v)
		(add-edge (list n a v)))

	  (defm delraw (n a v)
		(rem-edge (list n a v)))

	  (defm get-env-triggered-table ()
		env-triggered-table)

	  (defm add (node-or-edge &optional a v)
		(if (is-node node-or-edge)
			(let ((n node-or-edge))
			  (addraw n a v)
			  (! (obj-queue push-tail) n))
			(let ((e node-or-edge))
			  (let ((n (first e)))
				(add-edge e)
				(! (obj-queue push-tail) n)))))

	  (defm del (node-or-edge &optional a v)
		(if (is-node node-or-edge)
			(let ((n node-or-edge))
			  (delraw n a v)
			  (! (obj-queue push-tail) n))
			(let ((e node-or-edge))
			  (let ((n (first e)))
				(rem-edge e)
				(! (obj-queue push-tail) n)))))

	  (defm def-obj-edges (edges &key (add-rule-link t))
		(dolist (edge edges)
		  (def-obj-edge edge :add-rule-link add-rule-link)))

	  (defm def-obj-edge (edge &key (add-rule-link t))
		(add-obj-edge edge :add-rule-link add-rule-link)
		(let ((node (first edge)))
		  (dolist (node edge)
			(when (has-rules node)
			  (queue-node node)))
		  nil))

	  (defm define-obj (&key (add-rule-link t) (new-pool 0) (edges-fcn (lambda (x) x)))
		(let ((node (new-obj-node :new-pool new-pool)))
		  (when add-rule-link
			(let ((rule-link-edge `(,node global-rule-pool ,global-rule-pool)))
			  (add-edge rule-link-edge)
			  (funcall edges-fcn rule-link-edge)))
		  (let ((local-rule-link-edge `(,node local-rule-pool ,local-rule-pool)))
			(add-edge local-rule-link-edge)
			(funcall edges-fcn local-rule-link-edge))
		  node))

	  (defm add-obj-edge (edge &key (add-rule-link t))
		(add-edge edge)
		(let ((node (first edge)))
		  (when (and add-rule-link
					 (not (hget node 'global-rule-pool)))
			(addraw node 'global-rule-pool global-rule-pool))
		  (addraw node 'local-rule-pool local-rule-pool)
		  node))

	  (defm node-queued (n)
		(! (obj-queue is-queued) n))

	  (defm queue-node (n &key only-if-not-queued)
		(if (and only-if-not-queued
				 (node-queued n))
			nil
			(! (obj-queue push-tail) n)))

	  (defm get-queue () ;; debug fcn
		obj-queue)

	  (defm clear-queue ()
		(setq obj-queue (make-queue)))

	  (defm execute-queue (&key once (rule-mode :local-global))
		(block exq
		  (loop
		   ;; Interesting experiment, i.e., executing as a stack instead
		   ;; of a queue.  FFT worked, even a bit faster. Number of
		   ;; exec-all passes 2 instead of three. However seemed to be
		   ;; more dup of rule execs.
		   ;; (let ((obj (! (obj-queue pop-tail))))
		   (let ((obj (! (obj-queue pop-head))))

			 ;; Experiment -- see queue-len-graph.gnuplot. Plotted the
			 ;; queue length, then edited to prdouce a queue size
			 ;; printed with each rule test, with a spike to 1000 for
			 ;; each success. So you can see the pattern of areas
			 ;; with a high success rate, followed by stretches of
			 ;; failures. Worth formalizing better at some point.
			 ;;
			 ;; (print (list 'qlen (length (! (obj-queue as-list)))))

			 (if obj
				 (if (functionp obj)
					 (let ((r (funcall obj)))
					   (cond
						((or (null r) (eq r :done))
						 nil)
						((eq r :requeue)
						 (queue-node obj))))
					 (execute-obj obj :rule-mode rule-mode :cont
					   (lambda (m s e p)
						 (when once
						   (return-from exq nil))
						 (when m
						   (queue-node obj)))))
				 (return-from exq nil))))))

	  ;; An enum "type"
	  (defm match-status ()
		'(:failed :new-edges :no-new-edges))

	  ;; rule-mode
	  ;; :local-only						- local rules only
	  ;; :local-rule-pool-only				- local rule pool only
	  ;; :local-and-global-rule-pool-only	- local and global rule pools only
	  ;; :local-global						- local rules, then global rules  [default]
	  ;;
	  ;; Calls (cont <edge-creation-status> <match-status> <new-edges> <matched-edges>)
	  ;; 
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <match-status> == (match-status)
	  ;; <new-edges> == edges created 
	  ;; <matched-edges> == union of all matches (not split out by env)

	  (defm execute-obj (node &key (rule-mode :local-global) only-these-rules (cont (lambda (m s e p) (list m s))))
		(timer 'execute-obj
		  (lambda ()
			(let ((r nil)
				  (match-status :failed)
				  (new-edges nil)
				  (matched-edges nil))
			  (defr
				(defl m-and-e (rule node)
				  (match-and-execute-rule rule node :cont
					(lambda (m s e p)
					  (setq new-edges (hunion new-edges e))
					  (setq matched-edges (hunion matched-edges (mapunion (lambda (edges) edges) p)))
					  (when (or (and (not (eq match-status :new-edges))
									 (eq s :new-edges))
								(and (eq match-status :failed)
									 (eq s :no-new-edges)))
						(setq match-status s))
					  (when m
						(setq r t)))))
				(cond
				 (only-these-rules
				  (dolist (rule only-these-rules)
					(m-and-e rule node)))
				 ((eq rule-mode :local-rule-pool-only)
				  (let ((local-rule-pool (hget node 'local-rule-pool)))
					(when local-rule-pool
					  (let ((local-rule-pool-rules (dedup-rules (hget-all local-rule-pool 'lrp-rule))))
						(dolist (rule local-rule-pool-rules)
						  (m-and-e rule node))))))
				 ((eq rule-mode :local-and-global-rule-pool-only)
				  (let ((local-rule-pool (hget node 'local-rule-pool)))
					(when local-rule-pool
					  (let ((local-rule-pool-rules (dedup-rules (hget-all local-rule-pool 'lrp-rule))))
						(dolist (rule local-rule-pool-rules)
						  (m-and-e rule node)))))
				  (let ((global-rule-pool (hget node 'global-rule-pool)))
					(when global-rule-pool
					  (let ((global-rule-pool-rules (dedup-rules (hget-all global-rule-pool 'grp-rule))))
						(dolist (rule global-rule-pool-rules)
						  (m-and-e rule node))))))
				 ((or (eq rule-mode :local-only)
					  (eq rule-mode :local-global))
				  (let ()
					(let ((rules (hget-all node 'rule)))
					  (dolist (rule rules)
						(m-and-e rule node)))
					(when (not (eq rule-mode :local-only))
					  (let ((global-rule-pool (hget node 'global-rule-pool)))
						(when global-rule-pool
						  (let ((global-rules (dedup-rules (hget-all global-rule-pool 'grp-rule))))
							(dolist (rule global-rules)
							  (m-and-e rule node))))))))))
			  (funcall cont r match-status new-edges matched-edges)))))

	  (defm execute-all-objs (&key (rule-mode :local-global) (only-these-rules nil))
		(timer 'execute-all-objs
		  (lambda ()
			(let ((nodes (get-all-nodes))
				  (r nil))
			  (dolist (node nodes)
				(execute-obj node :rule-mode rule-mode :only-these-rules only-these-rules :cont
				  (lambda (m s e p)
					(when m
					  (setq r t)))))
			  r))))

	  ;; Repeated execute all objs, then execute the queue, until no obj runs in the all-obj scan
	  ;;
	  ;; Deprecated

	  (defm execute-all-objs-loop (&key (rule-mode :local-global))
		(block xxx
		  (let ((i 0))
			(loop
			 (let ((nodes (get-all-nodes))
				   (r nil))
			   (print `(execute-all-objs-loop execute-queue start ,i))
			   (execute-queue :local-rules-only local-rules-only)
			   (print `(execute-all-objs-loop execute-queue end ,i))
			   (dolist (node nodes)
				 (execute-obj node :rulemode rule-mode :cont
				   (lambda (m s e p)
					 (when m
					   (setq r t)))))
			   (setq i (+ i 1))
			   (when (not r)
				 (return-from xxx nil))))))
		nil)

	  ;; As above, but alternate between executing queue and executing the global-node
	  ;;
	  ;; Deprecated

	  (defm execute-global-loop (&key (rule-mode :local-global))
		(block egl
		  (let ((i 0)
				(nedges 0)
				(prev-nedges 0))
			(loop
			 (execute-queue :rule-mode rule-mode)
			 (execute-obj 'global-node :cont
			   (lambda (m s e p)
				 (setq nedges (length (get-all-edges)))
				 (when (= prev-nedges nedges)
				   (return-from egl nil))
				 (setq prev-nedges nedges))))))
		nil)

	  ;; By default, executions off the queue are local and global,
	  ;; and those in a full scan are local only.

	  ;; - local rules only
	  ;; - local rule pool only
	  ;; - local rules, then global rules
	  ;; - full-scan and queue-based
	  
	  (defm execute-global-all-objs-loop (&key (queue-rule-mode :local-global) 
											   (scan-rule-mode :local-only)
											   (all-objs-only-these-rules nil))		;; If non-nil, execute across all objs,
																					;; but only with rules in the passed list
		(block egl
		  (let ((i 0)
				(nedges 0)
				(prev-nedges 0))
			(defr
			  ;; Note!! Can't use the fcn name "log" because it is silently
			  ;; overridden by the built-in log (logarithm) fcn.
			  (defl log1 (name thunk)
				(format t "~%***~a~30t~a~35t~a" `(start ,name) i (date-time))
				(funcall thunk)
				(format t "~%***~a~30t~a~35t~a" `(end ,name) i (date-time))
				(setq i (+ i 1)))
			  (defl exec-until-no-new-edges (thunk)
				(block b
				  (let ((nedges 0)
						(prev-nedges 0))
					(loop
					 (setq nedges (length (get-all-edges)))
					 (when (= prev-nedges nedges)
					   (return-from b nil))
					 (setq prev-nedges nedges)
					 (funcall thunk))
					nil)))
			  (if all-objs-only-these-rules
				  (exec-until-no-new-edges
					  (lambda ()
						(log1 'all-objs-only-these-rules
							  (lambda ()
								(execute-all-objs :only-these-rules all-objs-only-these-rules)))))
				  (exec-until-no-new-edges
					  (lambda ()
						(exec-until-no-new-edges
							(lambda ()
							  (log1 'global-node
									(lambda ()
									  (execute-obj 'global-node :rule-mode :local-global :cont (lambda (m s e p) nil)))))) ;; scan-rule-mode
						(log1 'queue
							  (lambda ()
								(execute-queue :rule-mode queue-rule-mode)))
						(log1 'exec-all
							  (lambda ()
								(execute-all-objs :rule-mode scan-rule-mode))))))
			  nil))))

	  ;; Executes rule against each node in graph. However if rule
	  ;; contains an attach-to node, then just execute the rule on
	  ;; that node.

	  (defm execute-rule (rule &key nodes-out-fcn)
		(timer 'execute-rule
		  (lambda ()
			(let ((nodes (or (hget rule 'attach-to)
							 (get-all-nodes)))
				  (r nil))
			  (dolist (node (if (listp nodes) nodes (list nodes)))
				(execute-obj node :only-these-rules (list rule) :nodes-out-fcn nodes-out-fcn :cont
				  (lambda (m s e p)
					(when m
					  (setq r t)))))
			  r))))

	  (defm execute-rule-on-objs (rule nodes &key cont)
		(let ((nodes (or (hunion nodes (list (hget rule 'attach-to)))
						 (get-all-nodes)
						 ))
			  (r nil)
			  (nodes-out nil)
			  (match-status :failed))
		  (dolist (node (if (listp nodes) nodes (list nodes)))
			(let ((exec-obj-nodes-out nil))
			  (execute-obj node :only-these-rules (list rule) :cont
				(lambda (m s e p)
				  (setq exec-obj-nodes-out n)
				  (when (or (and (not (eq match-status :new-edges))
								 (eq s :new-edges))
							(and (eq match-status :failed)
								 (eq s :no-new-edges)))
					(setq match-status s))
				  (when m
					(setq nodes-out (hunion nodes-out exec-obj-nodes-out))
					(setq r t))))))
		  (funcall cont r match-status nodes-out)))

	  (defm all-matches-on-edges (rule-node obj-edges)
		(let ((rule-edges (! ((get-rule-components rule-node) preds))))
		  (let ((envlist (match-pat-obj-edge-lists rule-edges obj-edges nil nil rule-node))) ;; !!!!!!! Need to dedup this? 
			(let ((std-var-level (or (hget rule-node 'std-var-level) 0)))
			  (let ((std-bindings `((,(! (std-vars var-base-to-var) '?this-rule std-var-level) ,rule-node)
									(,(! (std-vars var-base-to-var) '?this-rule-name std-var-level) ,(hget rule-node 'name))
									(,(! (std-vars var-base-to-var) '?root-var std-var-level) nil) ;; This is a rootless match, so this and the obj are nil
									(,(! (std-vars var-base-to-var) '?this-obj std-var-level) nil))))
				(let ((envlist (mapcar (lambda (env) (append std-bindings env))
									   envlist)))
				  envlist))))))

	  ;; Extensions to query are upward-compat, to avoid changing
	  ;; tests, etc. Hence we retain the old query behavior of
	  ;; presuming uniqueness and returning a single value if the
	  ;; vardesc is atomic, i.e., a single var, and that a null
	  ;; vardesc means return all the envs.
	  ;;
	  ;; :not (clause ...) => Adds negated clauses to query (which
	  ;;						means first search with the main clauses, and match those
	  ;;						results with the not clauses, throwing out those edges which
	  ;;						match any of them).
	  ;;
	  ;; vardesc is single var => value
	  ;; vardesc is list of vars => list of bound value lists, each bound value list in the order of the given vars
	  ;; vardesc is null => list of all envs
	  ;; vardesc is :edges => list of all matched edges, i.e., envs resolved

	  (defm query (clauses &optional vardesc &key not rule-trace)
		(let ((envslist (query1 clauses not rule-trace)))
		  (if (is-var-name vardesc)
			  (env-lookup vardesc (first (first envslist)) :idempotent nil)
			  (let ((envs (dedup-list (mapcan (lambda (envs)
												(mapcar (lambda (env)
														  (env-prune env (! (std-vars base-vars))))
														envs))
											  envslist))))
				(if (eq vardesc :edges)
					(matched-edges-union clauses envs)
					(if vardesc
						(dedup-list (mapcar (lambda (env)
											  (mapcar (lambda (var)
														(env-lookup var env :idempotent nil))
													  vardesc))
											envs))
						envs))))))

	  (defm old-query (clauses &optional vardesc &key not rule-trace)
		(let ((envslist (query1 clauses not rule-trace)))
		  (if (and vardesc (symbolp vardesc))
			  (env-lookup vardesc (first (first envslist)))
			  (let ((envs (dedup-list (mapcan (lambda (envs)
												(mapcar (lambda (env)
														  (env-prune env (! (std-vars base-vars))))
														envs))
											  envslist))))
				(if vardesc
					(dedup-list (mapcar (lambda (env)
										  (mapcar (lambda (var)
													(env-lookup var env))
												  vardesc))
										envs))
					envs)))))

	  (defm query1 (clauses not-clauses rule-trace)
		(let ((all-clauses (hunion clauses not-clauses)))
		  (let ((rule (first (define-rule `(rule (pred ,@all-clauses)) :local t :add-to-local-rule-pool nil))))
			(let ((pred-edges (! ((get-rule-components rule) preds))))
			  (let ((og (make-objgraph)))
				(timer 'query-get-edges
				  (lambda ()
					(let ((obj-edges nil))
					  (dolist (pred-edge pred-edges)
						(let ((edge-set nil))
						  (dolist (node pred-edge)
							(when (not (is-var-name node))
							  (setq edge-set 
									(if edge-set
										(intersect edge-set (get-edges node))
										(get-edges node)))))
						  (setq obj-edges (hunion obj-edges edge-set))))
					  (dolist (obj-edge obj-edges)
						(! (og add-edge) obj-edge)))))
				(setq gog og)
				(! (og query2) clauses not-clauses rule-trace))))))

	  (defm query2 (clauses not-clauses rule-trace)
		(let ((rule-info (define-rule `(rule (name query) (pred ,@clauses) (not ,@not-clauses)) :local t :add-to-local-rule-pool nil)))
		  (let ((rule (first rule-info)))
			(when rule-trace
			  (trace-rule 'query))
			(dedup-list (mapcar (lambda (node)
								  (let ((r (all-matches-with-not rule node)))
									r))
								(get-all-nodes))))))

	  ;; All variants of hget call the main hget-aux below

	  (defm hget (node attr)
		(hget-aux node attr nil nil nil))

	  ;; list of all values with edge label attr

	  (defm hget-all (node attr)
		(hget-aux node attr nil nil t))

	  ;; General-purpose accessor which follows a chain of attributes/inverse attributes.
	  ;;
	  ;; Give the node-list, will get all nodes which are values of
	  ;; the first attr, then next, and so on, to produce a final list
	  ;; of nodes. Attrs of the form (inv <attr>) are accessed in inverse
	  ;; mode.

	  (defm hget-all-list (node-list attr-list)
		(let ((l node-list)
			  (l1 nil))
		  (dolist (attr attr-list)
			(dolist (n l)
			  (if (and (listp attr)
					   (eq (first attr) 'inv))
				  (setq l1 (hunion l1 (hget-inverse-all n (second attr))))
				  (setq l1 (hunion l1 (hget-all n attr)))))
			(setq l l1)
			(setq l1 nil))
		  l))

	  ;; Implemented by call edge version due to bug (see above)

	  (defm hget-inverse-all (node attr)
		(mapcar (lambda (x) (first x))
				(hget-edge-inverse-all node attr)))

	  ;; Like hget, but returns whole edge. Useful when it's attr-value, but
	  ;; the value is a list
	
	  (defm hget-edge (node attr)
		(hget-aux node attr nil t nil))

	  ;; Like hget-edge, but gets first element given third and attr

	  (defm hget-edge-inverse (node attr)
		(hget-aux node attr t t nil))

	  ;; Analogous to hget-all, returns all edges with that attr

	  (defm hget-edge-all (node attr)
		(hget-aux node attr nil t t))

	  ;; Like hget-edge-inverse, but returns all edges with the given node and attr

	  (defm hget-edge-inverse-all (node attr)
		(hget-aux node attr t t t))

	  ;;
	  ;; !!!!!!!!!! **** Seems to be bug in non-edge inverse-all ****
	  ;;
	  ;; Not the fastest attribute lookup; tried other ways,
	  ;; esp. assuming only one edge was present in the superqet
	  ;; lookup. This is not correct, and we need to check for validity
	  ;; by the node and attr being left-most (or other specific loc) in
	  ;; the edge. Also below the checks for edge length and edge
	  ;; existence are probably not needed, but removing them is only
	  ;; marginally more efficient.
	  ;;
	  ;; inverse = t -> last (third) element is object
	  ;; inverse = nil -> first element is object
	  ;; edge = t -> return full edge
	  ;; edge = nil -> return value
	  ;; all = t -> return all matching values or edges
	  ;; all = nil -> return edge or value of first match

	  (defm hget-aux (node attr inverse edge all) ;; private
		(block hg
		  (when node
			(if inverse
				(let ()
				  (setf (first hget-key-rest1) attr)
				  (setf (first hget-key-rest2) node))
				(let ()
				  (setf (first hget-key-rest1) node)
				  (setf (first hget-key-rest2) attr)))
			(let ((subqet hget-key-rest1))
			  (let ((sups (superqets subqet)))
				(let ((r nil))
				  (dolist (sup sups)
					(when (and (edge-exists sup) 
							   (= (length sup) 3))
					  (let ()
						(if inverse
							(let ()
							  (setf (first hget-sup-rest1) (second sup))
							  (setf (first hget-sup-rest2) (third sup)))
							(let ()
							  (setf (first hget-sup-rest1) (first sup))
							  (setf (first hget-sup-rest2) (second sup))))
						(when (equal subqet hget-sup-rest1)
						  (if (not all)
							  (return-from hg (if edge sup (third sup)))
							  (setq r (cons (if edge sup (third sup)) r)))))))
				  r))))))

	  ;; Note commennted-out dedup of the component lists. We'd like
	  ;; to avoid the dup entries, which result from lack of detecting
	  ;; dup elem-node base representations upron creation. Not clear
	  ;; how to solve that. But we can't dedup here because we need
	  ;; 1-1 correspondence between pred-nodes and preds.

	  (defm get-rule-components (rule-node)
		(defr
		  (defl mapcard (fcn list)
			;; (dedup-list (mapcar fcn list)))			;; !!!!!!!!!!!!!!!!!
			(mapcar fcn list))
		  (let ((pred-node-list (hget-all rule-node 'pred)))
			(let ((pred-list (mapcard (lambda (node)
										(edge-elem-node-to-list node))
									  pred-node-list)))
			  (let ((del-list (mapcard (lambda (node)
										 (edge-elem-node-to-list node))
									   (hget-all rule-node 'del))))
				(let ((add-node-list (hget-all rule-node 'add)))
				  (let ((add-list (mapcard (lambda (node)
											 (edge-elem-node-to-list node))
										   add-node-list)))
					(let ((not-list (mapcard (lambda (node)
											   (edge-elem-node-to-list node))
											 (hget-all rule-node 'not))))
					  (let ((add-main-list (mapcard (lambda (node) ;; For display purposes only
													  (edge-elem-node-to-list node))
													(hget-all rule-node 'add-main))))
						(let ((rule-components (make-rule-components)))
						  (! (rule-components set-components) pred-list pred-node-list del-list add-list add-node-list not-list add-main-list)
						  rule-components))))))))))

	  (defm has-rules (node)
		(or (hget node 'global-rule-pool)
			(hget node 'rule)))

	  (defm dedup-rules (rules)
		(dedup-list rules :id-func (lambda (rule)
									 (list rule (hget rule 'name)))))

	  ;; Returns the matched edges in order, corresponding to the order of rule-pred-edges. Thus pred[i] matches match-edges[i]
	  ;;    This ordering is important for subsequent dimension stats processing

	  (defm matched-edges (rule-pred-edges env)
		(let ((r nil))
		  (dolist (rule-edge rule-pred-edges)
			(when (not (eq (second rule-edge) 'new-node))
			  (let ((e nil))
				(dolist (rule-node rule-edge)
				  (setq e (append e (list (env-lookup rule-node env)))))
				(setq r (append r (list e))))))
		  r))

	  ;; Union of all edge sets produced by matched-edges in each env in envlist

	  (defm matched-edges-union (rule-pred-edges envlist)
		(let ((r nil))
		  (dolist (env envlist)
			(setq r (hunion r (matched-edges rule-pred-edges env))))
		  r))

	  (defm matched-edges-union-graph (rule-pred-edges envlist)
		(let ((r nil))
		  (dolist (env envlist)
			(setq r (hunion r (matched-edges rule-pred-edges env))))
		  (let ((g (make-graph)))
			(dolist (edge r)
			  (! (g add-edge) edge))
			g)))


	  (defm node-name-cat (l)
		(let ((s ""))
		  (dolist (x l)
			(setq s (format nil (if (equal s "") "~a~a" "~a-~a") s x)))
		  (intern s)))

	  ;; A path from a node up to an object with a rule property is
	  ;; constrained to be due to an _elem edge being added. Hence we
	  ;; look for that pattern and only if we find it do we get the
	  ;; path.
	  ;;
	  ;; Note this may not be adequate for some cases, where we might
	  ;; have a race condition relative to rule modification, i.e., we
	  ;; may need to sense adds, preds, and rule props too. I had that
	  ;; active in the first version of the edge-based fcn, but it
	  ;; really returns a lot of nodes and slows things down
	  ;; considerably. So we'll use just this low-level trigger for
	  ;; now.

	  (defm get-rule-neighborhood (edges)
		(timer 'get-rule-neighborhood
		  (lambda ()
			(block b
			  (let ((rule-pats '((?x _elem ?n ?y))))  ;; '((?x rule ?y)(?x pred ?y)(?x add ?y)(?x _elem ?n ?y))))
				(dolist (rule-pat rule-pats)
				  (dolist (edge edges)
					(when (match-one-edge rule-pat edge nil nil nil)
					  (let ((nodes (nodes edges)))
						(let ((elem-nodes
							   (dedup-list (mapcar (lambda (edge) (first edge))
												   (mapcan (lambda (node)
															 (get-edges-from-subqet (list '_elem node)))
														   nodes)))))
						  (let ((r (hunion
									(hget-all-list elem-nodes '((inv pred) (inv rule)))
									(hget-all-list elem-nodes '((inv add) (inv rule))))))
							(return-from b r))))))))))))

	  ;; e is a function name and literal arguments. So this "eval" is
	  ;; really just an apply of the function to the passed args

	  (defm do-eval (e)
		(apply (first e) (rest e)))


 	  ;; Calls (cont <edge-creation-status> <new-edges> <matched-edges> <matched-and-new-edges-env>)
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <new-edges> == edges created, of form ((add-node edge-list) ...)
	  ;; <matched-edges> == edges matched, of form (edges ...), one set of edges for each env 
	  ;; <matched-and-new-edges-env> ==  ((matched-edges new-edges) ...)   One set per env, where matched-edges and new-edges here are just straight lists thereof
	  ;;																	Note can get overlap: Only want to add edge once; thus, pick an arbitrary env if it appears in more than one.

	  (defm add-consequent-edges (obj-node pred-edges pred-nodes add-edges add-nodes not-edges rule-node envlist &key cont)
		(timer 'add-consequent-edges		;; !!!!!! If use cps, timer will include more than this call
		  (lambda ()
			(defr
			  (defl preds-contain-new-node-var (var)
				(block b
				  (dolist (pred-edge pred-edges)
					(when (and (equal var (first pred-edge))
							   (eq (second pred-edge) 'new-node))
					  (return-from b t)))))
			  (let ((edges add-edges))
				(let ((r nil)
					  (first-edge nil)
					  (new-edges (make-sur-map))
					  (rule-name (hget rule-node 'name))
					  (matched-edges nil)
					  (matched-and-new-edges-env nil))
				  (dolist (env envlist)
					(timer 'add-consequent-edges-per-env
					  (lambda ()
						(let ((all-node-hash (make-hash-table :test #'equal))
							  (obj-to-var-hash (make-hash-table :test #'equal))
							  (root-var (env-lookup '?root-var env))		;; Not used?
							  (print-list nil)
							  (env-new-edges (make-sur-map)))
						  (block yyy
							(let ()
							  (if (! (env-triggered-table rule-has-been-triggered) rule-node env)
								  (let ()
									(when (edge-exists `(rule-trace ,rule-name))
									  (print `(rule-trace add-consequent-edges already-triggered rule ,rule-node ,rule-name obj ,obj-node)))
									(gstat 'already-env-triggered-rules (lambda (x y) (+ x y)) (lambda () 1))
									;; (when (= (length envlist) 1)
									;;   (rem-edge `(,obj-node rule ,rule-node)))
									(return-from yyy nil))
								  (let ((te nil))
									#|
									(when (not (edge-exists `(,rule-node no-triggered)))
									(setq te (! (env-triggered-table insert) rule-node env matched-edges)))
									|#
									;; (when (= (length envlist) 1)
									;;    (rem-edge `(,obj-node rule ,rule-node)))
									(dolist (edge not-edges)
									  (let ((not-edge 
											 (mapcar (lambda (node)
													   (env-lookup node env))
													 edge)))
										(when (edge-exists not-edge)
										  (return-from yyy nil))))
									(let ((new-node-hash (make-hash-table :test #'equal))
										  (trig-insert-called nil))
									  (dolists ((edge add-node) (edges add-nodes))
										(block xxx
										  (let ((new-edge nil))
											(dolist (node edge)
											  (let ((xnew-node (let ((sn (env-lookup node env)))
																 (if (and (is-var-name node)
																		  (is-scoped-new-pool-node sn)
																		  (preds-contain-new-node-var node))
																	 (let ((nn (gethash sn new-node-hash)))
																	   (when (null nn)
																		 (setq nn (setf (gethash sn new-node-hash)
																						(define-obj
																						  :new-pool 1
																						  :add-rule-link t ;;; was nil
																						  :edges-fcn (lambda (edge) 
																									   ;; (print (list 'e1 edge))
																									   (let ((add-node (cond ((eq (second edge) 'local-rule-pool) local-pool-add-node)
																															 ((eq (second edge) 'global-rule-pool) global-pool-add-node))))
																										 (! (env-new-edges insert) edge edge)
																										 (! (new-edges insert) add-node edge)))))))
																	   nn)
																	 sn))))
												(let ((new-node
													   (cond
														((and (is-new-pool-node xnew-node)
															  (hget xnew-node 'next-new-node))
														 ;; (print (list 'a xnew-node 'next-new-node (hget xnew-node 'next-new-node)))
														 (hget xnew-node 'next-new-node))
														((is-new-pool-node xnew-node)
														 (let ((xxnew-node (gethash xnew-node new-node-hash)))
														   (when (null xxnew-node)
															 (setq xxnew-node (setf (gethash xnew-node new-node-hash) 
																					(define-obj
																					  :add-rule-link t ;;; was nil
																					  :edges-fcn (lambda (edge) 
																								   ;; (print (list 'e2 edge))
																								   (let ((add-node (cond ((eq (second edge) 'local-rule-pool) local-pool-add-node)
																														 ((eq (second edge) 'global-rule-pool) global-pool-add-node))))
																									 (! (env-new-edges insert) edge edge)
																									 (! (new-edges insert) add-node edge)))))))
														   xxnew-node))
														(t xnew-node))))
												  (setq new-edge (append new-edge (list new-node))))))
											(cond
											 ((eq (first new-edge) 'eval)
											  (setq new-edge (do-eval (rest new-edge)))))
											(when (eq (first new-edge) 'name-attr)
											  (let ((rule (second new-edge))
													(name (node-name-cat (rest (rest new-edge)))))
												(setq new-edge `(,rule name ,name))))
											(when (eq (first new-edge) 'print) ;; Hack, mainly for debug -- if print is head node of an edge, print it
											  (setq print-list (append print-list (list (rest new-edge)))))
											(when (and (not (edge-exists new-edge))
													   (not (eq (first new-edge) 'print)))
											  (when (null first-edge)
												(setq first-edge new-edge))
											  (when (not trig-insert-called)
												(! (env-triggered-table insert) rule-node env)
												(setq trig-insert-called t))
											  (dolist (new-node new-edge)
												(setf (gethash new-node all-node-hash) new-node))
											  (add-edge new-edge)
											  (! (env-new-edges insert) new-edge new-edge)
											  (! (new-edges insert) add-node new-edge)))))
									  ;; (print new-node-hash)
									  )))))
						  (when (not (= (hash-table-count all-node-hash) 0))
							(setq r t)
							(dolist (p print-list)
							  (print p))
							(let ((me (matched-edges pred-edges env)))
							  (setq matched-edges (cons me matched-edges))
							  (setq matched-and-new-edges-env (cons (list me (! (env-new-edges results))) matched-and-new-edges-env)))
							(when (edge-exists `(rule-break ,rule-name))
							  (cerror "Good luck!" (format nil "Rule-break ~a" rule-name)))
							;; (add-consequent-edges-info (list env (hash-table-to-list all-node-hash)))
							)
						  (let ((nodes nil))
							(maphash (lambda (k v)
									   (let ((node v))
										 (setq nodes (cons node nodes))))
									 all-node-hash)
							(let ((all-inst-rules nil))
							  #|
							  (maphash (lambda (k v)
							  (let ((var v))
							  (setq all-inst-rules (hunion all-inst-rules (hget-all var 'ti)))))
							  obj-to-var-hash)
							  |#
							  ;;
							  ;; !!!!!!!!!!!!!!!!!!! Do we want the inst-rules stuff?
							  ;;
							  (let ((nodes (hunion (get-rule-neighborhood (! (env-new-edges results))) nodes)))
								(dolist (node nodes)
								  (let ((var (gethash node obj-to-var-hash)))
									(let ((inst-rules (and var (hget-all var 'ti))))
									  (cond
									   ((and var inst-rules (has-rules node))
										(let ()
										  ;; (setq inst-rules all-inst-rules) ;; !!!!!!!!!!!!! 
										  (print (list 'queue-inst-rules node var (mapcar (lambda (r) (hget r 'name))
																						  inst-rules)))
										  (queue-node (let ((count 0))
														(lambda ()
														  (print (list 'inst-rules count node var (mapcar (lambda (r) (hget r 'name))
																										  inst-rules)))
														  (let ((r (execute-obj node :only-these-rules inst-rules)))
															(if (or r (>= count 4))
																:done
																(let ()
																  (setq count (+ count 1))
																  :requeue))))))))
									   ((has-rules node)
										(let ()
										  (if t ;; !!!!!!!!!!!!!!!!!!!
											  (queue-node node :only-if-not-queued t)
											  (queue-node (let ((count 0))
															(lambda ()
															  (let ((r (execute-obj node)))
																;; (print (list node count r))
																(if (not r) ;; (>= count 4)          ;; (or r (>= count 2))
																	:done
																	(let ()
																	  (setq count (+ count 1))
																	  :requeue)))))))
										  )))))))))))))
				  (when nil ;; r  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
					(print (list 'a (hget rule-node 'name) matched-and-new-edges-env)))
				  (funcall cont r (! (new-edges as-list)) matched-edges matched-and-new-edges-env)))))))
	
	  ;; Delete edges after evaluating their elements as vars. 
	  ;; First evaluates not-edges and for each env if any not-edge exists, then bail from that env

	  (defm del-consequent-edges (edges not-edges envlist)
		(dolist (env envlist)
		  (block xxx
			(dolist (edge not-edges)
			  (let ((not-edge 
					 (mapcar (lambda (node)
							   (env-lookup node env))
							 edge)))
				(when (edge-exists not-edge)
				  (return-from xxx nil))))
			(dolist (edge edges)
			  (let ((del-edge 
					 (mapcar (lambda (node)
							   (env-lookup node env))
							 edge)))
				(rem-edge del-edge)
				(! (env-triggered-table removed-edge) del-edge)))))
		nil)

	  (defm trace-rule (rule-name)
		(add-edge `(rule-trace ,rule-name)))

	  (defm untrace-rule (rule-name)
		(rem-edge `(rule-trace ,rule-name)))

	  (defm break-rule (rule-name)
		(add-edge `(rule-break ,rule-name)))

	  (defm unbreak-rule (rule-name)
		(rem-edge `(rule-break ,rule-name)))

	  (defm rule-stats (&optional (sort-colno 1))
		(! (rule-stats rule-stats) sort-colno))

	  (defm update-tested (rule-node)
		(! (rule-stats update-tested) rule-node))

	  (defm update-matched (rule-node)
		(! (rule-stats update-matched) rule-node))

	  (defm update-failed (rule-node)
		(! (rule-stats update-failed) rule-node))

	  (defm update-new-edges (rule-node)
		(! (rule-stats update-new-edges) rule-node))

	  (defm update-not-new-edges (rule-node)
		(! (rule-stats update-not-new-edges) rule-node))

	  (defm update-last-expand-len (rule-node len)
		(! (rule-stats update-last-expand-len) rule-node len))

	  (defm update-new-edges-max-expand-len (rule-node)
		(! (rule-stats update-new-edges-max-expand-len) rule-node))

	  (defm get-matched (rule-node)
		(! (rule-stats get-matched) rule-node))

	  ;; Calls (cont <edge-creation-status> <match-status> <new-edges> <matched-edges>)
	  ;; 
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <match-status> == (match-status)
	  ;; <new-edges> == edges created
	  ;; <matched-edges> == of form (edges ...), one set of edges per env

	  (defm match-and-execute-rule (rule-node obj-node &key cont)
		(bool-timer 'match-and-execute-rule-true 'match-and-execute-rule-false 
		  (lambda ()
			(let ((r nil)
				  (match-status nil)
				  (new-edges nil)
				  (matched-edges nil))
			  (when (not (edge-exists `(,rule-node disabled)))
				(let ((rule-name (hget rule-node 'name)))
				  (when (or am-traced
							(edge-exists `(rule-trace ,rule-name)))
					(print `(rule-trace match-and-execute-rule tested rule ,rule-node ,rule-name obj ,obj-node)))
				  (let ((envlist (all-matches rule-node obj-node)))
					(update-tested rule-node)
					(if envlist
						(let ((rule-comps (get-rule-components rule-node)))
						  (let ((pred-list (! (rule-comps preds)))
								(pred-node-list (! (rule-comps pred-nodes)))
								(del-list (! (rule-comps dels)))
								(add-list (! (rule-comps adds)))
								(add-node-list (! (rule-comps add-nodes)))
								(not-list (! (rule-comps nots))))
							(update-matched rule-node)
							(when (edge-exists `(rule-trace ,rule-name))
							  (print `(rule-trace match-and-execute-rule matched rule ,rule-node ,rule-name envlist ,envlist)))
							(del-consequent-edges del-list not-list envlist)
							(add-consequent-edges obj-node pred-list pred-node-list add-list add-node-list not-list rule-node envlist :cont
							  (lambda (m x-new-edges x-matched-edges matched-and-new-edges-env)
								(when m
								  ;; (print (list 'm rule-name x-new-edges x-matched-edges matched-and-new-edges-env))
								  )
								(setq new-edges x-new-edges)
								(setq matched-edges x-matched-edges)
								(if m
									(let ()
									  (setq match-status :new-edges)
									  (update-new-edges rule-node)
									  (update-new-edges-max-expand-len rule-node)
									  (setq r t))
									(let ()
									  (setq match-status :no-new-edges)
									  (update-not-new-edges rule-node)))

								(when (eq match-status :new-edges)
								  (let ((rule-name (hget rule-node 'name)))
									(dolist (mne-entry matched-and-new-edges-env)
									  (let ((matched-edges (first mne-entry)))
										(let ((new-edges (second mne-entry)))
										  (dolist (matched-edge matched-edges)
											(! (edge-to-trace insert) matched-edge (list 'pred seqno rule-node rule-name)))
										  (dolist (new-edge new-edges)
											(! (edge-to-trace insert) new-edge (list 'add seqno rule-node rule-name)))))
									  (setq seqno (+ seqno 1)))))

								(when (eq match-status :new-edges)
								  (let ((matched-edges-list matched-edges))
									(let ((rule-name (hget rule-node 'name)))
									  (dolist (matched-edges matched-edges-list)
										(dolists ((matched-edge matched-pred) (matched-edges (second (filter-new-node-pred-edges pred-list))))
										  (! (edge-to-pred insert) matched-edge (list seqno rule-name matched-pred)))
										(setq seqno (+ seqno 1))))))
								))))
						(let ()
						  (setq match-status :failed)
						  (update-failed rule-node)
						  )))))
			  (funcall cont r match-status new-edges matched-edges)))))

	  (defm match-and-execute-rule-on-edges (rule-node obj-edges &key cont)
		(timer 'match-and-execute-rule-on-edges
		  (lambda ()
			(let ((r nil)
				  (match-status nil)
				  (new-edges nil)
				  (matched-edges nil)
				  (matched-and-new-edges-env nil)
				  (rule-name (hget rule-node 'name)))
			  (when (edge-exists `(rule-trace ,rule-name))
				(print `(match-and-execute-rule-on-edges tested rule ,rule-node ,rule-name)))
			  (let ((envlist (all-matches-on-edges rule-node obj-edges)))
				(update-tested rule-node)
				(if envlist
					(let ((rule-comps (get-rule-components rule-node)))
					  (let ((pred-list (! (rule-comps preds)))
							(pred-node-list (! (rule-comps pred-nodes)))
							(del-list (! (rule-comps dels)))
							(add-list (! (rule-comps adds)))
							(add-node-list (! (rule-comps add-nodes)))
							(not-list (! (rule-comps nots))))
						(update-matched rule-node)
						(when (edge-exists `(rule-trace ,rule-name))
						  (print `(match-and-execute-rule-on-edges matched ,envlist)))
						(del-consequent-edges del-list not-list envlist)
						(add-consequent-edges nil pred-list pred-node-list add-list add-node-list not-list rule-node envlist :cont
						  (lambda (m x-new-edges x-matched-edges x-matched-and-new-edges-env)
							(when m
							  ;; (print x-new-edges)
							  )
							(setq new-edges x-new-edges)
							(setq matched-edges x-matched-edges)
							(setq matched-and-new-edges-env x-matched-and-new-edges-env)
							(if m
								(let ()
								  (setq match-status :new-edges)
								  (update-new-edges rule-node)
								  (setq r t))
								(let ()
								  (setq match-status :no-new-edges)
								  (update-not-new-edges rule-node)))))))
					(let ()
					  (setq match-status :failed)
					  (update-failed rule-node))))
			  (funcall cont r match-status new-edges matched-edges matched-and-new-edges-env)))))
	  
	  (defm match-and-execute-rules (rule-nodes obj-node)
		(let ((r nil))
		  (dolist (rule-node rule-nodes)
			(match-and-execute-rule rule-node obj-node :cont
			  (lambda (x-r match-status nodes-out matched-edges)
				(setq r (or r x-r)))))
		  r))
	  
	  ;; Given a node with zero or more _elem properties, return a list
	  ;; of nodes representing those elements as an edge.
	  ;;
	  ;; Note we used to use a fixed set elem0, elem1, ... This new
	  ;; form uses a 4-node edge, (x _elem 0 y), (x _elem 1 z), etc.
	  ;; 
	  ;; Also in this new form we'll enforce the use of _elem only for
	  ;; this purpose, i.e., as rule element tags.  We thus avoid slow
	  ;; checking of subqets for proper form.
	  ;;
	  ;;				!!!!!!!! NOTE !!!!!!!!!
	  ;; Note we're using a (non-invalidating) cache here. So the
	  ;; edges involved could be modififed. It's not likely, given the
	  ;; internal use of this construction, but it's possible.

	  (defm edge-elem-node-to-list (node)
		(timer 'edge-elem-node-to-list
		  (lambda ()
			(let ((r (! (edge-elem-node-to-list-cache lookup-one) node)))
			  (or r       ;; !!!!!!!!!!!!!!!!
				  (let ((r nil))
					(let ((edges (get-edges-from-subqet (list node '_elem))))
					  (let ((l (length edges)))
						(let ((a (make-array l)))
						  (dolist (edge edges)
							(setf (aref a (third edge)) (fourth edge)))
						  (dotimes (i l)
							(let ((v (aref a i)))
							  (setq r (append r (list v))))))))
					(! (edge-elem-node-to-list-cache insert) node r)
					r))))))

	  ;; Given an edge as a list, creates a node with _elem attrs
	  ;; linked to to each element of the edge. If pushfcn is passed,
	  ;; it is called with each edge added as an _elem attr. Returns
	  ;; the node.
	  
	  (defm list-to-edge-elem-node (edge &key pushfcn ordered (new-pool 0) use-this-node)			;; Note ordered is there for compat but is ignored
		(timer 'list-to-edge-elem-node
		  (lambda ()
			(let ((node (or use-this-node (new-obj-node :new-pool new-pool)))
				  (i 0))
			  (dolist (elem-node edge)
				(let ((elem-edge (add-edge (list node '_elem i elem-node))))
				  (when pushfcn
					(funcall pushfcn elem-edge)))
				(setq i (+ i 1)))
			  node))))

	  (defm x-list-to-edge-elem-node (edge node)
		(let ((r nil))
		  (list-to-edge-elem-node edge :use-this-node node :pushfcn (lambda (x) (setq r (cons x r))))
		  r))

	  (defm expand-rule-obj-edges (rule-graph obj-node rule-nodepos root-var)
		(timer 'expand-rule-obj-edges
		  (lambda ()
			(gstat 'expand-rule-obj-edges-len (lambda (v y) (+ (length v) y))
			  (lambda ()
				(defr
				  (defl xvar-match-filter-edges (x y z) y)
				  (let ((rule-node (! (rule-graph rule-node))))
					(let ((traced (edge-exists `(rule-trace ,(hget rule-node 'name)))))
					  (let ((obj-edges-result nil))
						(do ((rule-edge-list 
							  (bipartite-breadth-rule-walk-seq rule-graph root-var) ;;;;;;;;;      !!!!!!!!!!!!!!! ;
							  (rest rule-edge-list))
							 (obj-edges
							  (expand-edges `((,obj-node)) rule-nodepos root-var rule-node)
							  (expand-edges (var-match-filter-edges (first rule-edge-list) obj-edges rule-node) rule-nodepos root-var rule-node)))
							((null rule-edge-list) nil)
							(setq obj-edges-result (hunion obj-edges-result (var-match-filter-edges (first rule-edge-list) obj-edges rule-node)))
							(when traced
							  (let ((*print-length* nil))		;; 3
								(print `(rule-trace expand-rule-obj-edges rule ,rule-node ,(hget rule-node 'name)
													rule-edges ,(log-value (first rule-edge-list)) obj-edges ,(log-value obj-edges-result))))))
						(update-last-expand-len rule-node (length obj-edges-result))
						obj-edges-result)))))))))

	  ;; Given a set of edges, gets all their nodes, and returns the edges of those nodes
	  ;; rule-node passed for tracing purposes only

	  (defm expand-edges (edges rule-nodepos root-var rule-node)
		(timer 'expand-edges
		  (lambda ()
			(let ((visit-hash (make-hash-table :test #'equal)))
			  (let ((nodes (nodes edges)))
				(dolist (node nodes)
				  (let ((child-edges (get-children node 5 rule-nodepos root-var)))
					(dolist (child-edge child-edges)
					  (setf (gethash child-edge visit-hash) child-edge)))))
			  (let ((r nil))
				(maphash (lambda (k v)
						   (setq r (cons v r)))
						 visit-hash)
				(when (edge-exists `(rule-trace ,(hget rule-node 'name)))
					(print `(rule-trace expand-edges ,(log-value r))))
				r)))))

	  (defm is-const (bnode)
		(memq bnode '(elem is-elem-of next tree-next aup adn fft copy-array-struct ref odd even weave-next level sigma
						   od ev zero max rule30val interior up top center center-up
						   )))

	  (defm old-is-const (bnode)
		(and (is-node bnode)
			 (not (is-var-name bnode))
			 (not (is-new-obj-node bnode))
			 (not (numberp bnode))
			 (not (memq bnode '(x xfft r)))))

	  (defm get-children (bnode sigma-span rule-nodepos root-var)
		(cond
		 ((is-sigma-node bnode)
		  (append (get-obj-edges bnode)
				  (sigma-edges (hget-edge-inverse bnode 'sigma) sigma-span)))
		 ((and (is-const bnode)
			   (not (eq bnode root-var)))
		  nil)
		 ((is-node bnode)
		  (let ((poslist (when rule-nodepos (gethash bnode rule-nodepos))))
			(if (null poslist) ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				(get-edges bnode)
				(let ((r nil))
				  (dolist (pos poslist)
					(setq r (hunion (get-edges bnode pos) r)))
				  r))))
		 ((is-sigma-edge bnode)
		  (list (first bnode) (third bnode)))
		 ((is-edge bnode)
		  bnode)
		 (t nil)))

	  ;; Filter based on var-based match, ordered edges. Returns obj-edges which match a pattern in rule-edges.
	  ;;
	  ;; 2/11/18: Experimented with various forms of this, including
	  ;; weeding out dups with a local or global hash table. However
	  ;; calling dedup-list is most efficient. Also added length check
	  ;; and changed match-one-edge to iterative. Overall a good
	  ;; speedup.
	  ;;
	  ;; rule-node passed for tracing only

	  (defm var-match-filter-edges (rule-edges obj-edges rule-node)
		(timer 'var-match-filter-edges
		  (lambda ()
			(defr
			  (defl match-one-edge (pat-edge obj-edge)
				(when (= (length pat-edge) (length obj-edge))
				  (block b
					(dolist (pat-node pat-edge)
					  (let ((obj-node (first obj-edge)))
						(if (not (or (is-var-name pat-node)
									 (equal pat-node obj-node)))
							(return-from b nil)
							(setq obj-edge (rest obj-edge)))))
					t)))
			  (let ((traced (edge-exists `(rule-trace ,(hget rule-node 'name)))))
				(let ((r nil))
				  (dolist (rule-edge rule-edges)
					(dolist (obj-edge obj-edges)
					  (when (match-one-edge rule-edge obj-edge)
						(setq r (cons obj-edge r)))))
				  (let ((r (dedup-list r)))
					(when traced
					  (print `(rule-trace var-match-filter-edges rule ,rule-node ,(hget rule-node 'name) 
										  obj-edges-in ,(log-value obj-edges) obj-edges-out ,(log-value r))))
					r)))))))

	  ;; If any rule edge contains no common nodes, error, since would
	  ;; likely be missing many nodes which should match. This is really
	  ;; saying that we should not have a rule edge which is *all* vars.
	  ;;
	  ;; Also if common-nodes is empty, return nil. This seems drastic, but
	  ;; otherwise there is too much searching. So a rule must have at least
	  ;; one node in commmon with a matching object

	  (defm check-rule-edges (rule-edges common-nodes)		;; all-var-mod
		(let ()					;; !!!!!!!!!!!!!!!!!
		  (if common-nodes
			  (let ((r t))
				(dolist (rule-edge rule-edges)
				  (block b
					(dolist (rule-node rule-edge)
					  (when (or (member rule-node common-nodes)
								(member rule-node '(new-node)))
						(return-from b t)))
					;;			 (print (list 'check-rule-error rule-edge common-nodes))
					(setq r nil)
					nil))
				r)
			  nil)
		  t))

	  ;; obj-node must be a member of at least one edge of
	  ;; obj-edges. Otherwise we have strayed too far from the root node for
	  ;; matching.  This heuristic could be made better by a more thorough
	  ;; contiguity check, but this will probably eliminate a lot of false
	  ;; match cases.

	  (defm check-obj-containment (obj-node obj-edges)
		(block cc
		  (dolist (edge obj-edges)
			(when (member obj-node edge :test #'equal)
			  (return-from cc t)))
		  nil))


	;; nodes is a list of nodes and edges is a list of edges.  Returns
	;; edge list which is a subset of edges which contain one or more
	;; elements of nodes

	(defm edges-with-nodes (nodes edges)		;; all-var-mod
	  (block b
		(return-from b edges)					;; !!!!!!!!!!!!!!!!!!!!!!!!!
		(let ((edge-hash (make-hash-table :test #'equal)))
		  (dolist (edge edges)
			(dolist (node nodes)
			  (when (member node edge :test #'equal)
				(setf (gethash edge edge-hash) edge))))
		  (let ((r nil))
			(maphash (lambda (k v)
					   (setq r (cons v r)))
					 edge-hash)
			r))))

	(defm sigma-edges (edge xspan)
	  (let ((span xspan))
		(let ((edge-hash (make-hash-table :test #'equal)))
		  (let ((next-edge edge))
			(dotimes (i span)
			  (when (null next-edge)
				(return nil))
			  (setf (gethash next-edge edge-hash) next-edge)
			  (setq next-edge (hget-edge (third next-edge) 'sigma)))
			(setq next-edge edge)
			(dotimes (i span)
			  (when (null next-edge)
				(return nil))
			  (setf (gethash next-edge edge-hash) next-edge)
			  (setq next-edge (hget-edge-inverse (first next-edge) 'sigma)))
			(let ((result nil))
			  (maphash (lambda (k v)
						 (setq result (cons v result)))
					   edge-hash)
			  result)))))

	(defm sigma-sum (x1 x2)
	  (+ (if (eq x1 'sigma) 1 x1)
		 (if (eq x2 'sigma) 1 x2)))

	;; Returns a non-neg integer: max number of contiguously-linked sigma edges

	(defm sigma-span (rule-node)
	  (let ((rule-comps (get-rule-components rule-node)))
		(let ((pred (! (rule-comps preds))))
		  (let ((sigma-edges (mapcad (lambda (edge)
									   (when (eq (second edge) 'sigma)
										 edge))
									 pred)))
			(let ((new-edges nil)
				  (scan-edges sigma-edges)
				  (max-sum (if sigma-edges 1 0)))
			  (let ((result
					 (loop
					  (setq scan-edges (append scan-edges new-edges))
					  (setq new-edges nil)
					  (dolist (e1 scan-edges)
						(dolist (e2 scan-edges)
						  (when (and (not (equal e1 e2))
									 (equal (third e1)
											(first e2)))
							(let ((new-edge (list (first e1) (sigma-sum (second e1) (second e2)) (third e2))))
							  (let ((sum (second new-edge)))
								(when (> sum max-sum)
								  (setq max-sum sum)))
							  (when (and
									 (not (member new-edge scan-edges :test #'equal))
									 (not (member new-edge new-edges :test #'equal)))
								(setq new-edges (cons new-edge new-edges)))))))
					  (when (null new-edges)
						(return scan-edges)))))
				(values max-sum result)))))))

	(defm all-edge-matches (rule-edge)
	  (get-edges-from-subqet (filter-vars rule-edge)))

	(defm all-matches (rule-node obj-node)		;; all-var-mod -- not right now
	  (timer 'all-matches
		(lambda ()
		  (let ((rule-graph nil))
			(let ((root-vars (get-root-vars rule-node)))
			  (let ((h (hmake-hash-table :size 7 :test (lambda (x y) (and (equal (first x) (first y))
																		  (env-equal (second x) (second y)))))))
				(let ((possible-match-fcn (possible-match-fcn rule-node obj-node)))
				  (block b
					(dolist (root-var root-vars)
					  (let ((pinfo (funcall possible-match-fcn root-var)))
						;; (print (list 'i pinfo root-var))
						(let ((poss-match (first pinfo)))
						  (let ((scan-subst-status (second pinfo)))
							(when poss-match
							  ;; (print (list 'g3 (hget rule-node 'name) scan-subst-status))
							  (if (and (not (null scan-subst-status))
									   (not (eq scan-subst-status :unknown))
									   (let ((poss-match-env scan-subst-status))
										 ;; (print (list 'g4 (hget rule-node 'name) poss-match-env))
										 (! (env-triggered-table rule-has-been-triggered) rule-node poss-match-env)))
								  (let ()
									;; (print (list 'g2 (hget rule-node 'name) scan-subst-status))
									nil)
								  (let ()
									(when (null rule-graph)
									  (setq rule-graph (make-rule-graph rule-node)))
									;; (print (list 'p (hget rule-node 'name) root-var obj-node))
									(let ((envlist
										   (if t
											   (all-matches-aux rule-graph obj-node root-var)
											   (e-all-matches-aux rule-node obj-node root-var)))) ;;; Experimental version
									  (when (null envlist) ;; This means a false positive: possible-match was true, but real match failed.
 										;; (print (list 'f (hget rule-node 'name) root-var obj-node scan-subst-status))
										;; (let ((seqno (hget 'all-rules 'new-edges)))
										;; (add-edge `(,rule-node fposs ,seqno ,root-var ,obj-node)))
										)
									  ;; (print (list 'g1 (hget rule-node 'name) root-var obj-node poss-match scan-subst-status))
									  (when (and envlist poss-match (not (eq scan-subst-status t))) ;; Catch case when subst fails 
										;; (print (list 'g (hget rule-node 'name) root-var obj-node scan-subst-status))

										)
									  ;; Case when all-matches-aux fails, yet we got a valid env back from scan-and-subst
									  (when (and (null envlist) poss-match scan-subst-status (not (eq scan-subst-status :unknown)))
										;; (print (list 'h (hget rule-node 'name) root-var obj-node (log-value scan-subst-status)))
										)
									  (when envlist
										(dolist (env envlist)
										  (let ((env-info (list root-var env)))
											(setf (hgethash env-info h) env-info)))
										(return-from b nil)))))))))))) ;; To bail or not to bail, that is the question. Today we bail.
				(when (not (= (hhash-table-count h) 0))
				  (let ((std-var-level (or (hget rule-node 'std-var-level) 0)))
					(let ((envlist nil))
					  (let ((std-bindings `((,(! (std-vars var-base-to-var) '?this-rule std-var-level) ,rule-node)
											(,(! (std-vars var-base-to-var) '?this-rule-name std-var-level) ,(hget rule-node 'name))
											(,(! (std-vars var-base-to-var) '?this-obj std-var-level) ,obj-node))))
						(hmaphash (lambda (k v)
									(let ((root-var (first v))
										  (env (second v)))
									  (setq envlist (cons (append `(,@std-bindings
																	(,(! (std-vars var-base-to-var) '?root-var std-var-level) ,root-var))
																  env)
														  envlist))))
								  h)
						envlist))))))))))

	(defm all-matches-aux (rule-graph obj-node root-var)
	  (timer 'all-matches-aux
		(lambda ()
		  (block ama
			(let ((rule-node (! (rule-graph rule-node))))
			  (let ((rule-edges (! ((get-rule-components rule-node) preds))))
				(when rule-edges ;; If no preds, then it's a data rule, i.e., just adds. Should be executed once in the special loop, not done over and over.
				  (let ((sig-span (sigma-span rule-node))
						(rule-nodepos (nodepos rule-edges))
						(rule-nodes (nodes rule-edges)))
					(let ((obj-edges (let ()
									   (expand-rule-obj-edges rule-graph obj-node rule-nodepos root-var)
									   ;; (print (! (g hget) rule-node 'name))
									   ;; (h (second (filter-new-node-pred-edges rule-edges)))
									   )
									 ))
					  (let ((obj-nodes (nodes obj-edges)))
						(let ((common-nodes (intersect rule-nodes obj-nodes)))
						  (if (not (check-rule-edges rule-edges common-nodes))
							  (return-from ama nil)
							  (let ((xobj-edges (edges-with-nodes common-nodes obj-edges)))
								(let ((obj-edges (var-match-filter-edges rule-edges xobj-edges rule-node)))
								  (if (not (check-obj-containment obj-node obj-edges))
									  (return-from ama nil)
									  (let ((envlist (match-pat-obj-edge-lists rule-edges obj-edges root-var obj-node rule-node)))
										envlist))))))))))))))))

	;; Does all-matches, but also checks the not clause. Note this
	;; logic also appears in add-consequent-edges. Used in query.

	(defm all-matches-with-not (rule-node obj-node)
	  (let ((envs (all-matches rule-node obj-node)))
		(let ((not-edges (! ((get-rule-components rule-node) nots))))
		  (if (null not-edges)
			  envs
			  (mapcan (lambda (env)
						(block b
						  (dolist (edge not-edges)
							(let ((not-edge 
								   (mapcar (lambda (node)
											 (env-lookup node env))
										   edge)))
							  (when (edge-exists not-edge)
								(return-from b nil))))
						  (list env)))
					  envs)))))

	;; Returns ((E (E1 ... En)) ...), where each E is paired with a
	;; set of edge sets which must all overlap E.
	;;
	;; Note we do not intersect the edges, just the rules -- which
	;; provides the set of overlap constraints on the sets of edges
	;; returned.
	;;
	;; rule-edges is assumed sanitized, i.e., new-node edges are removed

	(defm xcross-intersect-rule-edges (rule-edges &key debug)
	  (timer 'xcross-intersect-rule-edges
		(lambda ()
		  (if (= (length rule-edges) 1)		;; Singleton case doesn't fall through the loops naturally
			  (list (list (get-edges-from-subqet (filter-vars (first rule-edges))) nil))
			  (let ((subset-hash (make-hash-table :test #'equal))
					(subset-value-hash (make-hash-table :test #'equal)))
				(dolist (rule-edge rule-edges)
				  (let ((obj-edges (get-edges-from-subqet (filter-vars rule-edge))))
					(setq obj-edges (or (gethash obj-edges subset-value-hash)
										(setf (gethash obj-edges subset-value-hash) obj-edges)))
					(when debug
					  (print (length obj-edges)))
					(setf (gethash rule-edge subset-hash) obj-edges)))
				(when (or debug am-traced)
				  (print (list 'subset-hash (hash-table-count subset-hash)))
				  (print (list 'subset-value-hash (hash-table-count subset-value-hash)))
				  (print (list 'subset-hash (hash-table-to-list subset-hash)))
				  (print (list 'subset-value-hash (hash-table-value-to-list subset-value-hash))))
				(let ((r nil))
				  (dolist (r1 rule-edges)
					(let ((r1-obj-edges (gethash r1 subset-hash)))
					  (let ((s nil))
						(dolist (r2 rule-edges)
						  (when (and (not (eq r1 r2))
									 (intersect r1 r2))
							(let ((r2-obj-edges (gethash r2 subset-hash)))
							  (when (not (eq r1-obj-edges r2-obj-edges))
								(setq s (cons r2-obj-edges s))))))
						(setq r (cons (list r1-obj-edges s) r)))))
				  r))))))

	;; Returns a list of ((Pi (Pn ... Pm)) ...) of predicate edges Pi
	;; and their overlapping predicate edges Pn ... Pm.
	;;
	;; Each P is a predicate edge, an element of rule-edges.

	(defm xc-filter (rule-edges)
	  (timer 'xc-filter
		(lambda ()
		  (let ((r nil))
			(dolist (r1 rule-edges)
			  (let ((l nil))
				(dolist (r2 rule-edges)
				  (when (and (not (eq r1 r2))
							 (intersect r1 r2))
					(setq l (cons r2 l))))
				(setq r (cons (list r1 l) r))))
			r))))

	;; Can short-circuit inner loop since the logic is for-all e in
	;; edges there-exists an e1 in edges1
	;; 
	;; Can cons together rather than union the output list since all
	;; results come from edges, which is presumed to be dup-free

	(defm g (edges edges1)
	  (timer 'g
		(lambda ()
		  ;; (print (list 'g (length edges) (length edges1)))
		  (let ((r nil))
			(dolist (e edges)
			  (block b1
				(dolist (e1 edges1)
				  (when (intersect e e1)
					(setq r (cons e r))
					(return-from b1 nil)))))
			r))))

	(defm f (edges edgeslist)
	  (defr
		(defl f1 (edges edgeslist)
		  (if (null edgeslist)
			  edges
			  (f1 (g edges (first edgeslist)) (rest edgeslist))))
		(timer 'f
		  (lambda ()
			(f1 edges edgeslist)))))

	(defm h (rule-edges)
	  (timer 'h
		(lambda ()
		  (let ((xinfolist (xcross-intersect-rule-edges rule-edges)))
			(let ((r nil))
			  (dolist (xinfo xinfolist)
				(let ((edges (first xinfo))
					  (edgeslist (second xinfo)))
				  (setq r (eq-hunion r (f edges edgeslist)))))
			  r)))))

	;; Returns a list (new-node-pred-edges remaining-pred-edges) 
	;;
	;; Retain original order in results, for stats-gathering and debug-printing purposes

	(defm filter-new-node-pred-edges (patlist)
	  (let ((new-node-pred-edges nil)
			(remaining-pred-edges nil))
		(dolist (pat patlist)
		  (if (eq (second pat) 'new-node)
			  (setq new-node-pred-edges (append new-node-pred-edges (list pat)))
			  (setq remaining-pred-edges (append remaining-pred-edges (list pat)))))
		(list new-node-pred-edges remaining-pred-edges)))

	;; Returns a list of envs. Each env is the set of bindings for a
	;; particular patlist match.

	(defm match-pat-obj-edge-lists (patlist objlist root-var obj-node rule-node)
	  (timer 'match-pat-obj-edge-lists
		(lambda ()
		  (let ((traced (edge-exists `(rule-trace ,(hget rule-node 'name)))))
			(when traced
			  (print `(rule-trace match-pat-obj-edge-lists input rule ,rule-node ,(hget rule-node 'name) root-var ,root-var patlist ,(log-value patlist) objlist ,(log-value objlist))))
			(let ((patlist-info (filter-new-node-pred-edges patlist)))
			  (let ((new-node-pred-edges (first patlist-info))
					(patlist (second patlist-info)))
				(when nil
				  (let ((p (xcross-intersect-rule-edges patlist)))
					(when p
					  (print patlist)
					  (print p))))
				(let ((new-node-env nil))
				  (dolist (new-node-pred-edge new-node-pred-edges)
					(setq new-node-env (cons (list (first new-node-pred-edge) (third new-node-pred-edge)) new-node-env)))
				  (let ((matches (x-match-all-pat-obj-edge-lists patlist objlist root-var obj-node rule-node)))
					(when matches
					  (let ((matches
							 (if new-node-env
								 (cons (list new-node-env) matches)
								 matches)))
						(let ((envs (cross-aux2 matches :rule-trace-info (when traced (list rule-node (hget rule-node 'name))))))
						  (when traced
							(print `(rule-trace match-pat-obj-edge-lists result rule ,rule-node ,(hget rule-node 'name) envs ,envs)))
						  envs)))))))))))

	(defm patlist-equal (patlist1 patlist2)
	  (set-equal patlist1 patlist2))

	(defm any-binding-invalid (edge-env valid-bindings-hash)
	  (block abi
		(dolist (binding edge-env)
		  (unless (equal binding '(t t))
			(unless (gethash binding valid-bindings-hash)
			  (return-from abi t))))))

	;; Returns
	;;				envs-list		a list of env lists, each env list is the set of bindings from matching one pat to the objlist.
	;;								If any contained env list is nil, envs-list is nil
	;;								I.e., ((env env ..) (env env ...) ...)
	;;
	;; Validity checks:
	;;	- All bindings of an env produced by a single edge match must refer to the same obj edge
	;;  - Each pat in patlist must match at least one edge in objlist
	;; 

	(defm x-match-all-pat-obj-edge-lists (patlist
										  objlist
										  root-var				;; Ignored if null
										  obj-node				;; Ditto -- coupled
										  rule-node				;; Passed only for diag
										  )
	  (block b
		(let ((binding-hash (make-hash-table :test #'equal)) ;; binding to pat
			  (var-hash (make-hash-table :test #'equal)) ;; var-to-pat
			  (edge-env-to-pat-hash (make-hash-table :test #'equal))
			  (pat-to-edge-envs-hash (make-hash-table :test #'equal)))
		  (dolist (pat patlist)
			(dolist (node pat)
			  (when (is-var-name node)
				(setf (gethash node var-hash) (cons pat (gethash node var-hash))))))
		  (dolist (pat patlist)
			(let ((pat-matched nil))
			  (dolist (obj objlist)
				(let ((env (match-one-edge pat obj root-var obj-node rule-node)))
				  (when env
					(setq pat-matched t)
					(setf (gethash env edge-env-to-pat-hash) pat)
					(setf (gethash pat pat-to-edge-envs-hash) (cons env (gethash pat pat-to-edge-envs-hash)))
					(dolist (binding env)
					  (unless (member pat (gethash binding binding-hash) :test #'equal)
						(setf (gethash binding binding-hash) (cons pat (gethash binding binding-hash))))))))
			  (when (not pat-matched)
				(return-from b nil))))
		  (let ((valid-bindings-hash (make-hash-table :test #'equal)))
			(maphash (lambda (k v)
					   (let ((binding k)
							 (binding-patlist v))
						 (let ((var (first binding)))
						   (let ((var-patlist (gethash var var-hash)))
							 (when (patlist-equal binding-patlist var-patlist)
							   (setf (gethash binding valid-bindings-hash) binding))))))
					 binding-hash)
			(let ((valid-edge-envs nil))
			  (maphash (lambda (k v)
						 (let ((edge-env k)
							   (pat v))
						   (unless (any-binding-invalid edge-env valid-bindings-hash)
							 (dolist (binding edge-env)
							   (unless (member edge-env valid-edge-envs :test #'equal)
								 (setq valid-edge-envs (cons edge-env valid-edge-envs)))))))
					   edge-env-to-pat-hash)
			  (let ((edge-envs-list nil)
					(envs-list nil))
				(maphash (lambda (k v)
						   (let ((pat k)
								 (envs v))
							 (let ((edge-envs (mapcad (lambda (env)
														(when (member env valid-edge-envs :test #'equal)
														  env))
													  envs)))
							   (setq edge-envs-list (cons edge-envs edge-envs-list))
							   (setq envs-list (cons envs envs-list)))))
						 pat-to-edge-envs-hash)
				;;		  envs-list	
				edge-envs-list
				))))))

	(defm match-one-edge (pat-edge obj-edge 
								   root-var			;; If null, is ignored
								   obj-node			;; If null, is ignored (implied by null root-var
								   rule-node)		;; Passed for diag purposes only
	  (defr
		(defl match-one-edge1 (pat-edge obj-edge)
		  (defr
			(defl match-one-edge-aux (pat-edge obj-edge)
			  (cond
			   ((null pat-edge)
				'((t t))) ;; literal-match case; dummy binding
			   ;; If we have only one of the roots, then whole edge does not match
			   ((or (and root-var
						 (equal (first pat-edge) root-var)
						 (not (equal (first obj-edge) obj-node)))
					(and root-var
						 (not (equal (first pat-edge) root-var))
						 (equal (first obj-edge)obj-node)))
				(return-from match-one-edge1 nil))
			   ((is-var-name (first pat-edge))
				(cons (list (first pat-edge) (first obj-edge))
					  (match-one-edge-aux (rest pat-edge) (rest obj-edge))))
			   ((equal (first pat-edge) (first obj-edge))
				(match-one-edge-aux (rest pat-edge) (rest obj-edge)))
			   (t
				(return-from match-one-edge1 nil))))
			(match-one-edge-aux pat-edge obj-edge)))
		(timer 'match-one-edge
		  (lambda ()
			(when (= (length pat-edge) (length obj-edge))
			  (let ((r (match-one-edge1 pat-edge obj-edge)))
				r))))))

	;;;;;; Experimental

	;; Good idea that doesn't quite work. Walk the rule and object in
	;; parallel, matching along the way and using the resulting envs
	;; to rewrite the rule for the next level. When all levels are
	;; done we have envs which match, and rules which are all literal
	;; edges. Main problem is that we don't weed out extra matches
	;; early enough, so cross-aux2 explodes.

	(defm e-all-matches-aux (rule-node obj-node root-var)
	  (block ama
  	    (let ((rule-edges (! ((get-rule-components rule-node) preds))))
		  (when rule-edges ;; If no preds, then it's a data rule, i.e., just adds.
			(let ((sig-span (sigma-span rule-node))
				  (rule-nodepos (nodepos rule-edges))
				  (rule-nodes (nodes rule-edges)))
			  (let ((expand-info (e-expand-rule-obj-edges rule-node obj-node rule-nodepos root-var)))
				(first expand-info)))))))

	(defm e-match-pat-obj-edge-lists (patlist objlist root-var obj-node rule-node)
	  (let ((patlist-info (filter-new-node-pred-edges patlist)))
		(let ((new-node-pred-edges (first patlist-info))
			  (patlist (second patlist-info)))
		  (let ((new-node-env nil))
			(dolist (new-node-pred-edge new-node-pred-edges)
			  (setq new-node-env (cons (list (first new-node-pred-edge) (third new-node-pred-edge)) new-node-env)))
			(let ((matches (e-match-all-pat-obj-edge-lists patlist objlist root-var obj-node rule-node)))
			  (when matches
				(if new-node-env
					(cross-aux2 (cons (list new-node-env) matches))
					(cross-aux2  matches))))))))

	;; Returns list of env lists

	(defm e-match-all-pat-obj-edge-lists (patlist objlist root-var obj-node rule-node)
	  (let ((binding-hash (make-hash-table :test #'equal)) ;; binding to pat
			(var-hash (make-hash-table :test #'equal)) ;; var-to-pat
			(edge-env-to-pat-hash (make-hash-table :test #'equal))
			(pat-to-edge-envs-hash (make-hash-table :test #'equal)))
		(dolist (pat patlist)
		  (dolist (node pat)
			(when (is-var-name node)
			  (setf (gethash node var-hash) (cons pat (gethash node var-hash))))))
		(dolist (pat patlist)
		  (dolist (obj objlist)
			(let ((env (match-one-edge pat obj root-var obj-node rule-node)))
			  (when env
				(setf (gethash env edge-env-to-pat-hash) pat)
				(setf (gethash pat pat-to-edge-envs-hash) (cons env (gethash pat pat-to-edge-envs-hash)))
				(dolist (binding env)
				  (unless (member pat (gethash binding binding-hash) :test #'equal)
					(setf (gethash binding binding-hash) (cons pat (gethash binding binding-hash)))))))))
		(let ((valid-bindings-hash (make-hash-table :test #'equal)))
		  (maphash (lambda (k v)
					 (let ((binding k)
						   (binding-patlist v))
					   (let ((var (first binding)))
						 (let ((var-patlist (gethash var var-hash)))
						   (when (patlist-equal binding-patlist var-patlist)
							 (setf (gethash binding valid-bindings-hash) binding))))))
				   binding-hash)
		  (let ((valid-edge-envs nil))
			(maphash (lambda (k v)
					   (let ((edge-env k)
							 (pat v))
						 (unless (any-binding-invalid edge-env valid-bindings-hash)
						   (dolist (binding edge-env)
							 (unless (member edge-env valid-edge-envs :test #'equal)
							   (setq valid-edge-envs (cons edge-env valid-edge-envs)))))))
					 edge-env-to-pat-hash)
			(let ((edge-envs-list nil)
				  (envs-list nil))
			  (maphash (lambda (k v)
						 (let ((pat k)
							   (envs v))
						   (let ((edge-envs (mapcad (lambda (env)
													  (when (member env valid-edge-envs :test #'equal)
														env))
													envs)))
							 (setq edge-envs-list (cons edge-envs edge-envs-list))
							 (setq envs-list (cons envs envs-list)))))
					   pat-to-edge-envs-hash)
			  ;;		  envs-list	
			  edge-envs-list
			  )))))

	(defm e-expand-rule-obj-edges (rule-node obj-node rule-nodepos root-var)
	  (let ((obj-edges-result nil))
		(let ((rule-graph (make-rule-graph rule-node)))
		  (let ((rule-edges-seq (bipartite-breadth-rule-walk-seq rule-graph root-var)))
			(let ((rule-edges-list '(()))
				  ;; (obj-edges `((,obj-node)))
				  (obj-edges (expand-rule-obj-edges rule-graph obj-node rule-nodepos root-var))
				  (envs '(((t t))))
				  (renvs nil))
			  (dolist (rule-edges-seq-elem rule-edges-seq)
				(let ((new-rule-edges-list nil))
				  ;; (print (list 'exb rule-edges-list new-rule-edges-list obj-edges envs))
				  (dolist (env envs)
					(dolist (rule-edges rule-edges-list)
					  (setq new-rule-edges-list
							(cons (matched-edges (append rule-edges-seq-elem rule-edges) env)
								  new-rule-edges-list))))
				  ;; (print (list 'exa rule-edges-list new-rule-edges-list obj-edges envs))
				  (setq rule-edges-list new-rule-edges-list)
				  ;; (setq obj-edges (expand-edges obj-edges rule-nodepos rule-node))
				  (setq renvs envs)
				  (setq envs nil)
				  (dolist (rule-edges rule-edges-list)
					;; (setq obj-edges (e-filter-by-common-nodes rule-edges obj-edges))
					(setq envs (append (e-match-pat-obj-edge-lists rule-edges obj-edges root-var obj-node rule-node) envs)))))
			  (list renvs rule-edges-list))))))

	(defm e-filter-by-common-nodes (rule-edges obj-edges)
	  (let ((rule-nodes (nodes rule-edges)))
		(let ((obj-nodes (nodes obj-edges)))
		  (let ((common-nodes (intersect rule-nodes obj-nodes)))
			(edges-with-nodes common-nodes obj-edges)))))

	;;;;;; End Experimental

	(defm remove-named-rule (rule-expr)
	  (block rnr
		(dolist (clause (rest rule-expr))
		  (let ((clause-type (first clause)))
			(when (eq clause-type 'name)
			  (let ((rule-name (second clause)))
				(let ((rule-nodes
					   (mapcan (lambda (node)
								 (when (eq (hget node 'type) 'rule)
								   (list node)))
							   (mapcar (lambda (edge)
										 (first edge))
									   (hget-edge-inverse-all rule-name 'name)))))
				  (dolist (rule-node rule-nodes)
					(let ((edges (get-edges rule-node)))
					  (dolist (edge edges)
						(rem-edge edge))))))
			  (return-from rnr nil)))))
	  nil)
	
	(defm define-rule (rule-expr &key (local nil) (new-pool 0) (add-to-local-rule-pool t))
	  (when (not (eq (first rule-expr) 'comment))
		(remove-named-rule rule-expr)
		(let ((rule (define-obj :add-rule-link t :new-pool new-pool)) ;; add-rule-link nil
			  (edge-list nil)
			  (add-to-node nil))
		  (defr

			(defl pushe (edge)
			  (setq edge-list (cons edge edge-list)))

			(defl xform-std-vars (edge)	;; Use new-pool level to scope the std vars (?this-obj, etc.)
			  (mapcar (lambda (x)
						(! (std-vars var-base-to-var) x new-pool))
					  edge))

			;; Note we also use new-pool level to scope vars we generate for nested added edges

			(defl new-var (i)
			  (intern (format nil "?_X-~a-~a" new-pool i)))

			(defl new-sn (i)
			  (intern (format nil "SN-~a-~a" new-pool (+ i 100))))

			;; Return list (preds adds), consisting of new pred and
			;; add edges, where we expand any lists in the third
			;; position of an add edge into an elem-based form. This
			;; model does not directly create new nodes and augments
			;; the rule with appropriate new-node preds, creating
			;; corresponding vars to hold the new node, and new adds
			;; for the elem-based edges using the new node.

			(defl xform-nested-adds (rule-expr)
			  (let ((adds nil)
					(preds nil)
					(i 0))
				(dolist (clause (rest rule-expr))
				  (let ((clause-type (first clause)))
					(cond
					 ((eq clause-type 'add)
					  (dolist (edge (rest clause))
						(if (and (not (null (third edge)))
								 (listp (third edge))
								 (not (eq (first (third edge)) 'rule)))
							(let ((add-edge (third edge)))
							  (let ((add-node (new-var i)))
								(let ((add-sn (new-sn i)))
								  (let ((add-list (x-list-to-edge-elem-node add-edge add-node)))
									(setq adds (append adds add-list))
									(setq adds (append adds (list (list (first edge) (second edge) add-node))))
									(setq preds (append preds (list (list add-node 'new-node add-sn))))
									(setq i (+ i 1))))))
							(setq adds (append adds (list edge))))))
					 ((eq clause-type 'pred)
					  (setq preds (append preds (rest clause)))))))
				(list preds adds)))

			(defl replace-preds-and-adds (rule-expr preds adds)
			  (defr
				(defl f (r)
				  (cond ((or (null r)
							 (not (listp r)))
						 r)
						((eq (first r) 'pred)
						 (cons 'pred preds))
						((eq (first r) 'add)
						 (cons 'add adds))
						(t
						 (cons (f (first r)) (f (rest r))))))
				(f rule-expr)))

			(let ()
			  (let ((rule-expr (let ((r (xform-nested-adds rule-expr))) (replace-preds-and-adds rule-expr (first r) (second r))))) ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				;; (let ((rule-expr rule-expr))
				(dolist (clause (rest rule-expr))
				  (let ((clause-type (first clause)))
					(cond
					 ((eq clause-type 'name)
					  (let ((rule-name (second clause)))
						(if (not (listp rule-name))
							(pushe (addraw rule 'name rule-name))
							(pushe (add-edge `(name-attr ,rule ,@rule-name))))))
					 ((eq clause-type 'root-var)
					  (let ((root-var (second clause)))
						(pushe (addraw rule 'root-var root-var))))
					 ((eq clause-type 'no-triggered)
					  (pushe (add-edge `(,rule no-triggered))))
					 ((eq clause-type 'local)
					  (setq local t)
					  (pushe (add-edge `(,rule local))))
					 ((eq clause-type 'disabled)
					  (setq local t)
					  (pushe (add-edge `(,rule disabled))))
					 ((eq clause-type 'global-node)
					  (setq add-to-global-node t))
					 ((eq clause-type 'attach-to)
					  (pushe (add-edge `(,rule attach-to ,(second clause))))
					  (setq add-to-node (second clause)))
					 ((member clause-type '(add) :test #'eq)
					  (dolist (edge (rest clause))
						(let ((edge (xform-std-vars edge)))
						  (cond
						   ((and (listp (third edge)) (eq (first (third edge)) 'rule))
							(let ((clause-node (new-obj-node)))

							  ;; A nested rule definition should be entirely local, both the new
							  ;; node (NNxx) returned at definition time and the resulting rule
							  ;; instantiated at run-time. So we don't add to the local rule pool, and
							  ;; it's a local rule as well. By doing this we avoid, in particular,
							  ;; issues with NNxx rule being matched against inadvertently. Such
							  ;; matches, though not prohibited, are normally undesirable.

							  (let ((rule-values (define-rule
												   (third edge)
												   :local t 
												   :new-pool (+ new-pool 1)
												   :add-to-local-rule-pool nil)))
								(let ((rule-node (first rule-values))
									  (rule-edges (second rule-values)))
								  (let ((rule-add-edge-node (list-to-edge-elem-node (append (reverse (rest (reverse edge)))
																							(list rule-node))
																					:ordered t
																					:pushfcn (lambda (edge) (pushe edge))
																					:new-pool new-pool)))
									(pushe (addraw rule 'add rule-add-edge-node))
									(pushe (addraw rule 'nested-rule rule-node)))
								  (dolist (rule-edge rule-edges)
									(let ((node (list-to-edge-elem-node rule-edge
																		:ordered t
																		:pushfcn (lambda (edge) (pushe edge))
																		:new-pool new-pool)))
									  (pushe (addraw rule 'add node))))))))
						   ((and nil ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
								 (not (null (third edge)))
								 (listp (third edge)))
							(let ((third-node-edges nil))
							  (let ((third-node (list-to-edge-elem-node (third edge)
																		:ordered t
																		:pushfcn (lambda (edge) (setq third-node-edges (cons edge third-node-edges)))
																		:new-pool (+ new-pool 0))))
								(let ((clause-node (list-to-edge-elem-node `(,(first edge) ,(second edge) ,third-node)
																		   :ordered t
																		   :pushfcn (lambda (edge) (pushe edge))
																		   :new-pool (+ new-pool 0))))
								  (pushe (addraw rule clause-type clause-node)) ;; add-main here too? !!!!!!!!!!!!!!
								  (dolist (edge third-node-edges)
									(addraw rule clause-type
											(list-to-edge-elem-node edge
																	:ordered t
																	:pushfcn (lambda (edge) (pushe edge))
																	:new-pool (+ new-pool 0))))))))
						   (t
							(let ((clause-node (list-to-edge-elem-node edge
																	   :ordered t
																	   :pushfcn (lambda (edge) (pushe edge))
																	   :new-pool new-pool)))
							  (pushe (addraw rule clause-type clause-node))
							  (pushe (addraw rule 'add-main clause-node))))))))
					 ((member clause-type '(pred del not) :test #'eq)
					  (dolist (edge (rest clause))
						(let ((edge (xform-std-vars edge)))
						  (let ((clause-node (list-to-edge-elem-node edge
																	 :ordered t
																	 :pushfcn (lambda (edge) (pushe edge))
																	 :new-pool new-pool)))
							(pushe (addraw rule clause-type clause-node))))))))))
			  (pushe (addraw rule 'type 'rule))
			  (pushe (addraw rule 'std-var-level new-pool))
			  (when (null (hget rule 'root-var))
				(pushe (addraw rule 'root-var nil)))
			  (if add-to-node
				  (addraw add-to-node 'rule rule)
				  (let ()
					(when (not local)
					  (addraw global-rule-pool 'grp-rule rule))
					(when add-to-local-rule-pool
					  (addraw local-rule-pool 'lrp-rule rule))))
			  (list rule edge-list))))))


	(defm bipartite-breadth-rule-walk-seq (rule-graph root-var)
	  (timer 'bipartite-breadth-rule-walk-seq
		(lambda ()
		  (let ((traced (edge-exists `(rule-trace ,(! (rule-graph name))))))
			(let ((root-node (if root-var root-var (first (first rule-edges))))) ;; !!!!!!!!! Fix: root-var heuristic
			  (let ((rule-edges-list nil))
				(bipartite-breadth-rule-walk rule-graph root-node :rule-traced traced :result-fcn
											 (lambda (x)
											   (when (is-edge (first x))
												 (when traced
												   #|
												   (print (list 'bipseq-result-fcn x))
												   |#
												   )
												 (setq rule-edges-list (append rule-edges-list (list x))))))
				(let ((prev nil)
					  (r nil))
				  (dolist (rule-edges rule-edges-list)
					(setq r (cons (set-subtract rule-edges prev) r))
					(setq prev rule-edges))
				  (let ((r (reverse r)))
					(when traced
					  (print `(rule-trace bipseq ,r)))
					r))))))))

	(defm bipartite-breadth-rule-walk (rule-graph root-node &key result-fcn rule-traced)
	  (let ((visit-hash (make-hash-table :test #'equal)))
		(defr
		  (defl bwb (bnodes)
			(when (or am-traced rule-traced)
			  (print (list 'bipwalk root-node bnodes)))
			(when (and bnodes result-fcn)
			  (funcall result-fcn bnodes))
			(let ((c (mapunion (lambda (bnode)
								 (and (not (gethash bnode visit-hash))
									  (let ()
										(setf (gethash bnode visit-hash) bnode)
										(! (rule-graph get-rule-children) bnode root-node))))
							   bnodes)))
			  (when c
				(bwb c))))
		  (setf (gethash root-node visit-hash) root-node)
		  (bwb (! (rule-graph get-rule-children) root-node root-node))
		  (let ((r nil))
			(maphash (lambda (k v)
					   (when (is-edge v)
						 (setq r (cons v r))))
					 visit-hash)
			r))))
	
	(defm get-rule-children (bnode root-var)
	  (cond
	   ((and (is-const bnode)					;; !!! Note we don't now go through constants!
			 (not (eq bnode root-var)))
		nil)
	   ((is-node bnode)
		(get-edges bnode))
	   ((is-edge bnode)
		bnode)
	   (t nil)))

	(defm bipartite-depth-rule-walk (rule-node root-var)
	  (let ((rule-graph (make-rule-graph)))
		(let ((root-node (if root-var root-var (first (first rule-edges)))))
		  ;; (print rule-edges)
		  (let ((visit-hash (make-hash-table :test #'equal)))
			(defr 
			  (defl bwb (bnode curlist)
				(cond
				 ((null bnode)
				  ;; (print (reverse curlist))
				  )
				 ((gethash bnode visit-hash)
				  ;; (print (reverse curlist))
				  )
				 (t
				  (setf (gethash bnode visit-hash) bnode)
				  (let ((children (! (rule-graph get-rule-children) bnode root-node)))
					(when (is-edge (first children))
					  (print (list 'children children)))
					(dolist (child children)
					  (bwb child (if (is-edge child) (cons child curlist) curlist)))))))
			  (defl bdrw (rule-graph root-node)
				(bwb root-node nil))
			  (bdrw rule-graph root-node))))))

	(defm rule-vars (rule-node)
	  (filter-in-vars (nodes (! ((get-rule-components rule-node) preds)))))

	;; [12/26/16, amended 4/8/17: We used to use a heurisrtic for the
	;; root-var if none was given, but we really needed to expand this
	;; to trying all rule nodes. Just as well -- trying all nodes is
	;; more in the spirit of the H language, where I wish to relieve
	;; the rule writer of the burden of having to know absolutely
	;; where to attach a rule. Wrt performance, we're taking advantage
	;; of the not-bad pre-matching heuristics (esp. the possible-match
	;; method), which can throw out most non-matches quickly.]

	;; Root vars are all vars or consts of a rule pred, excluding
	;; those vars or consts associated with the new-node
	;; pseudo-relation. E.g., the edge (?nn1 new-node sn1) would be
	;; eliminated, as well as all the nodes contained therein. Note
	;; need to use the set-subtract method below esp. because the
	;; format of the var name in a new-node edge is not prescribed, so
	;; we can have e.g. (?x new-node sn1).

	(defm get-root-vars (rule-node)
	  (let ((root-var (and (not disable-root-vars) (hget rule-node 'root-var))))
		(or (and root-var 
				 (list root-var))
			(let ((rule-edges-info (filter-new-node-pred-edges (! ((get-rule-components rule-node) preds)))))
			  (let ((new-node-rule-edges (first rule-edges-info))
					(other-rule-edges (second rule-edges-info)))
				(let ((new-node-rule-edge-nodes (nodes new-node-rule-edges))
					  (other-rule-edge-nodes (nodes other-rule-edges)))
				  (set-subtract other-rule-edge-nodes new-node-rule-edge-nodes)))))))

	(defm get-rule-consts (rule)
	  (let ((pred-list (! ((get-rule-components rule) preds))))
		(dedup-list (mapcan (lambda (edge)
							  (filter-vars edge))
							pred-list))))

	(defm get-rule-consts-pred (pred-list)
	  (dedup-list (mapcan (lambda (edge)
							(filter-vars edge))
						  pred-list)))

	;; Returns a function which when called with the root var, will do
	;; the check, in the rule context setup by the closure. Much
	;; faster than calling the whole thing over and over.

	(defm old-possible-match-fcn (rule-node obj-node)
	  (timer 'possible-match-fcn
		(lambda ()
		  (defr
			(defl get-rule-preds-root (preds root-var) ;; Returns <preds-containing-root-var>
			  (let ((r nil))
				(dolist (pred preds)
				  (when (member root-var pred :test #'equal)
					(setq r (cons pred r))))
				r))
			(let ((obj-edges (get-edges obj-node)))
			  (let ((obj-nodes (nodes obj-edges)))
				(let ((rule-comps (get-rule-components rule-node)))
				  (let ((preds (! (rule-comps preds))))
					(let ((pred-const-nodes (get-rule-consts-pred preds)))
					  (let ((ipo (intersect pred-const-nodes obj-nodes)))
						(lambda (root-var)
						  (timer 'possible-match
							(lambda ()
							  (let ((r (and ipo
											(let ((root-preds (get-rule-preds-root preds root-var)))
											  (let ((root-pred-const-nodes (get-rule-consts-pred root-preds)))
												(when am-traced
												  (print (list 'pmf root-var preds root-preds root-pred-const-nodes obj-nodes)))
												(= 
												 (length (intersect root-pred-const-nodes obj-nodes))
												 (length root-pred-const-nodes)))))))
								(gstat 'possible-match-true  (lambda (x y) (+ x y)) (lambda () (if r 1 0)))
								(gstat 'possible-match-false (lambda (x y) (+ x y)) (lambda () (if r 0 1)))
								(list r :unknown)))))))))))))))

	;; 3/25/19 -- Troubles with this new possible-match-fcn when tried
	;; with fe-rule-test. Had to change from detecing consts in
	;; scan-and-subst to just checking edge existence, since the
	;; copiers can have vars in the results. Also in scan-and-subst
	;; had to checn from detcing a null env to checking that it's
	;; stopped changing on each loop. Otherwise we always get a
	;; binding to a var.
	;;
	;; Also took out chain-check -- probably breaks due to the need to
	;; handle vars in some way not accounted for in the chain
	;; processing.  However scan-and-subst seems to supersede its need anyway.
	;;
	;; This could be pointing out that we should be careful in  straying too much from the isomrophism theme.

	(defm possible-match-fcn (rule-node obj-node)		;; all-var-mod
	  (timer 'possible-match-fcn
		(lambda ()
		  (defr

			(defmacro hprint (&rest args)
			  ;; nil
			  `(print (list ,@args))
			  )

			;; Returns nil if no match, an env if match, and :unknown if it's not certain

			(defl scan-and-subst (preds root-var obj-node)
			  (timer 'scan-and-subst
				(lambda ()
				  (block b
					(let ((qet-edge-count-cache (make-sur-map :input-size 31 :res-size 1)))
					  (defr
						(defl lookup-qet-edge-count (qet)
						  (! (qet-edge-count-cache lookup-one) qet))
						(defl insert-qet-edge-count (qet count)
						  (! (qet-edge-count-cache insert) qet count))
						(defl check-consts (pred) ;; T if all consts
						  (block b
							(dolist (node pred)
							  (when (is-var-name node)
								(return-from b nil)))
							t))
						(defl remove-vars (preds)
						  (mapcar (lambda (pred)
									(filter-vars pred))
								  preds))
						;; Using this local form just to emphasize that we're doing a subst and possibly leaving vars in the
						;; result. This is not the intent of matched-edges so we're just trying here to be clear wrt naming.
						(defl subst (preds env)
						  (matched-edges preds env))
						(let ((status nil))
						  (let ((env `((,root-var ,obj-node))))
							(let ((envout env))
							  (let ((prev-envout nil))
								(let ((edge-count-prod 1))
								  (let ((new-preds preds))
									(block b
									  (loop
									   (when (or (null env)
												 (env-equal envout prev-envout))
										 (return-from b nil))
									   (setq edge-count-prod 1)
									   (setq status nil)
									   (setq new-preds (subst new-preds env)) ;; Does subst of vars via env
									   (let ((new-env (let ((new-pred-qets (remove-vars new-preds)))
														;; (hprint 'e new-preds env new-pred-qets)
														(mapunion (lambda (pred pred-qet) ;; Should provide an env of new bindings 
																	(let ((edge-count (lookup-qet-edge-count pred-qet)))
																	  (let ((edge-count (or edge-count (insert-qet-edge-count pred-qet (count-edges-from-subqet pred-qet)))))
																		(setq edge-count-prod (* edge-count-prod edge-count))
																		;; (hprint 'e1 pred pred-qet)
																		(if (= edge-count 1)
																			(let ((edges (get-edges-from-subqet pred-qet)))
																			  (let ((edge (first edges)))
																				(let ((r (mapunion (lambda (pred-node obj-node)
																									 (when (is-var-name pred-node)
																									   (list (list pred-node obj-node))))
																								   pred edge)))
																				  ;; (hprint 'e2 r)
																				  r)))
																			(let ()
																			  ;; Set this flag if we have more than one edge in the qet lookup. Means non-det and need to return :unknown
																			  (when (> edge-count 1)
																				(setq status t))
																			  nil)))))
																  new-preds
																  new-pred-qets))))
										 ;; (hprint 'e3 new-env) 
										 (setq prev-envout envout)
										 (setq envout (hunion envout new-env))
										 (setq env new-env))))

									;; At this point new-preds should be all constants and all of its edges must exist for the function to be true
									;;
									;; Also, if we left the last loop and there were any non-unique edges from qets, 
									;; then we have the non-determisitic case and we really need to return :unknown

									#|
									(when (> edge-count-prod 1)
									;; (hprint 'e5 (hget rule-node 'name) obj-node edge-count-prod))
									|#

							  (let ((r (block b
										 (dolist (new-pred new-preds)
										   (when (or nil #|(not (check-consts new-pred))|#
													 (not (edge-exists new-pred)))
											 (return-from b nil)))
										 t)))
								(let ((r (if r
											 (let ()
											   ;; (hprint 'e4 envout preds root-var obj-node)
											   (hunion envout '((t t))))
											 (if status
												 :unknown
												 nil))))
								  (gstat 'scan-and-subst-success (lambda (x y) (+ x y)) (lambda () (if (and (not (null r)) (not (eq r :unknown))) 1 0)))
								  (gstat 'scan-and-subst-failure (lambda (x y) (+ x y)) (lambda () (if (null r) 1 0)))
								  (gstat 'scan-and-subst-unknown (lambda (x y) (+ x y)) (lambda () (if (eq r :unknown) 1 0)))
								  #|
								   (when (and (not (null r)) (not (eq r :unknown)))
									 (print `(scan-and-subst rule ,rule-node ,(hget rule-node 'name) env ,r)))
								  |#
								  r))))))))))))))

			(defl old-scan-and-subst (preds root-var obj-node)
			  (defr
				(defl check-consts (pred)
				  (block b
					(dolist (node pred)
					  (when (is-var-name node)
						(return-from b nil)))
					t))
				(defl check-form-1 (pred)			;; (const const var)
				  (and (= (length pred) 3)
					   (not (is-var-name (first pred)))
					   (not (is-var-name (second pred)))
					   (is-var-name (third pred))))
				(defl check-form-2 (pred)			;; (var const const)
				  (and (= (length pred) 3)
					   (is-var-name (first pred))
					   (not (is-var-name (second pred)))
					   (not (is-var-name (third pred)))))
				(defl subst (preds var val)
				  (let ((r nil))
					(dolist (pred preds)
					  (let ((new-pred nil))
						(dolist (node pred)
						  (setq new-pred (append new-pred (list (if (eq node var) val node)))))
						(setq r (cons new-pred r))))
					r))
				(let ((var root-var))
				  (let ((val obj-node))
					(let ((new-preds preds))
					  (block b
						(loop
						 (when (null var)
						   (return-from b nil))
						 (setq new-preds (subst new-preds var val))
						 (setq var nil)
						 (dolist (new-pred new-preds)
						   (cond
							((check-form-1 new-pred)
							 (let ((new-var (third new-pred)))
							   (let ((new-val (third (first (superqets (list (first new-pred) (second new-pred)))))))
								 (if new-val
									 (let ()
										 (setq var new-var)
										 (setq val new-val))
									 nil))))
							((check-form-2 new-pred)
							 (let ((new-var (first new-pred)))
							   (let ((new-val (first (first (superqets (list (second new-pred) (third new-pred)))))))
								 (if new-val
									 (let ()
									   (setq var new-var)
									   (setq val new-val))
									 nil))))))))
					  (block b
						(dolist (new-pred new-preds)
						  (when (or (not (check-consts new-pred))
									(not (edge-exists new-pred)))
							(return-from b nil)))
						t))))))
			
			(defl get-rule-preds-root (preds root-var) ;; Returns <preds-containing-root-var>
			  (let ((r nil))
				(dolist (pred preds)
				  (when (member root-var pred :test #'equal)
					(setq r (cons pred r))))
				r))

			;; Get list of pred chain pairs ((p1 p2) (p1 p2) ...)
			;; Looks for pattern (root-var attr1 var1)(var1 attr2 var2)

			(defl get-pred-chains-form-1 (preds root-var)
			  (let ((chains nil))
				(dolist (pred1 preds)
				  (dolist (pred2 preds)
					(when (not (eq pred1 pred2))
					  (when (and (= (length pred1) 3)
								 (= (length pred2) 3)
								 (is-var-name (third pred1))
								 (eq (first pred1) root-var)
								 (eq (third pred1) (first pred2))
								 (is-var-name (first pred1))
								 (not (is-var-name (second pred1)))
								 (not (is-var-name (second pred2)))
								 (is-var-name (third pred2)))
						(setq chains (cons (list pred1 pred2) chains))))))
				chains))

			;; Looks for pattern (root-var attr1 var1)(var2 attr2 var1)

			(defl get-pred-chains-form-2 (preds root-var)
			  (let ((chains nil))
				(dolist (pred1 preds)
				  (dolist (pred2 preds)
					(when (not (eq pred1 pred2))
					  (when (and (= (length pred1) 3)
								 (= (length pred2) 3)
								 (is-var-name (third pred1))
								 (eq (first pred1) root-var)
								 (eq (third pred1) (third pred2))
								 (is-var-name (first pred1))
								 (not (is-var-name (second pred1)))
								 (not (is-var-name (second pred2)))
								 (is-var-name (first pred2)))
						(setq chains (cons (list pred1 pred2) chains))))))
				chains))

			;; Given we have found a possible match via the root var
			;; at obj-node, look now for another variable connected to
			;; the root var, and find an obj corrseponding to it.
			;;
			
			(defl check-other-var-form-1 (preds root-var obj-node)
			  (block b
				(let ((pred-chains (get-pred-chains-form-1 preds root-var)))
				  (if pred-chains
					  (let ()
						;; (hprint 'v1 (hget rule-node 'name) preds root-var obj-node pred-chains)
						(dolist (pred-chain pred-chains)
						  (let ((pred1 (first pred-chain)))
							(let ((pred2 (second pred-chain)))
							  (let ((qet1 (list obj-node (second pred1))))
								(let ((superqet1 (first (superqets qet1))))
								  (let ((other-obj (third superqet1)))
									(let ((qet2 (list other-obj (second pred2))))
									  (if (qet-exists qet2)
										  (let ()
											;; (hprint 'c1 (hget rule-node 'name) preds root-var obj-node pred-chain)
											(return-from b t))
										  (let ()
											;; (hprint 'u1 (hget rule-node 'name) preds root-var obj-node pred-chain)
											(return-from b nil)))))))))))
					  t))))

			(defl check-other-var-form-2 (preds root-var obj-node)
			  (block b
				(let ((pred-chains (get-pred-chains-form-2 preds root-var)))
				  (if pred-chains
					  (let ()
						;; (hprint 'v2 (hget rule-node 'name) preds root-var obj-node pred-chains)
						(dolist (pred-chain pred-chains)
						  (let ((pred1 (first pred-chain)))
							(let ((pred2 (second pred-chain)))
							  (let ((qet1 (list obj-node (second pred1))))
								(let ((superqet1 (first (superqets qet1))))
								  (let ((other-obj (third superqet1)))
									(let ((qet2 (list (second pred2) other-obj)))
									  (if (qet-exists qet2)
										  (let ()
											;; (hprint 'c2 (hget rule-node 'name) preds root-var obj-node pred-chain)
											(return-from b t))
										  (let ()
											;; (hprint 'u2 (hget rule-node 'name) preds root-var obj-node pred-chain)
											(return-from b nil)))))))))))
					  t))))

			;; Reduces preds by removing all vars which are not the root var, and substituting obj-node for the root var.
			;; Then looks up the existence of the resulting qets. T iff all of them exist, else nil.

			(defl check-rule-preds-root-qets (preds root-var obj-node)
			  (let ((qets (filter-non-root-vars preds root-var obj-node)))
				(block b
				  (dolist (qet qets)
					(when (not (or (and (qet-exists qet)
										(or
										 (edge-exists qet)
										 (not (null (superqets qet)))))))
					  (return-from b nil)))
				  ;; (hprint 'q (hget rule-node 'name) preds root-var obj-node qets)
				  t)))

			(defl filter-non-root-vars (preds root-var obj-node)
			  (mapcar (lambda (pred)
						(mapcad (lambda (node)
								  (cond
								   ((eq node root-var)
									obj-node)
								   ((not (is-var-name node))
									node)
								   (t
									nil)))
								pred))
					  preds))

			(defl get-const-prop (preds root-var)
			  (block b
				(dolist (pred preds)
				  (when (or
						 (and (= (length pred) 2)
							  (eq root-var (first pred))
							  (not (is-var-name (second pred))))
						 (and (= (length pred) 3)
							  (eq root-var (first pred))
							  (not (is-var-name (second pred)))
							  (not (is-var-name (third pred)))))
					(return-from b pred)))))

			(let ()
			  ;; (hprint 'r (hget rule-node 'name) obj-node)
			  (let ((obj-edges (get-edges obj-node)))
				(let ((obj-nodes (nodes obj-edges)))
				  (let ((rule-comps (get-rule-components rule-node)))
					(let ((preds (second (filter-new-node-pred-edges (! (rule-comps preds))))))
					  (let ((pred-const-nodes (get-rule-consts-pred preds)))
						;; (let ((ipo (intersect pred-const-nodes obj-nodes)))	;; all-var-mod
						(let ((ipo obj-nodes))									;; all-var-mod
						  (lambda (root-var)			;; Returns (pos-match = {t, nil}, scan-and-subst-status = {:unknown, :not-called, <an env>, nil})
							(timer 'possible-match
							  (lambda ()
								(block b
								  (let ((r 
										 (and ipo
											  (or (is-var-name root-var)
												  (equal root-var obj-node))
											  (let ((root-preds (get-rule-preds-root preds root-var)))
												(let ((root-pred-const-nodes (get-rule-consts-pred root-preds)))
												  (let ((pred-qets-match (check-rule-preds-root-qets root-preds root-var obj-node)))
													(when am-traced
													  (print (list 'pmf root-var preds root-preds root-pred-const-nodes obj-nodes)))
													(and
													 pred-qets-match
													 (= (length (intersect root-pred-const-nodes obj-nodes))
														(length root-pred-const-nodes))
													 t #|(check-other-var-form-1 preds root-var obj-node)|#
													 t #|(check-other-var-form-2 preds root-var obj-node)|#)))))))
									(let ((r (if r
												 (let ((s (scan-and-subst preds root-var obj-node)))
												   
												   ;; (setq s :unknown)		;; !!!!!

												   ;; (hprint 's (hget rule-node 'name) root-var obj-node r s)
												   (if (null s)
													   (list nil nil)
													   (list t s)))
												 (list nil :not-called))))
									  ;; Not clear which of these stats is better
									  (gstat 'possible-match-true  (lambda (x y) (+ x y)) (lambda () (if (first r) 1 0)))
									  (gstat 'possible-match-false (lambda (x y) (+ x y)) (lambda () (if (first r) 0 1)))
									  ;; (bool-timer 'possible-match-true 'possible-match-false (lambda () (first r)))
									  r)))))))))))))))))


	;; rule-graph should really be a subclass of objgraph (class name rulegraph)
	;; !!! Esp with this hack of removing the initial edge!

	(defm make-rule-graph (rule-node)
	  (timer 'make-rule-graph
		(lambda ()
		  (let ((rule-graph (make-rulegraph))
				(rule-edges (! ((get-rule-components rule-node) preds))))
			(! (rule-graph set-rule-node) rule-node)
			;; (rem-edge '(global-node local-rule-pool local-rule-pool-node))
			;; (rem-edge '(global-node global-rule-pool-ref global-rule-pool-node))
			(dolist (rule-edge rule-edges)
			  (! (rule-graph add-edge) rule-edge))
			(! (rule-graph add-edge) `(,rule-node name ,(hget rule-node 'name)))
			rule-graph))))

	;; Builds new graph from current one by substituting nodes in the
	;; graph by the bindings in env. Note this is used when the
	;; objgraph is used as a rule-graph, but will work correctly for
	;; any objgraph. 

	(defm subst (env)
	  (let ((edges (get-all-edges)))
		(let ((g (make-objgraph)))
		  (dolist (edge edges)
			(let ((new-edge (mapcar (lambda (node) (env-lookup node env))
									edge)))
			  (! (g add-edge) new-edge)))
		  g)))

	;; Return two graphs, one with full subst done, the other with
	;; subst done on only those edges affected by the subst.

	(defm x-subst (env)
	  (defr
		(defl subst-edge (edge env)
		  (let ((did-subst nil))
			(let ((new-edge	(mapcar (lambda (node)
									  (let ((value (env-lookup node env)))
										(when (not (eq node value))
										  (setq did-subst t))
										value))
									edge)))
			  (list new-edge did-subst))))
		(let ((edges (get-all-edges)))
		  (let ((g (make-objgraph))
				(h (make-objgraph)))
			(dolist (edge edges)
			  (let ((subst-edge-info (subst-edge edge env)))
				(let ((new-edge (first subst-edge-info))
					  (did-subst (second subst-edge-info)))
				  (! (g add-edge) new-edge)
				  (when did-subst
					(! (h add-edge) new-edge)))))
			(list g h)))))


	;; Walk the rule adds looking for (?x rule ?y) and presume then
	;; that ?y is another rule, and recurse. Result is a sequence of
	;; rule nodes, each representing one of the nested rules.

	(defm nested-rules (rule-node)
	  (and rule-node
		   (let ((adds (make-rulegraph-adds self rule-node)))
			 (let ((rule-ref-edges (! (adds get-edges) 'rule 1)))
			   (let ((rule-ref-edges (mapcar (lambda (edge) (and (is-new-pool-node (third edge)) edge)) rule-ref-edges)))
				 (append (list rule-node)
						 (mapcan (lambda (rule-ref-edge)
								   (let ((rule-ref-node (third rule-ref-edge)))
									 (! (adds nested-rules) rule-ref-node)))
								 rule-ref-edges)))))))

	;; Note the horrid n^2 algorithm -- need to do better algorithm
	;; when this is verified.
	;; 
	;; Ditto above in that this applies mainly to rule-graphs, but
	;; will work at the objgraph level

	(defm largest-subqets-of-only-constants ()
	  (defr
		(defl is-all-constants (qet)
		  (block b
			(dolist (n qet)
			  (when (is-var-name n)
				(return-from b nil)))
			t))
		(let ((h (make-hash-table :test #'equal)))
		  (let ((subqets (all-subqets)))
			(dolist (s subqets)
			  (when (is-all-constants s)
				(setf (gethash s h) s)))
			(dolist (s1 subqets)
			  (dolist (s2 subqets)
				(when (and (not (eq s1 s2))
						   (is-all-constants s1)
						   (is-all-constants s2)
						   (! (qu is-subqet) s1 s2))
				  (remhash s1 h)))))
		  (hash-table-value-to-list h))))

	;; n^2 scan of rules

	(defm rule-dep-graph (&key rules except-rules)
	  (timer 'rule-dep-graph 
		(lambda ()
		  (let ((rulesq (mapcad (lambda (x)								;; Eqv to (query '((?r type rule)(?r name ?n)) '(?r ?n)), but does not create edges
								  (let ((n (hget x 'name)))
									(and n (list x n))))
								(hget-inverse-all 'rule 'type))))
			(let ((rulesq (mapcad (lambda (x) (when (not (is-new-pool-node (first x))) x)) rulesq)))	;; Hack!!!!!! Don't have good way of filtering out "orphan" rules like those tied NNx nodes
			  (print rulesq)
			  ;; (or local-pool-add-node (setq local-pool-add-node (list-to-edge-elem-node '(?x local-rule-pool ?y))))
			  ;; (or global-pool-add-node (setq global-pool-add-node (list-to-edge-elem-node '(?x global-rule-pool ?y))))
			  (dolist (r1q rulesq)
				(let ((r1 (first r1q))
					  (r1n (second r1q)))
				  (add-edge `(,r1 arity ,(length (second (filter-new-node-pred-edges (! ((get-rule-components r1) preds)))))))
				  (dolist (r2q rulesq)
					(let ((r2 (first r2q))
						  (r2n (second r2q)))
					  (when (and
							 (and (not (member r1n except-rules :test #'eq))
								  (not (member r2n except-rules :test #'eq)))
							 (or (null rules)
								 (and (member r1n rules :test #'eq)
									  (member r2n rules :test #'eq))))
						(let ((local-pool-add-node (list-to-edge-elem-node '(?x local-rule-pool ?y))))
						  (let ((global-pool-add-node (list-to-edge-elem-node '(?x global-rule-pool ?y))))
							(let ((r1as (append (hget-all r1 'add) (list local-pool-add-node global-pool-add-node))))
							  (let ((r2ps (hget-all r2 'pred)))
								(dolist (r1a r1as)
								  (dolist (r2p r2ps)
									(let ((r1ae (edge-elem-node-to-list r1a)))
									  (let ((r2pe (edge-elem-node-to-list r2p)))
										(let ((r1aes (format nil "~a" r1ae)))
										  (let ((r2pes (format nil "~a" r2pe)))
											(when (or (match-one-edge r1ae r2pe nil nil nil)
													  (match-one-edge r2pe r1ae nil nil nil))
											  (print (list (list r1n r1ae) (list r2n r2pe)))
											  ;; (add-edge (list 'rd 1 r1n "" "" r2n))
											  ;; (add-edge (list 'rd 2 r1n r1aes r2pes r2n))
											  (add-edge (list r1 'rd r2))
											  ;; (add-edge (list 'rr r1n r1a r2p r2n))

											  #|
										  ;; For work with rd-based eval, these extra edges clutter the system ; ; ; ;

										  ;; add and pred relations already present -- need x versions to avoid display overload ; ; ; ;

											  (add-edge (list r1 'xadd r1a))
											  (add-edge (list r1a 'rule-dep r2p))
											  (add-edge (list r2p 'xpred r2))
											  (add-edge (list r1a 'as-string r1aes))
											  (add-edge (list r2p 'as-string r2pes))
											  |#

									(let ((r1a-r1aes (symcat r1a '- r1aes)))
										  (let ((r2p-r2pes (symcat r2p '- r2pes)))
										  (add-edge (list r1 'xxadd r1a-r1aes))
										  (add-edge (list r1a-r1aes 'xrule-dep r2p-r2pes))
										  (add-edge (list r2p-r2pes 'xxpred r2))))


										  )))))))))))))))))))))


	(defm dimensions ()
	  (let ((r (sort (let ((e (! (edge-to-trace as-list))))
					   (let ((rule-to-max (make-hash-table)))
						 (dolist (x e)
						   (let ((h (make-hash-table)))
							 (dolist (y (second x))
							   (when (eq (first y) 'pred)
								 (let ((rule-name (third y)))
								   (setf (gethash rule-name h) (+ (or (gethash rule-name h) 0) 1)))))
							 (maphash (lambda (k v)
										(let ((rule-name k)
											  (edge-max v))
										  (setf (gethash rule-name rule-to-max) (max (or (gethash rule-name rule-to-max) 0) edge-max))))
									  h)))
						 (hash-table-to-list rule-to-max)))
					 (lambda (x y) (> (second x)(second y))))))
		r))

	(defm pred-dimensions ()
	  (let ((e (! (edge-to-pred as-list))))
		(let ((rule-pred-to-max (make-hash-table :test #'equal)))
		  (dolist (x e)
			(let ((entries (second x)))
			  (let ((h (make-hash-table :test #'equal)))
				(dolist (entry entries)
				  (let ((rule-name (second entry))
						(pred (third entry)))
					(let ((key (list rule-name pred)))
					  (setf (gethash key h) (+ (or (gethash key h) 0) 1)))))
				(maphash (lambda (k v)
						   (let ((key k)
								 (edge-max v))
							 (setf (gethash key rule-pred-to-max) (max (or (gethash key rule-pred-to-max) 0) edge-max))))
						 h))))
		  (hash-table-to-list rule-pred-to-max))))

	(defm edge-dimensions ()
	  (let ((e (! (edge-to-pred as-list))))
		(let ((r nil))
		  (dolist (x e)
			(setq r (cons (list (first x) (length (second x))) r)))
		  r)))

	;; Get the dimension of a given rule, without using trace tables
	;; and the like. Instead, given the rule, we find all matches, via
	;; query of its preds, getting back a set of envs. For each env,
	;; get its matched edges, and count each edge. The rule dimension
	;; is the max count, i.e., the max number of times an edge is
	;; shared by a pred match.

	(defm rule-dimension (rule-node &optional data-fcn)
	  (let ((max-edge-count 0))
		(let ((edge-count (make-sur-map)))
		  (let ((preds (! ((get-rule-components rule-node) preds))))
			(let ((preds (second (filter-new-node-pred-edges preds))))
			  (let ((envs (query preds)))
				(dolist (env envs)
				  (let ((edges (matched-edges preds env)))
					(dolist (edge edges)
					  (let ((n (or (! (edge-count lookup-one) edge) 0)))
						(! (edge-count remove-res) edge n)
						(! (edge-count insert) edge (+ n 1))
						(when (> (+ n 1) max-edge-count)
						  (setq max-edge-count (+ n 1))))))))))
		  (when data-fcn
			(funcall data-fcn edge-count)))
		max-edge-count))

	;; By dimension here we mean edge size, using the term from
	;; Abstract Simplicial Complexes (ASCs) (+ 1). By using this ASC
	;; term we remain consistent with the other dimension measures,
	;; which are based on the overlap of edge sets in the edge space
	;; induced by a rule.

	(defm edge-dimension-dist ()
	  (let ((edge-lengths (mapcar (lambda (x) (length x)) (get-all-edges))))
		(let ((l (max edge-lengths)))
		  (let ((a (make-array (+ l 1) :initial-element 0)))
			(dolist (x edge-lengths)
			  (setf (aref a x) (+ (aref a x) 1)))
			a))))

	;; Returns a graph of a full trace of rule execution

	(defm edge-trace-graph (&key rules except-rules except-nodes)
	  (defr
		(defl has-preds (events)
		  (block b
			(dolist (event events)
			  (when (eq (first event) 'pred)
				(return-from b t)))
			nil))
		(let ((g (make-objgraph)))
		  (let ((trace (! (edge-to-trace as-list))))
			(dolist (entry trace)
			  (let ((edge (first entry)))
				(let ((events (second entry)))
				  (when (has-preds events)
					(dolist (event events)
					  (let ((kind (first event)))
						(let ((seqno (second event)))
							(let ((rule-name (fourth event)))
							  (when (or (null rules)
										(memq rule-name rules))
								(when (or (null except-rules)
										  (not (memq rule-name except-rules)))
								  (when (or (null except-nodes)
											(not (intersect edge except-nodes)))
									(let ((rule-node (symcat rule-name seqno)))
									  (let ((edge-node (format nil "~a" edge)))
										(cond
										 ((eq kind 'add)
										  (! (g add-edge) (list rule-node 'a edge-node))
										  (! (g add-edge) (list rule-node 'shape (hget 'rule 'shape)))
										  (! (g add-edge) (list rule-node 'color (hget 'rule 'color))))
										 ((eq kind 'pred)
										  (! (g add-edge) (list edge-node 'p rule-node))
										  (! (g add-edge) (list rule-node 'shape (hget 'rule 'shape)))
										  (! (g add-edge) (list rule-node 'color (hget 'rule 'color)))))))))))))))))))
		  g)))


	;; Variant on above full trace: r1 is related to r2 iff there
	;; exits an edge e such that (r1 add e) and (e pred r2). Temporal
	;; (seqno) info is not included. This should provide a rule-dep
	;; type of graph, but with only rules deps that are really needed
	;; by a given run.

	(defm edge-trace-rule-graph (&key rules except-rules)
	  (defr
		(defl has-preds (events)
		  (block b
			(dolist (event events)
			  (when (eq (first event) 'pred)
				(return-from b t)))
			nil))
		(defl find-add (events)
		  (block b
			(dolist (event events)
			  (let ((kind (first event)))
				(when (eq kind 'add)
				  (return-from b event))))
			nil))
		(let ((g (make-objgraph)))
		  (let ((trace (! (edge-to-trace as-list))))
			(dolist (entry trace)
			  (let ((edge (first entry)))
				(let ((events (second entry)))
				  (when (has-preds events)
					(let ((add-event (find-add events)))
					  (let ((add-rule-name (fourth add-event)))
						(dolist (event events)
						  (let ((kind (first event)))
							(let ((seqno (second event)))
							  (let ((rule-name (fourth event)))
								(when (or (null rules)
										  (memq add-rule-name rules)
										  (memq rule-name rules))
								  (when (or (null except-rules)
											(and
											 (not (memq rule-name except-rules))
											 (not (memq add-rule-name except-rules))))
									(when (and (eq kind 'pred)
											   (not (null add-rule-name)))
									  (! (g add-edge) (list add-rule-name 'r rule-name))))))))))))))))
		  g)))

	;; Produces spanning dag via attr attr with attr s. Also adds edge (attr s)

	(defm spanning-dag (init-node attr)
	  (let ((levels (make-sur-map)))
		(depth init-node
			   (lambda (node level)
				 (! (levels insert-one) node level)
				 (hget-all node attr)))
		(add-edge '(attr s))
		(breadth init-node
				 (lambda (node level)
				   (let ((children (hget-all node attr)))
					 (dolist (child children)
					   (when (> (! (levels lookup-one) child) level)
						 (add-edge (list node 's child))))
					 children)))))

	(defm spanning-tree (init-node attr)
	  (let ((visited (make-sur-map)))
		(add-edge '(attr s))
		(defr
		  (defl d (node prev-node)
			(when (not (! (visited lookup-one) node))
			  (! (visited insert-one) node node)
			  (when prev-node
				(add-edge (list prev-node 's node)))
			  (let ((children (hget-all node attr)))
				(dolist (child children)
				  (d child node)))))
		  (d init-node nil))))

	(defm execute-rule-dep (init-rule-name &key init-edges rule-dep-rules rule-dep-rule-exceptions) ;; init-edges just for test
	  (let ((count 0)
			;; (pred-node-to-new-edges (make-sur-map))
			(pred-cycle-heads (make-sur-map)))
		(let ((init-rule-node (query `((?r type rule)(?r name ,init-rule-name)) '?r)))
		  (defstruct node-info
			(node)
			(type)) ;; :rule :pred :add
		  (let ((ni (make-node-info :node init-rule-node :type :rule)))
			(defr

			  (defl get-init-edges ()
				(or init-edges (get-all-edges)))

			  (defl exec-until-no-new-edges (thunk)
				(block b
				  (let ((nedges 0)
						(prev-nedges 0))
					(loop
					 (setq nedges (length (get-all-edges)))
					 (when (= prev-nedges nedges)
					   (return-from b nil))
					 (setq prev-nedges nedges)
					 (setq count (+ count 1))
					 (print (list 'exec count))
					 (funcall thunk))
					nil)))

			  ;; Clear preds edges except for pred cycle heads. Implies we need to re-install the init edges. This should be just "for now".
			  (defl clear-pred-edges ()
				(let ((pred-nodes (! (pred-node-to-new-edges inputs))))
				  (dolist (pred-node pred-nodes)
					(when (not (! (pred-cycle-heads lookup) pred-node))
					  (! (pred-node-to-new-edges remove) pred-node)))))

			  (defl install-init-edges (init-edges)
				(timer 'install-init-edges
				  (lambda ()
					(print (list 'init-edges (length init-edges)))
					(depth init-rule-node
						   (lambda (rule-node level)
							 (let ((rc (get-rule-components rule-node)))
							   (let ((pred-nodes (! (rc pred-nodes))))
								 (let ((preds (! (rc preds))))
								   (dolists ((pred-node pred) (pred-nodes preds))
									 (dolist (init-edge init-edges)
									   (when (match-one-edge pred init-edge nil nil nil)
										 (! (pred-node-to-new-edges insert) pred-node init-edge)))))))
							 (mapcar (lambda (x) (node-info-node x))
									 (rule-children-fcn (make-node-info :node rule-node :type :rule) level)))))))

			  (defl rule-children-fcn (ni level)
				(let ((node (node-info-node ni))
					  (node-type (node-info-type ni)))
				  (let ((rule-node node))
					(mapcar (lambda (node) (make-node-info :node node :type :rule))
							(hget-all-list (list node) '(add rule-dep (inv pred)))))))

			  (defl children-fcn (ni level)
				(let ((node (node-info-node ni))
					  (node-type (node-info-type ni)))
				  (cond
				   ((eq node-type :rule)
					(mapcar (lambda (x) (make-node-info :node x :type :add))
							(hget-all node 'add)))
				   ((eq node-type :add)
					(mapcar (lambda (x) (make-node-info :node x :type :pred))
							(hget-all node 'rule-dep)))
				   ((eq node-type :pred)
					(mapcar (lambda (x) (make-node-info :node x :type :rule))
							(hget-inverse-all node 'pred)))
				   (t (print 'error)))))

			  (defl make-rule-dep-graph ()
				(print (length (get-edges 'rule-dep)))
				(rule-dep-graph :rules rule-dep-rules :except-rules rule-dep-rule-exceptions)
				(print (length (get-edges 'rule-dep))))

			  (let ()
				(! (pred-node-to-new-edges clear))
				(! (edge-to-trace clear))
				(exec-until-no-new-edges
					(lambda ()
					  (! (pred-cycle-heads clear))
					  (let ((rule-graph-size0 (+ (length (get-edges 'rule-dep)) (length (get-edges 'xadd)) (length (get-edges 'xpred)))))
						(make-rule-dep-graph)
						(let ((rule-graph-size1 (+ (length (get-edges 'rule-dep)) (length (get-edges 'xadd)) (length (get-edges 'xpred)))))
						  (when (not (= rule-graph-size0 rule-graph-size1)) ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
							(let ((cycle-heads (breadth ni #'rule-children-fcn))) ;; Note these rule cycle heads are interesting to see but not used in this algorithm
							  ;; (print (mapcar (lambda (x) x) cycle-heads))
							  (let ((node-to-level (make-sur-map)))
								(depth init-rule-node ;; Establish levels (based on rule-rule relations)
									   (lambda (rule-node level)
										 (! (node-to-level insert) rule-node level)
										 (let ((rc (get-rule-components rule-node)))
										   (let ((pred-nodes (! (rc pred-nodes))))
											 (dolist (pred-node pred-nodes)
											   (! (node-to-level insert) pred-node level)))
										   (let ((add-nodes (! (rc add-nodes))))
											 (dolist (add-node add-nodes)
											   (! (node-to-level insert) add-node level))))
										 (mapcar (lambda (x) (node-info-node x))
												 (rule-children-fcn (make-node-info :node rule-node :type :rule) level))))
								;; (print (! (node-to-level as-list)))
								(depth ni ;; Establish pred cycle heads (based on rule, add, and pred relations)
									   (lambda (ni level)
										 (let ((node (node-info-node ni))
											   (node-type (node-info-type ni)))
										   (when (eq node-type :add)
											 (let ((add-node node))
											   (let ((add-node-level (! (node-to-level lookup-one) add-node)))
												 (let ((rule-dep-pred-nodes (hget-all node 'rule-dep)))
												   (dolist (rule-dep-pred-node rule-dep-pred-nodes)
													 (let ((rule-dep-pred-level (! (node-to-level lookup-one) rule-dep-pred-node)))
													   ;; (print (list add-node add-node-level rule-dep-pred-node rule-dep-pred-level))
													   (when (<= rule-dep-pred-level add-node-level)
														 (! (pred-cycle-heads insert) rule-dep-pred-node rule-dep-pred-node)))))))))
										 (children-fcn ni level)))
								;; (print (! (pred-cycle-heads inputs)))
								(let ((init-edges (get-init-edges)))
								  (let ((prev-dist nil)
										(prev-n-edges 0)
										(prev-n-nodes 0))
									(let ((rc (get-rule-components init-rule-node)))
									  (let ((init-pred-nodes (! (rc pred-nodes))))
										(dolist (init-pred-node init-pred-nodes)
										  ;; (! (pred-node-to-new-edges insert-list) init-pred-node init-edges) ;; !!!!!!!! Shouldn't need this if match init edges as above
										  )
										;; (! (pred-node-to-new-edges clear))
										(install-init-edges init-edges)
										(exec-until-no-new-edges
											(lambda ()
											  (let ()
												;; (clear-pred-edges)
												;; (install-init-edges init-edges)
												;; ; (print 'pred-new-edges)
												;; ; (dolist (n (! (pred-node-to-new-edges as-list))) (print (list (first n) (hget  (first (hget-inverse-all (first n) 'pred)) 'name) (second n))))
												;; ; (print 'end-pred-new-edges)
												(let ((cur-dist (! (pred-node-to-new-edges count-dist)))
													  (n-edges (length (get-all-edges)))
													  (n-nodes (length (get-all-nodes))))
												  ;; (print (list (set-subtract prev-dist cur-dist) (set-subtract cur-dist prev-dist)
												  ;;           (- n-edges prev-n-edges) (- n-nodes prev-n-nodes)))
												  (setq prev-dist cur-dist)
												  (setq prev-n-edges n-edges)
												  (setq prev-n-nodes n-nodes))
												(depth ni
													   (lambda (ni level)
														 (let ((node (node-info-node ni))
															   (node-type (node-info-type ni)))
														   (when (eq node-type :rule)
															 (let ((rule-node node))
															   ;; (format t "~%~a ~a" rule-node (not (null (member rule-node cycle-heads))))
															   (let ((rc (get-rule-components rule-node)))
																 (let ((pred-nodes (! (rc pred-nodes))))
																   (let ((preds (! (rc preds))))
																	 (let ((obj-edges nil))
																	   (block b
																		 (dolists ((pred-node pred) (pred-nodes preds))
																		   (let ((e (! (pred-node-to-new-edges lookup) pred-node)))
																			 ;; (print (list 'p (hget rule-node 'name) pred-node pred e))
																			 (if (and (null e)
																					  (not (eq (second pred) 'new-node)))
																				 (let ()
																				   (setq obj-edges nil)
																				   (return-from b nil))
																				 (setq obj-edges (hunion e obj-edges))))))
																	   (when obj-edges
																		 (match-and-execute-rule-on-edges rule-node obj-edges :cont
																		   (lambda (r m e p)
																			 ;; (print (list 'm (hget rule-node 'name) r m e p))
																			 (when (or (eq m :new-edges) (eq m :no-new-edges))
																			   (when (eq m :new-edges)
																				 (let ((matched-edges-list p))
																				   (let ((matched-edges (mapunion (lambda (edges) edges) matched-edges-list)))
																					 (let ((removed-edges (make-sur-map)))
																					   (dolist (pred-node pred-nodes) ;; Note these are preds of *current* rule
																						 (dolist (matched-edge matched-edges)
																						   (when (not (member matched-edge init-edges :test #'equal)) ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
																							 (when (not (! (removed-edges lookup) matched-edge)) ;; !!!!!!!!!!!!!!!!!!!!!!!!!
																							   (! (removed-edges insert) matched-edge matched-edge)
																							   ;; (print (list 'd pred-node matched-edge))
																							   (! (pred-node-to-new-edges remove-res) pred-node matched-edge) ;; pred deletion !!!!!!!!!!!!!!!!!!!!!!!
																							   ))))))
																				   (let ((rule-name (hget rule-node 'name)))
																					 (dolist (matched-edges matched-edges-list)
																					   (dolist (matched-edge matched-edges)
																						 (! (edge-to-trace insert) matched-edge (list 'pred seqno rule-name)))
																					   (setq seqno (+ seqno 1))))))
																			   (when (eq m :new-edges)
																				 (let ()
																				   (dolist (edge-info e)
																					 (let ((add-node (first edge-info))
																						   (new-edges (second edge-info)))
																					   (let ((pred-nodes (hget-all add-node 'rule-dep)))
																						 (dolist (pred-node pred-nodes)
																						   (dolist (new-edge new-edges)
																							 (when (match-one-edge (edge-elem-node-to-list  pred-node) new-edge nil nil nil)
																							   ;; (print (list 'i pred-node new-edge))
																							   (! (pred-node-to-new-edges insert) pred-node new-edge))))))))))
																			 (when nil ;; (eq m :failed) ;; !!!!!!!!!!!!!!!!!!!!!!!
																			   (dolist (pred-node pred-nodes) ;; Note these are preds of *current* rule
																				 (! (pred-node-to-new-edges remove) pred-node))))))))))))
														   (children-fcn ni level))))
												(insert-dims)))))))))))))))))))))

	(defm rule-dep-walkback (rule-name)
	  (let ((g (make-objgraph))
			(h (make-sur-map))
			(i 0)
			(s 1))
		  (defr
			(defl indent ()
			  (setq i (+ i s)))
			(defl undent ()
			  (setq i (- i s)))
			(defl prin (x)
			  (block b
				(return-from b nil)    ;;;;;;;; !!!!!!!!!!!
				(dotimes (j i)
				  (format t " "))
				(princ x)
				(terpri)))
			(defl w (rn)
			  (if (or (null rn)
					  (! (h lookup) rn))
				  nil
				  (let ()
					(prin (list 'rn rn))
					(! (h insert) rn t)
					(let ((ps (hget-inverse-all rn 'p)))
					  (indent)
					  (dolist (p ps)
						(prin (list 'p p))
						(let ((cs (hget-inverse-all p 'c)))
						  (indent)
						  (dolist (c cs)
							(prin (list 'c c))
							(let ((as (hget-inverse-all c 'a)))
							  (indent)
							  (dolist (a as)
								(prin (list 'a a))
								(! (g add-edge) (list rn 'd a))
								(w a))
							  (undent)))
						  (undent)))
					  (undent)))))
			(let ()
			  (w rule-name)
			  g))))

	)))

(defc rule-stats nil (graph)
  (let ((rule-stats-table (make-sur-map))
		(all-rules-tested 0)
		(all-rules-new-edges 0))

	(defstruct rule-stats-entry
	  (tested 0)
	  (matched 0)
	  (new-edges 0)
	  (not-new-edges 0)
	  (new-edges-max-expand-len 0)
	  (last-expand-len 0)
	  (failed 0))

	(defm get-entry (rule-node)
	  (let ((entry (! (rule-stats-table lookup-one) rule-node)))
		(when (null entry)
		  (setq entry (make-rule-stats-entry))
		  (! (rule-stats-table insert) rule-node entry))
		entry))

	(defm rule-stats (&optional (sort-colno 1))
	  (defr
		(defl symbol< (x y) (string< (symbol-name x) (symbol-name y)))
		(let ((m 0))
		  (let ((info (mapcan (lambda (l) 
								(let ((x (env-lookup '?x l)))
								  (when t ;; (hget x 'new-edges)
									(setq m (max m (length (symbol-name (! (graph hget) x 'name)))))
									(let ((e (get-entry x)))
									  (list (list (! (graph hget) x 'name)
												  (rule-stats-entry-tested e)
												  (rule-stats-entry-matched e)
												  (rule-stats-entry-new-edges e)
												  (rule-stats-entry-not-new-edges e)
												  (rule-stats-entry-failed e)
												  (rule-stats-entry-new-edges-max-expand-len e)))))))
							  (! (graph query) '((?x type rule))))))
			(let ((info (sort info (lambda (x y)
									 (let ((x (nth sort-colno x))
										   (y (nth sort-colno y)))
									   (if (and (symbolp x) (symbolp y))
										   (symbol< x y)
										   (> x y)))))))
			  (let ((s 10)
					(m (+ m 2)))
				(format t "~%name~vttested~vtmatched~vtnew-e~vtnot-new-e~vtfailed~vtmax-expand-len~%"
						(+ m (* s 0)) (+ m (* s 1)) (+ m (* s 2)) (+ m (* s 3)) (+ m (* s 4)) (+ m (* s 5)))
				(dolist (x info)
				  (format t "~%~a~vt~a~vt~a~vt~a~vt~a~vt~a~vt~a"
						  (nth 0 x) (+ m (* s 0))
						  (nth 1 x) (+ m (* s 1))
						  (nth 2 x) (+ m (* s 2))
						  (nth 3 x) (+ m (* s 3))
						  (nth 4 x) (+ m (* s 4))
						  (nth 5 x) (+ m (* s 5))
						  (nth 6 x))))))
		  nil)))

	(defm update-tested (rule-node)
	  (gstat 'me-tested (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((entry (get-entry rule-node)))
		(setf (rule-stats-entry-tested entry) (+ (rule-stats-entry-tested entry) 1)))
	  (setq all-rules-tested (+ all-rules-tested 1))
	  nil)

	(defm update-matched (rule-node)
	  (gstat 'me-matched (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((entry (get-entry rule-node)))
		(setf (rule-stats-entry-matched entry) (+ (rule-stats-entry-matched entry) 1)))
	  nil)

	(defm get-matched (rule-node)
	  (let ((entry (get-entry rule-node)))
		(rule-stats-entry-matched entry)))

	(defm update-failed (rule-node)
	  (gstat 'me-failed (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((entry (get-entry rule-node)))
		(setf (rule-stats-entry-failed entry) (+ (rule-stats-entry-failed entry) 1)))
	  nil)

	;; These two methods update the counts for executions which either
	;; produce new edges, or not (the latter can be match failures or
	;; already-matched without new edges being produced).
	;;
	;; The numbers can be computed from tested, matched, and new-edges
	;; however these methods need to be their own calls in order to
	;; drive the filter. [Filter removed 4/5/16. This comment is
	;; useful rationale.]

	(defm update-new-edges (rule-node)
	  (gstat 'me-matched-new-edges (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((entry (get-entry rule-node)))
		(setf (rule-stats-entry-new-edges entry) (+ (rule-stats-entry-new-edges entry) 1)))
	  (setq all-rules-new-edges (+ all-rules-new-edges 1))
	  nil)

	(defm update-not-new-edges (rule-node)
	  (gstat 'me-matched-not-new-edges (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((entry (get-entry rule-node)))
		(setf (rule-stats-entry-not-new-edges entry) (+ (rule-stats-entry-not-new-edges entry) 1)))
	  nil)

	(defm update-last-expand-len (rule-node len)
	  (let ((entry (get-entry rule-node)))
		(setf (rule-stats-entry-last-expand-len entry) (max (rule-stats-entry-last-expand-len entry) len))
		nil))

	(defm update-new-edges-max-expand-len (rule-node)
	  (let ((entry (get-entry rule-node)))
		(setf (rule-stats-entry-new-edges-max-expand-len entry) (max (rule-stats-entry-new-edges-max-expand-len entry) (rule-stats-entry-last-expand-len entry)))
		nil))

	))

(defc rulegraph objgraph nil
  (let ((rule-node nil))

	(defm set-rule-node (n)
	  (setq rule-node n))

	(defm rule-node ()
	  rule-node)

	(defm name ()
	  (hget rule-node 'name))

	(defm add-chain (edge) ;;;;; !!!!!!! Nop !!!!!!!
	  (timer 'rulegraph-add-chain
		(lambda ()
		  (block b
			(return-from b nil)
			(when (= (length edge) 3)
			  (let ((n0 (first edge))
					(n2 (third edge)))
				(let ((in-edges (get-edges n0 2))
					  (out-edges (get-edges n2 0)))
				  (when in-edges
					(dolist (in-edge in-edges)
					  (let ((n1 (second in-edge))
							(n2 (second edge))
							(n3 (third edge)))
						(when (and (not (is-var-name n1)) (not (is-var-name n2)))
						  (let ((key (list n1 n2)))
							(let ((chain (list in-edge edge)))
							  (! (chains insert) key chain)))
						  (when (not (is-var-name n3))
							(let ((key (list n1 n2 n3)))
							  (let ((chain (list in-edge edge)))
								(! (chains insert) key chain))))))))
				  (when out-edges
					(dolist (out-edge out-edges)
					  (let ((n1 (second edge))
							(n2 (second out-edge))
							(n3 (third out-edge)))
						(when (and (not (is-var-name n1)) (not (is-var-name n2)))
						  (let ((key (list n1 n2)))
							(let ((chain (list edge out-edge)))
							  (! (chains insert) key chain)))
						  (when (not (is-var-name n3))
							(let ((key (list n1 n2 n3)))
							  (let ((chain (list edge out-edge)))
								(! (chains insert) key chain))))))))
				  (when (and in-edges out-edges)
					(dolist (in-edge in-edges)
					  (dolist (out-edge out-edges)
						(let ((n1 (second in-edge))
							  (n2 (second edge))
							  (n3 (second out-edge)))
						  (when (and (not (is-var-name n1))
									 (not (is-var-name n2))
									 (not (is-var-name n3)))
							(let ((key (list n1 n2 n3)))
							  (let ((chain (list in-edge edge out-edge)))
								(! (chains insert) key chain))))))))))))
		  nil)))

	;; In a rule graph we can use exhaustive n^2 searches to find all
	;; the chains of interest, since the graph should be small.
	;;
	;; In building chains incrementally in the object graph, we need
	;; to assure we examine all necessary combinations -- may
	;; *require* that we compare against existing chains, rather than
	;; just have that be part of heuristic chain pruning.

	(defm build-chains ()
	  (let ((edges (get-all-edges)))
		(dolist (e1 edges)
		  (dolist (e2 edges)
			(when (and (not (eq e1 e2))
					   (= (length e1) 3)
					   (= (length e2) 3)
					   (equal (third e1) (first e2)))
			  (let ((n1 (second e1))
					(n2 (second e2))
					(n3 (third e2)))
				(when (and (not (is-var-name n1)) (not (is-var-name n2)))
				  (let ((key (list n1 n2)))
					(let ((chain (list e1 e2)))
					  (! (chains insert) key chain)))
				  (when (not (is-var-name n3))
					(let ((key (list n1 n2 n3)))
					  (let ((chain (list e1 e2)))
						(! (chains insert) key chain)))))))))
		(dolist (e1 edges)
		  (dolist (e2 edges)
			(dolist (e3 edges)
			  (when (and (not (or (eq e1 e2) (eq e1 e3) (eq e2 e3)))
						 (= (length e1) 3)
						 (= (length e2) 3)
						 (= (length e3) 3)
						 (equal (third e1) (first e2))
						 (equal (third e2) (first e3)))
				(let ((n1 (second e1))
					  (n2 (second e2))
					  (n3 (second e3)))
				  (when (and (not (is-var-name n1))
							 (not (is-var-name n2))
							 (not (is-var-name n3)))
					(let ((key (list n1 n2 n3)))
					  (let ((chain (list e1 e2 e3)))
						(! (chains insert) key chain)))
					(let ((n4 (third e3)))
					  (when (not (is-var-name n4))
						(let ((key (list n1 n2 n3 n4)))
						  (let ((chain (list e1 e2 e3)))
							(! (chains insert) key chain)))))))))))
		nil))

	(defm subsume-chains ()
	  (let ((keys (! (chains inputs))))
		(dolist (key1 keys)
		  (dolist (key2 keys)
			(when (and (not (eq key1 key2))
					   (! (qu is-subqet) key1 key2))
			  (! (chains remove) key1)))))
	  nil)))

(defc rulegraph-adds objgraph (graph rule-node)
  (let ()
	(defm init ()
	  (let ((add-edges (! ((! (graph get-rule-components) rule-node) adds))))
		(rem-edge '(global-node local-rule-pool local-rule-pool-node))
		(rem-edge '(global-node global-rule-pool-ref global-rule-pool-node))
		(dolist (add-edge add-edges)
		  (add-edge add-edge))))))


;; 2/1/19 We used to compare based on equalp, and flagged in a comment
;; that really a test based on env-equal would be better, since then we
;; wouldn't be dependent on the ordering of the bindings. That was ok
;; until we did possible-match scan-and-subst, which provides bindings
;; in a likely different order. So now we test based on a specialized
;; equality function for the entry, which uses env-equal.  However to
;; do this we needed to switch to an hhash-table, since CL hash tables
;; don't accept an arbitrary test.

(defstruct env-trig-entry			;; Moved outside env-triggeed class to see if helps perf -- 
									;; doesn't, really, but prevents sbcl from spewing forth zillions of warnings
  (rule-node nil)
  (env nil)
  (adds-hash nil))

(defc env-triggered nil (std-vars)
  (let ()
	(defr
	  (defl env-trig-entry-equal (e1 e2)
		(and (equal (env-trig-entry-rule-node e1) (env-trig-entry-rule-node e2))
			 (env-equal (env-trig-entry-env e1) (env-trig-entry-env e2))
			 (equal (env-trig-entry-adds-hash e1) (env-trig-entry-adds-hash e2))))
	  (let ((env-triggered-table (hmake-hash-table :test #'env-trig-entry-equal :size 1021)) ;; #'equalp
			(lte (make-env-trig-entry))
			(graph nil))

		(defm set-graph (g)
		  (setq graph g))

		(defm hash-adds (rule-node)	;; !!!!!!!!!!! NOP !!!!!!!!!!!
		  (block b
			(return-from b 0) ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			(let ((adds (! ((! (g get-rule-components) rule-node) adds))))
			  (let ((h (sxhash adds)))
				h))))

		;; Don't want to compare on ?root-var or ?this-obj since if
		;; otherwise envs are equal then we've already matched.
		;; 
		;; Retain binding order to assure later accurate comparison.

		(defm env-remove-bindings (env)
		  (env-prune env (! (std-vars var-cache))))

		(defm insert (rule-node env)
		  (timer 'env-triggered-insert
			(lambda ()
			  (let ((env (env-remove-bindings env)))
				(let ((te (lookup rule-node env)))
				  (when (null te)
					;; (print (list 'i rule-node (! (g hget) rule-node 'name) (hash-adds rule-node)))
					(setq te (make-env-trig-entry :rule-node rule-node :env env :adds-hash (hash-adds rule-node)))
					(setf (hgethash te env-triggered-table) te))
				  te)))))

		;; 2/26/18 Leave this as a dummy method, called from
		;; del-consequent-edges. We may want to resurrect the idea of
		;; retracting the match from deleted edges.

		(defm removed-edge (edge)
		  nil)

		(defm lookup (rule-node env)
		  (let ((env (env-remove-bindings env)))
			;; (print (list 'l rule-node (! (g hget) rule-node 'name) (hash-adds rule-node)))
			(setf (env-trig-entry-rule-node lte) rule-node)
			(setf (env-trig-entry-env lte) env)
			(setf (env-trig-entry-adds-hash lte) (hash-adds rule-node))
			(let ((te (hgethash lte env-triggered-table)))
			  (let ((r (and te t)))
				;; (print r)
				r))))

		(defm as-list () ;; For debug only
		  (list env-triggered-table))

		(defm clear () ;; For debug only
		  (hclrhash env-triggered-table)
		  nil)

		(defm rule-has-been-triggered (rule-node env)
		  (when am-traced
			(print (! (graph hget) rule-node 'name)))
		  (and (lookup rule-node env) t))))))

;; General surjective map, for many-to-one mappings. Default test for
;; input and result sets is equal.

(defc sur-map nil (&key (input-test #'equal) (res-test #'equal) (input-size 1021) (res-size 17))
  (let ((input-hash (make-hash-table :test input-test :size input-size)))
	(defm lookup (input)
	  (let ((r (gethash input input-hash)))
		(and r (hash-table-value-to-list r))))
	(defm lookup-one (input)					;; Assumes a single (or no) entry and just returns it
	  (block b
		(let ((r (gethash input input-hash)))
		  (when r
			(maphash (lambda (k v) (return-from b v)) r)))))
	(defm insert (input res)
	  (let ((res-hash (gethash input input-hash)))
		(when (null res-hash)
		  (setq res-hash (make-hash-table :test res-test :size res-size))
		  (setf (gethash input input-hash) res-hash))
		(setf (gethash res res-hash) res)
		res))
	(defm insert-list (input reslist)
	  (dolist (res reslist)
		(insert input res))
	  nil)
	(defm insert-one (input res)
	  (remove input)
	  (insert input res)
	  nil)
	(defm remove (input)
	  (remhash input input-hash)
	  nil)
	(defm remove-res (input res)
	  (let ((r (gethash input input-hash)))
		(when r 
		  (remhash res r)
		  (when (= (hash-table-count r) 0)
			(remove input)))
		nil))
	(defm inputs ()
	  (hash-table-key-to-list input-hash))
	(defm results ()
	  (let ((h (make-hash-table :test #'equal)))
		(maphash (lambda (k v)
				   (let ((res-hash v))
					 (maphash (lambda (k v)
								(let ((res v))
								  (setf (gethash res h) res)))
							  res-hash)))
				 input-hash)
		(hash-table-value-to-list h)))
	(defm clear ()
	  (clrhash input-hash)
	  nil)
	(defm count ()
	  (hash-table-count input-hash))
	(defm count-res (input)
	  (let ((res-hash (gethash input input-hash)))
		(if (null res-hash)
			0
			(hash-table-count res-hash))))
	(defm as-list ()
	  (let ((inputlist (hash-table-key-to-list input-hash)))
		(mapcar (lambda (input)
				  (list input (lookup input)))
				inputlist)))
	(defm count-stats ()
	  (let ((res-tot 0)
			(res-min 1e20)
			(res-max 0))
		(defstruct count-stat
		  (input-count nil)
		  (res-count-tot nil)
		  (res-count-avg nil)
		  (res-count-min nil)
		  (res-count-max nil))
		(maphash (lambda (k v)
				   (let ((c (hash-table-count v)))
					 (setq res-tot (+ c res-tot))
					 (when (> c res-max)
					   (setq res-max c))
					 (when (< c res-min)
					   (setq res-min c))))
				 input-hash)
		(let ((r (make-count-stat
				  :input-count (hash-table-count input-hash)
				  :res-count-tot res-tot
				  :res-count-avg (float (/ res-tot (hash-table-count input-hash)))
				  :res-count-min res-min
				  :res-count-max res-max)))
		  r)))
	(defm count-dist ()
	  (let ((r nil))
		(maphash (lambda (k v)
				   (let ((c (hash-table-count v)))
					 (setq r (cons (list k c) r))))
				 input-hash)
		(sort r (lambda (x y) (> (second x) (second y))))))))

(defc dummy-sur-map nil (&key (input-test #'equal) (res-test #'equal) (input-size 1021) (res-size 17))
  (let ()
	(defm lookup (input)
	  nil)
	(defm lookup-one (input)
	  nil)
	(defm insert (input res)
	  nil)
	(defm insert-list (input reslist)
	  nil)
	(defm remove (input)
	  nil)
	(defm remove-res (input res)
	  nil)
	(defm inputs ()
	  nil)
	(defm results ()
	  nil)
	(defm clear ()
	  nil)
	(defm count ()
	  0)
	(defm count-res (input)
	  0)
	(defm as-list ()
	  nil)
	(defm count-stats ()
	  nil)
	(defm count-dist ()
	  nil)))

;; x-sur-map
;; 
;; Uses hhash tables for input and res

(defc x-sur-map nil (&key (input-test #'equal) (res-test #'equal) (input-size 4093) (res-size 17))
  (let ((input-hash (hmake-hash-table :test input-test :size input-size)))
	(defm lookup (input)
	  (let ((r (hgethash input input-hash)))
		(and r (hhash-table-value-to-list r))))
	(defm lookup-one (input)					;; Assumes a single (or no) entry and just returns it
	  (block b
		(let ((r (hgethash input input-hash)))
		  (when r
			(hmaphash (lambda (k v) (return-from b v)) r)))))
	(defm insert (input res)
	  (let ((res-hash (hgethash input input-hash)))
		(when (null res-hash)
		  (setq res-hash (hmake-hash-table :test res-test :size res-size))
		  (setf (hgethash input input-hash) res-hash))
		(setf (hgethash res res-hash) res)
		nil))
	(defm remove (input)
	  (hremhash input input-hash)
	  nil)
	(defm remove-res (input res)
	  (let ((r (hgethash input input-hash)))
		(when r 
		  (hremhash res r)
		  (when (= (hhash-table-count r) 0)
			(remove input)))
		nil))
	(defm inputs ()
	  (hhash-table-key-to-list input-hash))
	(defm as-list ()
	  (let ((inputlist (hhash-table-key-to-list input-hash)))
		(mapcar (lambda (input)
				  (list input (lookup input)))
				inputlist)))
	(defm count-stats ()
	  (let ((res-tot 0)
			(res-min 1e20)
			(res-max 0))
		(defstruct count-stat
		  (input-count nil)
		  (res-count-tot nil)
		  (res-count-avg nil)
		  (res-count-min nil)
		  (res-count-max nil))
		(hmaphash (lambda (k v)
					(let ((c (hhash-table-count v)))
					  (setq res-tot (+ c res-tot))
					  (when (> c res-max)
						(setq res-max c))
					  (when (< c res-min)
						(setq res-min c))))
				  input-hash)
		(let ((r (make-count-stat
				  :input-count (hhash-table-count input-hash)
				  :res-count-tot res-tot
				  :res-count-avg (float (/ res-tot (hhash-table-count input-hash)))
				  :res-count-min res-min
				  :res-count-max res-max)))
		  r)))
	))



(defc dumper nil ()
  (let ((g nil)
		(elem-attrs nil)
		(gv-attrs nil)
		(nest-prefix 0)
		(node-map (make-sur-map))
		(node-set (make-sur-map))
		(designated-rule-edges (make-sur-map)))

	(defm set-graph (graph)
	  (setq g graph)
	  (setq elem-attrs (! (g get-elem-attrs)))
	  (setq gv-attrs (mapcar #'first (! (g query) '((?x gv-attr)) '(?x)))))
	
	(defm downcase (x)
	  (cond
	   ((symbolp x)
		(string-downcase (symbol-name x)))
	   ((stringp x)
		(string-downcase x))
	   (t x)))

	(defm designated-rule-entry-node (rule-node)
	  (let ((preds (! ((! (g get-rule-components) rule-node) preds))))
		(let ((s (sort preds (lambda (x y) (< (sxhash x) (sxhash y))))))
		  (symcat nest-prefix '- (! (g hget) rule-node 'name) '- (first (first s))))))

	;; "c:\Program Files (x86)\Graphviz2.38\bin\dot.exe" -Tjpeg fe.gv -o fe.jpg
	;; (let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "yyy3.gv" '(sigma even-func)))

	;; an attr is simply a node which must appear somewhere in an edge
	;; attrs is a list of attrs to draw. If attrs = t, draw all
	;; omitted-attrs is the set of attrs whose presence causes the edge not to be drawn, except if it's explicitly in the attrs list
	;; rules is a list of rules to draw. If rules = t, draw all

	(defm dump-gv-edges (file &key
							  (attrs t)
							  (omitted-attrs nil)
							  (rules t)
							  (omitted-rules nil)
							  (admit-string-attrs nil)					;; Admit edges with a string as the second member, regardless of whether it's in the attrs list
							  (rule-labels-only nil)					;; Only rule nodes will have labels. Edge labels unaffected.
							  (emit-labels t)
							  (omit-unmatched-rules t)
							  (emit-legend t)

							  (graph-type :digraph)						;; :digraph -- The data and each rule is a digraph, with its own cluster box.
																		;; :subgraph -- The data is a digraph with data edges and the rules are subgraphs
																		;; 			 of the data graph, each rule with its own cluster box.
																		;; :single-graph -- ??? ;; If true, generates a subgraph per rule, but not s separate digraph perf rule.
																							 ;; The rules in this way are shown directly connected to the data.
							  (edge-trace-rule-graph nil)
							  (gv-graph-props nil)						;; String of gv props, e.g., "rankdir=BT;"	Kind of a hack.
							  )
	  (let ()
		(defstruct node-entry
		  (name nil)
		  (do-not-emit nil)
		  (gv-attr-map (make-sur-map)))		;; gv-attr -> attr value
		(let ((omitted-attrs (append omitted-attrs elem-attrs '(triggered add pred del binding left right print note))))
		  (with-open-file (s file :direction :output)
			(defr
			  (defl string-concat (x y)
				(concatenate 'string x y))

			  (defl admit-edge (v)
				(let ((r 
					   (or 
						(and admit-string-attrs
							 (stringp (second v)))
						(and (eq attrs t)
							 (not (intersect v omitted-attrs)))
						(and (intersect v attrs)
							 (not (intersect (set-subtract v (intersect v attrs)) omitted-attrs))))))
				  ;; (print (list v r))
				  r))

			  ;; create-node-entry should only be called when we define all the node enrties as the first pass.

			  (defl create-node-entry (n &key two-input-op as-prop rule-name do-not-emit)
				(let ()
				  (! (node-set insert) n nil)
				  (let ((ne (make-node-entry)))
					(! (node-map insert-one) n ne)
					(setf (node-entry-name ne) (format nil "~a~a"
													   (if rule-name (format nil "~a-" rule-name) "")
													   n))
					(setf (node-entry-do-not-emit ne) do-not-emit)
					(if (and emit-labels
							 (not rule-labels-only)) ;; Rule labels are set separately
						(set-gv-attr n 'label n)
						(set-gv-attr n 'label ""))
					(set-gv-attr n 'fontname 'arial)
					(set-gv-attr n 'style 'filled)
					(when as-prop
					  (set-gv-attr n 'shape 'none))
					(dolist (gv-attr gv-attrs)
					  (when (! (g hget) n gv-attr)
						(! ((node-entry-gv-attr-map ne) insert-one) gv-attr (! (g hget) n gv-attr))))
					nil)))

			  (defl clone-node-entry (n)			;; Returns new sym
				(let ((r (gensym)))
				  (! (node-set insert) r nil)
				  (let ((old-ne (! (node-map lookup-one) n)))
					(let ((ne (make-node-entry)))
					  (setf (node-entry-name ne) r)
					  (setf (node-entry-gv-attr-map ne) (node-entry-gv-attr-map old-ne))		;; Note we don't copy the attr map
					  (! (node-map insert) r ne)
					  r))))

			  (defl do-not-emit (n)
				(let ((ne (! (node-map lookup-one) n)))
				  (node-entry-do-not-emit ne)))

			  (defl set-gv-attr (n attr value)
				(let ((ne (! (node-map lookup-one) n)))
				  (! ((node-entry-gv-attr-map ne) insert-one) attr value)
				  nil))

			  (defl get-gv-attr (n attr)
				(let ((ne (! (node-map lookup-one) n)))
				  (! ((node-entry-gv-attr-map ne) lookup-one) attr)))


			  (defl prepend-node-name (n l)
				(let ((ne (create-node-entry n)))
				  (setf (node-entry-name ne) (symcat l '- (node-entry-name ne)))
				  nil))
			  (defl get-node-name (n)
				(let ((ne (create-node-entry n)))
				  (node-entry-name ne)))

			  (defl set-node-label (n l)
				(let ((ne (create-node-entry n)))
				  (if emit-labels
					  (setf (node-entry-label ne) (format nil "~a" l))
					  (setf (node-entry-label ne) ""))
				  nil))
			  (defl append-to-node-label (n l)
				(let ((ne (create-node-entry n)))
				  (if emit-labels
					  (setf (node-entry-label ne) (string-concat (node-entry-label ne) l))
					  (setf (node-entry-label ne) ""))
				  nil))

			  (defl get-format (n)
				(let ()
				  (let ((ne (! (node-map lookup-one) n)))
					(let ((gv-attr-info-list (! ((node-entry-gv-attr-map ne) as-list))))
					  (let ((r (format nil "\"~a\" [" n)))
						(let ((comma ""))
						  (dolist (gv-attr-info gv-attr-info-list)
							(let ((attr (first gv-attr-info)))
							  (let ((value (first (second gv-attr-info))))
								(when value
								  (setq r (string-concat r (format nil "~a~a=\"~a\"" comma attr value)))
								  (setq comma ",")))))
						  (setq r (string-concat r (format nil "];" comma)))
						  r))))))

			  (defl old-get-format (n)
				(let ((ne (! (node-map lookup-one) n)))
				  (let ((r (format nil "\"~a\" [label=\"~a\"" (node-entry-name ne) (node-entry-label ne))))
					(when (node-entry-fontname ne)
					  (setq r (string-concat r (format nil ",fontname=~a" (node-entry-fontname ne)))))
					(when (node-entry-shape ne)
					  (setq r (string-concat r (format nil ",shape=~a" (node-entry-shape ne)))))
					(when (node-entry-color ne)
					  (setq r (string-concat r (format nil ",color=~a" (node-entry-color ne)))))
					(when (node-entry-style ne)
					  (setq r (string-concat r (format nil ",style=~a" (node-entry-style ne)))))
					(when (node-entry-width ne)
					  (setq r (string-concat r (format nil ",width=~a" (node-entry-width ne)))))
					(setq r (string-concat r (format nil "];")))
					r)))
			  (defl dump-gv-nodes ()
				(let ((nodes (! (node-set inputs))))
				  (dolist (n nodes)
					(when (not (do-not-emit n))
					  (format s "~a~%" (get-format n)))
					)))
			  (defl dump-gv-notes ()
				(when emit-legend
				  ;; Can add a large edge and cause undiagnosed some
				  ;; explosion in subqet processing. So now just
				  ;; append it following.
				  ;;
				  ;; (! (g add-edge) `(note body "attributes:" "\\n" ,@attrs))
				  (let ((title-fontsize (! (g query) '((set note title fontsize ?x)) '?x)))
					(let ((body-fontsize (! (g query) '((set note body fontsize ?x)) '?x)))
					  (let ((footer-fontsize (! (g query) '((set note footer fontsize ?x)) '?x)))
						(let ((notes (! (g get-edges) 'note)))
						  (let ((xxxnotes (cons `(note body "attributes:" "\\n" ,@attrs) notes)))		;; !!!!!!!!!!!!!!!!!!! Nop !!!!!!!!!!!!!!!!
							(let ((title "")
								  (body "")
								  (footer ""))
							  (let ((n ""))
								(dolist (note notes)
								  (when (eq (first note) 'note)
									(let ((type-data (if (member (second note) '(title footer body) :test #'eq)
														 (list (second note) (rest (rest note)))
														 (list 'body (rest note)))))
									  (let ((type (first type-data))
											(data (second type-data)))
										(dolist (p data)
										  (setq n (format nil "~a ~a" n p)))
										;; (setq n (format nil "~a~%" n))
										(setq n (format nil "~a\\n" n))
										(cond
										 ((eq type 'title)
										  (setq title (format nil "~a~a" title n)))
										 ((eq type 'body)
										  (setq body (format nil "~a~a" body n)))
										 ((eq type 'footer)
										  (setq footer (format nil "~a~a" footer n))))
										(setq n ""))))))
							  (let ()
								(format s "digraph Notes {~%")
								(format s "subgraph cluster {~%")
								(format s "style=filled;~%")
								(format s "color=springgreen;~%")
								(format s "label = \"Legend\";~%")
								(when (not (equal title ""))
								  (format s "__x1 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" title (or title-fontsize 18)))
								(when (not (equal body ""))
								  (format s "__x2 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" body (or body-fontsize 12)))
								(when (not (equal footer ""))
								  (format s "__x3 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" footer(or footer-fontsize 8)))
								(format s "__x1 -> __x2 -> __x3 [style=invis];~%")
								(format s "}~%")
								(format s "}~%"))))))))))
			  (defl dump-gv-edges-data (v)
				(when (not (and (eq (second v) 'type) (eq (third v) 'rule)))
				  (when (admit-edge v)
					#|
					(when (and (>= (length v) 1)
							   (! (g hget) (first v) 'color))
					  (format s "\"~a\" [color=~a,style=filled];~%" (first v) (! (g hget) (first v) 'color)))
					(when (and (= (length v) 4)
							   (! (g edge-exists) `(,(third v) two-input-op))
							   (! (g hget) (second v) 'color))
					  (format s "\"~a\" [color=~a,style=filled];~%" (second v) (! (g hget) (second v) 'color)))
					(when (and (= (length v) 4)
							   (! (g edge-exists) `(,(third v) two-input-op))
							   (! (g hget) (first v) 'color))
					  (format s "\"~a\" [color=~a,style=filled];~%" (first v) (! (g hget) (first v) 'color)))
					|#
					(let ((l (length v)))
					  (cond
					   ((and (= l 2)
							 (eq (second v) 'next))
						(format s "\"~a\" -> \"~a\" [label=\"~a\",style=\"setlinewidth(1)\"];~%"
								(first v) (first v) 
								(if emit-labels (second v) "")))
					   ((and (= l 3)
							 (eq (second v) 'fcn-color))
						nil)
					   ((and (= l 3)						;; Special-case coloring for rule30 nodes  *******************
							 (eq (second v) 'rule30val))
 						;; (format s "\"~a\" [color=~a,style=filled];~%" (first v) (if (and (numberp (third v)) (= (third v) 1)) "blue" "pink"))
						(format s "\"~a\" [color=~a,style=filled];~%" (first v) (if (and (numberp (third v)) (= (third v) 1)) "cyan" "magenta"))
						)
					   ((= l 3)
						(block b
						  (let ((in-node (first v)))
							(let ((out-node (third v)))
							  (let ((prop (second v)))
								(create-node-entry in-node)
								(create-node-entry out-node)
								(create-node-entry prop :do-not-emit t)
								(let ((prop-label (get-gv-attr prop 'label)))
								  (let ((edge-color (! (g hget) prop 'edge-color)))
									(let ((edge-color-string ""))
									  (dolist (r (list in-node out-node))
										(when (eq (! (g hget) r 'type) 'rule)
										  (let ((n (! (g hget) r 'name)))
											(when (member n omitted-attrs :test #'eq)
											  (return-from b nil))
											(when (memq graph-type '(:subgraph :single-graph))
											  (let ((rule-entry-node (designated-rule-entry-node r)))
												(when (not (! (designated-rule-edges lookup-one) (list r rule-entry-node)))
												  (! (designated-rule-edges insert) (list r rule-entry-node) (list r rule-entry-node))
												  (format s "\"~a\" -> \"~a\" [style=dotted,color=blue;weight=50];~%" 
														  r rule-entry-node)))))))
									  (when (eq prop 'note)
										(set-gv-attr out-node 'shape 'rectangle))
									  (when edge-color
										(setq edge-color-string (format nil ",color=~a" edge-color)))
									  (if emit-labels
										  (format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"~a];~%" 
												  in-node out-node prop-label edge-color-string)
										  (format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"~a];~%"
												  in-node out-node "" edge-color-string))))))))))
					   #|
					   ((eq (second v) 'zero)
						(create-node-entry (first v))
						(if emit-labels
							(set-gv-attr (first v) 'label (symcat (first v) '-z))
							(set-gv-attr (first v) 'label "")))
					   |#
					   ((and t
							 (= l 4)
							 (! (g edge-exists) `(,(third v) two-input-op)))
						(let ((i1 (first v))
							  (i2 (second v))
							  (fcn (third v))
							  (o (fourth v)))
						  (create-node-entry i1)
						  (create-node-entry i2)
						  (create-node-entry o)
						  (create-node-entry fcn :do-not-emit t)
						  (let ((fcn (clone-node-entry fcn)))
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i1 fcn)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i2 fcn)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" fcn o))))
					   ((= l 2)
						(let ((n (first v))
							  (p (second v)))
						  (create-node-entry n)
						  (create-node-entry p)
						  (let ((p (clone-node-entry p)))
							(set-gv-attr p 'shape 'none)
							(set-gv-attr p 'style 'solid)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\",label=\"\",arrowhead=none];~%" n p))))
					   ((and (= l 6)
							 (eq (first v) 'rd)) ;; Special case of a rule-dep edge		;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
						(let ((fmt (nth 1 v))
							  (r1n (nth 2 v))
							  (r1aes (format nil "~a" (nth 3 v)))
							  (r2pes (format nil "~a" (nth 4 v)))
							  (r2n (nth 5 v)))
						  #|
						  (format s "\"~a\" -> \"~a\" [fontname=arial,labelfontsize=6,headlabel=\"~a\",taillabel=\"~a\",style=\"setlinewidth(1)\"];~%" r1n r2n r2pes r1aes)
						  |#
						  (cond
						   ((= fmt 1)
							(create-node-entry r1n)
							(create-node-entry r2n)
							(format s "\"~a\" -> \"~a\" [fontname=arial,labelfontsize=6,headlabel=\"~a\",taillabel=\"~a\",style=\"setlinewidth(1)\"];~%" 
									r1n r2n "" ""))
						   ((= fmt 2)
							(let ((r1nx (symcat r1n 'x))
								  (r2nx (symcat r2n 'x)))
							  (create-node-entry r1nx)
							  (create-node-entry r2nx)
							  (format s "\"~ax\" -> \"~ax\" [fontname=arial,labelfontsize=6,headlabel=\"~a\",taillabel=\"~a\",style=\"setlinewidth(1)\"];~%" 
									  r1n r2n r2pes r1aes))))
						  ))
					   (t
						(dotimes (i l)
						  (let ((node (nth i v)))
							(format s "\"~a\" " node)
							(when (< i (- l 1))
							  (format s "-> "))))
						(format s ";~%")))))))
			  (defl dump-gv-edges-rules (v)
				(when (and (eq (second v) 'type)
						   (eq (third v) 'rule)
						   (or (eq rules t)
							   (member (! (g hget) (first v) 'name) rules))
						   (not (member (! (g hget) (first v) 'name) omitted-rules))
						   (not (! (g hget-edge-inverse) (first v) 'nested-rule)))
				  (let ((rule-node (first v)))
					(when (or (not omit-unmatched-rules)
						  (> (! (g get-matched) rule-node) 0))
					  (let ((rule-name (! (g hget) rule-node 'name)))
						(when (eq graph-type :digraph)
						  (format s "digraph \"~a\" {~%" rule-name))
						(dump-gv-rule rule-node)
						(when (eq graph-type :digraph)
						  (format s "}~%")))))))
			  (defl dump-gv-rule (rule)
				(when (or (not omit-unmatched-rules)
						  (> (! (g get-matched) rule) 0))
				  (let ((rule-components (! (g get-rule-components) rule))
						(rule-name (! (g hget) rule 'name)))
					(let ((p-rule-name (symcat nest-prefix '- rule-name)))
					  (let ((pred-edges (! (rule-components preds)))
							(del-edges (! (rule-components dels)))
							;;(add-edges (! (rule-components adds)))
							(add-edges (or (! (rule-components add-mains))
										   (! (rule-components adds))))
							(not-edges (! (rule-components nots)))
							(rule-nodes (! (rule-components all-nodes)))
							(i 0))
						(setq nest-prefix (+ nest-prefix 1))
						(setq node-map (make-sur-map))
						(setq node-set (make-sur-map))
						(when (memq graph-type '(:digraph :subgraph))
						  (format s "subgraph \"cluster-~a\" {~%" rule-name)
						  (format s "label = \"~a\";~%" rule-name))
						;; (format s "\"~a\" [label=\"~a\"];~%" rule rule-name)
						;; (format s "\"~a\" -> \"~a\" [label=\"~a\"];~%" "rules" rule-name  "")
						(dolist (rule-edges (list pred-edges del-edges add-edges not-edges))
						  (when (or (eq rule-edges pred-edges)
									(eq rule-edges add-edges))
							(setq i (+ i 1))
							(dolist (rule-edge rule-edges)
							  (when (not (and (eq rule-edges add-edges)
											  (or (eq (first rule-edge) 'print)
												  (eq (first rule-edge) 'note)
												  #| (eq (second rule-edge) 'rule) |# )))
								(let ((l (length rule-edge)))
								  (cond
								   ((and (= l 2)
										 (eq (second rule-edge) 'next))
									(format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" p-rule-name (first rule-edge) (first rule-edge))
									(format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a\",style=~a,fontname=arial,color=~a];~%"
											p-rule-name (first rule-edge)
											p-rule-name (first rule-edge)
											(second rule-edge)
											(if (eq rule-edges add-edges) "dashed" "solid")
											(if (eq rule-edges add-edges) "red" "black")))
								   ((= l 2)
									(let ((n (first rule-edge))
										  (p (second rule-edge)))
									  (create-node-entry n :rule-name p-rule-name)
									  (create-node-entry p :as-prop t :rule-name p-rule-name)
									  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"\",style=~a,fontname=arial,color=~a,arrowhead=none];~%"
											  p-rule-name n
											  p-rule-name p
											  (if (eq rule-edges add-edges) "dashed" "solid")
											  (if (eq rule-edges add-edges) "red" "black"))))
								   ((= l 3)
									;; (when (admit-edge (list (second rule-edge)))						;; !!!!!!!!!!!!!!!!!!!
									(when (not (member (second rule-edge) omitted-attrs :test #'eq))
									  (create-node-entry (first rule-edge) :rule-name p-rule-name)
									  (create-node-entry (third rule-edge) :rule-name p-rule-name)
									  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a~a\",style=~a,fontname=arial,color=~a];~%"
											  p-rule-name (first rule-edge)
											  p-rule-name (third rule-edge)
											  (if (member rule-edge del-edges :test #'equal) "X " "")
											  (second rule-edge)
											  (if (eq rule-edges add-edges) "dashed" "solid")
											  (cond
											   ((eq rule-edges add-edges) "red")
											   ((member rule-edge del-edges :test #'equal) "blue")
											   (t "black")))))
								   ((and (= l 4)
										 (! (g edge-exists) `(,(third rule-edge) two-input-op)))
									(let ((i1 (first rule-edge))
										  (i2 (second rule-edge))
										  (fcn-name (third rule-edge))
										  (o (fourth rule-edge)))
									  (let ((fcn (format nil "~a-~a-~a-~a-~a" p-rule-name fcn-name i1 i2 o))
											(color (! (g hget) fcn-name 'color)))
										(format s "\"~a\" [label=\"~a\",fontname=arial];~%" fcn fcn-name)
										(when color
										  (format s "\"~a\" [color=~a,style=filled]~%" fcn color))
										(format s "\"~a-~a\" -> \"~a\"" p-rule-name i1 fcn)
										(format s "[style=~a,color=~a];~%" 
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black"))
										(format s "\"~a-~a\" -> \"~a\"" p-rule-name i2 fcn)
										(format s "[style=~a,color=~a];~%"
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black"))
										(format s "\"~a\" -> \"~a-~a\"" fcn p-rule-name o)
										(format s "[style=~a,color=~a];~%"
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black")))))
								   (t
									(dotimes (i l)
									  (let ((rule-node (nth i rule-edge)))
										(format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" p-rule-name rule-node rule-node)))
									(dotimes (i l)
									  (let ((rule-node (nth i rule-edge)))
										(format s "\"~a-~a\"" p-rule-name rule-node)
										(when (< i (- l 1))
										  (format s " -> "))))
									(format s "[style=~a,fontname=arial,color=~a];~%"
											(if (eq rule-edges add-edges) "dashed" "solid")
											(if (eq rule-edges add-edges) "red" "black")))))))))
						(dump-gv-nodes)
						
						#|
											  (when (memq graph-type '(:digraph :subgraph))
						(format s "\"~a\" [label=\"~a\"];~%" rule rule))
											  |#

						(when (eq graph-type :single-graph)
									  (let ((rule-entry-node (designated-rule-entry-node rule)))
										(format s "\"~a\" -> \"~a\" [style=dotted,color=blue;weight=50];~%" 
												(get-node-name rule) (get-node-name rule-entry-node))))

						(let ((nested-rules (! (g hget-all) rule 'nested-rule)))
						  (dolist (nested-rule nested-rules)
							(let ((nested-rule-name (! (g hget) nested-rule 'name)))
							  (dump-gv-rule nested-rule)
							  (format s "\"~a\" -> \"~a\" [label=\"~a\"];~%" rule nested-rule  "NESTED-RULE"))))
						(when (memq graph-type '(:digraph :subgraph))
						  (format s "}~%"))
						(setq nest-prefix (- nest-prefix 1)))))))
			  (defl dump-rule-trace-edges ()
				(when edge-trace-rule-graph
				  (let ((attr (second (first (! (edge-trace-rule-graph get-edges-from-subqet) '(attr))))))
					(let ((attr (or attr 'r)))
					  (let ((edges (! (edge-trace-rule-graph get-edges) attr)))
						(dolist (edge edges)
						  (let ((rule-node1 (first edge)))
							(let ((rule-node2 (third edge)))
							  (create-node-entry rule-node1)
							  (set-node-label rule-node1 (! (g hget) rule-node1 'name))
							  (create-node-entry rule-node2)
							  (set-node-label rule-node2 (! (g hget) rule-node2 'name))
							  (format s "\"~a\" -> \"~a\" [label=\"\",style=solid,color=violet];~%" rule-node1 rule-node2)))))))))
			  (let ((*print-case* :downcase))
				(block b
				  (dump-gv-notes)
				  (format s "digraph G {~%")
				  (when gv-graph-props
					(format s "~a" gv-graph-props))
				  (! (g dump-edges) :dump-fcn #'dump-gv-edges-data :sort nil)
				  (dump-rule-trace-edges)
				  (dump-gv-nodes)
				  (when (memq graph-type '(:digraph))
					(format s "}~%"))
				  (! (g dump-edges) :dump-fcn #'dump-gv-edges-rules :sort nil)
				  (when (memq graph-type '(:subgraph :single-graph))
					(format s "}~%")))))))))))




(defc old-dumper nil ()
  (let ((g nil)
		(elem-attrs nil)
		(nest-prefix 0)
		(node-map (make-sur-map))
		(node-set (make-sur-map))
		(designated-rule-edges (make-sur-map)))

	(defm set-graph (graph)
	  (setq g graph)
	  (setq elem-attrs (! (g get-elem-attrs))))

	(defm downcase (x)
	  (cond
	   ((symbolp x)
		(string-downcase (symbol-name x)))
	   ((stringp x)
		(string-downcase x))
	   (t x)))

	(defm designated-rule-entry-node (rule-node)
	  (let ((preds (! ((! (g get-rule-components) rule-node) preds))))
		(let ((s (sort preds (lambda (x y) (< (sxhash x) (sxhash y))))))
		  (symcat nest-prefix '- (! (g hget) rule-node 'name) '- (first (first s))))))

	;; "c:\Program Files (x86)\Graphviz2.38\bin\dot.exe" -Tjpeg fe.gv -o fe.jpg
	;; (let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "yyy3.gv" '(sigma even-func)))

	;; an attr is simply a node which must appear somewhere in an edge
	;; attrs is a list of attrs to draw. If attrs = t, draw all
	;; omitted-attrs is the set of attrs whose presence causes the edge not to be drawn, except if it's explicitly in the attrs list
	;; rules is a list of rules to draw. If rules = t, draw all

	(defm dump-gv-edges (file &key
							  (attrs t)
							  (omitted-attrs nil)
							  (rules t)
							  (omitted-rules nil)
							  (string-attrs-only nil)
							  (rule-labels-only nil)					;; Only rule nodes will have labels. Edge labels unaffected.
							  (emit-labels t)
							  (omit-unmatched-rules t)
							  (emit-legend t)

							  (graph-type :digraph)						;; :digraph -- The data and each rule is a digraph, with its own cluster box.
																		;; :subgraph -- The data is a digraph with data edges and the rules are subgraphs
																		;; 			 of the data graph, each rule with its own cluster box.
																		;; :single-graph -- ??? ;; If true, generates a subgraph per rule, but not s separate digraph perf rule.
																							 ;; The rules in this way are shown directly connected to the data.
							  (edge-trace-rule-graph nil)
							  (gv-graph-props nil)						;; String of gv props, e.g., "rankdir=BT;"	Kind of a hack.
							  )
	  (let ()
		(defstruct node-entry
		  (name nil)
		  (label nil)
		  (fontname nil)
		  (shape nil)
		  (color nil)
		  (style nil)
		  (width nil))
		(let ((omitted-attrs (append omitted-attrs elem-attrs '(triggered add pred del binding left right print note))))
		  (with-open-file (s file :direction :output)
			(defr
			  (defl string-concat (x y)
				(concatenate 'string x y))
			  (defl admit-edge (v)
				(let ((r 
					   (or (and string-attrs-only
								(stringp (second v)))
						   (and (eq attrs t)
								(not (intersect v omitted-attrs)))
						   (and (intersect v attrs)
								(not (intersect (set-subtract v (intersect v attrs)) omitted-attrs))))))
				  ;; (print (list v r))
				  r))
			  (defl create-node-entry (n &key two-input-op as-prop rule-name)
				(let ()
				  (! (node-set insert) n nil)
				  (let ((ne (! (node-map lookup-one) n)))
					(or ne
						(let ((ne (make-node-entry)))
						  (setf (node-entry-name ne) (format nil "~a~a"
															 (if rule-name (format nil "~a-" rule-name) "")
															 n))
						  (if (and emit-labels
								   (not rule-labels-only)) ;; Rule labels are set separately
							  (setf (node-entry-label ne) (format nil "~a" n))
							  (setf (node-entry-label ne) ""))
						  (setf (node-entry-fontname ne) "arial")
						  (when as-prop
							(setf (node-entry-shape ne) "none"))
						  (! (node-map insert) n ne)
						  ne)))))
			  (defl prepend-node-name (n l)
				(let ((ne (create-node-entry n)))
				  (setf (node-entry-name ne) (symcat l '- (node-entry-name ne)))
				  nil))
			  (defl get-node-name (n)
				(let ((ne (create-node-entry n)))
				  (node-entry-name ne)))
			  (defl set-node-label (n l)
				(let ((ne (create-node-entry n)))
				  (if emit-labels
					  (setf (node-entry-label ne) (format nil "~a" l))
					  (setf (node-entry-label ne) ""))
				  nil))
			  (defl append-to-node-label (n l)
				(let ((ne (create-node-entry n)))
				  (if emit-labels
					  (setf (node-entry-label ne) (string-concat (node-entry-label ne) l))
					  (setf (node-entry-label ne) ""))
				  nil))
			  (defl set-node-shape (n s)
				(let ((ne (create-node-entry n)))
				  (setf (node-entry-shape ne) s)
				  nil))
			  (defl set-node-color (n c)
				(let ((ne (create-node-entry n)))
				  (setf (node-entry-color ne) c)
				  nil))
			  (defl set-node-style (n s)
				(let ((ne (create-node-entry n)))
				  (setf (node-entry-style ne) s)
				  nil))
			  (defl set-node-width (n w)
				(let ((ne (create-node-entry n)))
				  (setf (node-entry-width ne) w)
				  nil))
			  (defl get-format (n)
				(let ((ne (! (node-map lookup-one) n)))
				  (let ((r (format nil "\"~a\" [label=\"~a\"" (node-entry-name ne) (node-entry-label ne))))
					(when (node-entry-fontname ne)
					  (setq r (string-concat r (format nil ",fontname=~a" (node-entry-fontname ne)))))
					(when (node-entry-shape ne)
					  (setq r (string-concat r (format nil ",shape=~a" (node-entry-shape ne)))))
					(when (node-entry-color ne)
					  (setq r (string-concat r (format nil ",color=~a" (node-entry-color ne)))))
					(when (node-entry-style ne)
					  (setq r (string-concat r (format nil ",style=~a" (node-entry-style ne)))))
					(when (node-entry-width ne)
					  (setq r (string-concat r (format nil ",width=~a" (node-entry-width ne)))))
					(setq r (string-concat r (format nil "];")))
					r)))
			  (defl dump-gv-nodes ()
				(let ((nodes (! (node-set inputs))))
				  (dolist (n nodes)
					(format s "~a~%" (get-format n))
					)))
			  (defl dump-gv-notes ()
				(when emit-legend
				  ;; Can add a large edge and cause undiagnosed some
				  ;; explosion in subqet processing. So now just
				  ;; append it following.
				  ;;
				  ;; (! (g add-edge) `(note body "attributes:" "\\n" ,@attrs))
				  (let ((title-fontsize (! (g query) '((set note title fontsize ?x)) '?x)))
					(let ((body-fontsize (! (g query) '((set note body fontsize ?x)) '?x)))
					  (let ((footer-fontsize (! (g query) '((set note footer fontsize ?x)) '?x)))
						(let ((notes (! (g get-edges) 'note)))
						  (let ((xxxnotes (cons `(note body "attributes:" "\\n" ,@attrs) notes)))		;; !!!!!!!!!!!!!!!!!!! Nop !!!!!!!!!!!!!!!!
							(let ((title "")
								  (body "")
								  (footer ""))
							  (let ((n ""))
								(dolist (note notes)
								  (when (eq (first note) 'note)
									(let ((type-data (if (member (second note) '(title footer body) :test #'eq)
														 (list (second note) (rest (rest note)))
														 (list 'body (rest note)))))
									  (let ((type (first type-data))
											(data (second type-data)))
										(dolist (p data)
										  (setq n (format nil "~a ~a" n p)))
										;; (setq n (format nil "~a~%" n))
										(setq n (format nil "~a\\n" n))
										(cond
										 ((eq type 'title)
										  (setq title (format nil "~a~a" title n)))
										 ((eq type 'body)
										  (setq body (format nil "~a~a" body n)))
										 ((eq type 'footer)
										  (setq footer (format nil "~a~a" footer n))))
										(setq n ""))))))
							  (let ()
								(format s "digraph Notes {~%")
								(format s "subgraph cluster {~%")
								(format s "style=filled;~%")
								(format s "color=springgreen;~%")
								(format s "label = \"Legend\";~%")
								(when (not (equal title ""))
								  (format s "__x1 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" title (or title-fontsize 18)))
								(when (not (equal body ""))
								  (format s "__x2 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" body (or body-fontsize 12)))
								(when (not (equal footer ""))
								  (format s "__x3 [label = \"~a\",fontsize=~a,shape=rectangle,color=springgreen,style=filled];~%" footer(or footer-fontsize 8)))
								(format s "__x1 -> __x2 -> __x3 [style=invis];~%")
								(format s "}~%")
								(format s "}~%"))))))))))
			  (defl dump-gv-edges-data (v)
				(when (not (and (eq (second v) 'type) (eq (third v) 'rule)))
				  (when (admit-edge v)
					(when (and (>= (length v) 1)
							   (! (g hget) (first v) 'color))
					  (format s "\"~a\" [color=~a,style=filled];~%" (first v) (! (g hget) (first v) 'color)))
					(when (and (= (length v) 4)
							   (! (g edge-exists) `(,(third v) two-input-op))
							   (! (g hget) (second v) 'color))
					  (format s "\"~a\" [color=~a,style=filled];~%" (second v) (! (g hget) (second v) 'color)))
					(when (and (= (length v) 4)
							   (! (g edge-exists) `(,(third v) two-input-op))
							   (! (g hget) (first v) 'color))
					  (format s "\"~a\" [color=~a,style=filled];~%" (first v) (! (g hget) (first v) 'color)))
					(let ((l (length v)))
					  (cond
					   ((and (= l 2)
							 (eq (second v) 'next))
						(format s "\"~a\" -> \"~a\" [label=\"~a\",style=\"setlinewidth(1)\"];~%"
								(first v) (first v) 
								(if emit-labels (second v) "")))
					   ((and (= l 3)
							 (eq (second v) 'fcn-color))
						nil)
					   ((and (= l 3)
							 (eq (second v) 'rule30val))
 						;; (format s "\"~a\" [color=~a,style=filled];~%" (first v) (if (and (numberp (third v)) (= (third v) 1)) "blue" "pink"))
						(format s "\"~a\" [color=~a,style=filled];~%" (first v) (if (and (numberp (third v)) (= (third v) 1)) "cyan" "magenta"))
						)
					   ((= l 3)
						(block b
						  (let ((in-node (first v))
								(out-node (third v))
								(prop (second v)))
							(let ((in-node-color (! (g hget) prop 'in-node-color))
								  (out-node-color (! (g hget) prop 'out-node-color))
								  (edge-color (! (g hget) prop 'edge-color))
								  (edge-color-string ""))
							  (create-node-entry in-node)
							  (create-node-entry out-node)
							  (dolist (r (list in-node out-node))
								(when (eq (! (g hget) r 'type) 'rule)
								  (let ((n (! (g hget) r 'name)))
									;; (when (not (admit-edge (list n)))			;; !!!!!!!!!!!!!!!!!!!!!!!!
									(when (member n omitted-attrs :test #'eq)
									  (return-from b nil))
									(set-node-shape r (or (! (g hget) n 'shape) (! (g hget) 'rule 'shape)))
									(set-node-color r (or (! (g hget) n 'color) (! (g hget) 'rule 'color)))
									(set-node-style r (or (! (g hget) n 'style) (! (g hget) 'rule 'style)))
									(set-node-label r n)
									(when (memq graph-type '(:subgraph :single-graph))
									  (let ((rule-entry-node (designated-rule-entry-node r)))
										(when (not (! (designated-rule-edges lookup-one) (list r rule-entry-node)))
										  (! (designated-rule-edges insert) (list r rule-entry-node) (list r rule-entry-node))
										  (format s "\"~a\" -> \"~a\" [style=dotted,color=blue;weight=50];~%" 
												  (get-node-name r) (get-node-name rule-entry-node))))))))
							  (when (eq prop 'note)
								(set-node-shape out-node "rectangle"))
							  (let ((shape (! (g hget) in-node 'shape)))
								(when shape
								  (set-node-shape in-node shape)))
							  (let ((shape (! (g hget) out-node 'shape)))
								(when shape
								  (set-node-shape out-node shape)))
							  (let ((color (! (g hget) in-node 'color)))
								(cond
								 (color
								  (set-node-color in-node color)
								  (set-node-style in-node "filled"))
								 (in-node-color
								  (set-node-color in-node in-node-color)
								  (set-node-style in-node "filled"))))
							  (let ((color (! (g hget) out-node 'color)))
								(cond
								 (color
								  (set-node-color out-node color)
								  (set-node-style out-node "filled"))
								 (out-node-color
								  (set-node-color out-node out-node-color)
								  (set-node-style out-node "filled"))))
							  (when edge-color
								(setq edge-color-string (format nil ",color=~a" edge-color)))
							  (let ((in-node-name (get-node-name in-node))
									(out-node-name (get-node-name out-node)))
								(if emit-labels
									(format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"~a];~%" 
											in-node-name out-node-name prop edge-color-string)
									(format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"~a];~%"
											in-node-name out-node-name "" edge-color-string)))))))
					   ((eq (second v) 'zero)
						(append-to-node-label (first v) "-z"))
					   ((and (= l 4)
							 (! (g edge-exists) `(,(third v) two-input-op)))
						(let ((i1 (first v))
							  (i2 (second v))
							  (fcn-name (third v))
							  (o (fourth v)))
						  (let ((fcn (format nil "~a-~a" fcn-name o))
								(color (! (g hget) fcn-name 'color))
								(shape (! (g hget) fcn-name 'shape))
								(in-node-color (! (g hget) fcn-name 'in-node-color))
								(out-node-color (! (g hget) fcn-name 'out-node-color))
								(fcn-color (! (g hget) o 'fcn-color)))
							(when fcn-color
							  (setq color fcn-color))
							(create-node-entry fcn :two-input-op t)
							(create-node-entry i1)
							(create-node-entry i2)
							(create-node-entry o)
							(when in-node-color
							  (set-node-color i1 in-node-color)
							  (set-node-style i1 "filled")
							  (set-node-color i2 in-node-color)
							  (set-node-style i2 "filled"))
							(when out-node-color
							  (set-node-color o out-node-color)
							  (set-node-style o "filled"))
							(when color
							  (set-node-color fcn color)
							  (set-node-shape fcn "ellipse") ;; (if (= (random 5) 0) "invtrapezium" "none")
							  (set-node-style fcn "filled"))
							(when shape
							  (set-node-shape fcn shape))
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i1 fcn)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i2 fcn)
							(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" fcn o))))
					   ((= l 2)
						(let ((n (first v))
							  (p (second v)))
						  (create-node-entry n)
						  (create-node-entry p :as-prop t)
						  (format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\",label=\"\",arrowhead=none];~%" n p)))
					   ((and (= l 6)
							 (eq (first v) 'rd)) ;; Special case of a rule-dep edge		;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
						(let ((fmt (nth 1 v))
							  (r1n (nth 2 v))
							  (r1aes (format nil "~a" (nth 3 v)))
							  (r2pes (format nil "~a" (nth 4 v)))
							  (r2n (nth 5 v)))
						  #|
											  (format s "\"~a\" -> \"~a\" [fontname=arial,labelfontsize=6,headlabel=\"~a\",taillabel=\"~a\",style=\"setlinewidth(1)\"];~%" r1n r2n r2pes r1aes)
											  |#
						  (cond
						   ((= fmt 1)
							(create-node-entry r1n)
							(create-node-entry r2n)
							(format s "\"~a\" -> \"~a\" [fontname=arial,labelfontsize=6,headlabel=\"~a\",taillabel=\"~a\",style=\"setlinewidth(1)\"];~%" 
									r1n r2n "" ""))
						   ((= fmt 2)
							(let ((r1nx (symcat r1n 'x))
								  (r2nx (symcat r2n 'x)))
							  (create-node-entry r1nx)
							  (create-node-entry r2nx)
							  (format s "\"~ax\" -> \"~ax\" [fontname=arial,labelfontsize=6,headlabel=\"~a\",taillabel=\"~a\",style=\"setlinewidth(1)\"];~%" 
									  r1n r2n r2pes r1aes))))
						  ))
					   (t
						(dotimes (i l)
						  (let ((node (nth i v)))
							(format s "\"~a\" " node)
							(when (< i (- l 1))
							  (format s "-> "))))
						(format s ";~%")))))))
			  (defl dump-gv-edges-rules (v)
				(when (and (eq (second v) 'type)
						   (eq (third v) 'rule)
						   (or (eq rules t)
							   (member (! (g hget) (first v) 'name) rules))
						   (not (member (! (g hget) (first v) 'name) omitted-rules))
						   (not (! (g hget-edge-inverse) (first v) 'nested-rule)))
				  (let ((rule-node (first v)))
					(when (or (not omit-unmatched-rules)
						  (> (! (g get-matched) rule-node) 0))
					  (let ((rule-name (! (g hget) rule-node 'name)))
						(when (eq graph-type :digraph)
						  (format s "digraph \"~a\" {~%" rule-name))
						(dump-gv-rule rule-node)
						(when (eq graph-type :digraph)
						  (format s "}~%")))))))
			  (defl dump-gv-rule (rule)
				(when (or (not omit-unmatched-rules)
						  (> (! (g get-matched) rule) 0))
				  (let ((rule-components (! (g get-rule-components) rule))
						(rule-name (! (g hget) rule 'name)))
					(let ((p-rule-name (symcat nest-prefix '- rule-name)))
					  (let ((pred-edges (! (rule-components preds)))
							(del-edges (! (rule-components dels)))
							;;(add-edges (! (rule-components adds)))
							(add-edges (or (! (rule-components add-mains))
										   (! (rule-components adds))))
							(not-edges (! (rule-components nots)))
							(rule-nodes (! (rule-components all-nodes)))
							(i 0))
						(setq nest-prefix (+ nest-prefix 1))
						(setq node-map (make-sur-map))
						(setq node-set (make-sur-map))
						(when (memq graph-type '(:digraph :subgraph))
						  (format s "subgraph \"cluster-~a\" {~%" rule-name)
						  (format s "label = \"~a\";~%" rule-name))
						;; (format s "\"~a\" [label=\"~a\"];~%" rule rule-name)
						;; (format s "\"~a\" -> \"~a\" [label=\"~a\"];~%" "rules" rule-name  "")
						(dolist (rule-edges (list pred-edges del-edges add-edges not-edges))
						  (when (or (eq rule-edges pred-edges)
									(eq rule-edges add-edges))
							(setq i (+ i 1))
							(dolist (rule-edge rule-edges)
							  (when (not (and (eq rule-edges add-edges)
											  (or (eq (first rule-edge) 'print)
												  (eq (first rule-edge) 'note)
												  #| (eq (second rule-edge) 'rule) |# )))
								(let ((l (length rule-edge)))
								  (cond
								   ((and (= l 2)
										 (eq (second rule-edge) 'next))
									(format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" p-rule-name (first rule-edge) (first rule-edge))
									(format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a\",style=~a,fontname=arial,color=~a];~%"
											p-rule-name (first rule-edge)
											p-rule-name (first rule-edge)
											(second rule-edge)
											(if (eq rule-edges add-edges) "dashed" "solid")
											(if (eq rule-edges add-edges) "red" "black")))
								   ((= l 2)
									(let ((n (first rule-edge))
										  (p (second rule-edge)))
									  (create-node-entry n :rule-name p-rule-name)
									  (create-node-entry p :as-prop t :rule-name p-rule-name)
									  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"\",style=~a,fontname=arial,color=~a,arrowhead=none];~%"
											  p-rule-name n
											  p-rule-name p
											  (if (eq rule-edges add-edges) "dashed" "solid")
											  (if (eq rule-edges add-edges) "red" "black"))))
								   ((= l 3)
									;; (when (admit-edge (list (second rule-edge)))						;; !!!!!!!!!!!!!!!!!!!
									(when (not (member (second rule-edge) omitted-attrs :test #'eq))
									  (create-node-entry (first rule-edge) :rule-name p-rule-name)
									  (create-node-entry (third rule-edge) :rule-name p-rule-name)
									  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a~a\",style=~a,fontname=arial,color=~a];~%"
											  p-rule-name (first rule-edge)
											  p-rule-name (third rule-edge)
											  (if (member rule-edge del-edges :test #'equal) "X " "")
											  (second rule-edge)
											  (if (eq rule-edges add-edges) "dashed" "solid")
											  (cond
											   ((eq rule-edges add-edges) "red")
											   ((member rule-edge del-edges :test #'equal) "blue")
											   (t "black")))))
								   ((and (= l 4)
										 (! (g edge-exists) `(,(third rule-edge) two-input-op)))
									(let ((i1 (first rule-edge))
										  (i2 (second rule-edge))
										  (fcn-name (third rule-edge))
										  (o (fourth rule-edge)))
									  (let ((fcn (format nil "~a-~a-~a-~a-~a" p-rule-name fcn-name i1 i2 o))
											(color (! (g hget) fcn-name 'color)))
										(format s "\"~a\" [label=\"~a\",fontname=arial];~%" fcn fcn-name)
										(when color
										  (format s "\"~a\" [color=~a,style=filled]~%" fcn color))
										(format s "\"~a-~a\" -> \"~a\"" p-rule-name i1 fcn)
										(format s "[style=~a,color=~a];~%" 
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black"))
										(format s "\"~a-~a\" -> \"~a\"" p-rule-name i2 fcn)
										(format s "[style=~a,color=~a];~%"
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black"))
										(format s "\"~a\" -> \"~a-~a\"" fcn p-rule-name o)
										(format s "[style=~a,color=~a];~%"
												(if (eq rule-edges add-edges) "dashed" "solid")
												(if (eq rule-edges add-edges) "red" "black")))))
								   (t
									(dotimes (i l)
									  (let ((rule-node (nth i rule-edge)))
										(format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" p-rule-name rule-node rule-node)))
									(dotimes (i l)
									  (let ((rule-node (nth i rule-edge)))
										(format s "\"~a-~a\"" p-rule-name rule-node)
										(when (< i (- l 1))
										  (format s " -> "))))
									(format s "[style=~a,fontname=arial,color=~a];~%"
											(if (eq rule-edges add-edges) "dashed" "solid")
											(if (eq rule-edges add-edges) "red" "black")))))))))
						(dump-gv-nodes)
						
						#|
											  (when (memq graph-type '(:digraph :subgraph))
						(format s "\"~a\" [label=\"~a\"];~%" rule rule))
											  |#

						(when (eq graph-type :single-graph)
									  (let ((rule-entry-node (designated-rule-entry-node rule)))
										(format s "\"~a\" -> \"~a\" [style=dotted,color=blue;weight=50];~%" 
												(get-node-name rule) (get-node-name rule-entry-node))))

						(let ((nested-rules (! (g hget-all) rule 'nested-rule)))
						  (dolist (nested-rule nested-rules)
							(let ((nested-rule-name (! (g hget) nested-rule 'name)))
							  (dump-gv-rule nested-rule)
							  (format s "\"~a\" -> \"~a\" [label=\"~a\"];~%" rule nested-rule  "NESTED-RULE"))))
						(when (memq graph-type '(:digraph :subgraph))
						  (format s "}~%"))
						(setq nest-prefix (- nest-prefix 1)))))))
			  (defl dump-rule-trace-edges ()
				(when edge-trace-rule-graph
				  (let ((attr (second (first (! (edge-trace-rule-graph get-edges-from-subqet) '(attr))))))
					(let ((attr (or attr 'r)))
					  (let ((edges (! (edge-trace-rule-graph get-edges) attr)))
						(dolist (edge edges)
						  (let ((rule-node1 (first edge)))
							(let ((rule-node2 (third edge)))
							  (create-node-entry rule-node1)
							  (set-node-label rule-node1 (! (g hget) rule-node1 'name))
							  (create-node-entry rule-node2)
							  (set-node-label rule-node2 (! (g hget) rule-node2 'name))
							  (format s "\"~a\" -> \"~a\" [label=\"\",style=solid,color=violet];~%" rule-node1 rule-node2)))))))))
			  (let ((*print-case* :downcase))
				(block b
				  (dump-gv-notes)
				  (format s "digraph G {~%")
				  (when gv-graph-props
					(format s "~a" gv-graph-props))
				  (! (g dump-edges) :dump-fcn #'dump-gv-edges-data :sort nil)
				  (dump-rule-trace-edges)
				  (dump-gv-nodes)
				  (when (memq graph-type '(:digraph))
					(format s "}~%"))
				  (! (g dump-edges) :dump-fcn #'dump-gv-edges-rules :sort nil)
				  (when (memq graph-type '(:subgraph :single-graph))
					(format s "}~%")))))))))))



;; This class of set utils assumes that lists passed in are true sets,
;; i.e., no dups. And though generally ordering is not a concern,
;; order within a list is preserved, in that if x and y are elements
;; of the input list l, ordered from left to right as xi and yj such
;; that i<j, then any subset of l which contains x and and y will in
;; turn be indexed as xk and yl such that k<l. This helps retain order
;; as desired for the qet subclass.

(defc set-utils nil nil
  (let ()

	;; Generate all subsets of the elements of l

	(defm subsets (l)
	  (defr
		(defl within (a l)
		  (defr
			(defl within1 (a l)
			  (let ((r (list (list a))))
				(dolist (x l)
				  (setq r (append (list (cons a x)) r)))
				r))
			(append l (within1 a l))))
		(defl subsets1 (l)
		  (if (null l)
			  nil
			  (within (first l) (subsets1 (rest l)))))
		(cons nil (subsets1 l))))

	;; Generate all subsets of length n of the elements of l
	;; See poly.lisp for more exposition on the combinations fcn

	(defm subsets-length-n (l n)
	  (defr
		(defl combinations (m x)
		  (let ((n (length x)))
			(let ((r nil))
			  (defr
				(defl comb1 (k m l)
				  (if (= m 0)
					  (setq r (cons l r))
					  (do ((i k (1+ i)))
						  ((= i n) nil)
						  (comb1 (1+ i) (1- m) (cons (nth i x) l)))))
				(comb1 0 m nil))
			  r)))
		(combinations n l)))
))

;; Qet-utils, though derived from set-utils, assumes that lists may
;; have dups (i.e., are multi-sets). This can cause the same subset to
;; be generated multiple times, hence we deduplicate.

(defc qet-utils set-utils nil
  (let ()

	(defm subqets (q)
	  (dedup-list (subsets q)))

	(defm subqets-length-n (q n)
	  (dedup-list (subsets-length-n (reverse q) n)))

	(defm is-subqet (subqet qet)
	  (let ((q qet))
		(block b1
		  (dolist (s subqet)
			(block b2
			  (loop
			   (cond
				((null q)
				 (return-from b1 nil))
				((equal s (first q))
				 (setq q (rest q))
				 (return-from b2 nil))
				(t
				 (setq q (rest q)))))))
		  t)))))

;; A worthy experiment, tested with hunion. It's about twice as slow
;; as using the pointer hackery in this class inline. We'll keep this
;; around but will likely continue to use the by-hand version for
;; low-level operations.

(defc appender nil nil
  (let ((p0 (list nil)))
	(let ((p p0))
	  (defm append (x)
		(setf (rest p) (list x))
		(setq p (rest p))
		nil)
	  (defm as-list ()
		(let ((r (rest p0)))
		  (setf (rest p0) nil)
		  (setq p p0)
		  r)))))

(defun is-var-name (a)
  (and (symbolp a)
	   (eq (aref (symbol-name a) 0) #\?)))

(defun is-node-name (a)
  (or (is-var-name a)
	  (and (not (null a))
		   (symbolp a)
		   (eq (aref (symbol-name a) 0) #\N)
		   (digit-char-p (aref (symbol-name a) 1)))))

(defun is-node (x)
  (or (stringp x)
	  (numberp x)
	  (symbolp x)))

;; Checks for node of the form n<integer>, i.e., was generated by new-obj-node

(defun is-new-obj-node (node)
  (block b
	(when (symbolp node)
	  (let ((s (symbol-name node)))
		(let ((l (length s)))
		  (when (equal (aref s 0) #\N)
			(dotimes (i (- l 1))
			  (when (not (digit-char-p (aref s (+ i 1))))
				(return-from b nil)))
			t))))))


;; Also see edge-exists in the class graph. is-edge really means "is
;; in edge form" so must be used with care.

(defun is-edge (x)
  (listp x))

(defun is-sigma-edge (x)
  (and (is-edge x)
	   (eq (second x) 'sigma)))

(defun is-sigma-node (x)
  (and (is-node x)
	   (integerp x)
	   (>= x 0)))

(defun is-new-pool-node (node)
  (and (symbolp node)
	   (let ((name (symbol-name node)))
		 (and (eq (aref name 0) #\N)
			  (eq (aref name 1) #\N)))))

(defun is-scoped-new-pool-node (node)
  (and (symbolp node)
	   (let ((name (symbol-name node)))
		 (and (eq (aref name 0) #\S)
			  (eq (aref name 1) #\N)))))

(defun get-node (edge)
  (first edge))

(defun get-attr (edge)
  (second edge))

(defun get-value (edge)
  (third edge))

(defun env-lookup (node env &key (idempotent t))
  (block el
	(dolist (binding env)
	  (when (equal node (first binding))
		(return-from el (second binding))))
	(if idempotent  		;; was nil -- need node due to orig-match algorithm
		node
		nil)))

(defun env-equal (env1 env2)
  (block ee
    (if (not (= (length env1) (length env2)))
		nil
		(let ()
		  (dolist (b1 env1)
			(let ((v1 (second b1)))
			  (let ((v2 (env-lookup (first b1) env2 :idempotent nil)))
				(when (not (equal v1 v2))
				  (return-from ee nil)))))
		  t))))


;; Remove all bindings from env which contain a member of varlist on the variable side of the binding
;; Retain binding order of remaining entries.

(defun env-prune (env varlist)
  (mapcan (lambda (b)
			(if (memq (first b) varlist)
				nil
				(list b)))
		  env))

;; Take a list of env lists (list of envs envs-list), each env list is
;; the set of envs from matching one pat against a list of obj-edges.
;; 									  
;; Do the cross product by building up a set of envs. Look for 
;; conflicts at each step, and toss those envs in conflict, so they
;; are not further expanded.

(defun cross-aux2-info (info))

(defun cross-aux2 (envs-list &key record-lengths rule-trace-info)
  (timer 'cross-aux2
	(lambda ()
	  (let ((lengths (and record-lengths
						  (list (length envs-list) (mapcar (lambda (x) (length x)) envs-list)))))
		(defr
		  (defl cross-aux2-aux (envs-list)
			(let ((env-list-out '(()))
				  (r1 nil))
			  (dolist (envs envs-list)
				(dolist (p env-list-out)
				  (dolist (env envs)
					(let ((new-env (env-no-conflict-dedup (append env p))))
					  (when new-env
						(setq r1 (cons new-env r1))))))
				(setq env-list-out r1)
				(setq r1 nil))
			  env-list-out))
		  (let ((r (cross-aux2-aux envs-list)))
			(when record-lengths
			  (cross-aux2-info (cons (length r) lengths)))
			(when rule-trace-info
			  (let ((rule-node (first rule-trace-info)))
				(let ((rule-name (second rule-trace-info)))
			  (print `(rule-trace cross-aux2 rule ,rule-node ,rule-name envs-list-in ,envs-list envs-list-result ,r)))))
			r))))))

;; Experiment -- should be more efficient sorted, but does not work
;; correctly and not clear why. The std fft run with n=3 works
;; properly, but a large rule-30 rule-dep exec fails with explosion issues.

(let ((h (make-hash-table :test #'equal)))
  (defun new-cross-aux2 (envs-list &key (record-lengths t))
	(timer 'cross-aux2
	  (lambda ()
		(let ((envs-list (sort envs-list (lambda (x y) (< (length x) (length y))))))
		  (let ((lengths (and record-lengths
						  (list (length envs-list) (mapcar (lambda (x) (length x)) envs-list)))))
			(defr
			  (defl cross-aux2-aux (envs-list)
				(let ((env-list-out '(()))
					  (r1 nil))
				  (dolist (envs envs-list)
					(dolist (p env-list-out)
					  (dolist (env envs)
						(let ((new-env (env-no-conflict-dedup (append env p))))
						  (when new-env
							(setq r1 (cons new-env r1))))))
					(setq env-list-out r1)
					(setq r1 nil))
				  env-list-out))
			  (let ((r (gethash envs-list h)))
				(or r
					(let ((r (cross-aux2-aux envs-list)))
					  (setf (gethash envs-list h) r)
					  (when record-lengths
						(cross-aux2-info (cons (length r) lengths)))
					  r))))))))))

;; See notes in previous src file versions about tests of versions of
;; this function. In sum, this style, with no hash caches or other
;; tricks, is best.

(defun env-no-conflict-dedup (env)
  (timer 'env-no-conflict-dedup
	(lambda ()
	  (block b
		(let ((renv nil))
		  (dolist (binding env)
			(let ((var (first binding))
				  (val (second binding)))
			  (let ((rval (env-lookup var renv :idempotent nil)))
				(when (null rval)
				  (setq renv (cons binding renv))
				  (setq rval val))
				(when (not (equal val rval))
				  (return-from b nil)))))
		  renv)))))

(defun concat-envs (envlist)
  (if (null envlist)
	  nil
	  (append (first envlist)
			  (concat-envs (rest envlist)))))

;; Given a list of envs, finds the set with the smallest number of
;; different bound pairs, e.g., (x y) is a diff, (x x) is not.
;; Returns a list of envs
;;
;; [9/26/15: Only useful for iso method, since otherwise we do not
;; include constant-to-constant matches in the bindings]

(defun min-diff-env (envlist)
  (defun ndiffs (env)
	(let ((n 0))
	  (dolist (binding env)
		(when (not (equal (first binding) (second binding)))
		  (setq n (+ n 1))))
	  n))
  (let ((n 1e30)
		(r nil))
	(dolist (env envlist)
	  (let ((d (ndiffs env)))
		(cond
		 ((< d n)
		  (setq n d)
		  (setq r (list env)))
		 ((= d n)
		  (setq r (append r (list env))))
		 (t nil))))
	r))

(defun filter-vars (l)
  (let ((r nil))
	(dolist (x l)
	  (when (not (is-var-name x))
		(setq r (nconc r (list x)))))
	r))

(defun filter-in-vars (l)
  (let ((r nil))
	(dolist (x l)
	  (when (is-var-name x)
		(setq r (nconc r (list x)))))
	r))

(defun filter-out (list test)
  (let ((r nil))
	(dolist (x list)
	  (when (not (funcall test x))
		(setq r (cons x r))))
	r))

(defun filter-in (list test)
  (let ((r nil))
	(dolist (x list)
	  (when (funcall test x)
		(setq r (cons x r))))
	r))


;; 1/26/19 Modified these intersection and union operations to preserve order

;; See notes in previous file versions. This basic n^2 loop wins in
;; speed over other versions using hash tables.
;; 
;; Note uses equal and test is not passed in
;;

(let ((p0 (list nil)))
  (defun intersect (l1 l2)
	(timer 'intersect
	  (lambda ()
		(cond
		 ((eq l1 t)
		  l2)
		 ((eq l2 t)
		  l1)
		 (t
		  (let ((p p0))
			(dolist (x1 l1)
			  (dolist (x2 l2)
				(when (equal x1 x2)
				  (setf (rest p) (list x1))
				  (setq p (rest p))))))
		  (let ((r (rest p0)))
			(setf (rest p0) nil)
			r)))))))

(let ((p0 (list nil)))
  (defun intersect-with-test (l1 l2 &key (test #'equal))
	(timer 'intersect-with-test
	  (lambda ()
		(cond
		 ((eq l1 t)
		  l2)
		 ((eq l2 t)
		  l1)
		 (t
		  (let ((p p0))
			(dolist (x1 l1)
			  (dolist (x2 l2)
				(when (funcall test x1 x2)
				  (setf (rest p) (list x1))
				  (setq p (rest p))))))
		  (let ((r (rest p0)))
			(setf (rest p0) nil)
			r)))))))

(let ((test #'equal))
  (let ((h (make-hash-table :size 1021 :test test))) ;; 32768
	(let ((p0 (list nil)))
	  (defun hunion (l1 l2)
		(timer 'hunion
		  (lambda ()
			(cond
			 ((eq l1 t)
			  l2)
			 ((eq l2 t)
			  l1)
			 (t
			  (let ((p p0))
				(clrhash h)
				(dolist (x l1)
				  (when (null (gethash x h))
					(setf (gethash x h) x)
					(setf (rest p) (list x))
					(setq p (rest p))))
				(dolist (x l2)
				  (when (null (gethash x h))
					(setf (gethash x h) x)
					(setf (rest p) (list x))
					(setq p (rest p))))
				(let ((r (rest p0)))
				  (setf (rest p0) nil)
				  r))))))))))


;; Exp hunion using an appender class. Slow -- see appender class above

(let ((test #'equal))
  (let ((h (make-hash-table :size 1021 :test test))) ;; 32768
	(let ((app (make-appender)))
	  (defun exp-hunion (l1 l2)
		(timer 'hunion
		  (lambda ()
			(cond
			 ((eq l1 t)
			  l2)
			 ((eq l2 t)
			  l1)
			 (t
			  (let ()
				(clrhash h)
				(dolist (x l1)
				  (when (null (gethash x h))
					(setf (gethash x h) x)
					(! (app append) x)))
				(dolist (x l2)
				  (when (null (gethash x h))
					(setf (gethash x h) x)
					(! (app append) x)))
				(! (app as-list)))))))))))

(let ((test #'eq))
  (let ((h (make-hash-table :size 1021 :test test))) ;; 32768
	(let ((p0 (list nil)))
	  (defun eq-hunion (l1 l2)
		(timer 'hunion
		  (lambda ()
			(cond
			 ((eq l1 t)
			  l2)
			 ((eq l2 t)
			  l1)
			 (t
			  (let ((p p0))
				(clrhash h)
				(dolist (x l1)
				  (when (null (gethash x h))
					(setf (gethash x h) x)
					(setf (rest p) (list x))
					(setq p (rest p))))
				(dolist (x l2)
				  (when (null (gethash x h))
					(setf (gethash x h) x)
					(setf (rest p) (list x))
					(setq p (rest p))))
				(let ((r (rest p0)))
				  (setf (rest p0) nil)
				  r))))))))))

(defun set-equal (l1 l2 &key (test #'equal))
  (= (length l1)
	 (length l2)
	 (length (intersect-with-test l1 l2 :test test))))

;; l1 \ l2

;; Tested this and with intersect, and this is faster.
;;
;; Todo: Retain ordering. Not clear needed since this is used mainly
;; on sets of edges, whereas edges themselves need order preservation
;; more.

(defun set-subtract (l1 l2 &key (test #'equal))
  (timer 'set-subtract
	(lambda ()
	  (let ((r nil))
		(dolist (x l1)
		  (when (not (member x l2 :test test))
			(setq r (cons x r))))
		r))))

(defun old-set-subtract (l1 l2 &key (test #'equal))
  (timer 'set-subtract
	(lambda ()
	  (let ((i (intersect l1 l2))
			(r nil))
		(dolist (x1 l1)
		  (when (not (member x1 i :test test))
			(setq r (cons x1 r))))
		r))))

;; s1 and s2 are each sets of sets. Each set from s1 is intersected
;; with each set from s2. If the result is non-empty, then s1 and s2
;; are unioned into the final result.
;;
;; Note this differs from traditional def!!!!!!!!!!!! Is this why it's "x"?

(defun xcross-intersect (sets1 sets2)
  (let ((r nil))
	(dolist (s1 sets1)
	  (dolist (s2 sets2)
		(when (intersect s1 s2)
		  (setq r (hunion r (list s1)))
		  (setq r (hunion r (list s2))))))
	r))

(defun dedup-list (list &key (id-func (lambda (x) x)))
  (let ((objhash (make-hash-table :size 128 :test #'equal)))
	(mapcan (lambda (obj)
			  (cond
			   ((null obj)
				nil)
			   ((gethash (funcall id-func obj) objhash)
				nil)
			   (t
				(setf (gethash (funcall id-func obj) objhash) obj)
				(list obj))))
			list)))

;; l is a list of lists. Returns the list of the rest of those lists. I.e., rest as a vector operation

(defun restl (l)
  (if (null l)
	  nil
	  (cons (rest (first l)) (restl (rest l)))))

;; l is a list of lists. Returns the list of the first of those lists. I.e., first as a vector operation

(defun firstl (l)
  (if (null l)
	  nil
	  (cons (first (first l)) (firstl (rest l)))))

;; Like mapcar, but omits null results from the fcn call.
;;
;; Retains order of incoming list, i.e., (e1 e2 ...) => ((f e1) (f e2) ...)

(defun mapcad (fcn list)
  (let ((p0 (list nil)))
	(let ((p p0))
	  (dolist (l list)
		(let ((v (funcall fcn l)))
		  (when v
			(setf (rest p) (list v))
			(setq p (rest p)))))
	  (let ((r (rest p0)))
		(setf (rest p0) nil)
		r))))

;; mapunion and mapsunion, using an optimized internal function named using our old conventions.
;; The s in mapsunion means strict, which means return nil if any fcn result is nil
;;
;; These retain the order of the incoming lists, i.e., (e1 e2 ...) => ((f e1) (f e2) ...), 
;; where duplicates are discarded to the right, i.e., if fi = fj and j>i, then fj is omitted.

(defr
  (defl xmapcand1 (fcn list strict)		;; Version optimized for common case of a single arg list
	(block b
	  (let ((p0 (list nil)))
		(let ((p p0))
		  (let ((elem-hash (make-hash-table :size 512 :test #'equal)))
			(dolist (l list)
			  (let ((v (funcall fcn l)))
				(when (and strict
						   (null v))
				  (return-from b nil))
				(dolist (elem v)
				  (when (null (gethash elem elem-hash))
					(setf (gethash elem elem-hash) elem)
					(setf (rest p) (list elem))
					(setq p (rest p))))))
			(let ((r (rest p0)))
			  (setf (rest p0) nil)
			  r))))))
	(defl xmapcand (fcn lists strict)
	  (block b
		(let ((p0 (list nil)))
		  (let ((p p0))
			(let ((elem-hash (make-hash-table :size 512 :test #'equal)))
			  (defr
				(defl m (lists)
				  (if (null (first lists))
					  nil
					  (let ((v (apply fcn (firstl lists))))
						(when (and strict
								   (null v))
						  (return-from b nil))
						(dolist (elem v)
						  (when (null (gethash elem elem-hash))
							(setf (gethash elem elem-hash) elem)
							(setf (rest p) (list elem))
							(setq p (rest p))))
						(m (restl lists)))))
				(m lists)
				(let ((r (rest p0)))
				  (setf (rest p0) nil)
				  r)))))))
	(defun mapunion (fcn &rest lists)
	  (timer 'mapunion
		(lambda ()
		  (if (= (length lists) 1)
			  (xmapcand1 fcn (first lists) nil)
			  (xmapcand fcn lists nil)))))
	(defun mapsunion (fcn &rest lists)
	  (timer 'mapsunion
		(lambda ()
		  (if (= (length lists) 1)
			  (xmapcand1 fcn (first lists) t)
			  (xmapcand fcn lists t))))))

(defun memq (x l)
  (member x l :test #'eq))

(defun memqq (x l)
  (member x l :test (lambda (x y) (and (eq (first x) (first y)) (eq (second x) (second y))))))

(defun hash-table-to-list (h)
  (let ((r nil))
	(maphash (lambda (k v)
			   (setq r (cons (list k v) r)))
			 h)
	r))

(defun hash-table-key-to-list (h)
  (let ((r nil))
	(maphash (lambda (k v)
			   (setq r (cons k r)))
			 h)
	r))

(defun hash-table-value-to-list (h)
  (let ((r nil))
	(maphash (lambda (k v)
			   (setq r (cons v r)))
			 h)
	r))

(let ((perf-hash (make-hash-table :test #'eq)))
  (defstruct timerec 
	(sum 0)
	(count 0)
	(type 'time)) ;; { time, gen }
  (let ((display-order		;; Any not in this list go on the end
		 '(
		   main
		   me-tested
		   me-matched
		   me-failed
		   me-matched-new-edges
		   me-matched-not-new-edges
		   me-efficiency
		   me-redundancy
		   execute-all-objs
		   match-and-execute-rule
		   all-matches
		   all-matches-aux
		   match-one-edge
		   possible-match-fcn
		   possible-match
		   possible-match-false
		   possible-match-true
		   expand-rule-obj-edges
		   expand-rule-obj-edges-len
		   expand-edges
		   bipartite-breadth-rule-walk-seq
		   var-match-filter-edges
		   env-no-conflict-dedup
		   add-consequent-edges
		   add-consequent-edges-per-env
		   env-triggered-insert
		   env-triggered-removed-edge
		   already-env-triggered-rules
		   hunion
		   intersect
		   intersect-with-test
		   query-get-edges
		   )))
	(defun timer (name thunk)
	  (let ((start (get-internal-real-time)))
		(let ((v (funcall thunk)))
		  (let ((end (get-internal-real-time)))
			(let ((deltat (- end start)))
			  (let ((timerec (gethash name perf-hash)))
				(when (null timerec)
				  (setq timerec (make-timerec))
				  (setf (gethash name perf-hash) timerec))
				(setf (timerec-sum timerec) (+ deltat (timerec-sum timerec)))
				(setf (timerec-count timerec) (+ 1 (timerec-count timerec))))))
		  v)))
	(defun bool-timer (name-true name-false thunk)
	  (let ((start (get-internal-real-time)))
		(let ((v (funcall thunk)))
		  (let ((end (get-internal-real-time)))
			(let ((deltat (- end start)))
			  (let ((name (if v name-true name-false)))
				(let ((timerec (gethash name perf-hash)))
				  (when (null timerec)
					(setq timerec (make-timerec))
					(setf (gethash name perf-hash) timerec))
				  (setf (timerec-sum timerec) (+ deltat (timerec-sum timerec)))
				  (setf (timerec-count timerec) (+ 1 (timerec-count timerec)))))))
		  v)))
	(defun gstat (name statfcn thunk)
	  (let ((v (funcall thunk)))
		(let ((timerec (gethash name perf-hash)))
		  (when (null timerec)
			(setq timerec (make-timerec :type 'gen))
			(setf (gethash name perf-hash) timerec))
		  (setf (timerec-sum timerec) (funcall statfcn v (timerec-sum timerec)))
		  (setf (timerec-count timerec) (+ 1 (timerec-count timerec))))
		v))
	(defun perf-stats ()
	  (defr
		(defl div (x y)
		  (if (= y 0) most-positive-single-float (/ x y)))
		(let ()
		  (let ((eff (make-timerec :type 'gen))
				(red (make-timerec :type 'gen)))
			(let ((new-edges (timerec-sum (or (gethash 'me-matched-new-edges perf-hash) (make-timerec))))
				  (not-new-edges (timerec-sum (or (gethash 'me-matched-not-new-edges perf-hash) (make-timerec))))
				  (tested (timerec-sum (or (gethash 'me-tested perf-hash) (make-timerec))))
				  (matched (timerec-sum (or (gethash 'me-matched perf-hash) (make-timerec)))))
			  (setf (timerec-sum eff) (div (float new-edges) (float tested)))
			  (setf (timerec-count eff) 1)
			  (setf (timerec-sum red) (div (float not-new-edges) (float matched)))
			  (setf (timerec-count red) 1)
			  (setf (gethash 'me-efficiency perf-hash) eff)
			  (setf (gethash 'me-redundancy perf-hash) red)))
		  (let ((m 0))
			(maphash (lambda (k v)
					   (let ((name k))
						 (let ((l (length (symbol-name name))))
						   (when (> l m)
							 (setq m l)))))
					 perf-hash)
			(format t "~%~vtavg~vtcount~vtsum~%" (+ m 4)  (+ m 25) (+ m 40))
			(let ((extra-names nil))
			  (maphash (lambda (k v)
						 (let ((name k))
						   (when (not (member name display-order))
							 (setq extra-names (cons name extra-names)))))
					   perf-hash)
			  (dolist (name (append display-order extra-names))
				(let ((timerec (gethash name perf-hash)))
				  (when timerec
					(let ((sum (timerec-sum timerec))
						  (count (timerec-count timerec)))
					  (let ((units (if (eq (timerec-type timerec) 'time)
									   internal-time-units-per-second
									   1)))
						(let ((avg-time (/ (float (div (float sum) count)) units))
							  (sum-time (/ (float sum) units)))
						  (format t
								  "~a~vt~a~vt~a~vt~a~%"
								  name (+ m 4) avg-time (+ m 25) count (+ m 40) sum-time))))))))
			(format t "~%")
			nil))))
	(defun clear-perf-stats ()
	  (setq perf-hash (make-hash-table :test #'eq)))
	(defun get-perf-hash () perf-hash)))

(defun stream-first (stream)
  (first stream))

(defun stream-rest (stream)
  (timer 'stream-rest
		 (lambda ()
		   (funcall (second stream)))))

(defun stream-to-list (stream)
  (when stream
	(cons (stream-first stream) (stream-to-list (stream-rest stream)))))

;; (cross '(a b c) '(d e f) '(g h i))

(defun cross (&rest sets)
  (cross-aux sets))

(defun cross-aux (sets)
  (let ((r '(()))
		(r1 nil))
	(dolist (set sets)
	  (dolist (p r)
		(dolist (s set)
		  (setq r1 (cons (cons s p) r1))))
	  (setq r r1)
	  (setq r1 nil))
	r))

;; Thu Dec  1 17:14:42 EST 2016

(let ((day-names '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
	  (month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
  (defun date-time ()
	(multiple-value-bind
		(second minute hour date month year day-of-week dst-p tz)
		(get-decoded-time)
	  (let ((pm (>= hour 12)))
		(let ((hour (if pm (- hour 12) hour)))
		  (let ((s (format nil "~a ~a ~d ~d:~2,'0d:~2,'0d ~a (gmt~a~a) ~a"
						   (nth day-of-week day-names)
						   (nth (- month 1) month-names)
						   date
						   hour
						   minute
						   second
						   (if pm "pm" "am")
						   (if (>= tz 0) "-" "+")
						   tz
						   year)))
			s))))))

;; Accepts an arbitrary numberf of args of any type and cats their
;; printed representations together into a single symbol.

(defun symcat (&rest l)
  (defr
	(defl format-string (n)
	  (let ((r ""))
		(dotimes (i n)
		  (setq r (concatenate 'string r "~a")))
		r))
	(let ((len (length l)))
	  (intern (apply #'format nil (format-string len) l)))))

(defun sort-print (l test)
  (dolist (x (sort l test))
	(print x)))

;; Global trace "info" section

(defun match-and-execute-info (info))

(defun match-and-execute-env-info (info))

(defun match-pat-obj-edge-lists-info (info))

;; do-eval test section

(defun trans-diff (v r2 iseq sseq)
  (let ((d (- (or sseq 0) (or iseq 0))))
	(if (and (>= d 0)
			 (<= d 10))
		`(,v td ,r2)
		'(0))))

;; end do-eval test section


;; An experiment in debug. This module allows the subsitution of an
;; indicator for getting a value, rather than the value itself. This
;; supports printing logs where we want to print some value that's too
;; big to fit reasonably on one or a few lines.
;;
;; It's nicely self-contained, but does use packages, which are bad in
;; general. However, at least th eth emoment like this use, which
;; easily automates the definition of an arbitrary function, but it is
;; behind the scenes and easily deleted (just by deleting the
;; package), thus avoiding buildup of debug data

(let ((log-value-fcn-pkg-name 'log-value-fcn-pkg))
  (defr
	(defl tree-length (l)
	  (if (or (null l)
			  (not (listp l)))
		  1
		  (+ (tree-length (first l)) (tree-length (rest l)))))
	(let ()
	  ;; Return the value if its tree-length is less than a
	  ;; bound. tree-length is roughly proprtional to the total print
	  ;; length. If the bound is exceeded, return a fcn indicator.
	  (defun log-value (value)
		(if (< (tree-length value) 50)
			value
			(let ()
			  (when (null (find-package log-value-fcn-pkg-name))
				(make-package log-value-fcn-pkg-name))
			  (let ((f (intern (symbol-name (gensym)) log-value-fcn-pkg-name)))
				(setf (symbol-function f) (lambda () value))
				(symbol-name f)))))
	  (defun clear-log-value-pkg ()
		(when (find-package log-value-fcn-pkg-name)
		  (delete-package log-value-fcn-pkg-name)))
	  (defmacro get-log-value (fcn)
		`(log-value-fcn ',fcn))
	  (defun log-value-fcn (fcn-desc)
		(let ((fcn-name (if (stringp fcn-desc) fcn-desc (symbol-name fcn-desc))))
		  (let ((fcn (symbol-function (find-symbol fcn-name log-value-fcn-pkg-name))))
			(funcall fcn)))))))

(defc base-graph objgraph nil
  (let ()
	(defm add-natural-number-edges (n)
	  (dotimes (i n)
		(add-obj-edge `(,i sigma ,(+ i 1)) :add-rule-link t)))
	(defm read-rule-file (file)
	  (with-open-file (s file :direction :input)
		(loop
		 (let ((r (read s nil nil)))
		   (if (null r)
			   (return nil)
			   (let ((rinfo (define-rule r)))
				 (let ((rule-node (first rinfo)))
				   (add-edge `(,rule-node file ,file)))))))
		nil))
	(defm defg (rules)
	  (dolist (rule rules)
		(define-rule rule))
	  nil)
	(defm init ()
	  (objgraph-init))))

(defc the-graph base-graph nil
  (let ()

	(defm init ()
	  (base-graph-init)
	  (clear-perf-stats) ;; Note the perf stats are global
	  (add-natural-number-edges 20)
	  (do-defg)
	  (read-rule-file "tree.lisp")  ;;; 8/11/20 -- Fixed issues with this anbd it should hold as the default now
	  ;; (read-rule-file "globaltree.lisp")
	  (read-rule-file "rule30.lisp")
	  (read-rule-file "fft-delta.lisp")
	  (read-rule-file "fe.lisp")
	  (read-rule-file "copy-rule.lisp")
	  
	  (read-rule-file "display-data.lisp")

	  ;;; (read-rule-file "gettysburg-address.lisp")

	  ;; (read-rule-file "fe-no-copy.lisp")
	  ;; (read-rule-file "rule-dep.lisp")
	  )

	(defm do-defg ()
	  (defg
		'(
		  ;; Don't have a null add-rule-link capability for now -- so we always do

		  (rule
		   (name data)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node rule ?r)
			(?r name data)
			(?light new-node sn5)
			(?switch new-node sn6)
			(?room new-node sn7)
			(?add-out new-node sn8)
			(?add new-node sn9)
			(?add2 new-node sn10))
		   (add
			(print data)
			(off light-illum-mapping unlit)
			(on  light-illum-mapping lit)
			(occupied room-switch-mapping on)
			(unoccupied room-switch-mapping off)
			(?light is switch-room-obj)
			(?light name light1)
			(?light state undefined)
			(?light type light)
			(?switch is switch-room-obj)
			(?switch type switch)
			(?switch name switch1)
			(?switch state undefined)
			(?switch connected-to ?light)
			(?room is switch-room-obj)
			(?room type room)
			(?room name room1)
			(?room contains ?switch)
			(?room state undefined)
			(?add-out type add-out)
			(?add-out name add-out1)
			(?add-out value 0)
			(?add name add1)
			(?add type add)
			(?add input1 1)
			(?add input2 0)
			(?add output ?add-out)
			(?add2 name add2)
			(?add2 type addx)
			(?add2 input1 0)
			(?add2 input2 0)
			(?add2 value 0)
			(global-node next-color)
			(+ 1 2 3)
			(+ 2 2 4)
			(+ 2 3 5))
		   (del
			(global-node rule ?this-rule)))

		  (rule
		   (name std-notes)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node rule ?r)
			(?r name std-notes))
		   (add
			(print std-notes)
			(note footer "Copyright (c) 2020 Lawrence Stabile"))
		   (del
			(global-node rule ?this-rule)))
		  
		  ;; gen-inverse used now 

		  (rule
		   (name add-parent)
		   (disabled)
		   ;; (local)
		   (pred
			(?a elem ?e))
		   (add
			(print add-parent ?e ?a)
			(?e is-elem-of ?a)))

		  (rule
		   (name gen-inverse)
		   ;; (local)
		   ;; (disabled)
		   (attach-to inverse)
		   (root-var inverse)
		   (pred
			(?a inverse ?i))
		   (add
			(print gen-inverse ?a ?i)
			(global-rule-pool-node grp-rule
								   (rule
									(name (add-inverse ?i))
									(root-var ?x)
									(pred
									 (?x ?a ?y))
									(add
									 (print add-inverse ?a ?i ?x ?y)
									 (?y ?i ?x)))))
		   (del
			;; (?a global-rule-pool global-rule-pool-node)		;; !!!!!!!!!!!
			))

		  (rule
		   (name inverse-data)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node rule ?r)
			(?r name inverse-data))
		   (add
			(print inverse-data)
			(member inverse is-member-of)
			;; (member global-rule-pool global-rule-pool-node)		;; !!!!!!!!!!!!!
			(elem inverse is-elem-of)
			;; (elem global-rule-pool global-rule-pool-node)		;; !!!!!!!!!!!!!
			))

		  (rule
		   (name ev-init-gen)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node local-rule-pool ?p)
			(?p lrp-rule ?od-next)
			(?od-next name od-next))
		   (add
			(print ev-init-gen)
			(global-rule-pool-node grp-rule
								   (rule
									(name ev-init)
									;; (local)
									(root-var ?e0) ;; was ?a
									(pred
									 (?a elem ?e0)
									 (?e0 is-elem-of ?a)
									 (?e0 zero))
									(add
									 (print ev-init ?e0)
									 (?e0 ev)
									 (?e0 rule ?od-next))
									(del
									 (?this-obj rule ?this-rule)))))
		   (del
			(?this-obj rule ?this-rule)))

		  (rule
		   (name od-next)
		   (local)
		   ;; (root-var ?e0)
		   (pred
			;; (?a elem ?e0)				;; !!!!!!!!!!!!! 9/19/20 -- in removing this the system still works, and the expansion length shrinks by a lot, but still not constant across trials
			;; (?a elem ?e1)
			(?e0 is-elem-of ?a)
			(?e1 is-elem-of ?a)
			(?e0 next ?e1)
			(?e0 ev))
		   (add
			(print od-next ?e1 ?root-var)
			(?e1 od))
		   (del
			(?this-obj rule ?this-rule)
			))

		  (rule
		   (name ev-next)
		   (local)
		   ;; (root-var ?e0)
		   (pred
			;; (?a elem ?e0)				;; !!!!!!!!!!!!!!!!!!!! -- ditto 
			;; (?a elem ?e1)
			(?e0 is-elem-of ?a)
			(?e1 is-elem-of ?a)
			(?e0 next ?e1)
			(?e0 od))
		   (add
			(print ev-next ?e1 ?root-var)
			(?e1 ev))
		   (del
			(?this-obj rule ?this-rule)
			))

		  (rule
		   (name ev-od-opt)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node local-rule-pool ?p)
			(?p lrp-rule ?od-next)
			(?od-next name od-next)
			(?p lrp-rule ?ev-next)
			(?ev-next name ev-next))
		   (add
			(print ev-od-opt)
			(?od-next add (?e1 rule ?ev-next))
			(?ev-next add (?e1 rule ?od-next)))
		   (del
			(?this-obj rule ?this-rule)))

		  (rule
		   (name ev-od-obj-rule)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node local-rule-pool ?p)
			(?p lrp-rule ?ev-init)
			(?ev-init name ev-init))
		   (add
			(print ev-od-obj-rule)
			(ev-od-obj xrule ?ev-init)
			)
		   (del
			(global-node rule ?this-rule)))

		  (rule
		   (name add-rule)
		   (pred
			(?m type add)
			(?m input1 ?x))
		   (add
			(print add-rule-triggered ?m ?x)
			(?m rule (rule
					  (name add-aux-rule)
					  (root-var ?m)
					  (pred 
					   (?m input2 ?y)
					   (?x ?y + ?z)
					   (?m output ?o)
					   (?o value ?v))
					  (del
					   (?o value ?v))
					  (add
					   (?o value ?z))))))

		  (rule
		   (name addx-rule)
		   (pred
			(?m type addx)
			(?m input1 ?x)
			(?m input2 ?y)
			(?x ?y + ?z))
		   (add
			(print addx-rule ?m ?x ?y ?z)
			(?m value ?z)))

		  (rule 
		   (name switch-rule)
		   (local)
		   (pred
			(?s type switch)
			(?s event ?e)
			(?e light-illum-mapping ?i)
			(?s state ?x)
			(?s connected-to ?l)
			(?l state ?y))
		   (del
			(?s event ?e)
			(?s state ?x)
			(?l state ?y))
		   (add
			;; (print switch ?s state ?e name ?m light ?l old-state ?y old-illumination ?oldn new-illumination ?newn)
			(print ?s ?y ?i)
			(?s state ?e)
			(?l state ?i)))

		  (rule
		   (name room-rule)
		   (local)
		   (pred 
			(?room type room)
			(?room event ?e)
			(?e room-switch-mapping ?m)
			(?room state ?r)
			(?room contains ?switch)
			(?switch type switch))
		   (del
			(?room event ?e)
			(?room state ?r))
		   (add
			(print room ?room ?e ?switch)
			(?room state ?e)
			(?switch event ?m)))

		  (rule
		   (name switch-room-obj-rule)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred 
			(global-node local-rule-pool ?p)
			(?p lrp-rule ?room-rule)
			(?p lrp-rule ?switch-rule)
			(?room-rule name room-rule)
			(?switch-rule name switch-rule))
		   (add
			(print switch-room-obj-rule)
			(switch-room-obj rule ?room-rule)
			(switch-room-obj rule ?switch-rule))
		   (del
			(global-node rule ?this-rule)))

		  (rule
		   (name is-1-param)
		   (local)
		   (attach-to is)
		   (pred
			(?x is ?y ?p ?v)
			(?y rule ?r))
		   (del
			(?x is ?y ?p ?v))
		   (add
			(print is-1-param ?this-obj ?x ?y ?r ?p ?v)
			(?x rule ?r)
			(?x ?p ?v)
			(?x from-is-1-param-rule ?r)))

		  (rule
		   (name is-0-param)
		   (local)
		   (attach-to is)
		   (pred
			(?x is ?y)
			(?y rule ?r))
		   (del
			(?x is ?y))
		   (add
			(print is-0-param ?this-obj ?x ?y ?r)
			(?x rule ?r)
			(?x from-is-0-param-rule ?r)))

		  (rule
		   (name is-0-param-xrule)
		   (local)
		   (attach-to is)
		   (pred
			(?x is ?y)
			(?y xrule ?r))
		   (del
			(?x is ?y))
		   (add
			(print is-0-param-xrule ?this-obj ?x ?y ?r)
			(?x rule ?r)
			(?x from-is-0-param-xrule-rule ?r)))

		  (rule
		   (name is-not)
		   (disabled)
		   (pred
			(?x is-not ?y) ;; Put this...	; ; ; ;
			(?x is ?y)	   ;; ...and detect this ; ; ; ;
			(?y rule ?r)
			(?x rule ?r))
		   (add
			(print is-not ?x ?y ?r))
		   (del
			(?x is ?y)
			(?x is-not ?y)
			(?x rule ?r)
			(?this-obj rule ?this-rule)))

		  ;; An array is a node denoted in a rule as ?a with a set of nodes
		  ;; denoted by property elem related by next

		  (rule
		   (name even-new)
		   (local)
		   ;; (root-var ?a)
		   (root-var ?e0)
		   (pred
			(?a even ?a1)
			(?e0 is-elem-of ?a)
			(?e0 ev)
			(?e0 value ?v)
			(?nn1 new-node sn1)
			(?e0 local-rule-pool ?p)
			(?p lrp-rule ?self-cycle)
			(?p lrp-rule ?even-zero)
			(?p lrp-rule ?odd-zero)
			(?p lrp-rule ?odd-next)
			(?p lrp-rule ?even-next)
			(?self-cycle name self-cycle)
			(?even-zero name even-zero)
			(?odd-zero name odd-zero)
			(?odd-next name odd-next)
			(?even-next name even-next))
		   (add
			(print even-new ?this-obj ?a ?e0 ?nn1)
			(?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
			(?a1 elem ?nn1)
			(?nn1 value ?v)
			(?nn1 ref ?e0)

			(?nn1 en-ref ?e0)
			(?nn1 oe-ref ?e0)
			(?nn1 ref ?e0)

			(?nn1 is ev-od-obj)
			(?nn1 rule ?self-cycle)
			(?nn1 rule ?even-zero)
			(?nn1 rule ?odd-zero)
			(?nn1 rule ?odd-next)
			(?nn1 rule ?even-next)
			)
		   (del
			 (?this-obj rule ?this-rule)
			))

		  (rule
		   (name even-next)
		   (local)
		   (root-var ?ae0)
		   (pred
			(?a even ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae1 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?ae1 ref ?e1)
			(?e0 is-elem-of ?a)
			(?e0.5 is-elem-of ?a)
			(?e1 is-elem-of ?a)
			(?e0 next ?e0.5)
			(?e0.5 next ?e1))
		   (add
			(print even-next ?a ?a1 ?ae0 ?ae1 ?e0 ?e1)
			(?ae0 next ?ae1))
		   (del
			;; (?this-obj rule ?this-rule)
			))

		  (rule
		   (name odd-new)
		   (local)
		   ;; (root-var ?a)
		   (root-var ?e0) ;; Need rule to trigger on element not array to avoid non-det (single match)
		   (pred
			(?a odd ?a1)
			(?e0 is-elem-of ?a)
			(?e0 od)
			(?e0 value ?v)
			(?nn1 new-node sn1)
			(?e0 local-rule-pool ?p)
			(?p lrp-rule ?self-cycle)
			(?p lrp-rule ?even-zero)
			(?p lrp-rule ?odd-zero)
			(?p lrp-rule ?odd-next)
			(?p lrp-rule ?even-next)
			(?self-cycle name self-cycle)
			(?even-zero name even-zero)
			(?odd-zero name odd-zero)
			(?odd-next name odd-next)
			(?even-next name even-next))
		   (add
			(print odd-new ?a ?e0 ?nn1)
			(?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
			(?a1 elem ?nn1)
			(?nn1 value ?v)
			(?nn1 ref ?e0)

			(?nn1 on-ref ?e0)
			(?nn1 oe-ref ?e0)
			(?nn1 ref ?e0)

			(?nn1 is ev-od-obj)
			(?nn1 rule ?self-cycle)
			(?nn1 rule ?even-zero)
			(?nn1 rule ?odd-zero)
			(?nn1 rule ?odd-next)
			(?nn1 rule ?even-next)
			)
		   (del
			 (?this-obj rule ?this-rule)
			))

		  (rule
		   (name odd-next)
		   (local)
		   (root-var ?ae0)
		   (pred
			(?a odd ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae1 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?ae1 ref ?e1)
			(?e0 is-elem-of ?a)
			(?e0.5 is-elem-of ?a)
			(?e1 is-elem-of ?a)
			(?e0 next ?e0.5)
			(?e0.5 next ?e1))
		   (add
			(print odd-next ?a ?a1 ?ae0 ?ae1 ?e0 ?e1)
			(?ae0 next ?ae1))
		   (del
			;; (?this-obj rule ?this-rule)
			))


		  (rule
		   (name even-zero)
		   (local)
		   (pred
			(?a even ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?e0 zero))
		   (add
			(print even-zero ?this-obj ?a ?a1 ?ae0 ?e0)
			(?ae0 zero))
		   (del
			;; (?this-obj rule ?this-rule)
			))

		  (rule
		   (name odd-zero)
		   (local)
		   (pred
			(?a odd ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae0 ref ?e1)
			(?e0 next ?e1)
			(?e0 zero))
		   (add
			(print odd-zero ?this-obj ?a ?a1 ?ae0 ?e0)
			(?ae0 zero))
		   (del
			;; (?this-obj rule ?this-rule)
			))

		  (rule
		   (name self-cycle)
		   ;; (disabled)
		   (local)
		   ;; (root-var ?ae0)           ;; We get further with just the queue without a root var specified
		   (pred
			(?ae0 is-elem-of ?a1)
			(?ae0 oe-ref ?e1)
			(?e0 next ?e1)
			(?e1 next ?e0))
		   (add
			(print self-cycle ?ae0 ?a1 ?e0 ?e1)
			(?ae0 next))
		   (del
			;; (?this-obj rule ?this-rule) ;; don't del
			))

		  (rule
		   (name odd-new-rule-propagate)
		   (local)
		   (root-var ?a)
		   (pred
			(?a odd ?a1)
			(?e0 is-elem-of ?a)
			(?a local-rule-pool ?p)
			(?p lrp-rule ?odd-new)
			(?odd-new name odd-new))
		   (add
			(print odd-new-rule-propagate ?a ?e0)
			(?e0 rule ?odd-new)))

		  (rule
		   (name even-new-rule-propagate)
		   (local)
		   (root-var ?a)
		   (pred
			(?a even ?a1)
			(?e0 is-elem-of ?a)
			(?a local-rule-pool ?p)
			(?p lrp-rule ?even-new)
			(?even-new name even-new))
		   (add
			(print even-new-rule-propagate ?a ?e0)
			(?e0 rule ?even-new)))

		  (rule
		   (name even-tree-max)
		   (pred
			(?x even ?y)
			(?x odd ?z)
			(?z weave-next ?y)
			(?x oe-max))
		   (add
			(print even-tree-max ?this-obj ?x ?y)
			(?y oe-max)))

		  (rule
		   (name odd-tree-zero)
		   (pred
			(?x odd ?y)
			(?x even ?z)
			(?y weave-next ?z)
			(?x oe-zero))
		   (add
			(print odd-tree-zero ?this-obj ?x ?y)
			(?y oe-zero)))

		  (rule
		   (name odd-even-weave)
		   (pred
			(?p0 odd ?x00)
			(?p0 even ?x01)
			(?p1 odd ?x10)
			(?p1 even ?x11)
			(?x00 oe-zero)
			(?x11 oe-max)
			(?p0 oe-zero)
			(?p1 oe-max)
			(?p0 level ?l)
			(?p1 level ?l)
			(?x00 weave-next ?x01)
			(?x10 weave-next ?x11))
		   (add
			(print odd-even-weave ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
			(?p1 weave-next ?x00)))

		  (rule
		   (name weave-next-rule)
		   (pred
			(?p0 odd ?x00)
			(?p0 even ?x01)
			(?p1 odd ?x10)
			(?p1 even ?x11)
			(?x00 weave-next ?x01)
			(?x10 weave-next ?x11)
			(?p0 weave-next ?p1))
		   (add
			(print weave-next-rule ?this-obj ?x00 ?x01 ?x10 ?x11 ?p0 ?p1)
			(?x01 weave-next ?x10)))

		  (rule
		   (name copy-array-struct-next)
		   (local)
		   ;; (root-var ?ae0)
		   (pred
			(?a copy-array-struct ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae1 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?ae1 ref ?e1)
			(?e0 is-elem-of ?a)
			(?e1 is-elem-of ?a)
			(?e0 next ?e1))
		   (add
			(print copy-array-struct-next ?a ?a1 ?ae0 ?ae1 ?e0 ?e1)
			(?ae0 next ?ae1))
		   (del
			;; (?this-obj rule ?this-rule) ;; Don't del
			))

		  (rule
		   (name copy-array-struct-next-sing)
		   (local)
		   ;; (root-var ?ae0)
		   (pred
			(?a copy-array-struct ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?e0 is-elem-of ?a)
			(?e0 next))
		   (add
			(print copy-array-struct-next-sing ?a ?a1 ?ae0 ?e0)
			(?ae0 next)
			(?e0 casns-ref ?ae0))
		   (del
			;; (?this-obj rule ?this-rule)  ;; Don't del
			))

		  (comment
		   (rule
			(name copy-array-struct-zero)
			(local)	;;;;;;;;;;;;;;;
			;; (root-var ?ae0) ;;;;;;;;;;;;
			(pred
			 (?a copy-array-struct ?a1)
			 (?ae0 is-elem-of ?a1)
			 (?ae0 ref ?e0)
			 (?e0 zero))
			(add
			 (print copy-array-struct-zero ?this-obj ?a ?a1 ?ae0 ?e0)
			 (?ae0 zero)
			 (?a casz-ref ?ae0))
			(del
			 ;; (?this-obj rule ?this-rule) ;; Don't del ;
			 )))

		  (rule
		   (name copy-array-struct-zero)
		   (local)		;;;;;;;;;;;;;;;
		   ;; (root-var ?ae0) ;;;;;;;;;;;;
		   (pred
			(?a copy-array-struct ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?e0 zero))
		   (add
			(print copy-array-struct-zero ?this-obj ?a ?a1 ?ae0 ?e0)
			(?ae0 zero)
			(?a1 casz-ref1 ?ae0)
			(?a casz-ref ?e0))
		   (del
			;; (?this-obj rule ?this-rule) ;; Don't del
			))

		  (rule
		   (name copy-array-struct-new-gen)
		   (pred
			(?a copy-array-struct ?a1)
			(?a local-rule-pool ?p)
			(?p lrp-rule ?copy-array-struct-zero)
			(?p lrp-rule ?copy-array-struct-next)
			(?p lrp-rule ?copy-array-struct-next-sing)
			(?copy-array-struct-zero name copy-array-struct-zero)
			(?copy-array-struct-next name copy-array-struct-next)
			(?copy-array-struct-next-sing name copy-array-struct-next-sing))
		   (add
			(print copy-array-struct-new-gen ?a ?a1)
			(?a rule
				(rule
				 (name (copy-array-struct-new ?a))
				 (local)
				 (root-var ?a) ;; Should subst ?a above
				 ;; (root-var ?e0)
				 (pred
				  (?e0 is-elem-of ?a)
				  (?a level ?l)
				  (?nn1 new-node sn1))
				 (add
				  (print copy-array-struct-new ?a ?a1 ?e0 ?nn1)
				  (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
				  (?a1 elem ?nn1)
				  (?nn1 ref ?e0)
				  (?al level ?l)

				  (?nn1 casn-ref ?e0)

				  (?nn1 rule ?copy-array-struct-zero)
				  (?nn1 rule ?copy-array-struct-next)
				  (?nn1 rule ?copy-array-struct-next-sing)

				  )
				 (del
				  ;; (?this-obj rule ?this-rule) ;; Don't del
				  )))))

		  (comment
		   (rule
			(name copy-array-struct-new-gen)
			(pred
			 (?a copy-array-struct ?a1)
			 (?a local-rule-pool ?p)
			 (?p lrp-rule ?copy-array-struct-zero)
			 (?p lrp-rule ?copy-array-struct-next)
			 (?p lrp-rule ?copy-array-struct-next-sing)
			 (?copy-array-struct-zero name copy-array-struct-zero)
			 (?copy-array-struct-next name copy-array-struct-next)
			 (?copy-array-struct-next-sing name copy-array-struct-next-sing))
			(add
			 (print copy-array-struct-new-gen ?a ?a1)
			 (global-rule-pool-node grp-rule 
									(rule
									 (name (copy-array-struct-new ?a))
									 ;; (local)
									 (root-var ?a) ;; Should subst ?a above
									 ;; (root-var ?e0)
									 (pred
									  (?e0 is-elem-of ?a)
									  (?nn1 new-node sn1))
									 (add
									  (print copy-array-struct-new ?a ?a1 ?e0 ?nn1)
									  (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
									  (?a1 elem ?nn1)
									  (?nn1 ref ?e0)

									  (?nn1 casn-ref ?e0)

									  (?nn1 rule ?copy-array-struct-zero)
									  (?nn1 rule ?copy-array-struct-next)
									  (?nn1 rule ?copy-array-struct-next-sing)

									  )
									 (del
									  ;; (?this-obj rule ?this-rule) ;; Don't del
									  ))))))

		  (comment 
		   (rule
			(name copy-array-struct-new)
			;; (local)
			(root-var ?a)
			;; (root-var ?e0)
			(pred
			 (?a copy-array-struct ?a1)
			 (?e0 is-elem-of ?a)
			 (?nn1 new-node sn1)
			 (?a local-rule-pool ?p)
			 (?p lrp-rule ?copy-array-struct-zero)
			 (?p lrp-rule ?copy-array-struct-next)
			 (?p lrp-rule ?copy-array-struct-next-sing)
			 (?copy-array-struct-zero name copy-array-struct-zero)
			 (?copy-array-struct-next name copy-array-struct-next)
			 (?copy-array-struct-next-sing name copy-array-struct-next-sing))
			(add
			 (print copy-array-struct-new ?a ?a1 ?e0 ?nn1)
			 (?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
			 (?a1 elem ?nn1)
			 (?nn1 ref ?e0)

			 (?nn1 casn-ref ?e0)

			 (?nn1 rule ?copy-array-struct-zero)
			 (?nn1 rule ?copy-array-struct-next)
			 (?nn1 rule ?copy-array-struct-next-sing))
			(del
			 ;; (?this-obj rule ?this-rule) ;; Don't del
			 )))



		  (rule
		   (name fft-comb-rule-next-sing)
		   ;; (disabled)
		   (local)
		   ;; (root-var ?ey0)             ;; As with self-cycle, removing the root var allows the queue to get further -- in fact with no further global exec
		   (pred
			(?x0 ?x1 fft-comb ?y)
			(?ef0 is-elem-of ?x0)
			(?eg0 is-elem-of ?x1)
			(?ef0 next)
			(?eg0 next)
			(?ey0 is-elem-of ?y)
			(?ey1 is-elem-of ?y)
			(?ey0 next ?ey1)
			(?ey0 zero)			  ; Need this to assure a single match
			(?ef0 ?eg0 fft-hb ?ey0)
			(?ey0 local-rule-pool ?p)
			(?p lrp-rule ?fft-comb-rule-next-sing)
			(?fft-comb-rule-next-sing name fft-comb-rule-next-sing))
		   (add
			(print fft-comb-rule-next-sing ?x0 ?x1 ?y ?ef0 ?eg0 ?ey0 ?ey1 ?root-var)
			(?ef0 ?eg0 fft-hb ?ey1)
			(?ey1 rule ?fft-comb-rule-next-sing))  ;; Need this to assure rule propagated to next half-butterfly destination
		   (del
			 (?this-obj rule ?this-rule)
			))

		  (rule
		   (name fft-comb-rule-next-gen)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node local-rule-pool ?p)
			(?p lrp-rule ?fft-comb-rule-next-sing)
			(?fft-comb-rule-next-sing name fft-comb-rule-next-sing))
		   (add
			(print fft-comb-rule-next-gen)
			(local-rule-pool-node lrp-rule
								  (rule
								   (name fft-comb-rule-next)
								   (local)
								   (root-var ?ey0) ; Need root var on single output to be sure we get a single match
								   (pred
									(?x0 ?x1 fft-comb ?y)
									(?ef0 is-elem-of ?x0)
									(?ef1 is-elem-of ?x0)
									(?eg0 is-elem-of ?x1)
									(?eg1 is-elem-of ?x1)
									(?ef0 next ?ef1)
									(?eg0 next ?eg1)
									(?ey0 is-elem-of ?y)
									(?ey1 is-elem-of ?y)
									(?ey0 next ?ey1)
									(?ef0 ?eg0 fft-hb ?ey0))
								   (add
									(print fft-comb-rule-next ?x0 ?x1 ?y ?ef0 ?ef1 ?eg0 ?eg1 ?ey0 ?ey1)
									(?ef0 rule ?fft-comb-rule-next-sing)
									(?ef1 rule ?fft-comb-rule-next-sing)
									(?eg0 rule ?fft-comb-rule-next-sing)
									(?eg1 rule ?fft-comb-rule-next-sing)
									(?ey0 rule ?fft-comb-rule-next-sing)
									(?ey1 rule ?fft-comb-rule-next-sing)
									(?ef1 rule ?this-rule)
									(?eg1 rule ?this-rule)
									(?ey1 rule ?this-rule)
									(?ef1 ?eg1 fft-hb ?ey1))
								   (del
									(?this-obj rule ?this-rule)
									))))
		   (del
			(?this-obj rule ?this-rule)))

		  (rule
		   (name fft-comb-rule-zero)
		   (local)
		   ;; (root-var ?x0)  ;;;;;;;;;;;;;
		   (pred
			(?x0 ?x1 fft-comb ?y)
			(?e0 is-elem-of ?x0)
			(?e1 is-elem-of ?x1)
			(?ey is-elem-of ?y)
			(?e0 zero)
			(?e1 zero)
			(?ey zero)
			(?x0 local-rule-pool ?p)
			(?p lrp-rule ?fft-comb-rule-next)
			(?p lrp-rule ?fft-comb-rule-next-sing)
			(?fft-comb-rule-next name fft-comb-rule-next)
			(?fft-comb-rule-next-sing name fft-comb-rule-next-sing))
		   (add
			(print fft-comb-rule-zero  ?this-obj ?x0 ?x1 ?y ?e0 ?e1 ?ey)
			(?e0 rule ?fft-comb-rule-next)
			(?e0 rule ?fft-comb-rule-next-sing)
			(?e1 rule ?fft-comb-rule-next)
			(?e1 rule ?fft-comb-rule-next-sing)
			(?ey rule ?fft-comb-rule-next)
			(?ey rule ?fft-comb-rule-next-sing)
			(?e0 ?e1 fft-hb ?ey))
		   (del
			;; (?this-obj rule ?this-rule)
			))

		  (rule
		   (name fft-rule-zero)
		   (local)
		   (pred
			(?x fft ?y)
			(?x level 0)
			(?x local-rule-pool ?p)
			(?p lrp-rule ?fft-comb-rule-zero)
			(?fft-comb-rule-zero name fft-comb-rule-zero))
		   (add
			(print fft-rule-zero ?x ?y ?r)
			(?x copy-array-struct ?y)
			(?y level 0)
			(?x d ?y)			;; display-connection
			(?y rule ?fft-comb-rule-zero))
		   (del
			(?this-obj rule ?this-rule)
			))

		  (comment
		   (rule
			(name fft-rule-zero)
			(local)
			(pred
			 (?x fft ?y)
			 (?x local-rule-pool ?p)
			 (?p lrp-rule ?copy-array-struct-new)
			 (?p lrp-rule ?fft-comb-rule-zero)
			 (?copy-array-struct-new name copy-array-struct-new)
			 (?fft-comb-rule-zero name fft-comb-rule-zero))
			(add
			 (print fft-rule-zero ?x ?y ?r)
			 (?x copy-array-struct ?y)
			 (?x d ?y) ;; display-connection
			 (?x rule ?copy-array-struct-new)
			 (?y rule ?fft-comb-rule-zero))
			(del
			 (?this-obj rule ?this-rule)
			 )))

		  ;; This rule is global and bare-bones, with no rule-passing
		  ;; and related optimizations. It's modified explicitly by
		  ;; the optimizer in fft-delta.lisp.

		  (rule
		   (name fft-rule)
		   (pred
			(?x fft ?y)
			(?x level ?l)
			(?l1 sigma ?l)
			(?nn1 new-node sn1)
			(?nn2 new-node sn2)
			(?nn3 new-node sn3)
			(?nn4 new-node sn4))
		   (add
			(print fft-rule ?x ?y ?l)
			(?x even ?nn1)
			(?x odd ?nn2)
			(?nn2 weave-next ?nn1)
			(?nn1 oe ?x)
			(?nn2 oe ?x)
			(?nn1 oev 0)
			(?nn2 oev 1)
			(?x copy-array-struct ?y)
			(?nn1 fft ?nn3)
			(?nn2 fft ?nn4)
			(?nn3 ?nn4 fft-comb ?y)
			(?y level ?l)
			(?nn1 level ?l1)
			(?nn2 level ?l1))
		   (del
			(?this-obj rule ?this-rule)))

		  (rule
		   (name fft-top-rule)
		   (pred

			(?x fft-top)		;; An experiment in symbol-free matching, i.e., looking for the three
								;; commented-out edges rather than fft-top, for max locality. Works, but slow.
			;; (?x ?n1)
			;; (?n1 ?n2)
			;; (?n2 ?n3)
			
			(?x odd ?y)
			(?x even ?z)
			(?x rand ?r)
			(?r level ?level)
			(?x l ?l))
		   (add
			(print fft-top-rule ?x ?y ?z)
			(note title "FFT Butterflies with Rule-30 Random Deltas")
			(note fft 2^ ?l points "\\n" rule-30 ?level levels)
			(?y weave-next ?z)
			(?x weave-next ?y)
			(?y oe-zero)
			(?z oe-max)))

		  (rule
		   (name clean-fft-rule)
		   (local)
		   (pred
			(?x fft ?y)
			(?x level ?l)
			(?l1 sigma ?l)
			(?nn1 new-node sn1)
			(?nn2 new-node sn2)
			(?nn3 new-node sn3)
			(?nn4 new-node sn4))
		   (add
			(?x even ?nn1)
			(?x odd ?nn2)
			(?x copy-array-struct ?y)
			(?nn1 fft ?nn3)
			(?nn2 fft ?nn4)
			(?nn3 ?nn4 fft-comb ?y)
			(?nn1 level ?l1)
			(?nn2 level ?l1)))

		  (rule
		   (name clean-fft-comb-rule-zero)
		   (local)
		   (pred
			(?x0 ?x1 fft-comb ?y)
			(?e0 is-elem-of ?x0)
			(?e1 is-elem-of ?x1)
			(?ey is-elem-of ?y)
			(?e0 zero)
			(?e1 zero)
			(?ey zero))
		   (add
			(?e0 ?e1 fft-hb ?ey)))

		  (rule
		   (name clean-fft-comb-rule-next)
		   (local)
		   (pred
			(?x0 ?x1 fft-comb ?y)
			(?ef0 is-elem-of ?x0)
			(?ef1 is-elem-of ?x0)
			(?eg0 is-elem-of ?x1)
			(?eg1 is-elem-of ?x1)
			(?ef0 next ?ef1)
			(?eg0 next ?eg1)
			(?ey0 is-elem-of ?y)
			(?ey1 is-elem-of ?y)
			(?ey0 next ?ey1)
			(?ef0 ?eg0 fft-hb ?ey0))
		   (add
			(?ef1 ?eg1 fft-hb ?ey1)))

		  (rule
		   (name color-circle-data)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node rule ?r)
			(?r name color-circle-data))
		   (add
			(print color-circle-data)
			(navajowhite next-color moccasin)
			(moccasin next-color lemonchiffon)
			(lemonchiffon next-color seashell)
			(seashell next-color aliceblue)
			(aliceblue next-color lavender)
			(lavender next-color lavenderblush)
			(lavenderblush next-color mistyrose)
			(mistyrose next-color dodgerblue)
			(dodgerblue next-color deepskyblue)
			(deepskyblue next-color skyblue)
			(skyblue next-color lightskyblue)
			(lightskyblue next-color darkturquoise)
			(darkturquoise next-color mediumturquoise)
			(mediumturquoise next-color turquoise)
			(turquoise next-color cyan)
			(cyan next-color lightseagreen)
			(lightseagreen next-color palegreen)
			(palegreen next-color springgreen)
			(springgreen next-color darkkhaki)
			(darkkhaki next-color khaki)
			(khaki next-color yellow)
			(yellow next-color gold)
			(gold next-color salmon)
			(salmon next-color deeppink)
			(deeppink next-color pink)
			(pink next-color lightpink)
			(lightpink next-color palevioletred)
			(palevioletred next-color mediumvioletred)
			(mediumvioletred next-color violetred)
			(violetred next-color magenta)
			(magenta next-color violet)
			(violet next-color blueviolet)
			(blueviolet next-color purple)
			(purple next-color AntiqueWhite1)
			(AntiqueWhite1 next-color LemonChiffon1)
			(LemonChiffon1 next-color LemonChiffon2)
			(LemonChiffon2 next-color LemonChiffon3)
			(LemonChiffon3 next-color LavenderBlush1)
			(LavenderBlush1 next-color MistyRose2)
			(MistyRose2 next-color MistyRose3)
			(MistyRose3 next-color SlateBlue1)
			(SlateBlue1 next-color blue1)
			(blue1 next-color DodgerBlue3)
			(DodgerBlue3 next-color DeepSkyBlue3)
			(DeepSkyBlue3 next-color LightSkyBlue3)
			(LightSkyBlue3 next-color LightBlue1)
			(LightBlue1 next-color PaleTurquoise1)
			(PaleTurquoise1 next-color turquoise1)
			(turquoise1 next-color SpringGreen1)
			(SpringGreen1 next-color green2)
			(green2 next-color chartreuse2)
			(chartreuse2 next-color khaki1)
			(khaki1 next-color yellow1)
			(yellow1 next-color gold2)
			(gold2 next-color RosyBrown2)
			(RosyBrown2 next-color navajowhite))
		   (del
			(global-node rule ?this-rule)))

		  (rule
		   (name color-color)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node next-color)
			(?x next-color ?y))
		   (add
			(print color-color ?x)
			(?x color ?x))
		   (del
			(?this-obj rule ?this-rule)))

		  )))))

;; Local Variables:
;; eval: (put 'execute-obj 'lisp-indent-function 'defun)
;; eval: (put 'add-consequent-edges 'lisp-indent-function 'defun)
;; eval: (put 'dolists 'lisp-indent-function 'defun)
;; eval: (put 'match-and-execute-rule-on-edges 'lisp-indent-function 'defun)
;; End:
