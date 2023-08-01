;; Issue tags:
;;
;;  global-change -- Getting rid of the exposed global rule pool.
;;					 5/18/23 -- This is pretty much done, and the tag discharged. A couple of tags left below, and in
;;								test.lisp.
;;					 6/11/23 -- Further cleanup of global pool refs
;;
;;  reflexive-match-fix -- Matching say (?x next ?x) against (n1 next n1).
;;						   5/18/23 -- Discharged.
;;
;;  string-nodes -- nodes which are strings do not compare equal in edge storage, but they do in matching. So far these
;;					changes are a complete failure, e.g. when change the edgelist hash table to eql instead of equal,
;;					lots of things break.
;;
;;					But is leading to a new way to do matching, via new bipwalk, etc., so changes for this are marked
;;					string-nodes.
;;
;;					New bipwalk etc. led into a full subst-match algorithm.
;;
;;					4?/7/23 Subst-match looks like it's working pretty well, esp. for det matches or small numbers of
;;					matches. Right now it's only active when the root var is a var rather than a const, so we get an
;;					intial reduction by subst of the root. For const-attached rules it normally explodes easily, such
;;					that traditional all-matches-aux does better, since it whittles down the edges firrst to a
;;					manageable level, though it does the whittling from a lot of edges, and that number increases with
;;					the input size. Subst-match shines in this regard, since it uses the qet system to lookup edges in
;;					an indexed manner, with an ASC flavor.
;;
;;					4/18/23 Subst-match working quite well, using new model which finds smallest number of qet edges
;;					first. Looks like an excellent heuristic, and am using it by default to explore the new rule format
;;					stuff.
;;
;;					5/19/23 The rulegraph class should be checked to see if bipwalk and other methods can be removed.
;;
;;					5/19/23 -- Discharged, except for a couple of notes and a section of test.lisp.
;;
;; rem-add-main -- Remove add-main from the rule processing. Figure out another way to extract pretty rule graphs
;;
;; new-rule-format -- seq-based qets on; things seem to work with old rule format. If keep seq qets, need to split lookup at vars.
;;					  Try for a bit with new rule format and old matching
;;					  Did lookup of qets via var-split seqs in subst-match only, since that is the main target
;;					  function. With possible-match it's not so important.
;;
;;					  5/21/23 -- Discharged, with couple of tags left in h.lisp, and a section in test.lisp. Spinoff tags
;;								 better-root-var, qets-to-seqs, and dump-sn
;;
;; partial-match-info -- 4/18/23 Started to look for a good way to develop info on rule test failures, and how we might
;;						 trace them for debugging and other purposes. Part of this is trying to return from subst-match
;;						 pred and edge info on partial matches, in particular when we fail, it's because a set of preds
;;						 has been resolved into consts, but not all the edges exist. So we try to capture that, and it's
;;						 not trivial, since the set of edges which in fact matched can vary within subst-match, and can
;;						 vary across trying root-vars in a given test. So we've marked the project and commented-out
;;						 code for hacking with this, but not sure when will return to it. One primary question is if we
;;						 want to develop a trace graph which can help find failures, what does that look like and what
;;						 info do we need?
;;
;; better-root-var -- 5/2/23 Detect root var use and set designated root vars via heuristic. 
;;
;; all-var-mod -- Disabled 5/18/23. Some time ago made changes to enable a pred to have all vars. A good experiment, but
;;				  not practical to pursue right now. See write-up in doc.txt. Tag left in below with I hope appropriate
;;				  comments.
;;
;; qets-to-seqs -- Conversion of qets to seqs, e.g., optimize/dump qet-utils class.
;;				   5/21/23 -- Discharged. qet-utils class is gone. is-subqet now just used CL search, and is a graph method.
;;
;; dump-sn -- Remove the old "sn" new-node model. Can be simpler now, say, e.g., just (?nn1 new-node).
;;
;; check-rule-trace -- Change explicit rule-trae check with a method; allows options like break; the method's name is
;;					   check-rule-trace, so a change to that name qualifies as a tag.

(defc graph nil nil
  (let ((size 63)) ;; 1021 !!!!!!!!!!!!!!!!!!!!!!!
	(let (
		  (nodelist (make-sur-map :input-size size :res-size 17))
		  (edgelist (make-hash-table :test #'equal :size size)) ;; As noted above in the string-nodes comment, the system does not hold together with non-eqv strings.
		  (nodeposlist (make-hash-table :test #'equal :size size))
		  (subqet-map (make-sur-map :res-size 17))
		  (superqet-map (make-sur-map :res-size 17))
		  )

	  (defm add-edge (edge)		;; For truth-and-beauty, the only way nodes get added is via add-edge
		(defr
		  (defl add-node (node pos edge)
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
			  (log-stat 'node-dim (length (get-edges node)))
			  node))
		  (let ((e (gethash edge edgelist)))
			(when (null e)
			  (log-stat 'edge-size (length edge))
			  (setq e edge)
			  (setf (gethash edge edgelist) edge)
			  (let ((i 0))
				(dolist (n e)
				  (add-node n i e)
				  (setq i (+ i 1))))
			  (add-subqets e)
			  )
			e)))

	  (defm edge-exists (edge)
		(and (gethash edge edgelist) t))

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

	  ;; This is used with the cross-intersect evaluation model, which
	  ;; is slow but has nice theory. 
	  ;;
	  ;; Scans up the tree of superqets of subqet and returns all such
	  ;; superqets which are also edges
	  ;;
	  ;; Use a pass-through to avoid changing all call sites to self-ref

	  (defm get-edges-from-subqet (subqet)
		(! (self get-edges-from-subqet-aux) subqet))

	  (let ((get-edges-from-subqet-hash (make-hash-table :test #'equal :size 256)))
		(defm get-edges-from-subqet-aux (subqet)
		  (timer 'get-edges-from-subqet
			(lambda ()
			  (let ((map get-edges-from-subqet-hash))
				(clrhash map)
				(defr
				  (defl g (qet)
					(when (edge-exists qet)
					  (setf (gethash qet map) qet))
					(let ((sup (superqets qet)))
					  (dolist (qet sup)
						(g qet))))
				  (g subqet))
				(gstat 'num-edges-from-subqet (lambda (x y) (+ x y)) (lambda () (hash-table-count map)))
				;; Looks like this list conversion makes this fcn linear in the size of the result. 
				;; May not be a big deal, since the caller can only scan the resulting edges linearly anyway. But we may
				;; gain by returning and manipulating a hash table or struct/class.
				(let ((edges (hash-table-value-to-list map)))
				  edges))))))

	  ;; Does same scan as get-edges-from-subqet but just returns size of resulting hash table. Note this can be optimized by keeping a
	  ;; running count when inserting qets. Used in scan-and-subst
	  ;;
	  ;; For now, we use a cache, invalidated in add-subqet and rem-subqet. Makes a dramatic performance difference.

	  (let ((count-edges-from-subqet-hash (make-hash-table :test #'equal :size 256)))
		(let ((count-edges-from-subqet-cache (make-hash-table :test #'equal :size 256)))
		  (defm count-edges-from-subqet (subqet)
			(timer 'count-edges-from-subqet
			  (lambda ()
				(let ((cache count-edges-from-subqet-cache))
				  (let ((count (gethash subqet cache)))
					(or count
						(let ((map count-edges-from-subqet-hash))
						  (clrhash map)
						  (defr
							(defl g (qet)
							  (when (edge-exists qet)
								(setf (gethash qet map) qet))
							  (let ((sup (superqets qet)))
								(dolist (qet sup)
								  (g qet))))
							(g subqet))
						  (let ((count (hash-table-count map)))
							(setf (gethash subqet cache) count)
							count))))))))
		  (defm clear-count-edges-from-subqet-cache (subqet)
			(remhash subqet count-edges-from-subqet-cache))))

	  (let ((count-edges-from-subqet-hash (make-hash-table :test #'equal :size 256)))
		(defm old-count-edges-from-subqet (subqet)
		  (timer 'count-edges-from-subqet
			(lambda ()
			  (let ((map count-edges-from-subqet-hash))
				(clrhash map)
				(defr
				  (defl g (qet)
					(when (edge-exists qet)
					  (setf (gethash qet map) qet))
					(let ((sup (superqets qet)))
					  (dolist (qet sup)
						(g qet))))
				  (g subqet))
				(hash-table-count map))))))

	  ;; subqet is assumed singleton for now
	  ;;
	  ;; This is a way to get edges with a node at a given pos, without
	  ;; extra tables, i.e., we use the subqet lattice. Very slow, and
	  ;; probably has a pathology.
	  ;; 
	  ;; Not used now -- call commented out. Did not have to change for get-edges-from-subqet fix

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

	  ;; For testing. Returns a list where each node in the graph is
	  ;; paired with an array containing the number of edges with the
	  ;; node in position 0, 1, etc.
	  ;;
	  ;; ((<node> <pos-dist-array>) ...)

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
		(! (self nodes-aux) edges))

	  (let ((node-table (make-hash-table :test #'equal)))
		(defm nodes-aux (edges)
		  (clrhash node-table)
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

	  (defm is-subqet (subqet qet)
		(and (search subqet qet :test #'equal) t))

	  ;; Get all qets (including edges)

	  (defm all-qets ()
		(hunion (all-subqets) (get-all-edges)))

	  ;; predicate-fcn == (lambda (edge) ...) => Boolean. 
	  ;; Returns a new graph with the object graph filter via the predicate fcn
	  ;;
	  ;; Used with ASC stuff which is used only in test.lisp right now.

	  (defm graph-filter (predicate-fcn)
		(let ((r (make-objgraph)))
		  (dolist (e (get-all-edges))
			(when (funcall predicate-fcn e)
			  (! (r add-edge) e)))
		  r))

	  (defm make-qet-graph () ;; All qets as edges
		(let ((g (make-objgraph)))
		  (dolist (qet (all-qets))
			(! (g add-edge) qet))
		  g))

	  ;; For graphical purposes only at this point. Build a "Hasse
	  ;; Diagram" of the ASC formed by the edges of the graph and all
	  ;; its subqets.

	  ;; String nodes to dump, with an "up" relation

	  (defm make-hasse-graph (&key (levels-to-omit '(0))) ;; list of levels to omit, with zero as the bottom "T" level (omit by default)
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

	  (defm get-subqet-map () ;; Debug only
		subqet-map)

	  (defm get-superqet-map ()	;; Debug only
		superqet-map)

	  (defm add-subqets (edge)	;; qets-to-seqs -- subseqs, not subqets (subsets)
		(defr
		  (defl first-n (l n)
			(when (and (not (= n 0))
					   (>= (length l) n))
			  (cons (first l) (first-n (rest l) (- n 1)))))
		  (defl sublists-len-n (l n)
			(when (and l
					   (> n 0)
					   (>= (length l) n))
			  (cons (first-n l n) (sublists-len-n (rest l) n))))
		  (defl add-layer (qets sub-qets len)
			(when (not (null sub-qets))
			  (when (not (null qets))
				(dolist (qet qets)
				  (dolist (sub-qet sub-qets)
					(when (is-subqet sub-qet qet)
					  (add-subqet sub-qet qet)))))
			  (add-layer sub-qets (sublists-len-n edge len) (- len 1))))
		  (timer 'add-subqets
			(lambda ()
			  (add-layer nil (list edge) (- (length edge) 1))
			  nil))))

	  (defm add-subqet (sub super) ;; Add mapping in both directions
		(! (self clear-count-edges-from-subqet-cache) sub)
		(! (subqet-map insert) super sub)
		(! (superqet-map insert) sub super)
		nil)

	  (defm rem-subqet (qet)
		(dolist (s (subqets qet))
		  (! (superqet-map remove-res) s qet)
		  (! (self clear-count-edges-from-subqet-cache) s))
		(dolist (s (superqets qet))
		  (! (subqet-map remove-res) s qet)
		  (! (self clear-count-edges-from-subqet-cache) s))
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

	  ;; The edges in the graph, along with all their subqets, form an
	  ;; abstract simplicial complex (ASC). From these qets, form a
	  ;; pseudo-topo space by taking the pairwise union of all these
	  ;; edges and qets, continuing until no new qets are added.
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

	  ;; Passing an existing list of edges overrides accessing the edge set and sorting

	  (defm dump-edges (&key (edges nil) (sort t) (dump-fcn #'print))
		(if edges
			(dolist (edge edges)
			  (funcall dump-fcn edge))
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
							  
							first-nodes)))))
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

	  )))

(defc xgraph graph nil
  (let ((a 1))
	(defm m1 (x) (+ a x))
	(defm m2 ())
	(defm get-all-nodes ()
	  "Ha ha!!!")))

(defc rule-components nil nil
  (let ((pred-list nil)
		(del-list nil)
		(add-list nil)
		(not-list nil))
	(defm set-components (pred del add not)
	  (setq pred-list pred)
	  (setq del-list del)
	  (setq add-list add)
	  (setq not-list not))
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
	  (list pred-list del-list add-list not-list))
	(defm preds ()
	  pred-list)
	(defm dels ()
	  del-list)
	(defm adds ()
	  add-list)
	(defm nots ()
	  not-list)
	(defm signature ()
	  (timer 'signature
		(lambda ()
		  (+ (sxhash pred-list)
			 (sxhash add-list)
			 (sxhash del-list)
			 (sxhash not-list)))))))

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
		(edge-to-pred (make-sur-map))					 ;; Nop by using  make-dummy-sur-map; Active using make-sur-map
		(edge-to-add (make-sur-map))					 ;; Nop by using  make-dummy-sur-map; Active using make-sur-map
		(edge-to-trace (make-sur-map))					 ;; Nop by using  make-dummy-sur-map; Active using make-sur-map
		(local-pool-add-node nil)
		(global-pool-add-node nil)
		(std-vars (make-std-vars))
		(seqno 0)
		(rule-stats nil)
		(node-stats nil)
		(expand-edges-visit-hash (make-hash-table :test #'equal))
		)
	(let ((hget-key-rest2 (rest hget-key-rest1))
		  (hget-sup-rest2 (rest hget-sup-rest1))
		  (elem-attrs '(_elem))
		  (env-triggered-table (make-env-triggered std-vars))
		  )

	  (defm init ()
		(! (env-triggered-table set-graph) self)
		(setq rule-stats (make-rule-stats self))
		(setq node-stats (make-node-stats self))
		(addraw global-node 'local-rule-pool local-rule-pool)
		nil)

 	  (defm get-edge-to-pred ()
 		edge-to-pred)

 	  (defm get-edge-to-add ()
 		edge-to-add)

	  (defm get-edge-to-trace ()
		edge-to-trace)

	  (defm get-elem-attrs ()
		elem-attrs)

	  (defm std-vars ()	;; For debug only
		std-vars)

	  ;; Not we no longer to anything special to an "obj" node, ed.g. adding rule pool links and such, so we can
	  ;; probably just use new-node().

	  (defm new-obj-node ()
		(let ((node (intern (format nil "N~a" objnodeseq))))
		  (setq objnodeseq (+ objnodeseq 1))
		  node))

	  (defm get-obj-edges (node)
		(let ((edges (get-edges node)))
		  (let ((r nil))
			(dolist (edge edges)
			  (when (equal node (first edge))
				(setq r (cons edge r))))
			r)))

	  ;; Add a triplet edge of the form (node attr value)

	  (defm addraw (n a v)				;; Dump this and just use add-edge?
		(add-edge (list n a v)))

	  (defm delraw (n a v)				;;  Dump this since it's just used once, below?
		(rem-edge (list n a v)))

	  (defm get-env-triggered-table ()
		env-triggered-table)

	  (defm add (node-or-edge &optional a v)		;; This is used by some tests to inject edges and queue
													;; them. Perhaps define as an ACE adjunct.
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

	  (defm node-queued (n)
		(! (obj-queue is-queued) n))

	  ;; Shouldn't pass non-null values but don't want them in the queue if passed in error

	  (defm queue-node (n &key only-if-not-queued push-head)
		(when n
		  (if (and only-if-not-queued
				   (node-queued n))
			  nil
			  (if push-head
				  (! (obj-queue push-head) n)
				  (! (obj-queue push-tail) n)))))

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
			   ;; queue length, then edited to produce a queue size
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
						 (lambda (m s p)
						   (when once
							 (return-from exq nil))
						   (when m
							 (queue-node obj)))))
				   (return-from exq nil))))))

	  ;; An enum "type"
	  (defm match-status ()
		'(:failed :new-edges :no-new-edges))

	  ;; rule-mode
	  ;; The default, :local-global, is the standard execution order. The rest are for testing/debugging.
	  ;;
	  ;; :local-only						- local rules only
	  ;; :local-rule-pool-only				- local rule pool only
	  ;; :local-and-global-rule-pool-only	- local and global rule pools only
	  ;; :local-global						- local rules, then global rules  [default]
	  ;;
	  ;; Calls (cont <edge-creation-status> <match-status> <matched-edges>)
	  ;; 
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <match-status> == (match-status)
	  ;; <matched-edges> == union of all matches (not split out by env)

	  (defm execute-obj (node &key (rule-mode :local-global) (cont (lambda (m s e p) (list m s))))
		(timer 'execute-obj
		  (lambda ()
			(let ((r nil)
				  (match-status :failed)
				  (matched-edges nil))
			  (defr
				(defl while (thunk)
				  (let ((r (funcall thunk)))
					(when r
					  (while thunk))))
				(defl exec-obj-stat (match-status)
				  (! (node-stats update-tested) node)
				  (when (memq match-status '(:new-edges :no-new-edges))
					(! (node-stats update-matched) node))
				  (cond
				   ((eq match-status :new-edges)
					(! (node-stats update-new-edges) node))
				   ((eq match-status :no-new-edges)
					(! (node-stats update-not-new-edges) node))
				   ((eq match-status :failed)
					(! (node-stats update-failed) node))))
				(defl m-and-e (rule node)
				  (match-and-execute-rule rule node :cont
										  (lambda (m s p d)
											(setq matched-edges (hunion matched-edges (mapunion (lambda (edges) edges) p)))
											(when (or (and (not (eq match-status :new-edges))
														   (eq s :new-edges))
													  (and (eq match-status :failed)
														   (eq s :no-new-edges)))
											  (setq match-status s))
											(when m
											  (setq r t))
											d)))
				(cond
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
						  (m-and-e rule node))))))
				 ((or (eq rule-mode :local-only)
					  (eq rule-mode :local-global))
				  (let ()

					(let ((evaled-rules nil))
					  (while
						  (lambda ()
							(block b
							  (let ((rules (set-subtract (hget-all node 'rule) evaled-rules)))
								(dolist (rule rules)
								  (let ((r (m-and-e rule node)))
									(setq evaled-rules (cons rule evaled-rules))
									(when r
									  ;; (print (list 'refresh-list node rule (hget rule 'name) rules (hget-all node 'rule)))
									  (return-from b t)
									  )))
								nil)))))

					(when (not (eq rule-mode :local-only))
					  (when global-rule-pool
						(let ((global-rules (dedup-rules (hget-all global-rule-pool 'grp-rule))))
						  (dolist (rule global-rules)
							(m-and-e rule node))))))))
				(exec-obj-stat match-status))
			  (funcall cont r match-status matched-edges)))))

	  (defm execute-all-objs (&key (rule-mode :local-global))
		(timer 'execute-all-objs
		  (lambda ()
			(let ((nodes (get-all-nodes))
				  (r nil))
			  (dolist (node nodes)
				(execute-obj node :rule-mode rule-mode :cont
				  (lambda (m s p)
					(when m
					  (setq r t)))))
			  r))))


	  ;; By default, executions both off the queue and in the full scan are local and global. See comment.

	  (defm execute-global-all-objs-loop (&key (queue-rule-mode :local-global) 
											   (scan-rule-mode :local-global))  ;; Note this was local-only, but the
																				;; global rule pool should mostly be
																				;; small or empty, so we cover all the
																				;; ground by checking them both, and
																				;; doing that for each mode. Ideally
																				;; these modes go away or just stay for
																				;; debugging.
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
			  ($nocomment
			   (log-stat 'entropy (edge-asc-entropy))
			   (log-stat 'dist (edge-asc-dimension-dist)))
			  (exec-until-no-new-edges
				  (lambda ()
					(exec-until-no-new-edges
						(lambda ()
						  (log1 'global-node
								(lambda ()
								  (execute-obj 'global-node :rule-mode :local-global :cont (lambda (m s p) nil)))))) ;; scan-rule-mode
					(log1 'queue
						  (lambda ()
							(execute-queue :rule-mode queue-rule-mode)))
					(log1 'exec-all
						  (lambda ()
							(execute-all-objs :rule-mode scan-rule-mode)))))
			  nil))))

	  ;; clauses below is the incoming query, and has the form:
	  ;;
	  ;;	(<clause>... [:not <clause> ...])
	  ;;
	  ;; A <clause> is a pattern. We call the first list of clauses the pred-clauses, and the second the not-clauses.
	  ;;
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
		(let ((pred-clauses (subseq clauses 0 (or (search '(:not) clauses) (length clauses)))))
		  (let ((not-clauses (rest (subseq clauses (or (search '(:not) clauses) (length clauses))))))
			(let ((envs (query1 pred-clauses not-clauses rule-trace)))
			  (if (is-var-name vardesc)
				  (env-lookup vardesc (first envs) :idempotent nil)
				  (if (eq vardesc :edges)
					  (matched-edges-union pred-clauses envs)
					  (if vardesc
						  (dedup-list (mapcar (lambda (env)
												(mapcar (lambda (var)
														  (env-lookup var env :idempotent nil))
														vardesc))
											  envs))
						  envs)))))))

	  (defm query1 (clauses not-clauses rule-trace)
		(let ((all-clauses (hunion clauses not-clauses)))
		  (let ((rule (first (define-rule `(rule (pred ,@all-clauses)) :unattached t))))
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
		(let ((env-table (make-hash-table :test 'set-hash-test)))
		  (let ((rule-info (define-rule `(rule (name query) (pred ,@clauses) (not ,@not-clauses)) :unattached t)))
			(let ((rule (first rule-info)))
			  (when rule-trace
				(trace-rule 'query))
			  (dolist (node (get-all-nodes))
				(let ((envs (all-matches-with-not rule node)))
				  (dolist (env envs)
					(let ((env (env-prune env (! (std-vars base-vars)))))
					  (let ((env (set-subtract env '((t t)))))
						(setf (gethash env env-table) env))))))))
		  (hash-table-value-to-list env-table)))

	  ;; All variants of hget call the main hget-aux below

	  (defm hget (node attr)
		(hget-aux node attr nil nil nil))

	  ;; list of all values with edge label attr

	  (defm hget-all (node attr)
		(hget-aux node attr nil nil t))

	  ;; General-purpose accessor which follows a chain/tree of attributes/inverse attributes.
	  ;;
	  ;; Given the node-list, will get all nodes which are values of
	  ;; the first attr, then repeat on the resulting nodes with the
	  ;; next attr, and so on, to produce a final list of nodes. Attrs
	  ;; of the form (inv <attr>) are accessed in inverse mode.

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

	  (defm get-rule-components (rule-node)
		(defr
		  (defl get-clause-edges (clause-type)
			(let ((edges (get-edges-from-subqet (list rule-node clause-type))))
			  (mapcad (lambda (e)
						(when (and (equal (first e) rule-node) (equal (second e) clause-type))
						  (rest (rest e))))
					  edges)))
		  (timer 'get-rule-components
			(lambda ()
			  (let ((pred-list (get-clause-edges 'pred)))
				(let ((del-list (get-clause-edges 'del)))
				  (let ((add-list (get-clause-edges 'add)))
					(let ((not-list (get-clause-edges 'not)))
					  (let ((rule-components (make-rule-components)))
						(! (rule-components set-components) pred-list del-list add-list not-list)
						rule-components)))))))))

	  ;; Nodes can no longer just be "unattached" in the sense of being immune to rules. This became more effective with
	  ;; the change to global pool handling, i.e., it's all internal to the kernel and not visible in H. So below we
	  ;; just return true. But I'm leaving the function in as someday we may want for some reason to make the
	  ;; distinction.

	  (defm has-rules (node)
		(or t									;; We assume rules are always available in some way
			(hget node 'global-rule-pool)
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

	  (defm node-name-cat (l)
		(let ((s ""))
		  (dolist (x l)
			(setq s (format nil (if (equal s "") "~a~a" "~a-~a") s x)))
		  (intern s)))

	  ;; 10/23/22 The old way on this was to lookup the rule component tree from the _elem level, and then do rule^-1 to
	  ;; get objects which might run on the rule. Overall, the need was due to copying rules, which during the process
	  ;; of copying could be attempted to be run by anything with a rule attribute referring to that rule node. It would
	  ;; fail, since the rule was not yet built, and the object dequeued. Thus we needed a way to find that object again
	  ;; and queue it as the rule proceeds to be built.  We might try having some sort of "done" feedback for the
	  ;; copying, but this seems quite difficult. This old way worked, but was very slow.
	  ;;
	  ;; In the new model we have modified copy-rule to add a "for-rule" attr to each object which is the value of an
	  ;; _elem attr: (<node> for-rule <copied-rule-node>)
	  ;;
	  ;; This has the simple effect of keeping <copied-rule-node> on the queue. In get-rule-neighborhood, then, we just
	  ;; look for rule^-1 for any node of type rule we encounter.

	  ;; 10/23/22 -- Comment from the old way
	  ;;
	  ;; A path from a node up to an object with a rule property is constrained to be due to an _elem edge being added. Hence we look for
	  ;; that pattern and only if we find it do we get the path.
	  ;;
	  ;; Note this may not be adequate for some cases, where we might have a race condition relative to rule modification, i.e., we may
	  ;; need to sense adds, preds, and rule props too. I had that active in the first version of the edge-based fcn, but it really
	  ;; returns a lot of nodes and slows things down considerably. So we'll use just this low-level trigger for now.

	  (defm get-rule-neighborhood (nodes)
		(let ((r nil))
		  (dolist (node nodes)
			(when (eq (hget node 'type) 'rule)
			  (setq r (append (hget-inverse-all node 'rule) r))))
		  r))
		
 	  ;; Calls (cont <edge-creation-status> <matched-edges-list> <deleted-edges> <matched-and-new-edges-per-env>)
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <matched-edges-list> == edges matched, of form (edges ...), one set of edges for each env 
	  ;; <matched-and-new-edges-per-env> ==  ((matched-edges new-edges) ...)   One set per env, where matched-edges and new-edges here are just straight lists thereof
	  ;;																	Note can get overlap: Only want to add edge once; thus, pick an arbitrary env if it appears in more than one.
	  ;;
	  ;; new-rule-format: Do any sets of lists say preds/edges need to track in order anymore?

	  (defm add-consequent-edges (obj-node pred-edges add-edges not-edges del-edges rule-node envlist &key cont)
		(timer 'add-consequent-edges					;; !!!!!! If use cps, timer will include more than this call
		  (lambda ()
			(! (self do-add-consequent-edges) obj-node pred-edges add-edges not-edges del-edges rule-node envlist cont))))

	  (let ((env-new-edges (make-sur-map)))	
		(let ((new-or-dup-edge-list nil))			;; should be new or dup or print
		  (let ((all-node-hash (make-hash-table :test #'equal)))
			(let ((new-node-hash (make-hash-table :test #'equal)))
			  (let ((match-count-hash (make-hash-table :test #'equal))) ;; Temp for testing?  Count how many of a seqno-rule pair we have
				(defm get-match-count-list ()
				  (hash-table-to-list match-count-hash))
				(defm get-match-count-hash ()
				  match-count-hash)
				(defm do-add-consequent-edges (obj-node pred-edges add-edges not-edges del-edges rule-node envlist cont)
				  (defr
					;; Delete edges after evaluating their elements as vars. 
					;; First evaluates not-edges and for each env if any not-edge exists, then bail from that env
					;; Returns a list of deleted edges (which only exist as lists, not edges)
					(defl del-consequent-edges (edges not-edges envlist)
					  (let ((r nil))
						(check-rule-trace (hget rule-node 'name) (list 'del-consequent-edges edges not-edges envlist))
						(dolist (env envlist)
						  (block b
							(dolist (edge not-edges)
							  (let ((not-edge 
									 (mapcar (lambda (node)
											   (env-lookup node env))
											 edge)))
								(when (edge-exists not-edge)
								  (return-from b nil))))
							(dolist (edge edges)
							  (let ((del-edge 
									 (mapcar (lambda (node)
											   (env-lookup node env))
											 edge)))
								(when del-edge
								  (check-rule-trace (hget rule-node 'name) (list 'del-edge del-edge))
								  (rem-edge del-edge)
								  (setq r (cons del-edge r)))
								(! (env-triggered-table removed-edge) del-edge)))))
						r))
					(defl preds-contain-new-node-var (var)
					  (block b
						(dolist (pred-edge pred-edges)
						  (when (and (equal var (first pred-edge))
									 (eq (second pred-edge) 'new-node))
							(return-from b t)))))
					(defl has-no-adds (add-edges)
					  (block b
						(dolist (edge add-edges)
						  (when (not (eq (first edge) 'print))
							(return-from b nil)))
						t))
					(defl get-nodes-to-exec ()
					  (let ((exec-edge (first (get-edges-from-subqet '(exec)))))
						(if exec-edge
							(let ((exec-list (rest exec-edge)))
							  (rem-edge exec-edge)
							  exec-list))))
					(defl get-nodes-to-queue (new-edges new-node-hash all-node-hash)
					  (let ((queue-edge (first (get-edges-from-subqet '(queue)))))
						(if queue-edge
							(let ((queue-list (rest queue-edge)))
							  (rem-edge queue-edge)
							  queue-list)
							(let ((all-nodes (hash-table-value-to-list all-node-hash)))
							  (let ((new-nodes (hash-table-value-to-list new-node-hash)))
								(let ((rule-neighborhood-nodes (! (self get-rule-neighborhood) all-nodes)))
								  (let ((nodes-with-rules (mapcad (lambda (node)
																	(when (hget node 'rule)
																	  node))
																  all-nodes)))
									;; (print (list 'gnq all-nodes nodes-with-rules (length all-nodes) (length nodes-with-rules)))
									(let ((nodes (hunion
												  all-nodes
												  (hunion
												   nil ;; new-nodes
												   (hunion
													rule-neighborhood-nodes
													(hunion
													 nil ;; nodes-with-rules
													 nil))))))
									  nodes))))))))
					(defl old-get-nodes-to-queue (new-edges new-node-hash all-node-hash)
					  (let ((all-nodes (hash-table-value-to-list all-node-hash)))
						(let ((new-nodes (hash-table-value-to-list new-node-hash)))
						  (let ((rule-neighborhood-nodes (! (self get-rule-neighborhood) all-nodes)))
							(let ((nodes-with-rules (mapcad (lambda (new-edge)
															  (when (and (= (length new-edge) 3)
																		 (eq (second new-edge) 'rule))
																(first new-edge)))
															new-edges)))
							  (let ((nodes (hunion
											all-nodes
											(hunion
											 nil ;; new-nodes
											 (hunion
											  rule-neighborhood-nodes
											  (hunion
											   nil ;; nodes-with-rules
											   nil))))))
								nodes))))))
					(let ((r nil)
						  (first-edge nil)
						  (rule-name (hget rule-node 'name))
						  (matched-edges nil)
						  (deleted-edges nil)
						  (matched-and-new-edges-per-env nil))
					  (dolist (env envlist)
						(timer 'add-consequent-edges-per-env
						  (lambda ()
							(let ((root-var (env-lookup '?root-var env)) ;; Not used?
								  (print-list nil))
							  (clrhash all-node-hash)
							  (! (env-new-edges clear))
							  (setq new-or-dup-edge-list nil)
							  (block yyy
								(let ()
								  (if (! (env-triggered-table rule-has-been-triggered) rule-node env)
									  (let ()
										(check-rule-trace rule-name `(add-consequent-edges already-triggered rule ,rule-node ,rule-name obj ,obj-node))
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
										(let ((trig-insert-called nil))
										  (clrhash new-node-hash)
									  
										  ;; Check here if need to do a del if there are no adds. Do this here since will fall through the next loop if no adds are present.
										  (when (has-no-adds add-edges)
											(setq deleted-edges (append deleted-edges (del-consequent-edges del-edges not-edges envlist))))

										  (dolist (add-edge add-edges)
											(let ((edge-is-print (eq (first add-edge) 'print)))	;; This avoids sensing resulting edges as prints, as happened with for-rule
											  (block xxx
												(let ((new-edge nil))
												  (dolist (node add-edge)
													(let ((new-node (let ((sn (env-lookup node env)))			;; dump-sn
																	   (if (and (is-var-name node)
																				(is-scoped-new-pool-node sn)
																				(preds-contain-new-node-var node))
																		   (let ((nn (gethash sn new-node-hash)))
																			 (when (null nn)
																			   (setq nn (setf (gethash sn new-node-hash)
																							  (new-obj-node))))
																			 nn)
																		   sn))))
													  ;; Bound value will be a list if the var was a rest var, so append the contents in that case
													  ;; (print (list 'ace1  new-edge new-node))
													  (setq new-edge (append new-edge
																			 (cond
																			  ((listp new-node)
																			   new-node)
																			  ((eq new-node :undefined)
																			   nil)
																			  (t
																			   (list new-node)))))))
												  (when (eq (first new-edge) 'name-attr)
													(let ((rule (second new-edge))
														  (name (node-name-cat (rest (rest new-edge)))))
													  (setq new-edge `(,rule name ,name))))
												  (when edge-is-print ;; For debug -- if print is head node of an edge, print it
													(setq print-list (append print-list (list (rest new-edge)))))
												  (setq new-or-dup-edge-list (append new-or-dup-edge-list (list new-edge)))
												  (when (and (not (edge-exists new-edge))
															 (not (eq (first new-edge) 'print)))
													(when (null first-edge)
													  (setq first-edge new-edge)
													  ;; Be sure we're adding edges before deleting anything, i.e., want to avoid extra deletes
													  (setq deleted-edges (append deleted-edges (del-consequent-edges del-edges not-edges envlist))))
													(when (not trig-insert-called)
													  (! (env-triggered-table insert) rule-node env)
													  (setq trig-insert-called t))
													(dolist (new-node new-edge)
													  (setf (gethash new-node all-node-hash) new-node))
													(add-edge new-edge)
													(! (env-new-edges insert) new-edge new-edge))))))
										  ;; (print new-node-hash)
										  )))))
							  (check-rule-trace rule-name `(add-consequent-edges edge-scan-done rule ,rule-node ,rule-name obj ,obj-node ,(hash-table-count all-node-hash)))
							  ;;
							  ;; 11/10/22 Trying this to fix the empty-add print problem. 
							  ;;
							  (when (and (= (hash-table-count all-node-hash) 0)
										 print-list
										 (has-no-adds add-edges))
								(dolist (p print-list)
								  ;; Note recording this count does not tell us for sure how many matches, since there
								  ;; may be more than one print per add. So we assume just one here.
								  (setf (gethash (list seqno rule-name) match-count-hash)
										(+ (or (gethash (list seqno rule-name) match-count-hash) 0) 1))
								  (setf (gethash rule-name match-count-hash)
										(max (or (gethash rule-name match-count-hash) 0) (gethash (list seqno rule-name) match-count-hash)))
								  ($nocomment (print (list seqno (get-log-seqno) p)))))		;; Perf test taking out prints shows no speed diff
							  (when (not (= (hash-table-count all-node-hash) 0))
								(dolist (p print-list)
								  (setf (gethash (list seqno rule-name) match-count-hash)
										(+ (or (gethash (list seqno rule-name) match-count-hash) 0) 1))
								  (setf (gethash rule-name match-count-hash)
										(max (or (gethash rule-name match-count-hash) 0) (gethash (list seqno rule-name) match-count-hash)))
								  ($nocomment (print (list seqno (get-log-seqno) p)))
								  )
								(setq r t)
								(let ((me (matched-edges pred-edges env)))
								  (setq matched-edges (cons me matched-edges))
								  ;; Note use of ordered list of potentially redundant edges for tracing. Print edges are also included. 
								  (setq matched-and-new-edges-per-env (cons (list me new-or-dup-edge-list #|(! (env-new-edges results))|# ) matched-and-new-edges-per-env)))
								(when (edge-exists `(rule-break ,rule-name))
								  (cerror "Good luck!" (format nil "Rule-break ~a" rule-name)))
								;; (add-consequent-edges-info (list env (hash-table-to-list all-node-hash)))
								)
							  (let ((nodes (get-nodes-to-queue (! (env-new-edges results))  new-node-hash all-node-hash)))
								($comment
								 (when nodes 
								   (print (list 'ace2 nodes))))
								(dolist (node nodes)
								  (when (has-rules node)
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
																:requeue))))))))))
							  (let ((nodes (get-nodes-to-exec)))
								(dolist (node nodes)
								  (queue-node node :push-head t)))))))
					  (when nil ;; r
						(print (list 'a (hget rule-node 'name) matched-and-new-edges-per-env)))
					  ;; (print (list 'ace3 (! (new-edges as-list)) matched-edges))
					  (funcall cont r matched-edges deleted-edges matched-and-new-edges-per-env)))))))))
	
	  (defm trace-rule (rule-name)
		(add-edge `(rule-trace ,rule-name)))

	  (defm untrace-rule (rule-name)
		(rem-edge `(rule-trace ,rule-name)))

	  (defm break-rule (rule-name &optional (key t))
		(add-edge `(rule-trace ,rule-name))
		(add-edge `(rule-break ,rule-name ,key)))

	  (defm unbreak-rule (rule-name)
		(rem-edge `(rule-break ,rule-name)))
#|
:u
:u
:u
:u
:u
:u
:u
:u
:u
|#
	  (defm check-rule-trace (rule-name &optional l)
		(let ((r (edge-exists `(rule-trace ,rule-name))))
		  (when r
			(let ((b (hget 'rule-break rule-name)))
			  (if b
				  (when 
					  (or (eq b t)
						  (eq b (first l)))
					(let ((msg (format nil "rule-break ~a ~a ~a" rule-name b l)))
					  (cerror "xxx" msg)))
				  (when l
					(print `(rule-trace ,@l))))))
		  r))

	  (defm rule-stats (&optional (sort-colno 1))
		(! (rule-stats rule-stats) sort-colno))

	  (defm update-last-expand-len (rule-node len)
		(! (rule-stats update-last-expand-len) rule-node len))

	  (defm update-new-edges-max-expand-len (rule-node)
		(! (rule-stats update-new-edges-max-expand-len) rule-node))

	  (defm get-matched (rule-node)
		(! (rule-stats get-matched) rule-node))

	  (defm node-stats ()
		node-stats)

	  ;; Calls (cont <edge-creation-status> <match-status> <new-edges> <matched-edges> <deleted-edges>)
	  ;; 
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <match-status> == (match-status)
	  ;; <new-edges> == edges created
	  ;; <matched-edges> == of form (edges ...), one set of edges per env
	  ;; <deleted-edges> == t if any edges were deleted, else nil

	  (defm match-and-execute-rule (rule-node obj-node &key (cont (lambda (r match-status new-edges matched-edges deleted-edges)
																	(list r match-status new-edges matched-edges deleted-edges))))
		(bool-timer 'match-and-execute-rule-true 'match-and-execute-rule-false 
		  (lambda ()
			(let ((r nil)
				  (match-status nil)
				  (new-edges nil)
				  (matched-edges-list nil)
				  (deleted-edges nil))
			  (when (not (edge-exists `(,rule-node disabled)))
				(let ((rule-name (hget rule-node 'name)))
				  ($comment		;; check-rule-trace: why is this commented-out?
				   (when (or am-traced
							 (edge-exists `(rule-trace ,rule-name)))
					 (print `(rule-trace match-and-execute-rule tested rule ,rule-node ,rule-name obj ,obj-node))))
				  (check-rule-trace rule-name `(match-and-execute-rule tested rule ,rule-node ,rule-name obj ,obj-node))
				  (let ((envlist (all-matches rule-node obj-node)))
					(! (rule-stats update-tested) rule-node)
					(if envlist
						(let ((rule-comps (get-rule-components rule-node)))
						  (let ((pred-list (! (rule-comps preds)))
								(del-list (! (rule-comps dels)))
								(add-list (! (rule-comps adds)))
								(not-list (! (rule-comps nots))))
							(! (rule-stats update-matched) rule-node)
							(check-rule-trace rule-name `(match-and-execute-rule matched rule ,rule-node ,rule-name envlist ,envlist))
							;; Edges to be deleted are so deleted in add-consequent-edges. All dels are done before adds.
							(add-consequent-edges obj-node pred-list add-list not-list del-list rule-node envlist :cont
							  (lambda (m x-matched-edges-list x-deleted-edges matched-and-new-edges-per-env)
								(when m
								  ;; (print (list 'me1 rule-name x-matched-edges-list x-deleted-edges matched-and-new-edges-per-env))
								  ;; (print (list 'me2 rule-name pred-list add-list matched-and-new-edges-per-env))
								  )
								(setq matched-edges-list x-matched-edges-list)
								(setq deleted-edges x-deleted-edges)
								(when (or m
										  deleted-edges)
								  (log-stat 'new-edge-rule rule-name))
								(when deleted-edges
								  (dolist (del-edge deleted-edges)
									(! (edge-to-trace insert) del-edge (list 'del seqno rule-node rule-name obj-node))))
								(if m
									(let ()
									  (setq match-status :new-edges)

									  ($nocomment
									   (log-stat 'entropy (edge-asc-entropy))
									   (log-stat 'dist (edge-asc-dimension-dist)))

									  ($nocomment
									   ;;
									   ;; better-root-var
									   ;;
									   ;; 5/2/23
									   ;; Experiment -- after a certain number of successes, as recorded by the
									   ;; root-var-success-cnt attr, designate a set of root vars from the used-root-var
									   ;; attr, thus cementing them in, and other vars in the rule thereafter won't be tried.
									   ;;
									   ;; This heuristic should at least be based on the number of vars in a rule. The
									   ;; limit is chosen based on observation, and clearly can fail. So this is just a test.
									   ;;
									   ;; Thus far, looks like a good speed boost, with 2^3 fft std bench going from 24 to 20 secs.
									   ;;
									   (let ()
										 (when (not (edge-exists (list rule-node 'used-root-var-processed)))
										   (dolist (env envlist)
											 (let ((root-var (env-lookup '?root-var env :idempotent nil)))
											   ($comment		;; Some are nil because they're the nested form, e.g., ?root-var-1
												(when (null root-var)
												  (print (list 'me1 rule-node rule-name env))))
											   (when root-var
												 (add-edge (list rule-node 'used-root-var root-var))
												 (let ((cnt (hget rule-node 'root-var-success-cnt)))
												   (rem-edge (list rule-node 'root-var-success-cnt cnt))
												   (add-edge (list rule-node 'root-var-success-cnt (+ (or cnt 0) 1)))))))
										   (when (> (or (hget rule-node 'root-var-success-cnt) 0) 7)
											 (add-edge (list rule-node 'used-root-var-processed))
											 (rem-edge (list rule-node 'root-var :undefined))
											 (dolist (root-var (hget-all rule-node 'used-root-var))
											   (add-edge (list rule-node 'root-var root-var)))))))
									  
									  (! (rule-stats update-new-edges) rule-node)
									  (log-stat 'new-edge-by-rule 5000)
									  (update-new-edges-max-expand-len rule-node)
									  (setq r t))
									(let ()
									  (setq match-status :no-new-edges)
									  (! (rule-stats update-not-new-edges) rule-node)))
								(cond
								 ((eq match-status :new-edges)
								  (let ((rule-name (hget rule-node 'name)))
									(dolist (mne-entry matched-and-new-edges-per-env)
									  (let ((matched-edges (first mne-entry)))
										(let ((new-edges (second mne-entry)))
										  ;; (print (list 'me10 add-list new-edges))
										  (dolist (matched-edge matched-edges)
											(! (edge-to-trace insert) matched-edge (list 'pred seqno rule-node rule-name obj-node)))
										  (dolist (new-edge new-edges)
											(! (edge-to-trace insert) new-edge (list 'add seqno rule-node rule-name obj-node)))
										  (dolists ((matched-edge matched-pred) (matched-edges (second (filter-new-node-pred-edges pred-list))))
											(! (edge-to-pred insert) matched-edge (list seqno rule-name matched-pred)))
										  (dolists ((new-edge add) (new-edges add-list))
											(! (edge-to-add insert) new-edge (list seqno rule-name add)))))
									  (setq seqno (+ seqno 1)))))
								 ((eq match-status :no-new-edges)
								  (! (edge-to-trace insert) (list obj-node) (list 'tested match-status seqno rule-node rule-name obj-node)))
								 ((not (memq match-status (match-status)))
								  (print (list 'match-status-bogus match-status))))
								))))
						(let ()
						  (setq match-status :failed)
						  (! (rule-stats update-failed) rule-node)
						  (! (edge-to-trace insert) (list obj-node) (list 'tested match-status seqno rule-node rule-name obj-node))
						  )))))
			  (setq seqno (+ seqno 1))
			  (funcall cont r match-status matched-edges-list deleted-edges)))))

	  (defm expand-rule-obj-edges (rule-graph obj-node rule-nodepos root-var)
		(timer 'expand-rule-obj-edges
		  (lambda ()
			(defr
			  (defl xvar-match-filter-edges (x y z) y)
			  (let ((rule-node (! (rule-graph rule-node))))
				(let ((obj-edges-result nil))
				  (do ((rule-edge-list 
						(bipartite-breadth-rule-walk-seq rule-graph root-var) ;;;;;;;;;      !!!!!!!!!!!!!!! ;
						(rest rule-edge-list))
					   (obj-edges
						(expand-edges `((,obj-node)) rule-nodepos root-var rule-node)
						(expand-edges (var-match-filter-edges (first rule-edge-list) obj-edges rule-node) rule-nodepos root-var rule-node)))
					  ((null rule-edge-list) nil)
					  (setq obj-edges-result (hunion obj-edges-result (var-match-filter-edges (first rule-edge-list) obj-edges rule-node)))
					  (check-rule-trace (hget rule-node 'name) `(expand-rule-obj-edges rule ,rule-node ,(hget rule-node 'name)
																					   rule-edges ,(first rule-edge-list) obj-edges ,obj-edges-result)))
				  (update-last-expand-len rule-node (length obj-edges-result))
				  obj-edges-result))))))

	  ;; Is there a reason to keep this? (check-rule-trace)
	  
	  (defm old-expand-rule-obj-edges (rule-graph obj-node rule-nodepos root-var)
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
													rule-edges ,(first rule-edge-list) obj-edges ,obj-edges-result)))))
						(update-last-expand-len rule-node (length obj-edges-result))
						obj-edges-result)))))))))

	  ;; Given a set of edges, gets all their nodes, and returns the edges of those nodes
	  ;; rule-node passed for tracing purposes only

	  (defm expand-edges (edges rule-nodepos root-var rule-node)
		(timer 'expand-edges
		  (lambda ()
			(let ((visit-hash expand-edges-visit-hash))
			  (clrhash visit-hash)
			  (let ((nodes (nodes edges)))
				(dolist (node nodes)
				  (let ((child-edges (get-children node 5 rule-nodepos root-var)))
					(dolist (child-edge child-edges)
					  (setf (gethash child-edge visit-hash) child-edge)))))
			  (let ((r nil))
				(maphash (lambda (k v)
						   (setq r (cons v r)))
						 visit-hash)
				(check-rule-trace (hget rule-node 'name) `(expand-edges ,r))
				r)))))

	  (defm get-children (bnode sigma-span rule-nodepos root-var)
		(let ((r (cond
				  ((is-sigma-node bnode)
				   (append (get-obj-edges bnode)
						   (sigma-edges (hget-edge-inverse bnode 'sigma) sigma-span)))
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
				  (t nil))))
		  r))

	  ;; Filter based on var-based match, ordered edges. Returns obj-edges which match a pattern in rule-edges.
	  ;;
	  ;; 5/20/23: Note support for rest-vars added below.
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
			  ;; This is redundant with the objgraph method match-one-edge, but does not need to construct an env, thus
			  ;; allowing it to be much faster. But it means we need to be sure the functions track.
			  (defl match-one-edge (pat-edge obj-edge)
				(when (or (= (length pat-edge) (length obj-edge))
						  (has-rest-var pat-edge))
				  (block b
					(dolist (pat-node pat-edge)
					  (let ((obj-node (first obj-edge)))
						(cond
						 ((is-rest-var-name pat-node)
						  (return-from b t))
						 ((not (or (is-var-name pat-node)
								   (equal pat-node obj-node)))
						  (return-from b nil))
						 (t
						  (setq obj-edge (rest obj-edge))))))
					t)))
			  (let ((r nil))
				(dolist (rule-edge rule-edges)
				  (dolist (obj-edge obj-edges)
					(when (match-one-edge rule-edge obj-edge)
					  (setq r (cons obj-edge r)))))
				(let ((r (dedup-list r)))
				  (check-rule-trace (hget rule-node 'name)
									`(var-match-filter-edges rule ,rule-node ,(hget rule-node 'name) 
															 obj-edges-in ,obj-edges obj-edges-out ,r))
				  r))))))

	  ;; Is there a reason to keep this?

	  (defm old-var-match-filter-edges (rule-edges obj-edges rule-node)
		(timer 'var-match-filter-edges
		  (lambda ()
			(defr
			  ;; This is redundant with the objgraph method match-one-edge, but does not need to construct an env, thus
			  ;; allowing it to be much faster. But it means we need to be sure the functions track
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
										  obj-edges-in ,obj-edges obj-edges-out ,r)))
					r)))))))

	  ;; If any rule edge contains no common nodes, error, since would
	  ;; likely be missing many nodes which should match. This is really
	  ;; saying that we should not have a rule edge which is *all* vars.
	  ;;
	  ;; Also if common-nodes is empty, return nil. This seems drastic, but
	  ;; otherwise there is too much searching. So a rule must have at least
	  ;; one node in commmon with a matching object

	  (defm check-rule-edges (rule-edges common-nodes)
		(block b1
		  ;; (return-from b1 t)		;; all-var-mod: rerturn t from this if enabling all-vars
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
			  nil)))

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
	  ;; elements of nodes.
	  ;;
	  ;; In Berge, this would be a transversal of the hypergraph {edges} based on the set {nodes}.

	  (defm edges-with-nodes (nodes edges)
		(block b
		  ;; (return-from b edges) ;; all-var-mod: Return the edges is enabling all-vars
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

	  (defm all-matches (rule-node obj-node) ;; all-var-mod -- not right now [5/18/23 -- not sure what this means]
		(timer 'all-matches
		  (lambda ()
			(defr
			  (defl all-matches-hash-equal (x y)
				(and (equal (first x) (first y))
					 (env-equal (second x) (second y))))
			  (let ((rule-graph nil))
				(let ((root-vars (get-root-vars rule-node)))
				  (define-hash-table-test all-matches-hash-test all-matches-hash-equal set-hash)
				  (let ((h (make-hash-table :size 7 :test 'all-matches-hash-test)))
					(let ((possible-match-fcn (possible-match-fcn rule-node obj-node)))
					  (block b
						(dolist (root-var root-vars)
						  (let ((pinfo (funcall possible-match-fcn root-var)))
							;; (print (list 'i pinfo root-var))
							(let ((poss-match (first pinfo)))
							  (let ((scan-subst-status (second pinfo)))
								(when poss-match
								  ($comment (print (list 'am4 (hget rule-node 'name) scan-subst-status)))
								  (if (and (not (null scan-subst-status))
										   (not (eq scan-subst-status :unknown))
										   (let ((poss-match-env scan-subst-status))
											 ($comment (print (list 'am5 (hget rule-node 'name) poss-match-env)))
											 (! (env-triggered-table rule-has-been-triggered) rule-node poss-match-env)))
									  (let ()
										;; (print (list 'g2 (hget rule-node 'name) scan-subst-status))
										nil)
									  (let ()
										(when (null rule-graph)
										  (setq rule-graph (make-rule-graph rule-node)))
										;; (print (list 'p (hget rule-node 'name) root-var obj-node))
										(let ((envlist (all-matches-aux rule-graph obj-node root-var)))
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
											;; (print (list 'h (hget rule-node 'name) root-var obj-node scan-subst-status))
											)
										  (when envlist
											($comment (print (list 'am1 (hget rule-node 'name) obj-node root-var envlist #| scan-subst-status |# )))
											($comment 
											 (let ((rule-trace (edge-exists `(rule-trace ,(! (rule-graph name))))))
											   (let ((verbose (if rule-trace t '(s11))))
												 (let ((subst-match (! (rule-graph subst-match) self obj-node root-var :verbose verbose)))
												   (when t (not (envs-equal envlist subst-match))
														 (print (list 'am3 (hget rule-node 'name) obj-node root-var (envs-equal envlist subst-match) envlist subst-match)))))))
											(dolist (env envlist)
											  ;; (print (list 'am2 env))
											  (let ((env-info (list root-var env)))
												(setf (gethash env-info h) env-info)))
											;; (return-from b nil)  ;; To bail or not to bail, that is the question. Today we bail.
																	;; global-change -- no don't bail, get all root vars 
																	;; 5/18/23 -- Keep don't-bail, as dealing with root vars seems fundamental.
											)))))))))))
					(when (not (= (hash-table-count h) 0))
					  (let ((std-var-level (or (hget rule-node 'std-var-level) 0)))
						(let ((envlist nil))
						  (let ((std-bindings `((,(! (std-vars var-base-to-var) '?this-rule std-var-level) ,rule-node)
												(,(! (std-vars var-base-to-var) '?this-rule-name std-var-level) ,(hget rule-node 'name))
												(,(! (std-vars var-base-to-var) '?this-obj std-var-level) ,obj-node))))
							(maphash (lambda (k v)
									   (let ((root-var (first v))
											 (env (second v)))
										 (setq envlist (cons (append `(,@std-bindings
																	   (,(! (std-vars var-base-to-var) '?root-var std-var-level) ,root-var))
																	 env)
															 envlist))))
									 h)
							envlist)))))))))))

	  ;; To disable subst-match for testing, rename this method to something and rename all-matches-aux2 to
	  ;; all-matches-aux

	  (defm all-matches-aux (rule-graph obj-node root-var)
		(if (and (is-var-name root-var)
				 (not (! (rule-graph has-rest-vars))))
			(! (rule-graph subst-match) self obj-node root-var
			   :verbose 
			   nil
			   ;; '(s12)
			   ;; '(s0 s2 s4)
			   )
			(all-matches-aux2 rule-graph obj-node root-var)))

	  (defm all-matches-aux2 (rule-graph obj-node root-var)
		(timer 'all-matches-aux
		  (lambda ()
			(block ama
			  (let ((rule-node (! (rule-graph rule-node))))
				(let ((rule-edges (! ((get-rule-components rule-node) preds))))
				  (when rule-edges ;; If no preds, then it's a data rule, i.e., just adds. Should be executed once in the special loop, not done over and over.
					(let ((sig-span (sigma-span rule-node))
						  (rule-nodepos (nodepos rule-edges))
						  (rule-nodes (nodes rule-edges)))
					  (let ((obj-edges (expand-rule-obj-edges rule-graph obj-node rule-nodepos root-var)))
						($nocomment
						 (check-rule-trace (! (rule-graph name))
										   `(all-matches-aux ,(! (rule-graph name)) ,root-var ,rule-edges ,obj-edges)))
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
			(check-rule-trace (hget rule-node 'name)
							  `(match-pat-obj-edge-lists input rule ,rule-node ,(hget rule-node 'name) ,root-var ,patlist ,objlist))
			(let ((patlist-info (filter-new-node-pred-edges patlist)))
			  (let ((new-node-pred-edges (first patlist-info))
					(patlist (second patlist-info)))
				(let ((new-node-env nil))
				  (dolist (new-node-pred-edge new-node-pred-edges)
					(setq new-node-env (cons (list (first new-node-pred-edge) (third new-node-pred-edge)) new-node-env)))
				  (let ((matches (x-match-all-pat-obj-edge-lists patlist objlist root-var obj-node rule-node)))
					(when matches
					  (let ((matches
							 (if new-node-env
								 (cons (list new-node-env) matches)
								 matches)))
						(let ((envs (cross-aux2 matches :rule-trace-info (when (check-rule-trace (hget rule-node 'name))
																		   (list rule-node (hget rule-node 'name))))))
						  (check-rule-trace (hget rule-node 'name)
											`(match-pat-obj-edge-lists result rule ,rule-node ,(hget rule-node 'name) envs ,envs))
						  envs))))))))))

	  (defm patlist-equal (patlist1 patlist2)		;; Why use set-equal here?
		;;(set-equal patlist1 patlist2)
		(equal patlist1 patlist2))

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
	  ;;	- Each pat in patlist must match at least one edge in objlist
	  ;;

	  (defm x-match-all-pat-obj-edge-lists (patlist
											objlist
											root-var ;; Ignored if null
											obj-node ;; Ditto -- coupled
											rule-node ;; Passed only for diag
											)
		(block b
		  (let ((binding-hash (make-hash-table :test #'equal)) ;; binding to pat
				(var-hash (make-hash-table :test #'equal)) ;; var-to-pat
				(edge-env-to-pat-hash (make-hash-table :test #'equal))
				(pat-to-edge-envs-hash (make-hash-table :test #'equal)))
			(dolist (pat patlist)
			  (dolist (node pat)
				(when (is-var-name node)
				  ;; (setf (gethash node var-hash) (cons pat (gethash node var-hash)))
				  (setf (gethash node var-hash) (hunion (list pat) (gethash node var-hash))) ;; Use union to avoid dups
				  )))
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
				  ($comment
				   (let ((rule-trace (edge-exists `(rule-trace ,(hget rule-node 'name)))))
					 (when rule-trace
					   (print `(rule-trace x-match-pat-obj-edge-lists ,(hget rule-node 'name) ,root-var ,patlist ,objlist)))))
				  ;;		  envs-list	
				  edge-envs-list
				  ))))))

	  (defm has-rest-var (pat-edge)	;; t if has rest var as last node, else nil
		(block b
		  (mapl (lambda (l)
				  (when (null (rest l))
					(return-from b (is-rest-var-name (first l)))))
				pat-edge)))

	  (defm match-one-edge (pat-edge obj-edge 
									 root-var		;; If null, is ignored
									 obj-node		;; If null, is ignored (implied by null root-var
									 rule-node		;; Passed for diag purposes only
									 &key
									 pred-info)		;; If passed, use this to determine how to match a var in the
													;; pat-edge (pat-edge is a pred). For def see subst-match.
		(defr
		  ;; Will return list (rest-var pat-edge-prefix obj-edge-prefix obj-edge-suffix) if the last pat node is a rest var, else nil
		  (defl rest-var-info (pat-edge obj-edge)
			(defr
			  (defl doloop (pat-edge obj-edge pat-edge-prefix obj-edge-prefix obj-edge-suffix)
				(cond
				 ((null pat-edge)
				  nil)
				 ((and (is-rest-var-name (first pat-edge))
					   (null (rest pat-edge)))
				  (list (first pat-edge) pat-edge-prefix obj-edge-prefix obj-edge-suffix))
				 ((null obj-edge)
				  nil)
				 (t 
				  (doloop (rest pat-edge) (rest obj-edge)
						  (append pat-edge-prefix (list (first pat-edge)))
						  (append obj-edge-prefix (list (first obj-edge)))
						  (rest obj-edge)))))
			  (doloop pat-edge obj-edge nil nil nil)))
		  (defl match-one-edge1 (pat-edge obj-edge pred-info)
			(defr
			  (defl match-one-edge-aux (pat-edge obj-edge  pred-info)
				(cond
				 ((null pat-edge)
				  '((t t))) ;; literal-match case; dummy binding
				 ;; If we have only one of the roots, then whole edge does not match
				 ((or (and root-var
						   (not (eq root-var :undefined))
						   (equal (first pat-edge) root-var)
						   (not (equal (first obj-edge) obj-node)))
					  (and root-var
						   (not (eq root-var :undefined))
						   (not (equal (first pat-edge) root-var))
						   (equal (first obj-edge) obj-node)))
				  (return-from match-one-edge1 nil))
				 ((and (is-var-name (first pat-edge))
					   (or (null pred-info)
						   (eq (first  pred-info) 'v)))
				  (cons (list (first pat-edge) (first obj-edge))
						(match-one-edge-aux (rest pat-edge) (rest obj-edge) (rest pred-info))))
				 ((equal (first pat-edge) (first obj-edge))
				  (match-one-edge-aux (rest pat-edge) (rest obj-edge) (rest pred-info)))
				 (t
				  (return-from match-one-edge1 nil))))
			  (match-one-edge-aux pat-edge obj-edge  pred-info)))
		  (if (not (has-rest-var pat-edge))
			  (when (= (length pat-edge) (length obj-edge))
				(let ((r (dedup-list (match-one-edge1 pat-edge obj-edge  pred-info)))) ;; dedup added here -- not clear needed, but it's for truth and beauty
				  r))
			  (let ((rest-var-info (rest-var-info pat-edge obj-edge)))
				(let ((rest-var (first rest-var-info)))
				  (let ((pat-edge-prefix (second rest-var-info)))
					(let ((obj-edge-prefix (third rest-var-info)))
					  (when (= (length pat-edge-prefix) (length obj-edge-prefix))
						(let ((obj-edge-suffix (fourth rest-var-info)))
						  (let ((r (dedup-list (match-one-edge1 pat-edge-prefix obj-edge-prefix pred-info)))) ;; pred-info?
							(when r
							  (cons (list rest-var (or obj-edge-suffix :undefined)) r))))))))))))

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


	  ;; Orthogonal!
	  ;; 
	  ;; if *empty* then is global (only)
	  ;; 
	  ;; local						Put on lrp
	  ;; attach-to					Attach to x
	  ;; global						Put on grp
	  ;; 
	  ;; These are all not contradictory
	  ;; 
	  ;; local global
	  ;; local attach-to
	  ;; global attach-to
	  ;; local global attach-to
	  ;; 
	  ;; An empty nested rule is by default attached only at the point of
	  ;; def. It can also be declared local or global or attached within.
	  ;; 
	  ;; (rule
	  ;;  (name abc)
	  ;;  (local)
	  ;;  (global)
	  ;;  (attach-to x)
	  ;;  (pred
	  ;;   ()))
	  ;;

	  (defm define-rule (rule-expr &key (new-pool 0) (unattached nil))
		(when (not (eq (first rule-expr) 'comment))
		  (remove-named-rule rule-expr)
		  (let ((edge-list nil)
				(add-to-nodes nil)
				(add-to-lrp nil)
				(add-to-grp nil)
				(new-var-cnt 0)
				(new-sn-cnt 0)
				(clause-types-added nil))
			(defr

			  ;; Shadowing add-edge -- we dont't want to add an edge if the rule is nested
			  (defl add-edge (edge)
				(if (= new-pool 0)
					(! (self add-edge) edge)
					edge))

			  (defl pushe (edge)
				(setq edge-list (cons edge edge-list)))

			  ;; Doing this for refs in a rule is not right! Commented-out.
			  (defl xform-std-vars (edge) ;; Use new-pool level to scope the std vars (?this-obj, etc.)
				($comment (mapcar (lambda (x)
									(! (std-vars var-base-to-var) x new-pool))
								  edge))
				edge)

			  (defl new-var ()
				(let ((r (intern (format nil "?_X-~a-~a" new-pool new-var-cnt))))
				  (setq new-var-cnt (+ new-var-cnt 1))
				  r))

			  (defl new-sn ()
				(let ((r (intern (format nil "SN-~a-~a" new-pool (+ new-sn-cnt 100)))))
				  (setq new-sn-cnt (+ new-sn-cnt 1))
				  r))

			  (defl new-rule-node ()
				(if (= new-pool 0)
					(new-obj-node)
					(new-var)))

			  (let ((rule (new-rule-node)))
				(let ()
				  (let ((rule-expr rule-expr))
					(dolist (clause (rest rule-expr))
					  (let ((clause-type (first clause)))
						(cond
						 ((eq clause-type 'name)
						  (let ((rule-name (second clause)))
							(if (not (listp rule-name))
								(pushe (add-edge (list  rule 'name rule-name)))
								(pushe (add-edge `(name-attr ,rule ,@rule-name))))))
						 ((eq clause-type 'root-var)
						  (let ((root-var (second clause)))
							(pushe (add-edge (list rule 'root-var root-var)))))
						 ((eq clause-type 'no-triggered)
						  (pushe (add-edge `(,rule no-triggered))))
						 ((eq clause-type 'local)
						  (setq add-to-lrp t)
						  (pushe (add-edge `(,rule local))))
						 ((eq clause-type 'global)
						  (setq add-to-grp t))
						 ((eq clause-type 'disabled)
						  (pushe (add-edge `(,rule disabled))))
						 ((eq clause-type 'attach-to)
						  (dolist (node (rest clause))
							(setq add-to-nodes (cons node add-to-nodes))
							(pushe (add-edge `(,rule attach-to ,node)))))
						 ((member clause-type '(add) :test #'eq)
						  (dolist (edge (rest clause))
							(let ((edge (xform-std-vars edge)))
							  (cond
							   ((and (listp (third edge)) (eq (first (third edge)) 'rule))
								(let ((rule-values (define-rule
													 (third edge)
													 :new-pool (+ new-pool 1)
													 :unattached t)))
								  (let ((rule-node (first rule-values))
										(rule-edges (second rule-values)))
									(pushe (add-edge (list rule 'add rule 'nested-rule rule-node)))
									(pushe (add-edge (list rule 'pred rule-node 'new-node (new-sn))))
									(pushe (add-edge (list rule 'add (first edge) (second edge) rule-node)))
									(dolist (rule-edge rule-edges)
									  (pushe (add-edge (append (list rule 'add) rule-edge)))))))
							   (t
								(pushe (add-edge (append (list rule clause-type) edge))))))))
						 ((member clause-type '(pred del not) :test #'eq)
						  (setq clause-types-added (cons clause-type clause-types-added))
						  (dolist (edge (rest clause))
								(let ((edge (xform-std-vars edge)))
								  (pushe (add-edge (append (list rule clause-type) edge))))))))))


				  ;; We expect all rules to have a certain required set of properties, otherwise they cannot be
				  ;; copied. Required are pred, add, del, not, root-var, type, std-var-level, name. All but name are
				  ;; filled in by define-rule with defaults if not specified. So nameless rules cannot be
				  ;; copied. Properties of rule attachment (local, ...) given in a rule def do not copy; the rule
				  ;; invoking the copy is responsible for putting it where needed.

				  (when (not (memq 'del clause-types-added))
					(pushe (add-edge (list rule 'del))))
				  (when (not (memq 'not clause-types-added))
					(pushe (add-edge (list rule 'not))))

				  (when (null (hget rule 'root-var))
					(pushe (add-edge (list rule 'root-var :undefined))))

				  (pushe (add-edge (list rule 'type 'rule)))
				  (pushe (add-edge (list rule 'std-var-level new-pool)))

				  (when (not unattached)
					(when add-to-nodes
					  (dolist (n add-to-nodes)
						(add-edge (list n 'rule rule))))
					(when add-to-lrp
					  (add-edge (list local-rule-pool 'lrp-rule rule)))
					(setq add-to-grp (or add-to-grp
										 (and (null add-to-nodes)
											  (not add-to-lrp))))
					(when add-to-grp
					  (add-edge (list global-rule-pool 'grp-rule rule))))

				  (list rule edge-list)))))))

	  (defm bipartite-breadth-rule-walk-seq (rule-graph root-var)
		(timer 'bipartite-breadth-rule-walk-seq
		  (lambda ()
			(let ((root-node (if root-var root-var (first (first rule-edges))))) ;; !!!!!!!!! Fix: root-var heuristic
			  (let ((rule-edges-list nil))
				(bipartite-breadth-rule-walk rule-graph root-node :result-fcn
											 (lambda (x)
											   (when (is-edge (first x))
												 (setq rule-edges-list (append rule-edges-list (list x))))))
				(let ((prev nil)
					  (r nil))
				  (dolist (rule-edges rule-edges-list)
					(setq r (cons (set-subtract rule-edges prev) r))
					(setq prev rule-edges))
				  (let ((r (reverse r)))
					(check-rule-trace (! (rule-graph name)) 
									  `(bipseq ,r))
					r)))))))

	  (defm bipartite-breadth-rule-walk (rule-graph root-node &key result-fcn)
		(let ((visit-hash (make-hash-table :test #'equal)))
		  (defr
			(defl bwb (bnodes)
			  (when am-traced
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

	  ;; 5/2/23 Modified to get all root vars designated by the rule, rather than just assuming one.
	  ;; 
	  ;; [12/26/16, amended 4/8/17: We used to use a heuristic for the root-var if none was given, but we really needed
	  ;; to expand this to trying all rule nodes. Just as well -- trying all nodes is more in the spirit of the H
	  ;; language, where I wish to relieve the rule writer of the burden of having to know absolutely where to attach a
	  ;; rule. Wrt performance, we're taking advantage of the not-bad pre-matching heuristics (esp. the possible-match
	  ;; method), which can throw out most non-matches quickly.]
	  ;;
	  ;; Root vars are all vars or consts of a rule pred, excluding those vars or consts associated with the new-node
	  ;; pseudo-relation. E.g., the edge (?nn1 new-node sn1) would be eliminated, as well as all the nodes contained
	  ;; therein. Note need to use the set-subtract method below esp. because the format of the var name in a new-node
	  ;; edge is not prescribed, so we can have e.g. (?x new-node sn1).

	  (defm get-root-vars (rule-node)
		(let ((root-vars (set-subtract (hget-all rule-node 'root-var) '(:undefined))))
		  (let ((r (or root-vars
					   (let ((rule-edges-info (filter-new-node-pred-edges (! ((get-rule-components rule-node) preds)))))
						 (let ((new-node-rule-edges (first rule-edges-info)))
						   (let ((other-rule-edges (second rule-edges-info)))
							 (let ((new-node-rule-edge-nodes (nodes new-node-rule-edges)))
							   (let ((other-rule-edge-nodes (nodes other-rule-edges)))
								 (set-subtract other-rule-edge-nodes new-node-rule-edge-nodes)))))))))
			r)))

	  (defm old-get-root-vars (rule-node)
		(let ((root-var (hget rule-node 'root-var)))
		  (let ((r (or (and root-var 
							(not (eq root-var :undefined))
							(list root-var))
					   (let ((rule-edges-info (filter-new-node-pred-edges (! ((get-rule-components rule-node) preds)))))
						 (let ((new-node-rule-edges (first rule-edges-info)))
						   (let ((other-rule-edges (second rule-edges-info)))
							 (let ((new-node-rule-edge-nodes (nodes new-node-rule-edges)))
							   (let ((other-rule-edge-nodes (nodes other-rule-edges)))
								 (set-subtract other-rule-edge-nodes new-node-rule-edge-nodes)))))))))
			r)))

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

	  ;; 3/25/19 -- Troubles with this new possible-match-fcn when
	  ;; tried with fe-rule-test. Had to change from detecting
	  ;; consts in scan-and-subst to just checking edge existence,
	  ;; since the copiers can have vars in the results. Also in
	  ;; scan-and-subst had to change from detecting a null env to
	  ;; checking that it's stopped changing on each loop. Otherwise
	  ;; we always get a binding to a var.
	  ;;
	  ;; Also took out chain-check -- probably breaks due to the
	  ;; need to handle vars in some way not accounted for in the
	  ;; chain processing.  However scan-and-subst seems to
	  ;; supersede its need anyway.
	  ;;
	  ;; This could be pointing out that we should be careful in
	  ;; straying too much from the isomorphism theme.

	  (defm possible-match-fcn (rule-node obj-node)	;; all-var-mod
		(timer 'possible-match-fcn
		  (lambda ()
			(macrolet ((hprint (&rest args)
							   nil
							   ;; `(print (list ,@args))
							   ))
			  (defr
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
													 (null (env-no-conflict-dedup envout))
													 (env-equal envout prev-envout))
											 (return-from b nil))
										   (setq edge-count-prod 1)
										   (setq status nil)
										   (setq new-preds (subst new-preds env)) ;; Does subst of vars via env
										   (let ((new-env (let ((new-pred-qets (remove-vars new-preds)))
															(hprint 'e new-preds env new-pred-qets)
															(mapunion (lambda (pred pred-qet) ;; Should provide an env of new bindings 
																		(let ((edge-count (lookup-qet-edge-count pred-qet)))
																		  (let ((edge-count (or edge-count 
																								(insert-qet-edge-count pred-qet
																													   (! (self count-edges-from-subqet) pred-qet)))))
																			(setq edge-count-prod (* edge-count-prod edge-count))
																			(hprint 'e1 pred pred-qet)
																			(if (= edge-count 1)
																				(let ((edges (get-edges-from-subqet pred-qet)))
																				  (let ((edge (first edges)))					  ;; Need to do a one-edge match here?
																																  ;; Current algorithm seems to assume a single edge returned in qet lookup
																					(let ((r (mapunion (lambda (pred-node obj-node)
																										 (when (is-var-name pred-node)
																										   (list (list pred-node obj-node))))
																									   pred edge)))
																					  (hprint 'e2 r)
																					  r)))
																				(let ()
																				  ;; Set this flag if we have more than one edge in the qet lookup. Means non-det and need to return :unknown
																				  (when (> edge-count 1)
																					(setq status t))
																				  nil)))))
																	  new-preds
																	  new-pred-qets))))
											 (hprint 'e3 new-env) 
											 (setq prev-envout envout)
											 (setq envout (hunion envout new-env))
											 (setq env new-env))))

										;; At this point new-preds should be all constants and all of its edges must exist for the function to be true
										;;
										;; Also, if we left the last loop and there were any non-unique edges from qets, 
										;; then we have the non-determisitic case and we really need to return :unknown
										
										($comment
										 (when (> edge-count-prod 1)
										   (hprint 'e5 (hget rule-node 'name) obj-node edge-count-prod)))

										(let ((r (block b
												   (dolist (new-pred new-preds)
													 (when (or nil #|(not (check-consts new-pred))|#
															   (not (edge-exists new-pred)))
													   (return-from b nil)))
												   t)))
										  (let ((r (if r
													   (let ()
														 (hprint 'e4 envout preds root-var obj-node)
														 (hunion envout '((t t))))
													   (if status
														   :unknown
														   nil))))
											(gstat 'scan-and-subst-success (lambda (x y) (+ x y)) (lambda () (if (and (not (null r)) (not (eq r :unknown))) 1 0)))
											(gstat 'scan-and-subst-failure (lambda (x y) (+ x y)) (lambda () (if (null r) 1 0)))
											(gstat 'scan-and-subst-unknown (lambda (x y) (+ x y)) (lambda () (if (eq r :unknown) 1 0)))
											($comment
											 (when (and (not (null r)) (not (eq r :unknown)))
											   (print `(scan-and-subst rule ,rule-node ,(hget rule-node 'name) env ,r)))
											 )
											r))))))))))))))
		
			  (defl get-rule-preds-root (preds root-var) ;; Returns <preds-containing-root-var>
				(let ((r nil))
				  (dolist (pred preds)
					(when (member root-var pred :test #'equal)
					  (setq r (cons pred r))))
				  r))

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
				  (hprint 'q (hget rule-node 'name) preds root-var obj-node qets)
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
			  (hprint 'r (hget rule-node 'name) obj-node)
			  (let ((obj-edges (get-edges obj-node)))
				(let ((obj-nodes (nodes obj-edges)))
				  (let ((rule-comps (get-rule-components rule-node)))
					(let ((preds (second (filter-new-node-pred-edges (! (rule-comps preds))))))
					  (let ((pred-const-nodes (get-rule-consts-pred preds)))
						(let ((ipo (intersect pred-const-nodes obj-nodes)))	;; all-var-mod: Change this to (let ((ipo obj-nodes))...) to enable all-vars
						  (let ((has-rest-vars (block b (mapc (lambda (pred) (when (has-rest-var pred) (return-from b t))) preds) nil)))
							(lambda (root-var) ;; Returns (pos-match = {t, nil}, scan-and-subst-status = {:unknown, :not-called, <an env>, nil})
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
														(print (list 'pmf1 root-var preds root-preds root-pred-const-nodes obj-nodes obj-node obj-edges pred-qets-match)))
													  (and
													   pred-qets-match
													   (= (length (intersect root-pred-const-nodes obj-nodes))
														  (length root-pred-const-nodes))
													   )))))))
									  (when am-traced
										(print (list 'pmf2 r)))
									  (let ((r (or (and has-rest-vars '(t nil))
												   (if r
													   (let ((s (scan-and-subst preds root-var obj-node)))
												   
														 ;; (setq s :unknown)		;; !!!!!

														 (hprint 's (hget rule-node 'name) root-var obj-node r s)
														 (if (null s)
															 (list nil nil)
															 (list t s)))
													   (list nil :not-called)))))
										($comment (when (and (second r) (listp (second r)))
													(print (list 'pm1 rule-node (hget rule-node 'name) root-var preds r))))
										;; Not clear which of these stats is better
										(gstat 'possible-match-true  (lambda (x y) (+ x y)) (lambda () (if (first r) 1 0)))
										(gstat 'possible-match-false (lambda (x y) (+ x y)) (lambda () (if (first r) 0 1)))
										;; (bool-timer 'possible-match-true 'possible-match-false (lambda () (first r)))
										r)))))))))))))))))))

	  ;; Returns rulegraph, a subclass of graph. See that class for comments.

	  (defm make-rule-graph (rule-node)
		(let ((rule-graph (make-rulegraph rule-node (hget rule-node 'name) (hget rule-node 'root-var))))
		  (let ((rule-edges (! ((get-rule-components rule-node) preds))))
			(dolist (rule-edge rule-edges)
			  (! (rule-graph add-edge) rule-edge))
			rule-graph)))

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
							 (is-subqet s1 s2))
					(remhash s1 h)))))
			(hash-table-value-to-list h))))

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
	  ;; used by a pred match which produced edges.

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
	  
	  (defm edge-asc-dimension-dist ()
		(defr
		  (defl max (l)
			(let ((r 0))
			  (dolist (x l)
				(when (> x r)
				  (setq r x)))
			  r))
		  (let ((edge-lengths (mapcar (lambda (x) (length x)) (get-all-edges))))
			(let ((l (max edge-lengths)))
			  (let ((a (make-array (+ l 1) :initial-element 0)))
				(dolist (x edge-lengths)
				  (setf (aref a x) (+ (aref a x) 1)))
				a)))))

	  (defm edge-asc-entropy ()
		(let ((dist (edge-asc-dimension-dist)))
		  (let ((n (length dist)))
			(let ((base 2))		;; Let base be n for normalized-to-unity entropy. For now we'll think in bits.
			  (let ((m 0))
				(dotimes (i n)
				  (setq m (+ (aref dist i) m)))
				(let ((h 0))
				  (dotimes (i n)
					(let ((p (/ (aref dist i) m)))
					  (setq h (+ h (if (not (= p 0)) (* p (log p 2)) 0)))))
				  (- h)))))))

	  ;; Returns a graph of a full trace of rule execution.  
	  ;; 
	  ;; But note default is in-place, i.e., within the current
	  ;; object. It may be advantageous at some point to have a
	  ;; separate graph, so that is supported.
	  ;;
	  ;; (rule a edge) = edge has been added by rule
	  ;; (rule d edge) = edge has been deleted by rule
	  ;; (edge p rule) = edge is a pred of rule
	  ;; (rule1 am rule2) = rule1 has added an edge which adds a component to rule2
	  ;;
	  ;; q
	  ;; pe -- edge to pred-edge
	  ;; pr -- pred-edge to rule
	  ;; ae -- add-edge to edge
	  ;; ar -- rule to add-edge
	  ;;
	  ;; 5/25/23 Consigned the "tested" stuff to the old version, as I haven't found a good graphical model for it yet.

	  (defm edge-trace-graph (&key make-new-graph 
								   (rules-fcn (lambda (r) t))
								   (except-rules-fcn (lambda (r) nil))
								   (nodes-fcn (lambda (edge) t))
								   (except-nodes-fcn (lambda (edge) nil))
								   (trim-dangling-adds-and-preds nil))
		(defr
		  (defl has-preds (events)
			(block b
			  (dolist (event events)
				(when (eq (first event) 'pred)
				  (return-from b t)))
			  nil))
		  (defl trim-dangling-adds-and-preds (g)
			(let ((edges (! (g get-all-edges))))
			  (dolist (edge edges)
				(cond 
				 ((eq (second edge) 'a)
				  (let ((edge-node (third edge)))
					(when (not (! (g hget) edge-node 'p))
					  (! (g rem-edge) edge))))
				 ((eq (second edge) 'p)
				  (let ((edge-node (first edge)))
					(when (not (! (g hget-inverse-all) edge-node 'a))
					  (! (g rem-edge) edge))))))))
		  (let ((g (if make-new-graph (make-foundation) self)))
			(let ((g1 (make-objgraph)))
			  (let ((trace (! (edge-to-trace as-list))))
				(dolist (entry trace)							;; g1 is then a reproduction of the graph captured by the trace. Helps for getting rule names, etc.
				  (let ((edge (first entry)))
					(! (g1 add-edge) edge)))
				(let ((am-done nil))
				  (dolist (entry trace)
					(let ((edge (first entry)))
					  (let ((events (second entry)))
						(when t #|(has-preds events)|#
						  (dolist (event events)
							(let ((kind (first event)))
							  (let ((seqno (second event)))
								(let ((rule-name (fourth event)))
								  (let ((obj-node (fifth event)))
									(when (funcall rules-fcn rule-name)
									  (when (not (funcall except-rules-fcn rule-name))
										(when (funcall nodes-fcn edge)
										  (when (not (funcall except-nodes-fcn edge))
											(let ((rule-node (symcat rule-name '-- seqno)))
											  (! (g add-edge) (list rule-node 'name rule-name)) ;; We did use rule-node for the name, which also offered the seqno; may bring that back
											  (! (g add-edge) (list rule-node 'type 'et-rule))
											  (let ((edge-node (format nil "~a" edge)))
												(! (g add-edge) (list edge-node 'type 'et-edge))
												(cond
												 ((eq kind 'add)
												  (! (g add-edge) (list rule-node 'a edge-node))
												  (when (eq (! (g1 hget) (first edge) 'type) 'rule)
													(let ((rule-name2 (! (g1 hget) (first edge) 'name)))
													  (let ((rule-node2 (or (first (! (g hget-inverse-all) rule-name2 'name))
																			(symcat rule-name2 '-- seqno))))
														(! (g add-edge) (list rule-node2 'name rule-name2))
														(! (g add-edge) (list rule-node2 'type 'et-rule))
														(when (not (memq rule-name2 am-done))
														  (! (g add-edge) (list rule-node 'am rule-node2))
														  (setq am-done (cons rule-name am-done))))))
												  (dolist (node edge)
													(when (! (g1 hget) node 'rule)
													  (let ((g1-rule-nodes (! (g1 hget-all) node 'rule)))
														(dolist (g1-rule-node g1-rule-nodes)
														  (when (funcall rules-fcn (! (g1 hget) g1-rule-node 'name))
															(! (g add-edge) (list rule-node 'an node))
															($comment (let ((g1-rule-name (! (g1 hget) g1-rule-node 'name)))
																		(let ((new-rule-node (! (g new-obj-node))))
																		  (! (g add-edge) (list node 'rn new-rule-node))
																		  (! (g add-edge) (list new-rule-node 'label g1-rule-name))
																		  (! (g add-edge) (list new-rule-node 'type 'et-obj))))))))))
												  (when (memq obj-node edge)
													(! (g add-edge) (list obj-node 'type 'et-obj))
													(! (g add-edge) (list rule-node 'ao obj-node))))
												 ((eq kind 'del)
												  (! (g add-edge) (list rule-node 'd edge-node)))
												 ((eq kind 'pred)
												  (let ()
													($comment		;; Not sure what q is for and why this logic
													 (when (not (! (g edge-exists) (list edge-node 'q rule-name)))
													   (! (g add-edge) (list edge-node 'q rule-name))
													   (! (g add-edge) (list edge-node 'p rule-node))))
													(! (g add-edge) (list edge-node 'p rule-node))													
													(dolist (node edge)
													  (when (! (g1 hget) node 'rule)
														(let ((g1-rule-nodes (! (g1 hget-all) node 'rule)))
														  (dolist (g1-rule-node g1-rule-nodes)
															(when (funcall rules-fcn (! (g1 hget) g1-rule-node 'name))
															  (! (g add-edge) (list node 'pn rule-node))
															  '(let ((g1-rule-name (! (g1 hget) g1-rule-node 'name)))
																 (let ((new-rule-node (! (g new-obj-node))))
																   (! (g add-edge) (list node 'rn new-rule-node))
																   (! (g add-edge) (list new-rule-node 'label g1-rule-name))
																   (! (g add-edge) (list new-rule-node 'type 'et-obj)))))))))
													(when (memq obj-node edge)
													  (! (g add-edge) (list obj-node 'type 'et-obj))
													  (! (g add-edge) (list obj-node 'r rule-node))
													  (! (g add-edge) (list obj-node 'e edge-node))))))))))))))))))))))))

			(let ((g1 (make-objgraph)))
			  (let ((trace (! (edge-to-pred as-list))))
				(dolist (entry trace) ;; g1 is then a reproduction of the graph captured by the trace. Helps for getting rule names, etc.
				  (let ((edge (first entry)))
					(! (g1 add-edge) edge)))
				(dolist (entry trace)
				  (let ((edge (first entry)))
					(let ((events (second entry)))
					  (dolist (event events)
						(let ((seqno (first event)))
						  (let ((rule-name (second event)))
							(let ((pred (third event)))
							  (when (funcall rules-fcn rule-name)
								(when (not (funcall except-rules-fcn rule-name))
								  (when (funcall nodes-fcn edge)
									(when (not (funcall except-nodes-fcn edge))
									  (let ((rule-node (symcat rule-name '-- seqno)))
										(let ((edge-node (format nil "~a" edge)))
										  (let ((pred-node-name (format nil "~a" pred)))
											(let ((pred-node (format nil "~a--~a--~a" pred-node-name rule-name seqno)))
											  (! (g add-edge) (list pred-node 'type 'pred-node))
											  (! (g add-edge) (list pred-node 'label pred-node-name))
											  (! (g add-edge) (list edge-node 'pe pred-node))
											  (! (g add-edge) (list pred-node 'pr rule-node)))))))))))))))))))

			(let ((g1 (make-objgraph)))
			  (let ((trace (! (edge-to-add as-list))))
				(dolist (entry trace) ;; g1 is then a reproduction of the graph captured by the trace. Helps for getting rule names, etc.
				  (let ((edge (first entry)))
					(! (g1 add-edge) edge)))
				(dolist (entry trace)
				  (let ((edge (first entry)))
					(let ((events (second entry)))
					  (dolist (event events)
						(let ((seqno (first event)))
						  (let ((rule-name (second event)))
							(let ((add (third event)))
							  (when (funcall rules-fcn rule-name)
								(when (not (funcall except-rules-fcn rule-name))
								  (when (funcall nodes-fcn edge)
									(when (not (funcall except-nodes-fcn edge))
									  (let ((rule-node (symcat rule-name '-- seqno)))
										(let ((edge-node (format nil "~a" edge)))
										  (let ((add-node-name (format nil "~a" add)))
											(let ((add-node (format nil "~a--~a--~a" add-node-name rule-name seqno)))
											  (! (g add-edge) (list add-node 'type 'add-node))
											  (! (g add-edge) (list add-node 'label add-node-name))
											  (! (g add-edge) (list add-node 'ae edge-node))
											  (! (g add-edge) (list rule-node 'ar add-node)))))))))))))))))))
			
			(when trim-dangling-adds-and-preds
			  (trim-dangling-adds-and-preds g))
			g)))

	  (defm old-edge-trace-graph (&key make-new-graph 
								   (rules-fcn (lambda (r) t))
								   (except-rules-fcn (lambda (r) nil))
								   (nodes-fcn (lambda (edge) t))
								   (except-nodes-fcn (lambda (edge) nil))
								   (trim-dangling-adds-and-preds nil))
		(defr
		  (defl has-preds (events)
			(block b
			  (dolist (event events)
				(when (eq (first event) 'pred)
				  (return-from b t)))
			  nil))
		  (defl trim-dangling-adds-and-preds (g)
			(let ((edges (! (g get-all-edges))))
			  (dolist (edge edges)
				(cond 
				 ((eq (second edge) 'a)
				  (let ((edge-node (third edge)))
					(when (not (! (g hget) edge-node 'p))
					  (! (g rem-edge) edge))))
				 ((eq (second edge) 'p)
				  (let ((edge-node (first edge)))
					(when (not (! (g hget-inverse-all) edge-node 'a))
					  (! (g rem-edge) edge))))))))
		  (let ((g (if make-new-graph (make-foundation) self)))
			(let ((g1 (make-objgraph)))
			  (let ((trace (! (edge-to-trace as-list))))
				(dolist (entry trace)							;; g1 is then a reproduction of the graph captured by the trace. Helps for getting rule names, etc.
				  (let ((edge (first entry)))
					(! (g1 add-edge) edge)))
				(dolist (entry trace)
				  (let ((edge (first entry)))
					(let ((events (second entry)))
					  (when t #|(has-preds events)|#
						(dolist (event events)
						  (let ((kind (first event)))
							(let ((seqno (second event)))
							  (let ((rule-name (fourth event)))
								(let ((obj-node (fifth event)))
								  (when (funcall rules-fcn rule-name)
									(when (not (funcall except-rules-fcn rule-name))
									  (when (funcall nodes-fcn edge)
										(when (not (funcall except-nodes-fcn edge))
										  (let ((rule-node (symcat rule-name '-- seqno)))
											(! (g add-edge) (list rule-node 'name rule-name)) ;; We did use rule-node for the name, which also offered the seqno; may bring that back
											(! (g add-edge) (list rule-node 'type 'et-rule))
											(let ((edge-node (format nil "~a" edge)))
											  (! (g add-edge) (list edge-node 'type 'et-edge))
											  (cond
											   ((eq kind 'add)
												(! (g add-edge) (list rule-node 'a edge-node))
												(when (eq (! (g1 hget) (first edge) 'type) 'rule)
												  (let ((rule-name2 (! (g1 hget) (first edge) 'name)))
													(let ((rule-node2 (or (first (! (g hget-inverse-all) rule-name2 'name))
																		  (symcat rule-name2 '-- seqno))))
													  (! (g add-edge) (list rule-node2 'name rule-name2))
													  (! (g add-edge) (list rule-node2 'type 'et-rule))
													  (! (g add-edge) (list rule-node 'ar rule-node2)))))
												(dolist (node edge)
												  (when (! (g1 hget) node 'rule)
													(let ((g1-rule-nodes (! (g1 hget-all) node 'rule)))
													  (dolist (g1-rule-node g1-rule-nodes)
														(when (funcall rules-fcn (! (g1 hget) g1-rule-node 'name))
														  (! (g add-edge) (list rule-node 'an node))
														  ($comment (let ((g1-rule-name (! (g1 hget) g1-rule-node 'name)))
																	  (let ((new-rule-node (! (g new-obj-node))))
																		(! (g add-edge) (list node 'rn new-rule-node))
																		(! (g add-edge) (list new-rule-node 'label g1-rule-name))
																		(! (g add-edge) (list new-rule-node 'type 'et-obj))))))))))
												(when (memq obj-node edge)
												  (! (g add-edge) (list obj-node 'type 'et-obj))
												  (! (g add-edge) (list rule-node 'ao obj-node))))
											   ((eq kind 'del)
												(! (g add-edge) (list rule-node 'd edge-node)))
											   ((eq kind 'pred)
												(let ()
												  (when (not (! (g edge-exists) (list edge-node 'q rule-name)))
													(! (g add-edge) (list edge-node 'q rule-name))
													(! (g add-edge) (list edge-node 'p rule-node)))
												  (dolist (node edge)
													(when (! (g1 hget) node 'rule)
													  (let ((g1-rule-nodes (! (g1 hget-all) node 'rule)))
														(dolist (g1-rule-node g1-rule-nodes)
														(when (funcall rules-fcn (! (g1 hget) g1-rule-node 'name))
															(! (g add-edge) (list node 'pn rule-node))
															'(let ((g1-rule-name (! (g1 hget) g1-rule-node 'name)))
															   (let ((new-rule-node (! (g new-obj-node))))
																 (! (g add-edge) (list node 'rn new-rule-node))
																 (! (g add-edge) (list new-rule-node 'label g1-rule-name))
																 (! (g add-edge) (list new-rule-node 'type 'et-obj)))))))))
												  (when (memq obj-node edge)
													(! (g add-edge) (list obj-node 'type 'et-obj))
													(! (g add-edge) (list obj-node 'r rule-node))
													(! (g add-edge) (list obj-node 'e edge-node)))))
											   ((eq kind 'tested)
												(let ((new-rule-node (! (g new-obj-node))))
												  (let ((new-t-node (! (g new-obj-node))))
													(let ((new-seqno-node (! (g new-obj-node))))
													  (! (g add-edge) (list obj-node 'type 'et-obj))
													  (! (g add-edge) (list new-rule-node 'type 'et-rule))
													  (! (g add-edge) (list new-rule-node 'name rule-name))
													  (! (g add-edge) (list new-t-node 'label 't))
													  (! (g add-edge) (list new-seqno-node 'label seqno))
													  (! (g add-edge) (list obj-node new-t-node new-seqno-node new-rule-node))))))))))))))))))))))))
			(when trim-dangling-adds-and-preds
			  (trim-dangling-adds-and-preds g))
			g)))

	  ;; Variant on above full trace: r1 is related to r2 iff there
	  ;; exists an edge e such that (r1 add e) and (e pred r2). Temporal
	  ;; (seqno) info is not included. This should provide a rule-dep
	  ;; type of graph, but with only rules deps that are really needed
	  ;; by a given run.

	  (defm edge-trace-rule-graph (&key rules (except-rules-fcn (lambda (x) nil)))
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
									(when (and
										   (not (funcall except-rules-fcn rule-name))
										   (not (funcall except-rules-fcn add-rule-name)))
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

	  )))

;; This defstruct was inside the rule-stats defc, but seems to be
;; somewhat faster on the outside. Conjecture is the compiler can't
;; optimize-out runtime redef. See similar comment herein about sbcl.

(defstruct rule-stats-entry
  (tested 0)
  (matched 0)
  (new-edges 0)
  (not-new-edges 0)
  (new-edges-max-expand-len 0)
  (last-expand-len 0)
  (failed 0))

(defc rule-stats nil (graph)
  (let ((rule-stats-table (make-sur-map))
		(all-rules-tested 0)
		(all-rules-new-edges 0))
	(let ((x (let ((*print-level* 1))
			   (let ((*print-length* 1))
				 ;; (show-stack)		;; Good technique!
				 )
			   nil)))

	  (defm get-entry (rule-node)
		(let ((entry (! (rule-stats-table lookup-one) rule-node)))
		  (when (null entry)
			(setq entry (make-rule-stats-entry))
			(! (rule-stats-table insert) rule-node entry))
		  entry))

	  (defm rule-stats (&optional (sort-colno 1))
		(defr
		  (defl symbol< (x y) (string< (symbol-name x) (symbol-name y)))
		  (defl div (x y)
			(if (= y 0) 0 (/ x y)))
		  (let ((m 0))
			(let ((info (mapcan (lambda (l) 
								  (let ((x (env-lookup '?x l)))
									(when t ;; (hget x 'new-edges)
									  (setq m (max m (length (symbol-name (! (graph hget) x 'name)))))
									  (let ((e (get-entry x)))
										(list (list x
													(! (graph hget) x 'name)
													(rule-stats-entry-tested e)
													(rule-stats-entry-matched e)
													(rule-stats-entry-new-edges e)
													(rule-stats-entry-not-new-edges e)
													(rule-stats-entry-failed e)
													(rule-stats-entry-new-edges-max-expand-len e)
													(div (* (float (rule-stats-entry-new-edges e)) 100) (float (rule-stats-entry-tested e)))
													(div (* (float (rule-stats-entry-not-new-edges e)) 100) (float (rule-stats-entry-tested e)#|(rule-stats-entry-matched e)|#))
													(div (* (float (rule-stats-entry-failed e)) 100) (float (rule-stats-entry-tested e)#|(rule-stats-entry-matched e)|#))))))))
								(! (graph query) '((?x type rule))))))
			  (let ((info (sort info (lambda (x y)
									   (let ((x (nth sort-colno x))
											 (y (nth sort-colno y)))
										 (if (and (symbolp x) (symbolp y))
											 (symbol< x y)
											 (> x y)))))))
				(let ((s 10)
					  (m (+ m 2)))
				  (let ((args (list (+ 6 (* s 0)) (+ m (* s 1)) (+ m (* s 2)) (+ m (* s 3)) (+ m (* s 4)) (+ m (* s 5)) (+ m (* s 6)) (+ m 6 (* s 7)) (+ m 8 (* s 8)) (+ m 10 (* s 9)))))
					(apply #'format t "~%0~vt1~vt2~vt3~vt4~vt5~vt6~vt7~vt8~vt9~vt10" args)
					(apply #'format t "~%node~vtname~vttested~vtmatched~vtnew-e~vtnot-new-e~vtfailed~vtmax-expand-len~vtefficiency%~vtredundancy%~vtfailure%~%" args)
					(dolist (x info)
					  (format t "~%~a~vt|~a~vt~a~vt~a~vt~a~vt~a~vt~a~vt~a~vt~,2f~vt~,2f~vt~,2f"
							  (nth 0 x) (+ 6 (* s 0))
							  (nth 1 x) (+ m (* s 1))
							  (nth 2 x) (+ m (* s 2))
							  (nth 3 x) (+ m (* s 3))
							  (nth 4 x) (+ m (* s 4))
							  (nth 5 x) (+ m (* s 5))
							  (nth 6 x) (+ m (* s 6))
							  (nth 7 x) (+ m 6 (* s 7))
							  (nth 8 x) (+ m 8 (* s 8))
							  (nth 9 x) (+ m 10 (* s 9))
							  (nth 10 x)))))))
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

	  ;; Dump a stat with expand len and number of nodes in expanded set of edges -- anything else?
	  ;;   Maybe develop a dump-stat file, which has a bunch of things over time

	  (defm update-last-expand-len (rule-node len)
		(log-stat 'expand len)
		(let ((entry (get-entry rule-node)))
		  (setf (rule-stats-entry-last-expand-len entry) (max (rule-stats-entry-last-expand-len entry) len))
		  nil))

	  (defm update-new-edges-max-expand-len (rule-node)
		(let ((entry (get-entry rule-node)))
		  (setf (rule-stats-entry-new-edges-max-expand-len entry) (max (rule-stats-entry-new-edges-max-expand-len entry) (rule-stats-entry-last-expand-len entry)))
		  (log-stat 'expand-max (rule-stats-entry-new-edges-max-expand-len entry))
		  nil))

	  )))

(defstruct node-stats-entry
  (tested 0)
  (matched 0)
  (new-edges 0)
  (not-new-edges 0)
  (failed 0))

(defc node-stats nil (graph)
  (let ((node-stats-table (make-sur-map))
		(all-nodes-tested 0)
		(all-nodes-new-edges 0))
	(let ((x (let ((*print-level* 1))
			   (let ((*print-length* 1))
				 ;; (show-stack)		;; Good technique!
				 )
			   nil)))

	  (defm get-entry (node)
		(let ((entry (! (node-stats-table lookup-one) node)))
		  (when (null entry)
			(setq entry (make-node-stats-entry))
			(! (node-stats-table insert) node entry))
		  entry))

	  (defm node-stats (&key (sort-colno 0) (nodes nil))
		(defr
		  (defl node-comp (x y)
			(let ((x (nth sort-colno x))
				  (y (nth sort-colno y)))
			  (cond
			   ((and (numberp x) (numberp y))
				(> x y))
			   ((and (symbolp x) (symbolp y))
				(symbol< x y))
			   ((and (stringp x) (stringp y))
				(string< x y)))))
		  (defl symbol< (x y) (string< (symbol-name x) (symbol-name y)))
		  (defl node-descr (node) ;; Dig out some properties heuristically for a bit of extra info in stats		   
			(let ((x (! (graph hget) node 'name)))
			  (let ((x (or x (! (graph hget) node 'type))))
				x)))
		  (defl div (x y)
			(if (= y 0) 0 (/ x y)))
		  (let ((max-col-0 0))
			(let ((max-col-1 0))
			  (let ((info (mapcan (lambda (x)
									(let ((e (get-entry x)))
									  (let ((x-descr (node-descr x)))
										(setq max-col-0 (max max-col-0 (length (format nil "~a" x))))
										(setq max-col-1 (max max-col-1 (length (format nil "~a" x-descr))))
										(list (list x
													x-descr
													(node-stats-entry-tested e)
													(node-stats-entry-matched e)
													(node-stats-entry-new-edges e)
													(node-stats-entry-not-new-edges e)
													(node-stats-entry-failed e)
													(div (* (float (node-stats-entry-new-edges e)) 100) (float (node-stats-entry-tested e)))
													(div (* (float (node-stats-entry-not-new-edges e)) 100) (float (node-stats-entry-tested e)))
													(div (* (float (node-stats-entry-failed e)) 100) (float (node-stats-entry-tested e))))))))
								  (or nodes (! (node-stats-table inputs))))))
				(let ((info (sort info #'node-comp)))
				  (let ((s 10))
					(let ((m (+ max-col-0 max-col-1 2)))
					  (let ((args (list (+ max-col-0 (* s 0)) (+ m (* s 0)) (+ m (* s 1)) (+ m (* s 2)) (+ m (* s 3)) (+ m (* s 4)) (+ m (* s 5)) (+ m 6 (* s 6)) (+ m 8 (* s 7)) (+ m 10 (* s 8)))))
						(apply #'format t "~%0~vt1~vt2~vt3~vt4~vt5~vt6~vt7~vt8~vt9~vt10" args)
						(apply #'format t "~%node~vtname~vttested~vtmatched~vtnew-e~vtnot-new-e~vtfailed~vtefficiency%~vtredundancy%~vtfailure%~%" args)
						(dolist (x info)
						  (format t "~%~a~vt~a~vt~a~vt~a~vt~a~vt~a~vt~a~vt~,2f~vt~,2f~vt~,2f"
								  (nth 0 x) (+ max-col-0 (* s 0))
								  (nth 1 x) (+ m (* s 0))
								  (nth 2 x) (+ m (* s 1))
								  (nth 3 x) (+ m (* s 2))
								  (nth 4 x) (+ m (* s 3))
								  (nth 5 x) (+ m (* s 4))
								  (nth 6 x) (+ m (* s 5))
								  (nth 7 x) (+ m 6 (* s 6))
								  (nth 8 x) (+ m 8 (* s 7))
								  (nth 9 x)))))))))
			nil)))

	  (defm update-tested (node)
		(gstat 'eo-tested (lambda (x y) (+ x y)) (lambda () 1))
		(let ((entry (get-entry node)))
		  (setf (node-stats-entry-tested entry) (+ (node-stats-entry-tested entry) 1)))
		(setq all-nodes-tested (+ all-nodes-tested 1))
		nil)

	  (defm update-matched (node)
		(gstat 'eo-matched (lambda (x y) (+ x y)) (lambda () 1))
		(let ((entry (get-entry node)))
		  (setf (node-stats-entry-matched entry) (+ (node-stats-entry-matched entry) 1)))
		nil)

	  (defm get-matched (node)
		(let ((entry (get-entry node)))
		  (node-stats-entry-matched entry)))

	  (defm update-failed (node)
		(gstat 'eo-failed (lambda (x y) (+ x y)) (lambda () 1))
		(let ((entry (get-entry node)))
		  (setf (node-stats-entry-failed entry) (+ (node-stats-entry-failed entry) 1)))
		nil)

	  ;; These two methods update the counts for executions which either
	  ;; produce new edges, or not (the latter can be match failures or
	  ;; already-matched without new edges being produced).
	  ;;
	  ;; The numbers can be computed from tested, matched, and new-edges
	  ;; however these methods need to be their own calls in order to
	  ;; drive the filter. [Filter removed 4/5/16. This comment is
	  ;; useful rationale.]

	  (defm update-new-edges (node)
		(gstat 'eo-matched-new-edges (lambda (x y) (+ x y)) (lambda () 1))
		(let ((entry (get-entry node)))
		  (setf (node-stats-entry-new-edges entry) (+ (node-stats-entry-new-edges entry) 1)))
		(setq all-nodes-new-edges (+ all-nodes-new-edges 1))
		nil)

	  (defm update-not-new-edges (node)
		(gstat 'eo-matched-not-new-edges (lambda (x y) (+ x y)) (lambda () 1))
		(let ((entry (get-entry node)))
		  (setf (node-stats-entry-not-new-edges entry) (+ (node-stats-entry-not-new-edges entry) 1)))
		nil)

	  )))

;; Belongs in subst-match, moved outside for perf.

(defstruct k
  pred
  pred-info			   ;; Entry per pred-node, d => data, v => var. Change after subst.
  orig-pred)		   ;; Just for debugging right now

;; We had this as a subclass of objgraph, but for perf reasons we'd
;; like to stick with graph. So avoid hget and other shortcuts and use
;; the qet system directly. Also moved get-rule-children into here.

(defc rulegraph graph (rule-node rule-name root-var)
  (let ()

	(defm rule-node ()
	  rule-node)

	(defm name ()
	  rule-name)

	(defm get-rule-children (bnode root-var)
	  (cond
	   ((and (is-var-name root-var)
			 (is-node bnode)
			 (is-var-name bnode))
		(get-edges bnode))
	   ((and (not (is-var-name root-var))
			 (is-node bnode))
		(get-edges bnode))
	   ((is-edge bnode)
		bnode)
	   (t nil)))

	;; A version is also defined in objgraph; this one is "new". Right now this and the depth version are only called
	;; from test, with various kinds of printing.

	(defm bipartite-breadth-rule-walk (root-node &key result-fcn rule-traced)
	  (breadth root-node
			   (lambda (bnode level)
				 (when result-fcn
				   (funcall result-fcn bnode level))
				 (let ((children (get-rule-children bnode root-node)))
				   children))))

	(defm bipartite-depth-rule-walk (root-node &key result-fcn rule-traced)
	  (depth root-node
			 (lambda (bnode level)
			   (when result-fcn
				 (funcall result-fcn bnode level))
			   (let ((children (get-rule-children bnode root-node)))
				 children))))

	;; defstruct k moved outside rulegraph for perf

	(let ((envs (make-hash-table :size 63 :test #'equal)))
	  (let ((qet-edge-len-cache (make-sur-map)))
		(let ((doloop-cnt 0))
		  (let () #| ((ks-with-existing-edges (make-sur-map :input-test #'equalp :res-test #'equalp))) |#		;; partial-match-info
			(defm subst-match (objgraph obj-node root-var &key verbose)
			  (macrolet ((xprint (&rest x) nil))
				(timer 'subst-match
				  (lambda ()
					(let ((g objgraph))
					  (let ((use-singleton-qets (and (! (self has-single-const-preds))
													 (not (is-var-name root-var)))))
						(defr
						  (defl yprint (tag &rest x)
							(when (or (eq verbose t)
									  (memq tag verbose))
							  (print (cons tag x))))
						  (defl make-ks ()
							(let ((preds (second (! (g filter-new-node-pred-edges) (get-all-edges)))))
							  (mapcar (lambda (pred)
										(make-k :pred pred 
												:pred-info (mapcar (lambda (node) (if (is-var-name node) 'v 'd)) pred)
												:orig-pred pred))
									  preds)))
						  (defl get-new-node-env ()
							(let ((new-node-preds (first (! (g filter-new-node-pred-edges) (get-all-edges)))))
							  (mapcar (lambda (new-node-pred) (list (first new-node-pred) (third new-node-pred))) new-node-preds)))
						  ;; Splits l at vars and returns a list of subseqs at those var boundaries
						  ;; E.g. (filter-vars-to-qets '(1 2 ?x 3 4 ?y 5 6 ?z) => ((1 2) (3 4) (5 6))
						  (defl filter-vars-to-qets (pred pred-info)
							(defr
							  (defl is-var (node node-info)
								(and (is-var-name node)
									 (eq node-info 'v)))
							  (defl doloop (l i c r)
								(if (null l)
									(append r (when c (list c)))
									(if (is-var (first l) (first i))
										(doloop (rest l) (rest i) nil (append r (list c)))
										(doloop (rest l) (rest i) (append c (list (first l))) r))))
							  (doloop pred pred-info  nil nil)))
						  (defl check-consts (ks) ;; T if all preds have only consts, no vars
							(block b
							  (dolist (k ks)
								(let ((pred-info (k-pred-info k)))
								  (dolist (node pred-info)
									(when (eq node 'v)
									  (return-from b nil)))))
							  t))
						  (defl check-const (k)
							(let ((pred-info (k-pred-info k)))
							  (block b
								(dolist (node pred-info)
								  (when (eq node 'v)
									(return-from b nil)))
								t)))
						  (defl subst (ks env) ;; Returns new ks
							(mapcar (lambda (k)
									  (let ((pred (k-pred k)))
										(let ((pred-info (k-pred-info k)))
										  (let ((pred-info-list (mapcar (lambda (node pred-info-node)
																		  (if (eq pred-info-node 'v)
																			  (let ((val (env-lookup node env :idempotent nil)))
																				(if val
																					(list val 'd)
																					(list node pred-info-node)))
																			  (list node pred-info-node)))
																		pred pred-info)))
											(make-k :pred (mapcar (lambda (x) (first x)) pred-info-list)
													:pred-info (mapcar (lambda (x) (second x)) pred-info-list)
													:orig-pred (k-orig-pred k))))))
									ks))
						  (defl find-min-edges-k (ks)
							(let ((n 1e38))
							  (let ((min-k nil))
								(dolist (k ks)
								  (let ((pred (k-pred k)))
									(let ((pred-info (k-pred-info k)))
									  (when (not (check-const k))
										(let ((qets (filter-vars-to-qets pred pred-info)))
										  (let ((len (! (qet-edge-len-cache lookup-one) qets)))	;; Caching the lengths seems only marginally effective
											(when (null len)
											  (let ((new-edges (mapunion (lambda (qet) (when (or use-singleton-qets (> (length qet) 1)) (! (g get-edges-from-subqet) qet))) qets)))
												(when new-edges
												  (setq len (length new-edges))
												  (! (qet-edge-len-cache insert-one) qets len))))
											(when (and len (< len n))
											  (setq n len)
											  (setq min-k k))))))))
								(xprint 's11 min-k)
								min-k)))
						  (defl match (ks) ;; Returns a list of envs. Each env is a singleton binding, and all the bindings are for the same variable
							(defr
							  (defl find-var-binding (env)
								(block b
								  (dolist (binding env)
									(when (is-var-name (first binding))
									  (return-from b (list binding))))
								  nil))
							  (let ((k-to-match (find-min-edges-k ks)))
								(let ((k k-to-match))
								  (when k
									(let ((pred (k-pred k)))
									  (let ((pred-info (k-pred-info k)))
										(let ((qets (filter-vars-to-qets pred pred-info)))
										  (let ((new-edges (mapunion (lambda (qet) (when (or use-singleton-qets (> (length qet) 1)) (! (g get-edges-from-subqet) qet))) qets)))
											(let ((r (mapcad (lambda (edge)
															   (find-var-binding (! (g match-one-edge) pred edge nil nil nil :pred-info pred-info)))
															 new-edges)))
											  (xprint 's9 pred new-edges r)
											  r))))))))))
						  (defl edges-exist (ks) ;; All pred edges need to exist
							(block b
							  (dolist (k ks)
								(let ((pred (k-pred k)))
								  (when (not (! (g edge-exists) pred))
									(return-from b nil))))
							  t))
						  (defl x-edges-exist (ks)							;; partial-match-info  Use this version to store the partial match info
							($comment
							 (block b
							   (let ((kes nil))
								 (let ((r t))
								   (dolist (k ks)
									 (let ((pred (k-pred k)))
									   (if (not (! (g edge-exists) pred))
										   (setq r nil)
										   (setq kes (append kes (list k))))))
								   (when (not r)
									 (! (ks-with-existing-edges insert-one) kes kes))
								   r)))))
						  (defl doloop (lvl cnt ks env-chain)
							(xprint 's1 ks lvl cnt doloop-cnt)
							(setq doloop-cnt (+ doloop-cnt 1))
							(if (check-consts ks)
								(let ()
								  (xprint 's4 env-chain doloop-cnt)
								  (when (edges-exist ks)
									(xprint 's2 env-chain doloop-cnt)
									(setf (gethash env-chain envs) env-chain) ;; Tests show we can get dups so hash table is needed
									nil))
								(let ((new-envs (match ks)))
								  (xprint 's10 new-envs)
								  (let ((i 0))
									(dolist (new-env new-envs)
									  (let ((ks (subst ks new-env)))
										(doloop (+ lvl 1) i ks (append new-env env-chain))
										(setq i (+ i 1))))))))
						  (let ((env `((,root-var ,obj-node))))
							(xprint 's0 rule-name root-var obj-node)
							(let ((ks (make-ks)))
							  (let ((ks (subst ks env)))
								(clrhash envs)
								(doloop 0 0 ks env)
								(let ((new-node-env (get-new-node-env)))
								  (let ((envs-list (hash-table-value-to-list envs)))
									($comment (when (null envs-list)							;; partial-match-info 
												(xprint 's12 rule-name root-var obj-node
														(mapcar (lambda (k) (k-orig-pred k)) ks)
														(! (ks-with-existing-edges inputs)))))
									(mapcar (lambda (env) (append env new-node-env)) envs-list)))))))))))))))))

	;; Also in objgraph (as-is)
	;;
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

	;; Also in objgraph
	;;
	;;     This looks like it's not called in the standard runs
	;;
	(defm get-root-vars ()
	  (cerror "xxx" "yyy")
	  (let ((r (or (and root-var 
						(list root-var))
				   (let ((rule-edges-info (filter-new-node-pred-edges (get-all-edges))))
					 (let ((new-node-rule-edges (first rule-edges-info)))
					   (let ((other-rule-edges (second rule-edges-info)))
						 (let ((new-node-rule-edge-nodes (nodes new-node-rule-edges)))
						   (let ((other-rule-edge-nodes (nodes other-rule-edges)))
							 (set-subtract other-rule-edge-nodes new-node-rule-edge-nodes)))))))))
		r))

	;; For testing -- assessing numbers of vars in rules (in the preds)

	(defm get-vars ()
	  (let ((rule-edges-info (filter-new-node-pred-edges (get-all-edges))))
		(let ((other-rule-edges (second rule-edges-info)))
		  (let ((other-rule-edge-nodes (nodes other-rule-edges)))
			(mapcad (lambda (node) (when (is-var-name node) node)) other-rule-edge-nodes)))))

	(let ((rule-is-connected-via-vars-cache nil))
	  (defm rule-is-connected-via-vars (&key verbose)
		(timer 'rule-is-connected-via-vars
		  (lambda ()
			(if rule-is-connected-via-vars-cache
				(first rule-is-connected-via-vars-cache)
				(let ((root-vars (get-root-vars)))
				  (let ((r (block b
							 (dolist (root-var root-vars)
							   (when (is-var-name root-var)
								 (when verbose
								   (print root-var))
								 (let ((edges-from-walk nil))
								   (bipartite-breadth-rule-walk 
									root-var
									:result-fcn (lambda (bnode level)
												  (when (is-edge bnode)
													(setq edges-from-walk (cons bnode edges-from-walk)))))
								   (let ((all-edges (second (filter-new-node-pred-edges (get-all-edges)))))
									 (let ((edges-from-walk (second (filter-new-node-pred-edges edges-from-walk))))
									   (when verbose
										 (print (list all-edges edges-from-walk)))
									   (when 
										   (= (length all-edges)
											  (length edges-from-walk))
										 (return-from b t)))))))
							 (return-from b nil))))
					(setq rule-is-connected-via-vars-cache (list r))
					r))))))
	  ;; Since we make a new rulegraph every time we call all-matches, we might not need to call clear the cache at
	  ;; all. But we might do better making the rulegraph up higher. Not clear when we'll need this call.
	  (defm clear-rule-is-connected-via-vars-cache ()
		(setq clear-rule-is-connected-via-vars-cache nil)))

	;; True if each pred has just one node when the vars are removed. This means that in subst-match we need to relax
	;; the single-node qet prohibition to accommodate these rules, when the root var is a const.

	(let ((has-single-const-preds-cache nil))
	  (defm has-single-const-preds ()
		(if has-single-const-preds-cache
			(first has-single-const-preds-cache)
			(let ()
			  (setq has-single-const-preds-cache
					(list 
					 (block b
					   (let ((preds (second (! (g filter-new-node-pred-edges) (get-all-edges)))))
						 (dolist (pred preds)
						   (when (> (length (filter-vars pred)) 1)
							 (return-from b nil))))
					   t)))))))

	;; This is also a candidate for caching, using the signature
	(defm has-rest-vars ()
	  (block b (mapc (lambda (pred) (when (! (g has-rest-var) pred) (return-from b t))) (get-all-edges)) nil))
))

(defc rulegraph-adds objgraph (graph rule-node)
  (let ()
	(defm init ()
	  (let ((add-edges (! ((! (graph get-rule-components) rule-node) adds))))
		(rem-edge '(global-node local-rule-pool local-rule-pool-node))
		(rem-edge '(global-node global-rule-pool-ref global-rule-pool-node))
		(dolist (add-edge add-edges)
		  (add-edge add-edge))))))

;; 5/9/23 Finally discovered a clisp method to define a test and hash fcn for a hash table. So we do that below and can
;; use CL hash tables. Though this does not seem to be in the CL std, sbcl has a variant (of the same name), but abcl
;; does not.
;;
;; 2/1/19 We used to compare based on equalp, and flagged in a comment that really a test based on env-equal would be
;; better, since then we wouldn't be dependent on the ordering of the bindings. That was ok until we did possible-match
;; scan-and-subst, which provides bindings in a likely different order. So now we test based on a specialized equality
;; function for the entry, which uses env-equal.  However to do this we needed to switch to an hhash-table, since CL
;; hash tables don't accept an arbitrary test.

(defstruct env-trig-entry			;; Moved outside env-triggered class to see if helps perf -- 
									;; doesn't, really, but prevents sbcl from spewing forth zillions of warnings
  (rule-node nil)
  (env nil)
  (adds-hash nil))

(defc env-triggered nil (std-vars)
  (let ()
	(defr
	  (defl env-hash (env)
		(let ((r 0))
		  (dolist (binding env)
			(setq r (+ r (sxhash binding))))
		  r))
	  (defl env-trig-entry-equal (e1 e2)
		(and (equal (env-trig-entry-rule-node e1) (env-trig-entry-rule-node e2))
			 (env-equal (env-trig-entry-env e1) (env-trig-entry-env e2))
			 (equal (env-trig-entry-adds-hash e1) (env-trig-entry-adds-hash e2))))
	  (defl env-trig-entry-hash (e)
		(+ (sxhash (env-trig-entry-rule-node e))
		   (env-hash (env-trig-entry-env e))
		   (env-trig-entry-adds-hash e)))
	  (define-hash-table-test env-hash-test env-trig-entry-equal env-trig-entry-hash)
	  (let ((env-triggered-table (make-hash-table :test 'env-hash-test)))
		(let ((lte (make-env-trig-entry)))
		  (let ((graph nil))

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
						(setf (gethash te env-triggered-table) te))
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
				(let ((te (gethash lte env-triggered-table)))
				  (let ((r (and te t)))
					;; (print r)
					r))))
			
			(defm as-list () ;; For debug only
			  (list env-triggered-table))

			(defm clear () ;; For debug only
			  (clrhash env-triggered-table)
			  nil)

			(defm rule-has-been-triggered (rule-node env)
			  (when am-traced
				(print (! (graph hget) rule-node 'name)))
			  (and (lookup rule-node env) t))))))))

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

;; Experimental hunion using an appender class. Slow -- see appender class above

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

(defun is-var-name (a)
  (and (symbolp a)
	   (eq (aref (symbol-name a) 0) #\?)))

(defun is-rest-var-name (a)
  (and (symbolp a)
	   (let ((s (symbol-name a)))
		 (and (eq (aref s 0) #\?)
			  (eq (aref s 1) #\*)))))

(defun is-node (x)
  (or (stringp x)
	  (numberp x)
	  (symbolp x)
	  (functionp x)))		;; Function allowed for test, e.g., can add Hoss objects to the graph. May be interesting
							;; for a later "compiling" model.

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

(defun is-scoped-new-pool-node (node)
  (and (symbolp node)
	   (let ((name (symbol-name node)))
		 (and (eq (aref name 0) #\S)
			  (eq (aref name 1) #\N)))))

(defun env-lookup (node env &key (idempotent t))
  (block el
	(dolist (binding env)
	  (when (equal node (first binding))
		(return-from el (second binding))))
	(if idempotent ;; was nil -- need node due to orig-match algorithm
		node
		nil)))

;; The new one below discards the dummy '(t t) entries in assessing equality. This came up with the new subst-match
;; work. For that it also was needed for testing that new-node nodes be discarded when assessing equality. So that has
;; its own env-equal, just for testing. The old one worked ok for kernel operations, so let's keep it here for now.

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

;; Slow!!!!!!!!!!

(defun new-env-equal (env1 env2)
  (block b
	(let ((h (make-sur-map)))
	  (dolist (b env1)
		(when (not (equal b '(t t)))
		  (! (h insert-one) b b)))
	  (dolist (b env2)
		(when (not (equal b '(t t)))
		  (if (! (h lookup-one) b)
			  (! (h remove) b)
			  (return-from b nil))))
	  (if (= (! (h count)) 0)
		  t
		  nil))))

;; For testing only, specifically subst-match.
;; Each pair of elements from the sets need to be env-equal.
;; Tough to make an optimized version -- this version should only be used for testing.
;; Also need special env-equal which would not fly in the kernel.

(defun envs-equal (envs1 envs2)
  (defr
	(defl env-equal (env1 env2)
	  (block b
		(let ((h (make-sur-map)))
		  (dolist (b env1)
			(when (and (not (equal b '(t t)))
					   (not (is-scoped-new-pool-node (second b)))
					   (is-var-name (first b)))
			  (! (h insert-one) b b)))
		  (dolist (b env2)
			(when (and (not (equal b '(t t)))
					   (not (is-scoped-new-pool-node (second b)))
					   (is-var-name (first b)))
			  (if (! (h lookup-one) b)
				  (! (h remove) b)
				  (return-from b nil))))
		  (if (= (! (h count)) 0)
			  t
			  nil))))
	(defl memb (env envs)
	  (block b
		(dolist (env1 envs)
		  (when (env-equal env env1)
			(return-from b t)))
		nil))
	(if (not (= (length envs1) (length envs2)))
		nil
		(block b
		  (dolist (env1 envs1)
			(when (not (memb env1 envs2))
			  (return-from b nil)))
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

(defun cross-aux2 (envs-list &key (record-lengths nil) rule-trace-info)
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
			(when rule-trace-info		;; check-rule-trace: An outlier to using the method as it's a defun
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

;; Splits l at vars and returns a list of subseqs at those var boundaries
;; E.g. (filter-vars-to-qets '(1 2 ?x 3 4 ?y 5 6 ?z) => ((1 2) (3 4) (5 6))

(defun filter-vars-to-qets (l)
  (defr
	(defl doloop (l c r)
	  (if (null l)
		  (append r (when c (list c)))
		  (if (is-var-name (first l))
			  (doloop (rest l) nil (append r (list c)))
			  (doloop (rest l) (append c (list (first l))) r))))
	(doloop l nil nil)))

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

;; (cross '(a b c) '(d e f) '(g h i))

(defun cross (&rest sets)
  (cross-aux sets))

(defun cross-aux (sets)
  (timer 'cross-aux
	(lambda ()
	  (let ((r '(()))
			(r1 nil))
		(dolist (set sets)
		  (dolist (p r)
			(dolist (s set)
			  (setq r1 (cons (append p (list s)) r1))))
		  (setq r r1)
		  (setq r1 nil))
		r))))

(defun old-cross-aux (sets)
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

;; Global trace "info" section

(defun match-and-execute-info (info))

(defun match-and-execute-env-info (info))

(defun match-pat-obj-edge-lists-info (info))

(defc base-graph objgraph nil

(let ()
  (defm add-natural-number-edges (n)
	(dotimes (i n)
	  (add-edge `(,i sigma ,(+ i 1)))
	  ))
  (defm defg (rules)
	(dolist (rule rules)
	  (define-rule rule))
	nil)
  (defm init ()
	(objgraph-init))))

(defc foundation base-graph nil
  (let ()
	(defm init ()
	  (base-graph-init)
	  (add-natural-number-edges 20)
	  (read-rule-file "foundation.lisp"))))

(defc rule-30-test foundation nil
  (let ()
	(defm init ()
	  (clear-counters)
	  (clear-perf-stats)
	  (foundation-init)
	  (read-rule-file "rule30.lisp")
	  )
	(defm run (levels)
	  (add-natural-number-edges levels)
	  (define-rule `(rule
					 (name init)
					 (attach-to global-node)
					 (pred
					  (global-node rule ?r)
					  (?r name init))
					 (add
					  (print init)
					  (r level ,levels)
					  (r rule-30-top)
					  (r local-rule-pool local-rule-pool-node))
					 (del
					  (global-node rule ?this-rule))))
	  (timer 'main
		(lambda ()
		  (execute-global-all-objs-loop))))))

(defc the-graph foundation nil
  (let ()

	(defm init ()
	  (foundation-init)
	  (clear-perf-stats) ;; Note the perf stats are global
	  (read-rule-file "fft.lisp")
	  ;; (read-rule-file "new-fft.lisp")
	  (read-rule-file "tree.lisp") ;;; 8/11/20 -- Fixed issues with this and it should hold as the default now
	  ;; (read-rule-file "globaltree.lisp")
	  (read-rule-file "rule30.lisp")
	  (read-rule-file "fft-delta.lisp")

	  (read-rule-file "fe.lisp")
	  (read-rule-file "copy-rule.lisp")
	  
	  ;; (read-rule-file "display-rules.lisp")
	  ;; (read-rule-file "fft-display-rules.lisp")

	  ;; (read-rule-file "gettysburg-address.lisp")

	  ;; (read-rule-file "fe-no-copy.lisp")
	  ;; (read-rule-file "rule-dep.lisp")
	  )

	))

;; Local Variables:
;; eval: (put 'execute-obj 'lisp-indent-function 'defun)
;; eval: (put 'add-consequent-edges 'lisp-indent-function 'defun)
;; eval: (put 'dolists 'lisp-indent-function 'defun)
;; eval: (put 'macrolet 'lisp-indent-function 'defun)
;; fill-column: 120
;; End:
