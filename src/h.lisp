;;
;; Issue tags:
;;
;;  Archived 7/23/24 -- prior to switching to new table-based count-edges-from-subqet and removing old code.
;;
;;  Archived 7/17/24 -- prior to removing succ-rule-freq, rule-freq-graph, failure-handling experiment,
;;                      successful-exec-obj-list, freq-obj-queue, failed-queue.  See doc.txt, look for this archive
;;                      date.
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
;;					that traditional all-matches-aux does better, since it whittles down the edges first to a
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
;;						 9/24/23 Added code to calc and print unmatched preds on failure. Still just for test. See tags
;;						 below; need to adjust them and delete older code.
;;
;;						 6/26/24 Removed partial-match code from subst-match. Further efforts on partial match info will
;;						 be via the old all-matches-aux.
;;
;;						 6/27/24 subst-matrch now has a tag 'subst-match-fail which dumps detailed info on the
;;						 failures. Perf not reduced too much. Leave this log-style stuff like this for now to see if it
;;						 helps with debugging or anything else.
;;
;; better-root-var -- 5/2/23 Detect root var use and set designated root vars via heuristic.
;;					  10/10/23  Turned off for now as if appears to be just as fast or faster, and
;;							    higher efficiency, without the heuristic. It's also simpler for further
;;							    root-var analysis to assume there is no heuristic.
;;					  8/7/24 Add to this tag the handling of root var by low-level operations such as match-one-edge.
;;
;; match-perf -- Improve match perf by looking at the various pieces, e.g., poss-match. First we've shorted out
;;					 scan-and-subst, and everything seems to be faster. This tag is related to the previous two, in
;;					 particular, how we handle root vars is critical to perf. But also, we'll need to balance how we
;;					 approach partial match with the desire to fail quickly if a full match is not possible.
;;
;; all-var-mod -- Disabled 5/18/23. Some time ago made changes to enable a pred to have all vars. A good experiment, but
;;				  not practical to pursue right now. See write-up in doc.txt. Tag left in below with I hope appropriate
;;				  comments.
;;
;; qets-to-seqs -- Conversion of qets to seqs, e.g., optimize/dump qet-utils class.
;;				   5/21/23 -- Discharged. qet-utils class is gone. is-subqet now just uses CL search, and is a graph method.
;;
;; dump-sn -- Remove the old "sn" new-node model. Can be simpler now, say, e.g., just (?nn1 new-node).
;;
;; check-rule-trace -- Change explicit rule-trace check with a method; allows options like break; the method's name is
;;					   check-rule-trace, so a change to that name qualifies as a tag.
;;					   9/7/23 -- Yes, still at it, see new-check-rule-trace as it looks like a promising direction.
;;
;; fix-set-subtract-order -- Discharged. Using CL set-difference does it.
;;
;; dump-all-edges -- Absurd quest to be sure. How do we graph all the edges?  What concepts are required? Can we draw
;;					 down on "all" some and still show meaningful stuff? See test.lisp.
;;
;; exec-queue -- Whether and how to implement new control strategies. Doing an explicit queue or exec has been disabled as of 9/3/23.
;;				 See comment in rule30.lisp with heading "9/5/23 Regarding is/is-not rules".
;;
;; del-on-rematch -- In ace, we do the dels even though we've matched before and not added new edges. The idea is that
;;					 for example we probably still want to delete rules at the end of a propagation of rules around a
;;					 chain. As of 9/4/23 looks like it's working ok.
;;
;; elim-edge-to-trace -- Should we? It has not shown to be that useful and it seems that jpg snapshots offer a better
;;						 debugging tool. Note right now it's turned off, using the dummy-sur-map.
;;						 9/20/23 -- May still be useful for rule-deps. Needs thought.
;;
;; re-queue-behavior -- Found that when we exec the queue, if the exec of the obj just popped off succeeds, re-queue the
;;						obj. My original idea was probably that if an obj succeeds, it's worth looking at again after
;;						some other objs have run. Not doing this, as a test, causes failures in that we get no
;;						butterflies for n above 3. Not even the exec-all loops pick them up. Need to know why, but
;;						clearly right now we keep the re-queue heuristic.
;;
;; auto-add-gnlrp -- We still auto-add the edge (global-node local-rule-pool local-rule-pool) when make objgraph.
;;
;; debug-tool -- Good technique for debugging. Length limits to avoid screen overload.
;; 
;;				(let ((x (let ((*print-level* 1))
;;						   (let ((*print-length* 1))
;; 							 (show-stack) 
;; 							 )
;; 						   nil))))
;;
;; multi-types -- Allow the attribute "type" to take on multi values, which means using memq instead of eq in the code.
;;					Discharged 6/4/24
;;
;; enable-subst-match -- Turn on subst-match for more types of matches, specifically, to allow subst-match to run on a
;;						  const root-var, and when so allow single-const qet lookup. Leaves only rest-vars not covered
;;						  by subst-match yet.
;;

(defc graph nil nil
  (let ((size 63)) ;; 1021
	(let (
		  (nodelist (make-sur-map :input-size size :res-size 17))
		  (edgelist (make-hash-table :test #'equal :size size)) ;; As noted above in the string-nodes comment, the system does not hold together with non-eqv strings.
		  (nodeposlist (make-hash-table :test #'equal :size size))
		  (subqet-map (make-sur-map :res-size 17))
		  (superqet-map (make-sur-map :res-size 17))
		  (edges-from-subqet-count (make-hash-table :test #'equal :size size))
		  )

	  (defm get-edges-from-subqet-count-table ()		;; Debug only
		edges-from-subqet-count)

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
			  ($comment (log-stat 'node-dim (length (get-edges node))))
			  node))
		  (let ((e (gethash edge edgelist)))
			(if (null e)
				(let ()
				  ($comment (log-stat 'edge-size (length edge)))
				  (setf (gethash edge edgelist) edge)
				  (let ((i 0))
					(dolist (n edge)
					  (add-node n i edge)
					  (setq i (+ i 1))))
				  (add-subqets edge)
				  edge)
				e))))

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


	  ;; Using tab\le rather than cache is much better perf, especially as size grows.

	  (defm count-edges-from-subqet (subqet)
		(timer 'count-edges-from-subqet
		  (lambda ()
			(or (gethash subqet edges-from-subqet-count) 0))))

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

	  (defm superqets-exist (qet)
		(! (superqet-map lookup) qet :check-existence-only t))

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
			(superqets-exist qet)))

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
		(let ((subqet-table (make-hash-table :test #'equal :size 13)))	
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
						(add-subqet sub-qet qet)
						(setf (gethash sub-qet subqet-table) sub-qet)))))
				(add-layer sub-qets (sublists-len-n edge len) (- len 1))))
			(timer 'add-subqets
			  (lambda ()
				(setf (gethash edge edges-from-subqet-count) (+ (or (gethash edge edges-from-subqet-count) 0) 1))
				(add-layer nil (list edge) (- (length edge) 1))
				(maphash (lambda (k v)
						   (let ((qet v))
							 (setf (gethash qet edges-from-subqet-count) (+ (or (gethash qet edges-from-subqet-count) 0) 1))))
						 subqet-table)
				nil)))))

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

	  (defm old-new-rem-subqets (edge)
		(let ((subqet-table (make-hash-table :test #'equal :size 13)))	
		  (defr
			(defl decr-count (qet)
			  (when qet
				(when (null (gethash qet subqet-table))
				  (setf (gethash qet subqet-table) qet)
				  (setf (gethash qet edges-from-subqet-count) (- (or (gethash qet edges-from-subqet-count) 0) 1)))
				(let ((subqets (subqets qet)))
				  (dolist (subqet subqets)
					(decr-count subqet)))))
			(defl rem-qet (qet)
			  (when (not (null qet))
				(let ((subqets (subqets qet)))
				  (remhash qet edges-from-subqet-count)
				  (rem-subqet qet)
				  (dolist (subqet subqets)
					(when (and (= (length (superqets subqet)) 0)
							   (not (edge-exists subqet)))
					  (rem-qet subqet))))
				nil))
			(clrhash subqet-table)
			(decr-count edge)
			(rem-qet edge)
			nil)))

	  (defm rem-subqets (edge)
		(let ((subqet-table (make-hash-table :test #'equal :size 13)))	
		  (defr
			(defl decr-count (qet)
			  (when qet
				(when (null (gethash qet subqet-table))
				  (setf (gethash qet subqet-table) qet)
				  (setf (gethash qet edges-from-subqet-count) (- (or (gethash qet edges-from-subqet-count) 0) 1)))
				(let ((subqets (subqets qet)))
				  (dolist (subqet subqets)
					(decr-count subqet)))))
			(defl rem-qets ()
			  (maphash (lambda (k v)
						 (let ((qet v))
						   (when (and (= (length (superqets qet)) 0)
									  (not (edge-exists qet)))
							 (rem-subqet qet))))
					   subqet-table)
			  nil)
			(clrhash subqet-table)
			(decr-count edge)
			(rem-qets)
			nil)))

	  (defm old-rem-subqets (edge)
		(defr
		  (defl rem-qet (qet)
			(print (list 'rq qet))
			(when (not (null qet))
			  (let ((subqets (subqets qet)))
				(rem-subqet qet)				;; Use qet to get subqets before removing it
				(dolist (subqet subqets)
				  (when (and (= (length (superqets subqet)) 0)			;; Note use of length here may cause perf loss
							 (not (edge-exists subqet)))
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

	  ;; excl-set = exclusion set (hitting set?) -- any edge which overlaps with excl-set is not used.
	  ;;
	  ;; Does breadth-first search so always returns a shortest path.
	  ;;
	  ;; Could use breadth fcn -- a little more elegant

	  (defm path (n1 n2 &key excl-set trace)
		(let ((visit-hash (make-hash-table :test #'equal)))
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
				r))
			(defl b2 ()
			  (block b
				(defr
				  ;; bni = bnode-info = (bnode chain)
				  (defl b1 (bnis)
					(when bnis
					  (b1 (mapunion (lambda (bni)
									  (let ((bnode (first bni))
											(bnode-chain (second bni)))
									  	(if (equal bnode n2)
											(return-from b (cons n2 bnode-chain))
											(when (not (gethash bnode visit-hash))
											  (setf (gethash bnode visit-hash) bnode)
											  (mapcar (lambda (bnode1) (list bnode1 (cons bnode bnode-chain)))
													  (get-children bnode))))))
									bnis))))
				  (b1 (list (list n1 nil))))))
			(reverse (b2)))))

	  )))

(defc xgraph graph nil
  (let ((a 1))
	(defm m1 (x) (+ a x))
	(defm m2 ())
	(defm get-all-nodes ()
	  "Ha ha!!!")))

;; (r add ?x abc ?y) (r pred ?x abc ?y) (r not ?x abc ?y) (r del ?x abc ?y)
;; (r local) (r global) (r attach-to x) (r name n) (r disabled)

(defc rule-components nil (pred-list del-list add-list not-list pred-rulegraph)
  (let ()
	(defm as-list () ;; For debug only
	  (list pred-list del-list add-list not-list))
	(defm preds ()
	  pred-list)
	(defm dels ()
	  del-list)
	(defm adds ()
	  add-list)
	(defm nots ()
	  not-list)
	(defm pred-rulegraph ()
	  pred-rulegraph)
	(defm all-nodes ()			;; Used by the dumper
	  (let ((h (make-hash-table :test #'equal)))
		(dolist (l (append pred-list del-list add-list not-list))
		  (dolist (n l)
			(setf (gethash n h) n)))
		(let ((r nil))
		  (maphash (lambda (k v)
					 (setq r (cons v r)))
				   h)
		  r)))
	))

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

	;; pred-fcn is (lambda (x) (some-predicate x)) -> x, where x is the object in the queue, if the predicate is true of
	;; x, else nil. x if the first object so encountered starting from end denoted by end-type (default head).

	(defm find (pred-fcn &key (end-type :head))			;; end-type element-of (:head :tail)
	  (find-and-remove pred-fcn end-type nil))

	;; Like find, but removes the object so found
	(defm remove (pred-fcn &key (end-type :head))
	  (find-and-remove pred-fcn end-type t))

	;; private:

	(defm find-and-remove (pred-fcn end-type do-remove)
	  (let ((queue-entry-accessor (case end-type (:head #'queue-entry-prev) (:tail #'queue-entry-next))))
		(defr
		  (defl doloop (entry) ;; May expand to include limits or other bells and whistles
			(when entry
			  (let ((v (queue-entry-value entry)))
				(if (funcall pred-fcn v)
					(let ()
					  (when do-remove
						(remove-one entry))
					  v)
					(doloop (funcall queue-entry-accessor entry))))))
		  (doloop (case end-type (:head head) (:tail tail))))))

	;; Removes from the queue all entries "equal" to the given entry

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
		nil))

	(defm get-head ()		;; Debug fcn
	  head)

	(defm get-tail ()		;; Debug fcn
	  tail)
	
	))

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

(defstruct et-entry
  type			;; :add :pred :del :edge-to-add :edge-to-pred :queued :already-queued :dequeued :failed :no-new-edges :new-edges
  trace-seqno
  rule-seqno
  rule-edge
  rule-node
  rule-name
  obj-node)

(defc edge-to-trace nil ()
  (let ((trace-seqno 0))
	(let ((rule-seqno 0))
	  (let ((et-table (make-sur-map)))
		;; Record initial state, e.g., as via define-rule, before rule eval begins. 
		(defm init-trace (graph)	;; Note we have a std class fcn "init" already
		  (let ((g graph))
			(dolist (e (! (g get-all-edges)))
			  (insert-er e e :edge-to-add 'kernel 'kernel nil))))
		;; edge-to-trace, :add :pred :del
		(defm insert-et (edge type rule-node rule-name obj-node)
		  (! (et-table insert) edge (make-et-entry :trace-seqno trace-seqno :rule-seqno rule-seqno :type type
												   :rule-node rule-node :rule-name rule-name :obj-node obj-node))
		  (setq trace-seqno (+ trace-seqno 1)))
		;; node event, :queued :already-queued :dequeued :failed :no-new-edges :new-edges :new-edges-from
		(defm insert-en (node type rule-node rule-name obj-node)
		  (! (et-table insert) node (make-et-entry :trace-seqno trace-seqno :rule-seqno rule-seqno :type type
												   :rule-node rule-node :rule-name rule-name :obj-node obj-node))
		  (setq trace-seqno (+ trace-seqno 1)))
		;; edge to rule clause, either a pred or add, :edge-to-add :edge-to-pred
		(defm insert-er (edge rule-edge type rule-node rule-name obj-node)
		  (! (et-table insert) edge (make-et-entry :trace-seqno trace-seqno :rule-seqno rule-seqno :type type
												   :rule-edge rule-edge :rule-node rule-node :rule-name rule-name :obj-node obj-node))
		  (setq trace-seqno (+ trace-seqno 1)))
		(defm incr-rule-seqno ()
		  (setq rule-seqno (+ rule-seqno 1)))
		(defm get-rule-seqno ()
		  rule-seqno)
		(defm get-trace-seqno ()
		  trace-seqno)
		(defm as-list ()
		  (! (et-table as-list)))
		;; Returns the edge trace in flattened, trace-seq order.
		(defm get-flatlist ()
		  (let ((flatlist nil))
			(let ((trace (as-list)))
			  (dolist (entry trace)
				(let ((edge (first entry)))
				  (let ((events (second entry)))
					(dolist (event events)
					  (setq flatlist (cons (list edge event) flatlist)))))))
			(let ((flatlist (sort flatlist 
								  (lambda (x y)
									(< (et-entry-trace-seqno (second x)) (et-entry-trace-seqno (second y)))))))
			  flatlist)
			flatlist))))))

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

(defc objgraph graph nil
  (let ((obj-queue (make-queue))
		(global-node 'global-node)
		(global-rule-pool 'global-rule-pool-node)
		(local-rule-pool 'local-rule-pool-node)
		(hget-key-rest1 (list nil nil))
		(hget-sup-rest1 (list nil nil))
		(edge-to-trace (make-edge-to-trace))
		(std-vars (make-std-vars))
		(rule-stats nil)
		(expand-edges-visit-hash (make-hash-table :test #'equal))
		)
	(let ((hget-key-rest2 (rest hget-key-rest1))
		  (hget-sup-rest2 (rest hget-sup-rest1))
		  (env-triggered-table (make-env-triggered std-vars))
		  )

	  (defm init ()
		(! (env-triggered-table set-graph) self)
		(setq rule-stats (make-rule-stats self))
		;; Would be nice to get rid of auto-add of this edge     auto-add-gnlrp
		(addraw global-node 'local-rule-pool local-rule-pool)
		nil)

	  (defm get-edge-to-trace ()
		edge-to-trace)

	  (defm std-vars ()	;; For debug only
		std-vars)

	  ;; Note we no longer do anything special to an "obj" node, e.g. adding rule pool links and such, so we can
	  ;; probably just name this fcn new-node().

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
		  (when (chkptag 'queue-node)
			(print (list 'queue-node n)))
		  (if (and only-if-not-queued
				   (node-queued n))
			  (let ()
				(! (edge-to-trace insert-en) n :already-queued nil nil nil)
				nil)
			  (let ()
				(! (edge-to-trace insert-en) n :queued nil nil nil)
				(if push-head
					(! (obj-queue push-head) n)
					(! (obj-queue push-tail) n))))))

	  (defm get-queue () ;; debug fcn
		obj-queue)

	  (defm clear-queue ()
		(setq obj-queue (make-queue)))

	  ;; Do a straight execution of the queue, popping off the head, *however* if the obj exec succeeds, re-queue the
	  ;; obj.
	  ;;
	  ;; re-queue-behavior

	  (defm execute-queue (&key once (rule-mode :local-global))
		(defr
		  (defl pop-next ()
			(let ((r (! (obj-queue pop-head))))
			  (when (chkptag 'eq4)
				(when r
				  (print (list 'eq4 r))))
			  r))
		  (let ((failed-queue-count 0))
			(block exq
			  (loop

			   ;; Interesting experiment, i.e., executing as a stack instead
			   ;; of a queue.  FFT worked, even a bit faster. Number of
			   ;; exec-all passes 2 instead of three. However seemed to be
			   ;; more dup of rule execs.
			   ;; (let ((obj (! (obj-queue pop-tail))))
			 
			   (let ((obj (pop-next)))
				 (when (null obj) ;; Null means queue empty
				   (return-from exq nil))
				 (! (edge-to-trace insert-en) obj :dequeued nil nil nil)
				 (when (chkptag 'pop-head)
				   (print (list 'pop-head obj)))

				 ;; Experiment -- see queue-len-graph.gnuplot. Plotted the
				 ;; queue length, then edited to produce a queue size
				 ;; printed with each rule test, with a spike to 1000 for
				 ;; each success. So you can see the pattern of areas
				 ;; with a high success rate, followed by stretches of
				 ;; failures. Worth formalizing better at some point.
				 ;;
				 ;; (print (list 'qlen (length (! (obj-queue as-list)))))

				 (if (functionp obj)
					 (let ((r (funcall obj)))
					   (cond
						((or (null r) (eq r :done))
						 nil)
						((eq r :requeue)
						 (queue-node obj))))
					 (execute-obj obj :rule-mode rule-mode :cont
					   (lambda (m s p r f)
						 ;; (print (list 'eq42  obj (mapcar (lambda (n) (hget n 'name)) r) (mapcar (lambda (n) (hget n 'name)) f)))
						 (when once
						   (return-from exq nil))
						 (when m ;; If obj exec succeeds, re-queue   re-queue-behavior
						   (queue-node obj)))))))))))

	  ;; An enum "type"
	  (defm match-status ()
		'(:failed :new-edges :no-new-edges))

	  ;; rule-mode
	  ;; The default, :local-global, is the standard execution order. The rest are for testing/debugging.
	  ;;
	  ;; :local-only						- local rules only
	  ;; ??? :local-rule-pool-only				- local rule pool only
	  ;; xxx :local-and-global-rule-pool-only	- local and global rule pools only
	  ;; :local-global						- local rules, then global rules  [default]
	  ;; :all								- Union of attached rules, local rule pool, and global rule pool
	  ;;
	  ;; Calls (cont <edge-creation-status> <match-status> <matched-edges> <successful-rules> <failed-rules>)
	  ;; 
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <match-status> == (match-status)
	  ;; <matched-edges> == union of all matches (not split out by env)
	  ;; <successful-rules> == all rules which resulted in new edges
  	  ;; <failed-rules> == rules which did not match

	  (defm execute-obj (node &key 
							  (rule-mode :local-global)
							  (cont (lambda (m s p r f) (list m s))))
		(timer 'execute-obj
		  (lambda ()
			(let ((r nil)
				  (match-status :failed)
				  (m-and-e-match-status nil)	;; Ack! a real hack with all the flags
				  (matched-edges nil)
				  (evaled-rules nil)
				  (successful-rules nil))		;; Resulted in new edges
			  (defr
				(defl while (thunk)
				  (let ((r (funcall thunk)))
					(when r
					  (while thunk))))
				(defl get-all-rules ()
				  (let ((rules (hget-all node 'rule)))
					(let ((rule-orders (mapcad (lambda (x) (when (equal (first x) node) (intersect (rest (rest x)) rules))) (get-edges-from-subqet `(,node rule-order)))))
					  (let ((unordered-rules (set-subtract rules (mapunion (lambda (x) x) rule-orders))))
						(let ((r (append (mapappend (lambda (x) x) rule-orders) unordered-rules)))
						  ;; (print (list 'eo1 node 'ros rule-orders unordered-rules r))
						  r)))))
				(defl m-and-e (rule node)
				  (match-and-execute-rule rule node :cont
										  (lambda (m s n p d)
											(setq m-and-e-match-status s)
											(setq matched-edges (hunion matched-edges (mapunion (lambda (edges) edges) p)))
											(when (or (and (not (eq match-status :new-edges))
														   (eq s :new-edges))
													  (and (eq match-status :failed)
														   (eq s :no-new-edges)))
											  (setq match-status s))
											(when m
											  (setq r t))
											d)))
				(let ()
				  (cond
				   ((eq rule-mode :local-rule-pool-only)
					(let ((local-rule-pool (hget node 'local-rule-pool)))
					  (when local-rule-pool
						(let ((local-rule-pool-rules (dedup-rules (hget-all local-rule-pool 'lrp-rule))))
						  (dolist (rule local-rule-pool-rules)
							(m-and-e rule node))))))
				   ((eq rule-mode :all)
					(let ((rules (hunion (hget-all local-rule-pool 'lrp-rule)
										 (hunion
										  (hget-all global-rule-pool 'grp-rule)
										  (get-all-rules)))))
					  (dolist (rule rules)
						(let ((r (m-and-e rule node)))
						  (setq evaled-rules (cons rule evaled-rules))
						  (when (eq m-and-e-match-status :new-edges)
							(setq successful-rules (cons rule successful-rules)))))))
				   ((or (eq rule-mode :local-only)
						(eq rule-mode :local-global))
					(let ()
					  (while
						  (lambda ()
							(block b
							  (let ((rules (set-subtract (get-all-rules) evaled-rules)))
								(dolist (rule rules)
								  (let ((r (m-and-e rule node)))
									(setq evaled-rules (cons rule evaled-rules))
									(when (eq m-and-e-match-status :new-edges)
									  (setq successful-rules (cons rule successful-rules)))
									(when r
									  (return-from b t))))
								nil))))
					  (when (not (eq rule-mode :local-only))
						(when global-rule-pool
						  (let ((global-rules (dedup-rules (hget-all global-rule-pool 'grp-rule))))
							(dolist (rule global-rules)
							  (m-and-e rule node))))))))
				  (when (chkptag 'eo2)
					(when (eq match-status :new-edges)
					  (let ((evaled-rule-names (mapcar (lambda (rule-node) (hget rule-node 'name)) evaled-rules)))
						(let ((successful-rule-names (mapcar (lambda (rule-node) (hget rule-node 'name)) successful-rules)))
						  (print (list 'eo2-1 r match-status matched-edges evaled-rule-names))
						  (print (list 'eo2-2 'succrules successful-rule-names))
						  (print (list 'eo2-3 'nrules (length evaled-rules) (length successful-rules)))))))
				  (let ((failed-rules (set-subtract (get-all-rules) successful-rules)))
					(funcall cont r match-status matched-edges successful-rules failed-rules))))))))

	  (defm execute-all-objs (&key (rule-mode :local-global))
		(timer 'execute-all-objs
		  (lambda ()
			(let ((nodes (get-all-nodes))
				  (r nil))
			  (dolist (node nodes)
				(execute-obj node :rule-mode rule-mode :cont
				  (lambda (m s p r f)
					(when m
					  (setq r t)))))
			  r))))

	  ;; By default, executions both off the queue and in the full scan are local and global. See comment.
	  ;;
	  ;; Below, we run the different kinds of evaluators in order in a loop: global-node, queue, exec-all. We stop when
	  ;; a sequence of any three has produced no new edges.

	  (defm execute-global-all-objs-loop (&key (queue-rule-mode :local-global) 
											   (scan-rule-mode :local-global)	;; Note this was local-only, but the
																				;; global rule pool should mostly be
																				;; small or empty, so we cover all the
																				;; ground by checking them both, and
																				;; doing that for each mode. Ideally
																				;; these modes go away or just stay for
																				;; debugging.
											   )
		(let ((i 0)
			  (no-new-edges-cnt 0))
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
			(defl exec-and-check-no-new-edges (thunk)
			  (let ((nedges (length (get-all-edges))))
				(funcall thunk)
				(let ((new-nedges (length (get-all-edges))))
				  (= nedges new-nedges))))
			(defl exec-all-objs-and-queue (rule-mode)
			  (timer 'exec-all-objs-and-queue 
				(lambda ()
				  (let ((nodes (get-all-nodes)))
					(dolist (node nodes)
					  (execute-obj node :rule-mode rule-mode :cont (lambda (m s p r f) nil))
					  (timer 'exec-all-objs-and-queue-exec-queue
						(lambda ()
						  (execute-queue :rule-mode queue-rule-mode))))))))
			(let ()
			  (block b
				(loop 
				 (exec-until-no-new-edges
					 (lambda ()
					   (log1 'global-node
							 (lambda ()
							   (execute-obj 'global-node :rule-mode :local-global :cont (lambda (m s p r f) nil))))))
				 (setq no-new-edges-cnt (+ no-new-edges-cnt 1))
				 (when (= no-new-edges-cnt 3) 
				   (return-from b nil))
				 (let ((no-new-edges
						(exec-and-check-no-new-edges 
						 (lambda ()
						   (log1 'queue
								 (lambda ()
								   (execute-queue :rule-mode queue-rule-mode)))))))
				   (setq no-new-edges-cnt (if no-new-edges (+ no-new-edges-cnt 1) 0))
				   (when (= no-new-edges-cnt 3)
					 (return-from b nil)))
				 (let ((no-new-edges
						(exec-and-check-no-new-edges 
						 (lambda ()
						   (log1 'exec-all
								 (lambda ()
								   ;; (execute-all-objs :rule-mode scan-rule-mode)				;; !!!!!!!! Running the queue per-obj here
								   (exec-all-objs-and-queue scan-rule-mode)
								   ))))))
				   (setq no-new-edges-cnt (if no-new-edges (+ no-new-edges-cnt 1) 0))
				   (when (= no-new-edges-cnt 3)
					 (return-from b nil)))))
			  ($comment
			   (log-stat 'entropy (edge-asc-entropy))
			   (log-stat 'dist (edge-asc-dimension-dist)))))))

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
	  ;; vardesc is list of one var => list of bound values (flattened version of next variant)
	  ;; vardesc is list of vars => list of bound value lists, each bound value list in the order of the given vars
	  ;; vardesc is null => list of all envs
	  ;; vardesc is :edges => list of all matched edges, i.e., envs resolved
	  ;;
	  ;; Subst-match not used below as it can loop explosively, in particular on
	  ;;	  (! (g query) '((?r type rule)(?r name ?n)(?r file ?f)))
	  ;; Looks like the multiple occurences of a given file name to a rule is the issue.

	  (defm query (clauses &optional vardesc &key rule-trace)
		(let ((pred-clauses (subseq clauses 0 (or (search '(:not) clauses) (length clauses)))))
		  (let ((not-clauses (rest (subseq clauses (or (search '(:not) clauses) (length clauses))))))
			(let ((envs (query1 pred-clauses not-clauses rule-trace)))
			  (if (is-var-name vardesc)
				  (env-lookup vardesc (first envs) :idempotent nil)
				  (if (eq vardesc :edges)
					  (matched-edges-union pred-clauses envs)
					  (if vardesc
						  (let ((r (dedup-list (mapcar (lambda (env)
														 (mapcar (lambda (var)
																   (env-lookup var env :idempotent nil))
																 vardesc))
													   envs))))
							(if (= (length vardesc) 1)
								(mapcar (lambda (x) (first x)) r)
								r))
						  envs)))))))

	  (defm query1 (clauses not-clauses rule-trace)
		(let ((all-clauses (hunion clauses not-clauses)))
		  (let ((rule-info (define-rule `(rule (pred ,@all-clauses)) :unattached t)))
			(let ((rule-edges (second rule-info)))
			  (let ((rule (first rule-info)))
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
					($nocomment (setq gog og))		;; Could get large
					(let ((r (! (og query2) clauses not-clauses rule-trace)))
					  (dolist (rule-edge rule-edges)
						(rem-edge rule-edge))
					  r))))))))

	  (defm query2 (clauses not-clauses rule-trace)
		(let ((env-table (make-hash-table :test (set-hash-test))))
		  (let ((rule-info (define-rule `(rule (name query) (pred ,@clauses) (not ,@not-clauses)) :unattached t)))
			(let ((rule (first rule-info)))
			  (when rule-trace
				(trace-rule 'query))
			  (dolist (node (get-all-nodes))
				(let ((envs (all-matches-with-not rule node :use-all-matches-aux2 t)))		;; Avoid issues with subst-match loop explosion
				  (dolist (env envs)
					(let ((env (env-prune env (cons 't (! (std-vars base-vars))))))
					  (setf (gethash env env-table) env)))))))
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

	  ;; Implemented by calling edge version due to bug (see above)

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
		(! (self get-rule-components-aux) rule-node))

	  (defm invalidate-rule-components-cache-entry (rule-node)
		(timer 'invalidate-rule-components-cache-entry
		  (lambda ()
			(! (self invalidate-rule-components-cache-entry-aux) rule-node))))

	  (let ((rule-components-cache (make-sur-map)))
		(defm invalidate-rule-components-cache-entry-aux (rule-node)
		  (! (rule-components-cache remove) rule-node))
		(defm get-rule-components-aux (rule-node)
		  (defr
			(defl get-clause-edges (clause-type)
			  (let ((edges (get-edges-from-subqet (list rule-node clause-type))))
				(mapcad (lambda (e)
						  (when (and (equal (first e) rule-node) (equal (second e) clause-type))
							(rest (rest e))))
						edges)))
			(timer 'get-rule-components
			  (lambda ()
				(let ((rule-comps (! (rule-components-cache lookup-one) rule-node)))
				  (or rule-comps
					  (let ((pred-list (get-clause-edges 'pred)))
						(let ((del-list (get-clause-edges 'del)))
						  (let ((add-list (get-clause-edges 'add)))
							(let ((not-list (get-clause-edges 'not)))
							  (let ((pred-rulegraph (make-rulegraph rule-node (hget rule-node 'name) (hget rule-node 'root-var))))
								(dolist (pred pred-list)
								  (! (pred-rulegraph add-edge) pred))
								(let ((rule-comps (make-rule-components pred-list del-list add-list not-list pred-rulegraph)))
								  (! (rule-components-cache insert-one) rule-node rule-comps)
								  rule-comps)))))))))))))

	  ;; Nodes can no longer just be "unattached" in the sense of being immune to rules. This became more effective with
	  ;; the change to global pool handling, i.e., it's all internal to the kernel and not visible in H.  So we could
	  ;; just return true, but we optimize a little by checking whether the global pool is empty, and if so, we check
	  ;; for local rules.

	  (defm has-rules (node)
		(or (not (null (get-edges 'global-rule-pool-node)))
			(not (null (hget node 'rule)))))

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
				  (let ((v (env-lookup rule-node env)))
					(setq e (append e (if (listp v) v (list v))))))
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
			(when (memq 'rule (hget-all node 'type))
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
			(! (self do-add-consequent-edges) obj-node pred-edges add-edges not-edges del-edges rule-node (hget rule-node 'name) envlist cont))))

	  (let ((env-new-edges (make-sur-map)))	
		(let ((new-or-dup-edge-list nil))			;; should be new or dup or print
		  (let ((all-node-hash (make-hash-table :test #'equal)))
			(let ((new-node-hash (make-hash-table :test #'equal)))
			  (defm do-add-consequent-edges (obj-node pred-edges add-edges not-edges del-edges rule-node rule-name envlist cont)
				(defr
				  ;; Delete edges after evaluating their elements as vars. 
				  ;; First evaluates not-edges and for each env if any not-edge exists, then bail from that env
				  ;; Returns a list of deleted edges (which only exist as lists, not edges)
				  (defl del-consequent-edges (edges not-edges envlist)
					(let ((r nil))
					  (check-rule-trace rule-name (list 'del-consequent-edges edges not-edges envlist))
					  (dolist (env envlist)
						(block b
						  (dolist (edge not-edges)
							(let ((not-edge
								   (mapappend (lambda (node)
												(let ((node (env-lookup node env)))
												  (cond
													((listp node)
													 node)
													((eq node :undefined)
													 nil)
													(t
													 (list node)))))
											  edge)))
							  (when (edge-exists not-edge)
								(return-from b nil))))
						  (dolist (edge edges)
							(let ((del-edge
								   (mapappend (lambda (node)
												(let ((node (env-lookup node env)))
												  (cond
													((listp node)
													 node)
													((eq node :undefined)
													 nil)
													(t
													 (list node)))))
											  edge)))
							  (when del-edge
								(check-rule-trace rule-name (list 'del-edge del-edge))
								(when (chkptag 'ace4)
								  (print (list 'ace4 'del del-edge)))
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
				  (defl get-implicit-nodes-to-queue (new-edges new-node-hash all-node-hash)
					(let ((all-nodes (hash-table-value-to-list all-node-hash)))
					  (let ((new-nodes (hash-table-value-to-list new-node-hash)))
						(let ((rule-neighborhood-nodes (! (self get-rule-neighborhood) all-nodes)))
						  (let ((nodes-with-rules (mapcad (lambda (node) ;; BUG?!!! global rules bypassed?
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
							  nodes))))))
				  ;;
				  ;; This gets a little hokey, but worth a try. (exec <node>) will be considered transient, and so
				  ;; that edge should be deleted. (<node> exec) will be left in. We presume here that we'll not have a
				  ;; huge nunmber of these right now.
				  ;;

				  ;; !!!! Disabled !!!!

				  (defl get-explicit-nodes-to-exec ()
					(block b
					  (return-from b nil)
					  (let ((exec-edges (get-edges-from-subqet '(exec))))
						(let ((prefix-exec-edges (mapcad (lambda (e) (when (eq (first e) 'exec) e)) exec-edges)))
						  (let ((suffix-exec-edges (mapcad (lambda (e) (when (and (= (length e) 2) (eq (second e) 'exec)) e)) exec-edges)))
							(let ((nodes-to-exec (set-subtract (hunion (nodes prefix-exec-edges) (nodes suffix-exec-edges)) '(exec))))
							  (dolist (edge prefix-exec-edges)
								(rem-edge edge))
							  nodes-to-exec))))))

				  (defl old-get-explicit-nodes-to-exec ()
					(block b
					  ;; (return-from b nil)
					  (let ((exec-edges (mapcad (lambda (e) (when (eq (first e) 'exec) e)) (get-edges-from-subqet '(exec)))))
						(when (and exec-edges
								   (= (length exec-edges) 1))
						  (let ((exec-edge (first exec-edges)))
							(let ((exec-list (rest exec-edge)))
							  (rem-edge exec-edge)
							  exec-list))))))

				  ;; !!!! Disabled !!!!

				  (defl get-explicit-nodes-to-queue ()
					(block b
					  (return-from b nil)
					  (let ((queue-edge (first (get-edges-from-subqet '(queue)))))
						(if queue-edge
							(let ((queue-list (rest queue-edge)))
							  (rem-edge queue-edge)
							  queue-list)))))
					
				  (defl get-queue-info ()
					(mapcar (lambda (n) (list n (hget-all-list (list n) '(rule name)))) 
							(! (obj-queue as-list))))

				  ;; If we're adding an edge with a rule node as first element, invalidate the cache.
				  
				  (defl check-rule-components-cache (edge)
					(let ((node (first edge)))
					  (when (memq 'rule (hget-all node 'type))
						(invalidate-rule-components-cache-entry node))))

				  (defl print-info (p env)
					($nocomment	;; Perf test taking out prints shows no speed diff
					 (let ((root-var (env-lookup '?root-var env :idempotent nil)))
					   (print (list (! (edge-to-trace get-rule-seqno)) #|(get-log-seqno)|# obj-node root-var p))
					   )))
					
				  (let ((r nil)
						(first-edge nil)
						(matched-edges nil)
						(deleted-edges nil)
						(matched-and-new-edges-per-env nil)
						(n-edges-added 0))
					(dolist (env envlist)
					  (timer 'add-consequent-edges-per-env
						(lambda ()
						  (let ((root-var (env-lookup '?root-var env))
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
									  (when (not (edge-exists `(,rule-node no-triggered))) ; ;
									  (setq te (! (env-triggered-table insert) rule-node env matched-edges))) ; ;
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
										  (let ((edge-is-print (eq (first add-edge) 'print))) ;; This avoids sensing resulting edges as prints, as happened with for-rule
											(block xxx
											  (let ((new-edge nil))
												(dolist (node add-edge)
												  (let ((new-node (let ((sn (env-lookup node env)))	;; dump-sn
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
												(if edge-is-print ;; For debug -- if print is head node of an edge, print it
													(setq print-list (append print-list (list (rest new-edge))))
													(setq new-or-dup-edge-list (append new-or-dup-edge-list (list new-edge))))
												
												(when (and (not (edge-exists new-edge))
														   (not (eq (first new-edge) 'print)))
												  ;; Note if a rule just has prints and adds no edges, triggered table
												  ;; does not get activated. A relatively minor bug which I suppose should
												  ;; be fixed at some point.
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
												  (setq n-edges-added (+ n-edges-added 1))
												  (check-rule-components-cache new-edge)
												  (! (rule-stats update-root-var) rule-node root-var)
												  (when (chkptag 'ace4)
													(print (list 'ace4 'add new-edge)))
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
								(print-info p env)))

							;; del-on-rematch
							;;
							;; Here, we do the dels even though we've matched before and not added new edges. The idea
							;; is that we probably still want to delete rules at the end of a propagation of rules
							;; around a chain.

							($nocomment	;; Still problems with this?
							 (when (= (hash-table-count all-node-hash) 0)
							   (del-consequent-edges del-edges not-edges envlist)))

							(when (not (= (hash-table-count all-node-hash) 0))
							  (dolist (p print-list)
								(print-info p env))
							  (setq r t)
							  (let ((me (matched-edges pred-edges env)))
								(setq matched-edges (cons me matched-edges))
								;; Note use of ordered list of potentially redundant edges for tracing. Print edges are also included. 
								(setq matched-and-new-edges-per-env (cons (list me new-or-dup-edge-list #|(! (env-new-edges results))|# ) matched-and-new-edges-per-env)))
							  ;; (add-consequent-edges-info (list env (hash-table-to-list all-node-hash)))
							  )
							(let ((explicit-nodes-to-exec (get-explicit-nodes-to-exec)))
							  (let ((explicit-nodes-to-queue (get-explicit-nodes-to-queue)))
								(if (or explicit-nodes-to-exec explicit-nodes-to-queue)
									(let ()
									  (dolist (node explicit-nodes-to-exec)
										(queue-node node :push-head t))
									  (dolist (node explicit-nodes-to-queue)
										(queue-node node :push-head nil)))
									(let ((nodes (get-implicit-nodes-to-queue (! (env-new-edges results))  new-node-hash all-node-hash)))
									  ($comment
									   (when nodes 
										 (print (list 'ace2 nodes))))
									  (dolist (node nodes)
										(when (has-rules node)
										  (queue-node node :only-if-not-queued t)))
									  (when nodes
										(when (chkptag 'ace5)
										  (print (list 'ace5 (get-queue-info))))
										(when (chkptag 'ace6)
										  (print (list 'ace6 (! (obj-queue as-list))))))))))))))
					(! (rule-stats update-max-env-size) rule-node (length envlist))
					(! (rule-stats update-max-edges-added) rule-node n-edges-added)
					(when r
					  (check-rule-trace rule-name `(ace-new-edges rule ,rule-node ,rule-name obj ,obj-node)))
					(funcall cont r matched-edges deleted-edges matched-and-new-edges-per-env))))))))
	
	  (defm trace-rule (rule-name)
		(add-edge `(rule-trace ,rule-name)))

	  (defm untrace-rule (rule-name)
		(rem-edge `(rule-trace ,rule-name)))

	  (defm break-rule (rule-name &optional (key t) (fcn nil))
		(add-edge `(rule-trace ,rule-name))
		(add-edge `(rule-break ,rule-name))
		(add-edge `(rule-break-info ,rule-name ,key ,fcn)))

	  (defm unbreak-rule (rule-name)
		(rem-edge `(rule-break ,rule-name)))

	  ;;
	  ;; This gets called from the kernel to ascertain whether we should do any tracing or breaking action. 
	  ;; 
	  ;; Edges calling for such action are added similarly to qets, where we include subqets. In this case an edge of
	  ;; the form (rule-trace <rule-name>) is always added, and it is very fast to check for a fixed edge. So we check
	  ;; for this first and only go further if this edge is found.
	  ;;
	  ;; So if (rule-trace <rule-name>) is found we then look for an edge of the form (rule-break <rule-name>). If this
	  ;; is found we will then look for yet another edge whose prefix is (rule-break <rule-name>). The rest of that edge
	  ;; is then an info list, the trace info, a set of directives and options for controlling the trace/break.
	  ;;
	  ;; The interpretation of the info is that the first element is a key, a symbol, used to specify that an action
	  ;; should be triggered via the break info.  The remainder is a key- (ie, context-) specific list of info; if any
	  ;; member of this list is a function, it is called as a thunk to resolve the value. So the info is only returned
	  ;; lazily, ie only evaluated if a break is actually detected.
	  ;;
	  ;; We only do an actual trace if the break form is not present; i.e., break takes priority. And at this point we
	  ;; have no further options for trace.
	  ;;

	  (defm check-rule-trace (rule-name &optional trace-info)
		(defr
		  (defl resolve-lazy-values (l)
			(mapcar (lambda (x) (if (functionp x) (funcall x) x)) l))
		  (let ((l trace-info))
			(let ((r (edge-exists `(rule-trace ,rule-name))))
			  (when r
				(let ((b (edge-exists `(rule-break ,rule-name))))
				  (if b
					  (let ((break-info-list (get-edges-from-subqet `(rule-break-info ,rule-name))))
						(dolist (break-info break-info-list)
						  (let ((key (third break-info)))
							(let ((fcn (fourth break-info)))
							  (cond
							   ((and (not (null fcn))
									 (or (eq key t)
										 (eq key (first l))))
								(funcall fcn (resolve-lazy-values l)))
							   ((or (eq key t)
									(eq key (first l)))
								(let ((msg (format nil "rule-break ~a ~a ~a" rule-name b (resolve-lazy-values l))))
								  (cerror "xxx" msg))))))))
					  (when l
						(print `(rule-trace ,@(resolve-lazy-values l)))))))
			  r))))

	  (defm old-check-rule-trace (rule-name &optional trace-info)
		(let ((l trace-info))
		  (let ((r (edge-exists `(rule-trace ,rule-name))))
			(when r
			  (let ((b (edge-exists `(rule-break ,rule-name))))
				(if b
					(let ((break-info (rest (rest (first (get-edges-from-subqet `(rule-break-info ,rule-name)))))))
					  (let ((key (first break-info)))
						(let ((fcn (second break-info)))
						  (cond
						   ((and (not (null fcn))
								 (or (eq key t)
									 (eq key (first l))))
							(funcall fcn l))
						   ((or (eq key t)
								(eq key (first l)))
							(let ((msg (format nil "rule-break ~a ~a ~a" rule-name b l)))
							  (cerror "xxx" msg)))))))
					(when l
					  (print `(rule-trace ,@l))))))
			r)))

	  (defm get-rule-stats ()	;; With the advent of the pass-through next, this should only be for debug
		rule-stats)
	  
	  (defm rule-stats (&rest x)		;; Call the method by hand to pass the keyword args without denoting them explicitly
		(funcall (funcall rule-stats 'rule-stats) x))

	  (defm update-last-expand-len (rule-node len)
		(! (rule-stats update-last-expand-len) rule-node len))

	  (defm update-new-edges-max-expand-len (rule-node)
		(! (rule-stats update-new-edges-max-expand-len) rule-node))

	  (defm get-matched (rule-node)
		(! (rule-stats get-matched) rule-node))

	  ;; Calls (cont <edge-creation-status> <match-status> <new-edges> <matched-edges> <deleted-edges>)
	  ;; 
	  ;; <edge-creation-status> == t if any new edges were created, else nil
	  ;; <match-status> == (match-status)
	  ;; <new-edges> == edges created
	  ;; <matched-edges> == of form (edges ...), one set of edges per env
	  ;; <deleted-edges> == t if any edges were deleted, else nil

	  (defm match-and-execute-rule (rule-node obj-node
											  &key
											  (cont (lambda (r match-status new-edges matched-edges deleted-edges)
													  (list r match-status new-edges matched-edges deleted-edges)))
											  )
		(bool-timer 'match-and-execute-rule-true 'match-and-execute-rule-false 
		  (lambda ()
			(let ((rule-name (hget rule-node 'name)))
			  (let ((r nil)
					(match-status nil)
					(new-edges nil)
					(matched-edges-list nil)
					(deleted-edges nil))
				(when (not (edge-exists `(,rule-node disabled)))
				  ($comment ;; check-rule-trace: why is this commented-out?
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
								($comment
								 (when (or m
										   deleted-edges)
								   (log-stat 'new-edge-rule rule-name)))
								(when deleted-edges
								  (dolist (del-edge deleted-edges)
									(! (edge-to-trace insert-et) del-edge :del rule-node rule-name obj-node)))
								(if m
									(let ()
									  (setq match-status :new-edges)

									  ($comment
									   (log-stat 'entropy (edge-asc-entropy))
									   (log-stat 'dist (edge-asc-dimension-dist)))

									  ($comment
									   ;;
									   ;; better-root-var
									   ;;
									   ;; 10/10/23
									   ;; Turned off for now as if appears to be just as fast or faster, and
									   ;; higher efficiency, without the heuristic. It's also simpler for further
									   ;; root-var analysis to assume there is no heuristic.
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
											   ($comment ;; Some are nil because they're the nested form, e.g., ?root-var-1
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
									  ($comment (log-stat 'new-edge-by-rule 5000))
									  (update-new-edges-max-expand-len rule-node)
									  (setq r t))
									(let ()
									  (setq match-status :no-new-edges)
									  (! (rule-stats update-not-new-edges) rule-node)))
								(cond
								 ((eq match-status :new-edges)
								  (! (edge-to-trace insert-en) obj-node :new-edges rule-node rule-name obj-node)
								  (dolist (mne-entry matched-and-new-edges-per-env)
									(let ((matched-edges (first mne-entry)))
									  (let ((new-edges (second mne-entry)))
										(dolist (matched-edge matched-edges)
										  (! (edge-to-trace insert-et) matched-edge :pred rule-node rule-name obj-node))
										(let ((node-table (make-hash-table :test #'equal)))
										  (dolist (new-edge new-edges)
											(dolist (node new-edge)
											  (if (and (not (equal node obj-node))
													   (null (gethash node node-table)))
												  (let ()
													(! (edge-to-trace insert-en) node :new-edges-from rule-node rule-name obj-node)
													(setf (gethash node node-table) node))))
											(! (edge-to-trace insert-et) new-edge :add rule-node rule-name obj-node)))
										(dolists ((matched-edge matched-pred) (matched-edges (second (filter-new-node-pred-edges pred-list))))
										  (! (edge-to-trace insert-er) matched-edge matched-pred :edge-to-pred rule-node rule-name obj-node))
										;; To get the right match-up in trace, need to remove print edges from adds
										(dolists ((new-edge add) (new-edges (mapcad (lambda (e) (when (not (eq (first e) 'print)) e)) add-list)))
										  (! (edge-to-trace insert-er) new-edge add :edge-to-add rule-node rule-name obj-node))))))
								 ((eq match-status :no-new-edges)
								  (! (edge-to-trace insert-en) obj-node :no-new-edges rule-node rule-name obj-node))
								 ((not (memq match-status (match-status)))
								  (print (list 'match-status-bogus match-status))))
								))))
						(let ()
						  (setq match-status :failed)
						  (check-rule-trace rule-name (list 'match-and-execute-rule-failed rule-node rule-name obj-node
															(lambda () (all-matches rule-node obj-node))))
						  (! (rule-stats update-failed) rule-node)
						  (! (edge-to-trace insert-en) obj-node :failed rule-node rule-name obj-node)
						  ))))
				(! (edge-to-trace incr-rule-seqno))
				(when (chkptag 'me4)
				  (print (list 'me4 obj-node rule-name rule-node r match-status))) ;; This gives us a good addition to the basic output sequence since it shows each rule test
				(funcall cont r match-status new-edges matched-edges-list deleted-edges))))))

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

	  ;; all-var-mod -- not right now [5/18/23 -- not sure what this means]
	  
	  (defm all-matches (rule-node obj-node &key use-all-matches-aux2)
		(! (self do-all-matches) rule-node obj-node use-all-matches-aux2))

	  (defr
		(defl all-matches-hash-equal (x y)
		  (and (equal (first x) (first y))
			   (env-equal (second x) (second y))))
		(let ((hash-test-name (hdefine-hash-table-test #'all-matches-hash-equal #'set-hash)))
		  (defm do-all-matches (rule-node obj-node use-all-matches-aux2)
			(timer 'all-matches
			  (lambda ()
				(let ((rulegraph (! ((get-rule-components rule-node) pred-rulegraph))))
				  (let ((root-vars (get-root-vars rule-node)))
					(let ((h (make-hash-table :size 7 :test hash-test-name)))
					  (let ((possible-match-fcn (possible-match-fcn rule-node obj-node rulegraph)))
						(when possible-match-fcn
						  (block b
							(dolist (root-var root-vars)
							  (let ((poss-match (funcall possible-match-fcn root-var)))
								(when (chkptag 'am1)
								  (print (list 'am1 (hget rule-node 'name) obj-node poss-match root-var)))
								(when poss-match
								  (let ()
									(let ((envlist (all-matches-aux rulegraph obj-node root-var :use-all-matches-aux2 use-all-matches-aux2)))
									  (when (null envlist) ;; This means a false positive: possible-match was true, but real match failed.
										(when (chkptag 'am3)
										  (print (list 'am3 (hget rule-node 'name) obj-node root-var))))
									  (when envlist
										(when (chkptag 'am2)
										  ($comment (print (list 'am2 (hget rule-node 'name) obj-node root-var envlist)))
										  (print (list 'am2 (hget rule-node 'name) obj-node root-var (length envlist) (let ((g (intern (symbol-name (gensym))))) (set g envlist) g)))
										  )
										(dolist (env envlist)
										  (let ((env-info (list root-var env)))
											(setf (gethash env-info h) env-info)))
										;; (return-from b nil)  ;; To bail or not to bail, that is the question. Today we bail.
										;; global-change -- no don't bail, get all root vars 
										;; 5/18/23 -- Keep don't-bail, as dealing with root vars seems fundamental.
										)))))))))
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
							  envlist))))))))))))

	  ;; To disable subst-match for testing, rename this method to something and rename all-matches-aux2 to
	  ;; all-matches-aux

	  (defm all-matches-aux (rule-graph obj-node root-var &key use-all-matches-aux2)
		(if (and (not use-all-matches-aux2)
				 (or
				  t ;; (is-var-name root-var)   ;; enable-subst-match
				  ;; These experiments did not increase the speed as expected. Subst-match slower but so is other stuff.
				  ;; (< (! (self count-edges-from-subqet) (list root-var)) 10)
				  ;; (eq (! (rule-graph name)) 'weave-next-rule)
				  )
				 (not (! (rule-graph has-rest-vars))))
			(! (rule-graph subst-match) self obj-node root-var)
			(all-matches-aux2 rule-graph obj-node root-var)))

	  (defm all-matches-aux2 (rule-graph obj-node root-var &key (rule-name (! (rule-graph name))))	;; rule-name arg for tracing purposes
		(timer 'all-matches-aux2
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

	  (defm all-matches-with-not (rule-node obj-node &key use-all-matches-aux2)
		(let ((envs (all-matches rule-node obj-node :use-all-matches-aux2 use-all-matches-aux2)))
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
					(setq new-node-env (cons (list (first new-node-pred-edge) (third new-node-pred-edge)) new-node-env))) ;; dump-sn   Here we snap binding, eg (?nn1 sn1)
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

	  ;; better-root-var: Examine the need for the root var in this and related low-level matchers. E.g. the logic of
	  ;; only having one of the roots in -aux seems like it should not be necessary.

	  (defm match-one-edge (pat-edge obj-edge 
									 root-var		;; If null, is ignored
									 obj-node		;; If null, is ignored (implied by null root-var)
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
				 ;; If we have only one of the roots, then whole edge does not match   better-root-var
				 ((or (and root-var
						   (not (eq root-var :undefined))
						   (equal (first pat-edge) root-var)
						   (not (equal (first obj-edge) obj-node)))
					  (and root-var
						   (not (eq root-var :undefined))
						   (not (equal (first pat-edge) root-var))
						   (equal (first obj-edge) obj-node)))
				  ;; (print (list 'moea1 pat-edge obj-edge root-var obj-node))		;; When this is in it looks like only "xis" picks up this case    better-root-var
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
								   (when (memq 'rule (hget-all node 'type))
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
		(! (self define-rule-aux) rule-expr :new-pool new-pool :unattached unattached))

	  (let ((new-var-cnt 0)
			(new-sn-cnt 0))
		(defm define-rule-aux (rule-expr &key (new-pool 0) (unattached nil))
		  (when (not (eq (first rule-expr) 'comment))
			(remove-named-rule rule-expr)
			(let ((edge-list nil)
				  (add-to-nodes nil)
				  (add-to-lrp nil)
				  (add-to-grp nil)
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
							((eq clause-type 'type)
							 (let ((type (second clause)))
							   (pushe (add-edge (list rule 'type type)))))
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
							 (setq add-to-grp t)
							 (pushe (add-edge `(,rule global))))
							((eq clause-type 'disabled)
							 (pushe (add-edge `(,rule disabled))))
							((eq clause-type 'attach-to)
							 (dolist (node (rest clause))
							   (setq add-to-nodes (cons node add-to-nodes))
							   (pushe (add-edge `(,rule attach-to ,node)))))
							((and (memq clause-type '(add))
								  (rest clause))
							 (dolist (edge (rest clause))
							   (let ((edge (xform-std-vars edge)))
								 (cond
								   ((and (listp (first (last edge))) (eq (first (first (last edge))) 'rule))
									(let ((rule-values (define-rule
														   (first (last edge))
														   :new-pool (+ new-pool 1)
														   :unattached t)))
									  (let ((rule-node (first rule-values))
											(rule-edges (second rule-values)))
										(pushe (add-edge (list rule 'add rule 'nested-rule rule-node)))
										(pushe (add-edge (list rule 'pred rule-node 'new-node (new-sn))))
										;; (pushe (add-edge (list rule 'add (first edge) (second edge) rule-node)))
										(pushe (add-edge `(,rule add ,@(butlast edge) ,rule-node)))
										(dolist (rule-edge rule-edges)
										  (pushe (add-edge (append (list rule 'add) rule-edge)))))))
								   (t
									(pushe (add-edge (append (list rule clause-type) edge))))))))
							((and (memq clause-type '(pred del not))
								  (rest clause))
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
					(pushe (add-edge (list rule 'separate-display-node t)))

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

					(list rule edge-list))))))))

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
		(let ((rule-graph (! ((get-rule-components rule-node) pred-rulegraph))))		  
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

	  (defm get-rule-consts (rule)
		(let ((pred-list (! ((get-rule-components rule) preds))))
		  (dedup-list (mapcan (lambda (edge)
								(filter-vars edge))
							  pred-list))))

	  (defm get-rule-consts-pred (pred-list)
		(dedup-list (mapcan (lambda (edge)
							  (filter-vars edge))
							pred-list)))

	  ;; Returns a function which when called with the root var, will do the check, in the rule context setup by the
	  ;; closure. Much faster than calling the whole thing over and over.

	  ;; 10/17/23 -- Removed scan-and-subst. With advent of subst-match it's less useful, and adds overhead. Overall
	  ;; speed increased by quite a bit, though there are more calls now to subst-match.
	  ;; 
	  ;; 3/25/19 -- Troubles with this new possible-match-fcn when tried with fe-rule-test. Had to change from detecting
	  ;; consts in scan-and-subst to just checking edge existence, since the copiers can have vars in the results. Also
	  ;; in scan-and-subst had to change from detecting a null env to checking that it's stopped changing on each
	  ;; loop. Otherwise we always get a binding to a var.
	  ;;
	  ;; Also took out chain-check -- probably breaks due to the need to handle vars in some way not accounted for in
	  ;; the chain processing.  However scan-and-subst seems to supersede its need anyway.
	  ;;
	  ;; This could be pointing out that we should be careful in straying too much from the isomorphism theme.

	  (defm possible-match-fcn (rule-node obj-node rulegraph)	;; all-var-mod
		(timer 'possible-match-fcn
		  (lambda ()
			(macrolet ((hprint (&rest args)
						 nil
						 ;; `(print (list ,@args))
						 ))
			  (defr

				  (defl get-rule-preds-root (preds root-var) ;; Returns <preds-containing-root-var>
					(let ((r nil))
					  (dolist (pred preds)
						(when (member root-var pred :test #'equal)
						  (setq r (cons pred r))))
					  r))

				  ;; Reduces preds by removing all vars which are not the root var, and substituting obj-node for the root var.
				  ;; Then looks up the existence of the resulting qets. T iff all of them exist, else nil.
				  ;;
				  ;; 1/31/24 Had to fix since his doesn't work now that we're using contiguous subseqs only! Since we can
				  ;; have (<root-obj> a ?u b ?v), and we won't necessarily have (a b) as a qet. So we need to do the same
				  ;; qet extraction we do in subst-match. Code cloned to some degree. Found this problem with the "or"
				  ;; test. See test.lisp, look for 'adding an "or" clause'.

				  (defl check-rule-preds-root-qets (preds root-var obj-node)
					(let ((qets (filter-non-root-vars preds root-var obj-node)))
					  (block b
						(dolist (qet qets)
						  (when (not (or (and (qet-exists qet)
											  (or
											   (edge-exists qet)
											   (not (null (superqets qet)))))))
							(return-from b nil)))
						t)))

				(defl filter-non-root-vars (preds root-var obj-node)
				  (mapunion (lambda (pred)
							  (let ((qets (filter-vars-to-qets pred)))
								qets))
							(mapcar (lambda (pred)
									  (mapcar (lambda (node) (if (eq node root-var) obj-node node)) pred))
									preds)))

				;; !! Fcn name and logic cloned in subst-match !!
				(defl filter-vars-to-qets (pred)
				  (defr
					(defl is-var (node)
					  (is-var-name node))
					(defl doloop (l c r)
					  (if (null l)
						  (append r (when c (list c)))
						  (if (is-var (first l))
							  (doloop (rest l)  nil (append r (when c (list c))))
							  (doloop (rest l) (append c (list (first l))) r))))
					(doloop pred nil nil)))

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

				;; Returns t if qets found, nil if not, and :undefined if doesn't apply
				
				(defl check-obj-qets (obj-node rulegraph)
				  (if (! (rulegraph has-seq-vars))
					  :undefined
					  (block b
						(let ((pred-consts (! (rulegraph get-pred-consts))))
						  (when (member obj-node pred-consts :test #'equal)
							(return-from b :undefined))
						  (dolist (const pred-consts)
							(when (or (qet-exists (list obj-node const))
									  (qet-exists (list const obj-node)))
							  (return-from b t)))
						  nil))))

				(defl check-pred-const-obj-nodes-intersection (obj-node rule-node)
				  (let ((obj-edges (get-edges obj-node)))
					(let ((obj-nodes (nodes obj-edges)))
					  (let ((rule-comps (get-rule-components rule-node)))
						(let ((preds (second (filter-new-node-pred-edges (! (rule-comps preds))))))
						  (let ((pred-const-nodes (get-rule-consts-pred preds)))
							(let ((ipo (intersect pred-const-nodes obj-nodes)))	;; all-var-mod: Change this to (let ((ipo obj-nodes))...) to enable all-vars
							  ipo)))))))
				
				(let ()
				  (let ((ipo (let ((coq (check-obj-qets obj-node rulegraph)))
							   (if (eq coq :undefined)
								   (check-pred-const-obj-nodes-intersection obj-node rule-node)
								   coq))))
					(let ((rule-comps (get-rule-components rule-node)))					;; Note calls here redundant wrt check-pred-const-obj-nodes-intersection
					  (let ((preds (second (filter-new-node-pred-edges (! (rule-comps preds))))))
						(let ((has-rest-vars (block b (mapc (lambda (pred) (when (has-rest-var pred) (return-from b t))) preds) nil)))
						  (when ipo
							(lambda (root-var) ;; Returns pos-match = {t, nil}
							  (timer 'possible-match
								(lambda ()
								  (block b
									(let ((r 
										   (and (or (is-var-name root-var)
													(equal root-var obj-node))
												(let ((obj-edges (get-edges obj-node))) ;; Note calls here redundant wrt check-pred-const-obj-nodes-intersection
												  (let ((obj-nodes (nodes obj-edges)))
													(let ((root-preds (get-rule-preds-root preds root-var)))
													  (let ((root-pred-const-nodes (get-rule-consts-pred root-preds)))
														(let ((pred-qets-match (check-rule-preds-root-qets root-preds root-var obj-node)))
														  (when am-traced
															(print (list 'pmf1 root-var preds root-preds root-pred-const-nodes obj-nodes obj-node obj-edges pred-qets-match)))
														  (and
														   pred-qets-match
														   (= (length (intersect root-pred-const-nodes obj-nodes))
															  (length root-pred-const-nodes))
														   )))))))))
									  (let ((r (or r has-rest-vars)))
										;; Not clear which of these stats is better
										(gstat 'possible-match-true  (lambda (x y) (+ x y)) (lambda () (if r 1 0)))
										(gstat 'possible-match-false (lambda (x y) (+ x y)) (lambda () (if r 0 1)))
										;; (bool-timer 'possible-match-true 'possible-match-false (lambda () r))
										r)))))))))))))))))

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

	  ;; Max number of times d a rule r was matched by some distinct edge e. List of (r d), sorted by d.
	  
	  (defm dimensions ()
		(let ((r (sort (let ((et-info-list (! (edge-to-trace as-list))))
						 (let ((rule-to-max (make-hash-table)))
						   (dolist (et-info et-info-list)
							 (let ((h (make-hash-table)))
							   (dolist (et-entry (second et-info))
								 (when (eq (et-entry-type et-entry) :pred)
								   (let ((rule-name (et-entry-rule-name et-entry)))
									 (setf (gethash rule-name h) (+ (or (gethash rule-name h) 0) 1)))))
							   (maphash (lambda (k v)
										  (let ((rule-name k)
												(edge-max v))
											(setf (gethash rule-name rule-to-max) (max (or (gethash rule-name rule-to-max) 0) edge-max))))
										h)))
						   (hash-table-to-list rule-to-max)))
					   (lambda (x y) (> (second x)(second y))))))
		  r))

	  ;; Max number of times d a pred p in a rule r was matched by some distinct edge e. List of ((r p) d), sorted by d.
	  
	  (defm pred-dimensions ()
		(let ((r (sort (let ((et-info-list (! (edge-to-trace as-list))))
						 (let ((pred-to-max (make-hash-table :test #'equal)))
						   (dolist (et-info et-info-list)
							 (let ((h (make-hash-table :test #'equal)))
							   (let ((matched-edge (first et-info)))
								 (dolist (et-entry (second et-info))
								   (when (eq (et-entry-type et-entry) :edge-to-pred)
									 (let ((rule-name (et-entry-rule-name et-entry)))
									   (let ((pred (et-entry-rule-edge et-entry)))
										 (let ((key (list rule-name pred)))
										   (setf (gethash key h) (+ (or (gethash key h) 0) 1)))))))
								 (maphash (lambda (k v)
											(let ((key k)
												  (edge-max v))
											  (setf (gethash key pred-to-max) (max (or (gethash key pred-to-max) 0) edge-max))))
										  h))))
						   (hash-table-to-list pred-to-max)))
					   (lambda (x y) (> (second x)(second y))))))
		  r))

	  

	  (defm edge-dimensions ()
		(defr
		  (defl num-edge-to-pred (et-entry-list)
			(let ((r 0))
			  (dolist (et-entry et-entry-list)
				(when (eq (et-entry-type et-entry) :edge-to-pred)
				  (setq r (+ r 1))))
			  r))
		  (sort (let ((e (! (edge-to-trace as-list))))
				  (let ((r nil))
					(dolist (x e)
					  (setq r (cons (list (first x) (num-edge-to-pred (second x))) r)))
					r))
				(lambda (x y) (> (second x)(second y))))))

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

	  ;; Calc the euler char of the entire graph. Any subgraphing desired must be done separately.
	  ;; See doc.txt.
	  ;;
	  ;; Returns list (euler-char orientable-genus non-orientable-genus)

	  (defm euler-char-genus ()
		(defr
		  (defl euler-char-knill (asc)	;; Knill formula
			(let ((omegas (mapcar (lambda (set) (expt -1 (- (length set) 1)))
								  asc)))
			  (let ((r 0))
				(dolist (x omegas)
				  (setq r (+ r x)))
				(list r (/ (- 2 r) 2) (- 2 r) asc))))
		  (let ((s (mapappend (lambda (x) (subsets x)) (get-all-edges))))
			(let ((asc nil))
			  (dolist (x s)
				(when (and x
						   (not (member x asc :test #'set-equal)))
				  (setq asc (cons x asc))))
			  (euler-char-knill asc)))))

	  ;; Returns a graph of a full trace of rule execution.  
	  ;; 
	  ;; (rule a edge) = edge has been added by rule
	  ;; (rule d edge) = edge has been deleted by rule
	  ;; (edge p rule) = edge is a pred of rule
	  ;; (rule1 am rule2) = rule1 has added an edge which adds a component to rule2 (i.e., and "add which modifies another rule"
	  ;; (obj-node ao rule) = rule node was run on obj-node (detected via :add). Note gv with just ao looks somewhat
	  ;;					   interesting, as it tells you what rules ran on what objects.
	  ;; (obj-node r rule) = inverse of ao
	  ;; (obj-node e edge-node) = what edge contains obj-node 
	  ;;
	  ;; q
	  ;; pe -- edge to pred-edge
	  ;; pr -- pred-edge to rule
	  ;; ae -- add-edge to edge
	  ;; ar -- rule to add-edge
	  ;;
	  ;; 5/25/23 Consigned the "tested" stuff to the old version, as I haven't found a good graphical model for it yet.

	  (defm edge-trace-graph (&key (rules '(t))
								   (omitted-rules nil)
								   (trim-dangling-adds-and-preds nil)
								   (min-freq 0))
		(defr
		  (defl has-preds (events)
			(block b
			  (dolist (event events)
				(when (eq (et-entry-type event) :edge-to-pred)
				  (return-from b t)))
			  nil))
		  (defl find-add (events)
			(block b
			  (dolist (event events)
				(let ((kind (et-entry-type event)))
				  (when (eq kind :edge-to-add)
					(return-from b event))))
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
		  (defl random-frac-list (frac l-in)
			(let ((l l-in))
			  (let ((r nil))
				(let ((n (floor (* frac (length l)))))
				  (dotimes (i n)
					(let ((x (nth (random (- n i)) l)))
					  (setq r (cons x r))
					  (setq l (set-subtract l (list x))))))
				r)))
		  (let ((g (make-foundation)))
			(let ((g1 (make-objgraph)))
			  (let ((trace (! (edge-to-trace as-list))))
				(dolist (entry trace)							;; g1 is then a reproduction of the graph captured by the trace. Helps for getting rule names, etc.
				  (let ((edge (first entry)))
					(when (listp edge)
					  (! (g1 add-edge) edge))))
				;; This pass does a, p, d, ao, am, an, q, r, e
				(let ((am-done nil))
				  (dolist (entry trace)
					(let ((edge (first entry)))
					  (when (listp edge)
						(let ((events (second entry)))
						  (when t #|(has-preds events)|#
							(dolist (event events)
							  (let ((kind (et-entry-type event)))
								(let ((seqno (et-entry-rule-seqno event)))
								  (let ((rule-name (et-entry-rule-name event)))
									(let ((obj-node (et-entry-obj-node event)))
									  (when (admit-rule rule-name :rules rules :omitted-rules omitted-rules)
										(let ((rule-node (symcat rule-name '-- seqno)))
										  (! (g add-edge) (list rule-node 'name rule-name)) ;; We did use rule-node for the name, which also offered the seqno; may bring that back
										  (! (g add-edge) (list rule-node 'type 'et-rule))
										  (let ((edge-node (format nil "~a" edge)))
											(! (g add-edge) (list edge-node 'type 'et-edge))
											(cond
											  ((eq kind :add)
											   (! (g add-edge) (list rule-node 'a edge-node))
											   (when (memq 'rule (! (g1 hget-all) (first edge) 'type))
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
													   (when (admit-rule (! (g1 hget) g1-rule-node 'name) :rules rules :omitted-rules omitted-rules)
														 (! (g add-edge) (list rule-node 'an node))
														 ($comment (let ((g1-rule-name (! (g1 hget) g1-rule-node 'name)))
																	 (let ((new-rule-node (! (g new-obj-node))))
																	   (! (g add-edge) (list node 'rn new-rule-node))
																	   (! (g add-edge) (list new-rule-node 'label g1-rule-name))
																	   (! (g add-edge) (list new-rule-node 'type 'et-obj))))))))))
											   (when (memq obj-node edge)
												 (! (g add-edge) (list obj-node 'type 'et-obj))
												 (! (g add-edge) (list rule-node 'ao obj-node))))
											  ((eq kind :del)
											   (! (g add-edge) (list rule-node 'd edge-node)))
											  ((eq kind :pred)
											   (let ()
												 ($comment ;; Not sure what q is for and why this logic
												  (when (not (! (g edge-exists) (list edge-node 'q rule-name)))
													(! (g add-edge) (list edge-node 'q rule-name))
													(! (g add-edge) (list edge-node 'p rule-node))))
												 (! (g add-edge) (list edge-node 'p rule-node))													
												 (dolist (node edge)
												   (when (! (g1 hget) node 'rule)
													 (let ((g1-rule-nodes (! (g1 hget-all) node 'rule)))
													   (dolist (g1-rule-node g1-rule-nodes)
														 (when (admit-rule (! (g1 hget) g1-rule-node 'name) :rules rules :omitted-rules omitted-rules)
														   (! (g add-edge) (list node 'pn rule-node))
														   ($comment (let ((g1-rule-name (! (g1 hget) g1-rule-node 'name)))
																	   (let ((new-rule-node (! (g new-obj-node))))
																		 (! (g add-edge) (list node 'rn new-rule-node))
																		 (! (g add-edge) (list new-rule-node 'label g1-rule-name))
																		 (! (g add-edge) (list new-rule-node 'type 'et-obj))))))))))
												 (when (memq obj-node edge)
												   (! (g add-edge) (list obj-node 'type 'et-obj))
												   (! (g add-edge) (list obj-node 'r rule-node))
												   (! (g add-edge) (list obj-node 'e edge-node))))))))))))))))))))
					
				;; This pass does pe, pr, ae, ar
				(dolist (info trace)
				  (let ((edge (first info)))
					(when (listp edge)
					  (let ((events (second info)))
						(dolist (event events)
						  (when (memq (et-entry-type event) '(:edge-to-pred :edge-to-add))
							(let ((seqno (et-entry-rule-seqno event)))
							  (let ((rule-name (et-entry-rule-name event)))
								(let ((rule-edge (et-entry-rule-edge event)))
								  (when (admit-rule rule-name :rules rules :omitted-rules omitted-rules)
									(let ((rule-node (symcat rule-name '-- seqno)))
									  (let ((edge-node (format nil "~a" edge)))
										(if (eq (et-entry-type event) :edge-to-pred)
											(let ((pred-node-name (format nil "~a" rule-edge)))
											  (let ((pred-node (format nil "~a--~a--~a" pred-node-name rule-name seqno)))

												

												(! (g add-edge) (list pred-node 'type 'pred-node))
												(! (g add-edge) (list pred-node 'label pred-node-name))
												(! (g add-edge) (list edge-node 'pe pred-node))
												(! (g add-edge) (list pred-node 'pr rule-node))))
											(let ((add-node-name (format nil "~a" rule-edge)))
											  (let ((add-node (format nil "~a--~a--~a" add-node-name rule-name seqno)))
												(! (g add-edge) (list add-node 'type 'add-node))
												(! (g add-edge) (list add-node 'label add-node-name))
												(! (g add-edge) (list add-node 'ae edge-node))
												(! (g add-edge) (list rule-node 'ar add-node)))))))))))))))))
				(when trim-dangling-adds-and-preds
				  (trim-dangling-adds-and-preds g))
				g)))))
	  
	  (defm old-edge-trace-graph (&key (rules-fcn (lambda (r) t))
								   (except-rules-fcn (lambda (r) nil))
								   (nodes-fcn (lambda (edge) t))
								   (except-nodes-fcn (lambda (edge) nil))
								   (trim-dangling-adds-and-preds nil)
								   (min-freq 0))
		(defr
		  (defl has-preds (events)
			(block b
			  (dolist (event events)
				(when (eq (et-entry-type event) :edge-to-pred)
				  (return-from b t)))
			  nil))
		  (defl find-add (events)
			(block b
			  (dolist (event events)
				(let ((kind (et-entry-type event)))
				  (when (eq kind :edge-to-add)
					(return-from b event))))
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
		  (defl random-frac-list (frac l-in)
			(let ((l l-in))
			  (let ((r nil))
				(let ((n (floor (* frac (length l)))))
				  (dotimes (i n)
					(let ((x (nth (random (- n i)) l)))
					  (setq r (cons x r))
					  (setq l (set-subtract l (list x))))))
				r)))
		  (let ((g (make-foundation)))
			(let ((g1 (make-objgraph)))
			  (let ((trace (! (edge-to-trace as-list))))
				(dolist (entry trace)							;; g1 is then a reproduction of the graph captured by the trace. Helps for getting rule names, etc.
				  (let ((edge (first entry)))
					(when (listp edge)
					  (! (g1 add-edge) edge))))
				;; This pass does a, p, d, ao, am, an, q, r, e
				(let ((am-done nil))
				  (dolist (entry trace)
					(let ((edge (first entry)))
					  (when (listp edge)
						(let ((events (second entry)))
						  (when t #|(has-preds events)|#
							(dolist (event events)
							  (let ((kind (et-entry-type event)))
								(let ((seqno (et-entry-rule-seqno event)))
								  (let ((rule-name (et-entry-rule-name event)))
									(let ((obj-node (et-entry-obj-node event)))
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
													((eq kind :add)
													 (! (g add-edge) (list rule-node 'a edge-node))
													 (when (memq 'rule (! (g1 hget-all) (first edge) 'type))
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
													((eq kind :del)
													 (! (g add-edge) (list rule-node 'd edge-node)))
													((eq kind :pred)
													 (let ()
													   ($comment ;; Not sure what q is for and why this logic
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
																 ($comment (let ((g1-rule-name (! (g1 hget) g1-rule-node 'name)))
																			 (let ((new-rule-node (! (g new-obj-node))))
																			   (! (g add-edge) (list node 'rn new-rule-node))
																			   (! (g add-edge) (list new-rule-node 'label g1-rule-name))
																			   (! (g add-edge) (list new-rule-node 'type 'et-obj))))))))))
													   (when (memq obj-node edge)
														 (! (g add-edge) (list obj-node 'type 'et-obj))
														 (! (g add-edge) (list obj-node 'r rule-node))
														 (! (g add-edge) (list obj-node 'e edge-node)))))))))))))))))))))))

				;; This pass does pe, pr, ae, ar
				(dolist (info trace)
				  (let ((edge (first info)))
					(when (listp edge)
					  (let ((events (second info)))
						(dolist (event events)
						  (when (memq (et-entry-type event) '(:edge-to-pred :edge-to-add))
							(let ((seqno (et-entry-rule-seqno event)))
							  (let ((rule-name (et-entry-rule-name event)))
								(let ((rule-edge (et-entry-rule-edge event)))
								  (when (funcall rules-fcn rule-name)
									(when (not (funcall except-rules-fcn rule-name))
									  (when (funcall nodes-fcn edge)
										(when (not (funcall except-nodes-fcn edge))
										  (let ((rule-node (symcat rule-name '-- seqno)))
											(let ((edge-node (format nil "~a" edge)))
											  (if (eq (et-entry-type event) :edge-to-pred)
												  (let ((pred-node-name (format nil "~a" rule-edge)))
													(let ((pred-node (format nil "~a--~a--~a" pred-node-name rule-name seqno)))
													  (! (g add-edge) (list pred-node 'type 'pred-node))
													  (! (g add-edge) (list pred-node 'label pred-node-name))
													  (! (g add-edge) (list edge-node 'pe pred-node))
													  (! (g add-edge) (list pred-node 'pr rule-node))))
												  (let ((add-node-name (format nil "~a" rule-edge)))
													(let ((add-node (format nil "~a--~a--~a" add-node-name rule-name seqno)))
													  (! (g add-edge) (list add-node 'type 'add-node))
													  (! (g add-edge) (list add-node 'label add-node-name))
													  (! (g add-edge) (list add-node 'ae edge-node))
													  (! (g add-edge) (list rule-node 'ar add-node))))))))))))))))))))

				  (when trim-dangling-adds-and-preds
				  (trim-dangling-adds-and-preds g))
				  g)))))

	  ;;
	  ;; General admit-rule for in particular making visual representations, where typically we want to prune down the
	  ;; rules to mitigate display overload.
	  ;;
	  ;; Default is rules = (t), where t represents match-any, and omitted-rules = nil. Each may be a list, and if an
	  ;; element is a string, it's taken to mean that the string is contained in the (typically symbol) name (print
	  ;; format) of the rule name. There is right now no way to denote a literal string as a rule name.
	  ;;
	  ;; (admit-rule rule-name :rules '(init "tree") :omit-rules '(color-color))
	  ;; 

	  (defm admit-rule (rule &key (rules '(t)) (omitted-rules nil))
		(block b
		  (dolist (r rules)
			(when (or (eq r t)
					  (equal rule r)
					  (and (stringp r)
						   (search r (string-downcase (format nil "~a" rule)))))
			  (dolist (o omitted-rules)
				(when (or (equal rule o)
						  (and (stringp o)
							   (search o (string-downcase (format nil "~a" rule)))))
				  (return-from b nil)))
			  (return-from b t)))))

	  ;; Var-based trace graph

	  (defm edge-trace-var-graph (&key (min-freq 0) (rules '(t)) (omitted-rules nil))
		(defr
		  (defl has-preds (events)
			(block b
			  (dolist (event events)
				(when (eq (et-entry-type event) :edge-to-pred)
				  (return-from b t)))
			  nil))
		  (defl find-add (events)
			(block b
			  (dolist (event events)
				(let ((kind (et-entry-type event)))
				  (when (eq kind :edge-to-add)
					(return-from b event))))
			  nil))
		  (let ((g (make-foundation)))
			(let ((trace (! (edge-to-trace as-list))))
			  ;; This pass does the var-based part of the graph: v, ap, freq
			  (let ((freq-table (make-sur-map))) ;; Count of ap edge instances
				(dolist (entry trace)
				  (let ((edge (first entry)))
					(let ((events (second entry)))
					  (when (has-preds events)
						(let ((add-event (find-add events)))
						  (when add-event
							(let ((add-rule-name (et-entry-rule-name add-event)))
							  (when (admit-rule add-rule-name :rules rules :omitted-rules omitted-rules)
								(let ((add-rule-node (et-entry-rule-node add-event)))
								  (let ((add-edge (et-entry-rule-edge add-event)))
									(let ((add-seqno (et-entry-rule-seqno add-event)))
									  (dolist (event events)
										(let ((type (et-entry-type event)))
										  (when (eq type :edge-to-pred)
											(let ((rule-node (et-entry-rule-node event)))
											  (let ((rule-name (et-entry-rule-name event)))
												(when (admit-rule rule-name :rules rules :omitted-rules omitted-rules)
												  (let ((pred-edge (et-entry-rule-edge event)))
													(let ((env (mapcar (lambda (x y) (list x y)) add-edge pred-edge)))
													  ($comment (let ((*print-pretty* nil))
																  (print (list 'etv add-rule-name add-edge rule-name pred-edge env))))
													  (dolists ((binding node) (env edge))
														(mlet (((x y) binding))
														  (when (is-var-name y) ;; "value" of the binding must be a var
															(let ((node-node (symcat add-rule-name "-" node)))
															  (let ((rule-var-add (symcat add-rule-name "-" x)))
																(let ((rule-var-pred (symcat rule-name "-" y)))
																  (let ((rule-var-add-name x))
																	(let ((rule-var-pred-name y))
																	  (let ((ap-edge `(,rule-var-add ap ,rule-var-pred)))
																		(! (g add-edge) ap-edge)
																		(! (freq-table insert-one) ap-edge (+ (or (! (freq-table lookup-one) ap-edge) 0) 1))
																		(! (freq-table insert-one) rule-var-add (+ (or (! (freq-table lookup-one) rule-var-add) 0) 1)))
																	  (when (hget x 'type)
																		(! (g add-edge) `(,rule-var-add type ,(hget x 'type))))
																	  (when (hget y 'type)
																		(! (g add-edge) `(,rule-var-pred type ,(hget y 'type))))
																	  (if (hget x 'name)
																		  (! (g add-edge) `(,rule-var-add name ,(hget x 'name)))
																		  (! (g add-edge) `(,rule-var-add label ,rule-var-add-name)))
																	  (if (hget y 'name)
																		  (! (g add-edge) `(,rule-var-pred name ,(hget y 'name)))
																		  (! (g add-edge) `(,rule-var-pred label ,rule-var-pred-name)))


																	  (! (g add-edge) `(,node-node label ,node))
																	  (! (g add-edge) `(,rule-var-add na ,node-node))
																	  (! (g add-edge) `(,node-node np ,rule-var-pred))
																	  
																	  (! (g add-edge) `(,rule-node type rule))
																	  (! (g add-edge) `(,add-rule-node type rule))
																	  (! (g add-edge) `(,rule-node name ,rule-name))
																	  (! (g add-edge) `(,add-rule-node type rule))
																	  (! (g add-edge) `(,add-rule-node name ,add-rule-name))
																	  (! (g add-edge) `(,add-rule-node v ,rule-var-add))
																	  (! (g add-edge) `(,rule-node v ,rule-var-pred))
																	  (! (g add-edge) `(,add-rule-node type gv-cluster))
																	  (! (g add-edge) `(,add-rule-node gv-cluster-relation v))
																	  (! (g add-edge) `(,rule-node type gv-cluster))
																	  (! (g add-edge) `(,rule-node gv-cluster-relation v)))))))))))))))))))))))))))))
				(dolist (i (! (freq-table as-list)))
				  (let ((ap-edge (first i)))
					(when (listp ap-edge)
					  (let ((count (first (second i))))
						(let ((tot (! (freq-table lookup-one) (first ap-edge))))
						  (let ((freq (float (/ count tot))))
							(when (>= freq min-freq)
							  (! (g add-edge) (list (first ap-edge) 'freq freq (third ap-edge))))))))))
				g)))))

	  ;; Variant on above full trace: r1 is related to r2 iff there
	  ;; exists an edge e such that (r1 add e) and (e pred r2). Temporal
	  ;; (seqno) info is not included. This should provide a rule-dep
	  ;; type of graph, but with only rules deps that are really needed
	  ;; by a given run, rather than by analysis.

	  (defm edge-trace-rule-graph (&key rules (except-rules-fcn (lambda (x) nil)))
		(defr
		  (defl has-preds (events)
			(block b
			  (dolist (event events)
				(when (eq (et-entry-type event) :pred)
				  (return-from b t)))
			  nil))
		  (defl find-add (events)
			(block b
			  (dolist (event events)
				(let ((kind (et-entry-type event)))
				  (when (eq kind :add)
					(return-from b event))))
			  nil))
		  (let ((g (make-objgraph)))
			(let ((trace (! (edge-to-trace as-list))))
			  (dolist (entry trace)
				(let ((edge (first entry)))
				  (let ((events (second entry)))
					(when (has-preds events)
					  (let ((add-event (find-add events)))
						(when add-event
						  (let ((add-rule-name (et-entry-rule-name add-event)))
							(dolist (event events)
							  (let ((kind (et-entry-type event)))
								(let ((seqno (et-entry-rule-seqno event)))
								  (let ((rule-name (et-entry-rule-name event)))
									(when (or (null rules)
											  (memq add-rule-name rules)
											  (memq rule-name rules))
									  (when (and
											 (not (funcall except-rules-fcn rule-name))
											 (not (funcall except-rules-fcn add-rule-name)))
										(when (and (eq kind :pred)
												   (not (null add-rule-name)))
										  (! (g add-edge) (list add-rule-name 'r rule-name)))))))))))))))))
			g)))

	  ;; Assume a graph g of the form (<x> attr <y>), where attr is considerd an edge label, and return a new graph
	  ;; consisting edges of g which form cycles to cycle heads, starting from init-node.
  
	  (defm spanning-dag (init-node attr)
		(let ((new-g (make-graph)))
		  (let ((levels (make-sur-map)))
			(depth init-node
				   (lambda (node level)
					 (! (levels insert-one) node level)
					 (hget-all node attr)))
		  (breadth init-node
				   (lambda (node level)
					 (let ((children (hget-all node attr)))
					   (dolist (child children)
						 (when (> (! (levels lookup-one) child) level)
						   (! (new-g add-edge) (list node attr child))))
					   children))))
		  new-g))
	  
	  ;; Produces spanning dag via attr attr with attr s. Also adds edge (attr s)

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
  (failed 0)
  (new-edges-max-expand-len 0)
  (last-expand-len 0)
  (max-env-size 0)
  (max-edges-added 0)  
  (root-vars nil)
  (rule-node nil)
  (rule-name nil)
  )

(defstruct (col-info
			 (:constructor make-col-info (name fmt accessor)))
  name
  fmt
  accessor
  tab)

(defc rule-stats nil (graph)
  (let ((rule-stats-table (make-sur-map))
		(all-rules-tested 0)
		(all-rules-new-edges 0)
		(node-name-max 0)		;; max length of a node name
		(rule-name-max 0))		;; max length of a rule name
	(defr
	  (defl symbol< (x y) (string< (symbol-name x) (symbol-name y)))
	  (defl div (x y)
		(if (= y 0) 0 (/ x y)))
	  ;; Given a list of numbers (x1 x2 x3 ...) returns (x1 (+ x1 x2) (+ x1 x2 x3) ...)
	  (defl accum (l)
		(reverse (maplist (lambda (x) (apply #'+ x)) (reverse l))))
	  (defl smember (str symbols)
		(or (eq symbols t)
			(block b
			  (dolist (sym symbols)
				(when (sequal str sym)
				  (return-from b t))))))
	  (defl sequal (str sym)
		(equal (string-downcase str) (string-downcase (symbol-name sym))))
	  (defl slength (str)
		(cond
		  ((equal str "node")
		   node-name-max)
		  ((equal str "name")
		   rule-name-max)
		  (t (length str))))
	  
	  (let ((full-col-info-list
			 (mapcar (lambda (x) (apply #'make-col-info x))
					 `(
					   ("node"				"~a~vt"		,(lambda (e) (rule-stats-entry-rule-node e)))
					   ("name"				"|~a~vt"	,(lambda (e) (rule-stats-entry-rule-name e)))
					   ("tested"			"~a~vt"		,(lambda (e) (rule-stats-entry-tested e)))
					   ("matched"			"~a~vt"		,(lambda (e) (rule-stats-entry-matched e)))
					   ("new-e"				"~a~vt"		,(lambda (e) (rule-stats-entry-new-edges e)))
					   ("not-new-e"			"~a~vt"		,(lambda (e) (rule-stats-entry-not-new-edges e)))
					   ("failed"			"~a~vt"		,(lambda (e) (rule-stats-entry-failed e)))
					   ("max-expand-len"	"~a~vt"		,(lambda (e) (rule-stats-entry-new-edges-max-expand-len e)))
					   ("max-env-size"		"~a~vt"		,(lambda (e) (rule-stats-entry-max-env-size e)))
					   ("max-edges-added"	"~a~vt"		,(lambda (e) (rule-stats-entry-max-edges-added e)))
					   ("max-root-vars"		"~a~vt"		,(lambda (e) (length (rule-stats-entry-root-vars e))))
					   ("efficiency%"		"~,2f~vt"	,(lambda (e) (div (* (float (rule-stats-entry-new-edges e)) 100) (float (rule-stats-entry-tested e)))))
					   ("redundancy%"		"~,2f~vt"	,(lambda (e) (div (* (float (rule-stats-entry-not-new-edges e)) 100) (float (rule-stats-entry-tested e)))))
					   ("failure%"			"~,2f~vt"	,(lambda (e) (div (* (float (rule-stats-entry-failed e)) 100) (float (rule-stats-entry-tested e)))))
					   ))))

		(defm get-entry (rule-node)
		  (let ((entry (! (rule-stats-table lookup-one) rule-node)))
			(if (null entry)
				(let ()
				  (setq entry (make-rule-stats-entry :rule-node rule-node :rule-name (! (graph hget) rule-node 'name)))
				  (! (rule-stats-table insert) rule-node entry))
				(when (null (rule-stats-entry-rule-name entry))		;; Race condition possible eg via update-root-var so we should be sure name gets filled in
				  (setf (rule-stats-entry-rule-name entry) (! (graph hget) rule-node 'name))))
			entry))

		;; Command-line hack, but nice we can do this. rslambda is a misnomer, since it takes no var list and instead
		;; expands to a lambda list with all column names included. Avoids extra typing on the cmd line.
		;; Example:
		;;		(! (g rule-stats)
		;;				:sort 4
		;;				:filter (rslambda (and (= max-expand-len 0) (< max-env-size 4)))
		;;				:columns '(name tested new-e max-env-size max-expand-len efficiency%))

		(defmacro rslambda (e)
		  (let ((col-names (mapcar (lambda (col) (intern (string-upcase (col-info-name col)))) full-col-info-list)))
			`(lambda ,col-names (declare (ignorable ,@col-names)) ,e)))

		(defm rule-stats (&key (sort 1) (columns t) (filter nil))
		  (defr

			(defl col-info-subset ()
			  (mapcad (lambda (col)
						(when (smember (col-info-name col) columns)
						  col))
					  full-col-info-list))

			(defl set-tabs (col-info-list)
			  (mapc (lambda (col tab) (setf (col-info-tab col) tab))
					col-info-list
					(accum (mapcar (lambda (col) (+ (slength (col-info-name col)) 1)) col-info-list))))

			(defl print-header (col-info-list)
			  (format t "~%")
			  (let ((i 0))
				(dolist (col col-info-list)
				  (format t "~a~vt" i (col-info-tab col))
				  (setq i (+ i 1))))
			  (format t "~%")
			  (dolist (col col-info-list)
				(format t "~a~vt" (col-info-name col) (col-info-tab col))))
			
			(defl print-info (stats-entry col-info-list)
			  (format t "~%")
			  (dolist (col col-info-list)
				(let ((val (funcall (col-info-accessor col) stats-entry)))
				  (let ((fmt (col-info-fmt col)))
					(let ((tab (col-info-tab col)))
					  (format t fmt val tab))))))
			
			(defl get-rules () ;; Here, we're doing a query "by hand" to avoid adding more stats while we're processing them.
			  (let ((pat '(?x type rule)))
				(let ((qet (rest pat)))
				  (let ((rules (mapcad (lambda (edge)
										 (env-lookup '?x (! (graph match-one-edge) pat edge nil nil nil) :idempotent nil))
									   (! (g get-edges-from-subqet) qet))))
					rules))))

			(defl get-accessor (col-name-or-num) ;; Inefficient -- use only for one-offs like getting sort accessor
			  (if (numberp col-name-or-num)
				  (col-info-accessor (nth col-name-or-num (col-info-subset)))
				  (block b
					(dolist (col-info full-col-info-list)
					  (when (sequal (col-info-name col-info) col-name-or-num)
						(return-from b (col-info-accessor col-info)))))))

			;; For filter fcn
			(defl get-accessor-fcns (col-names)
			  (let ((col-names (mapcar (lambda (col-name) (string-downcase (symbol-name col-name))) col-names)))
				(mapcad (lambda (col-info)
						  (when (member (col-info-name col-info) col-names :test #'equal)
							(col-info-accessor col-info)))
						full-col-info-list)))
			;;
			;; This uses a clisp-specific function, system::closure-lambda-list, to get the lambda-list from a
			;; function. But the function must be compiled. While we want to compile anyway, for max perf, it's too bad
			;; to be constrained, and we're not portable. Will need redesign if want to port. There is also an advantage
			;; to being able to use a lambda-expr directly, since then any immediate env is captured. Doing so portably
			;; without the lambda-list access probably requires inventing a macro like hlambda or something.
			;;
			;; Would it help just to define lambda-params for all cols? Not clear where lexical lambda-eval would occur.
			;;
			(defl compile-filter ()
			  (setf (symbol-function 'rule-stats-filter) filter)
			  (compile 'rule-stats-filter)
			  (let ((lambda-list (system::closure-lambda-list (symbol-function 'rule-stats-filter))))
				(let ((accessor-fcns (get-accessor-fcns lambda-list)))
				  (let ((accessor-expr (mapcar (lambda (accessor-fcn) `(funcall ,accessor-fcn entry)) accessor-fcns)))
					(let ((filter-fcn
						   `(lambda (entry) (funcall ,(symbol-function 'rule-stats-filter) ,@accessor-expr))))
					  filter-fcn)))))

			(let ((sort-colno sort))
			  (let ((filter-fcn
					 (when filter
					   (compile-filter))))
				(let ((stats-entry-list
					   (mapcad (lambda (x)
								 (setq rule-name-max (max rule-name-max (length (symbol-name (! (graph hget) x 'name)))))
								 (setq node-name-max (max node-name-max (length (symbol-name x))))
								 (let ((e (get-entry x)))
								   (let ((admit (or (null filter-fcn)
													(eval `(,filter-fcn ,e)))))
									 (when admit
									   e))))
							   (get-rules))))
				  (let ((stats-entry-list
						 (let ((accessor (get-accessor sort-colno)))
						   (sort stats-entry-list (lambda (x y)
													(let ((x (funcall accessor x))
														  (y (funcall accessor y)))
													  (if (and (symbolp x) (symbolp y))
														  (symbol< x y)
														  (> x y))))))))
					(let ((col-info-list (col-info-subset)))
					  (set-tabs col-info-list)
					  (print-header col-info-list)
					  (dolist (stats-entry stats-entry-list)
						(print-info stats-entry col-info-list))
					  nil)))))))

		(defm get-rule-stats-table ()	;; For debug
		  rule-stats-table)

		(defm get-all-rules-tested ()
		  all-rules-tested)

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
		  ($comment (log-stat 'expand len))
		  (let ((entry (get-entry rule-node)))
			(setf (rule-stats-entry-last-expand-len entry) (max (rule-stats-entry-last-expand-len entry) len))
			nil))

		(defm update-new-edges-max-expand-len (rule-node)
		  (let ((entry (get-entry rule-node)))
			(setf (rule-stats-entry-new-edges-max-expand-len entry) (max (rule-stats-entry-new-edges-max-expand-len entry) (rule-stats-entry-last-expand-len entry)))
			($comment (log-stat 'expand-max (rule-stats-entry-new-edges-max-expand-len entry)))
			nil))

		(defm update-max-env-size (rule-node env-size)
		  (let ((entry (get-entry rule-node)))
			(setf (rule-stats-entry-max-env-size entry) (max (rule-stats-entry-max-env-size entry) env-size))
			($comment (log-stat 'max-env-size (rule-stats-entry-new-edges-max-env-size entry)))
			nil))

		(defm update-max-edges-added (rule-node n-edges-added)
		  (let ((entry (get-entry rule-node)))
			(setf (rule-stats-entry-max-edges-added entry) (max (rule-stats-entry-max-edges-added entry) n-edges-added))
			($comment (log-stat 'max-edges-added (rule-stats-entry-new-edges-max-edges-added entry)))
			nil))

		(defm update-root-var (rule-node root-var)
		  (let ((entry (get-entry rule-node)))
			(let ((root-vars (rule-stats-entry-root-vars entry)))
			  (when (not (memq root-var root-vars))
				(setf (rule-stats-entry-root-vars entry) (cons root-var root-vars)))
			  nil)))

		))))

;; Belongs in subst-match, moved outside for perf.

(defstruct k
  pred
  pred-info			   ;; Entry per pred-node, d => data, v => var. Update after subst.
  orig-pred)		   ;; Just for debugging right now

;; We had this as a subclass of objgraph, but for perf reasons we'd like to stick with graph. So avoid hget and other
;; shortcuts and use the qet system directly. Also moved get-rule-children into here.

;; We cache creation of a rulegraph, so most operations here can be done once and saved, with no cache clearing
;; required.

(defc rulegraph graph (rule-node rule-name root-var)
  (let ()

	(defm rule-node ()
	  rule-node)

	(defm name ()
	  rule-name)

	(defm get-preds ()
	  (get-all-edges))

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
		(let ((pred-status (make-hash-table :test #'equal))) ;; :matched, :unmatched, :not-tested)
		  (let ((doloop-cnt 0))
			(defm subst-match (objgraph obj-node root-var &key (rule-name (name))) ;; rule-name arg for tracing purposes
			  (macrolet ((xprint (tag &rest x)
								 nil
								 ;; `(ptag ,tag ,@x)
						   ))
				(timer 'subst-match
				  (lambda ()
					(let ((g objgraph))
					  (let ((has-single-const-preds (! (self has-single-const-preds))))
						(let ((use-singleton-qets (and t ;; has-single-const-preds  ;; enable-subst-match
													   (not (is-var-name root-var)))))
						  (xprint 's18 has-single-const-preds (! (self has-single-const-preds)) (not (is-var-name root-var)) use-singleton-qets)
						  (defr
							(defl init-pred-status (ks)
							  (dolist (k ks)
								(setf (gethash (k-orig-pred k) pred-status) (list :untested))))
							(defl print-pred-status (tag)
							  (print (list tag))
							  (maphash (lambda (k v)
										 (print (list tag obj-node root-var rule-name k v)))
									   pred-status))
							(defl make-ks ()
							  (let ((preds (second (! (g filter-new-node-pred-edges) (get-preds)))))
								(mapcar (lambda (pred)
										  (make-k :pred pred 
												  :pred-info (mapcar (lambda (node) (if (is-var-name node) 'v 'd)) pred)
												  :orig-pred pred))
										preds)))
							(defl get-new-node-env ()
							  (let ((new-node-preds (first (! (g filter-new-node-pred-edges) (get-preds)))))
								(mapcar (lambda (new-node-pred) (list (first new-node-pred) (third new-node-pred))) new-node-preds))) ;; dump-sn
							;; Splits l at vars and returns a list of subseqs at those var boundaries
							;; E.g. (filter-vars-to-qets '(1 2 ?x 3 4 ?y 5 6 ?z)) => ((1 2) (3 4) (5 6))
							;;
							;; !! Fcn name and logic cloned in possible-match-fcn !!
							(defl filter-vars-to-qets (pred pred-info)
							  (defr
								(defl is-var (node node-info)
								  (and (is-var-name node)
									   (eq node-info 'v)))
								(defl doloop (l i c r)
								  (if (null l)
									  (append r (when c (list c)))
									  (if (is-var (first l) (first i))
										  (doloop (rest l) (rest i) nil (append r (when c (list c))))
										  (doloop (rest l) (rest i) (append c (list (first l))) r))))
								(doloop pred pred-info  nil nil)))
							(defl check-consts (ks) ;; T if all preds have only consts, no vars
							  (block b
								(dolist (k ks)
								  (when (memq 'v (k-pred-info k))
									(return-from b nil)))
								t))
							(defl check-const (k)
							  (not (memq 'v (k-pred-info k))))
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
											(let ((len nil))
											  (if (> (length qets) 1) ;; If more than one qet cannot just add; need to find size of union
												  (let ()
													(setq len (! (qet-edge-len-cache lookup-one) qets))
													(when (null len)
													  (let ((new-edges (mapunion (lambda (qet)
																				   (when (or use-singleton-qets (> (length qet) 1))
																					 (! (g get-edges-from-subqet) qet))) qets)))
														(when new-edges
														  (setq len (length new-edges))
														  (! (qet-edge-len-cache insert-one) qets len)))))
												  (let ((qet (first qets))) ;; One or zero qets
													(when qet
													  (setq len (! (g count-edges-from-subqet) qet)))))
											  (when (and len (< len n))
												(setq n len)
												(setq min-k k))))))))
								  (xprint 's11 use-singleton-qets min-k)
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
											(let ((new-edges (mapunion (lambda (qet)
																		 (when (or use-singleton-qets (> (length qet) 1))
																		   (! (g get-edges-from-subqet) qet))) qets)))
											  (xprint 's16 qets new-edges)
											  (let ((r (mapcad (lambda (edge)
																 (find-var-binding (! (g match-one-edge) pred edge nil nil nil :pred-info pred-info)))
															   new-edges)))
												#|
												(when (eq (gethash (k-orig-pred k) pred-status) :untested) ; ; ;
												(setf (gethash (k-orig-pred k) pred-status) (if r :matched :unmatched))) ; ; ;
												|#
												(setf (gethash (k-orig-pred k) pred-status) (append (gethash (k-orig-pred k) pred-status) (list (if r :matched :unmatched))))
												(xprint 's9 pred new-edges r)
												r))))))))))
							(defl edges-exist (ks) ;; All pred edges need to exist
							  (block b
								(dolist (k ks)
								  (let ((pred (k-pred k)))
									(when (not (! (g edge-exists) pred))
									  (return-from b nil))))
								t))
							(defl doloop (lvl cnt ks env-chain)
							  (xprint 's1 ks lvl cnt doloop-cnt)
							  (xprint 's14 env-chain)
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
									;; (gstat 'match-num-envs (lambda (x y) (+ x y)) (lambda () (length new-envs)))
									(let ((i 0))
									  (dolist (new-env new-envs)
										(let ((ks (subst ks new-env)))
										  (doloop (+ lvl 1) i ks (append new-env env-chain))
										  (setq i (+ i 1))))))))
							(let ((env `((,root-var ,obj-node))))
							  (xprint 's0 rule-name root-var obj-node)
							  (xprint 's17 (! (self has-single-const-preds)) (not (is-var-name root-var)) use-singleton-qets)
							  (! (qet-edge-len-cache clear)) ;; !!!!!!!!!!!! Clearing cache
							  (clrhash pred-status)
							  (setq doloop-cnt 0)
							  (let ((global-ks (make-ks)))
								(let ((ks (make-ks)))
								  (init-pred-status ks)
								  (let ((ks (subst ks env)))
									(clrhash envs)
									(doloop 0 0 ks env)
									(xprint 's16 doloop-cnt)
									;; (gstat 'match-doloop-cnt (lambda (x y) (+ x y)) (lambda () doloop-cnt))
									(let ((new-node-env (get-new-node-env)))
									  (let ((envs-list (hash-table-value-to-list envs)))
										(when (null envs-list)
										  (let ((fail-tag 'subst-match-fail))
											(when (chkptag fail-tag)
											  (print-pred-status fail-tag))))
										(mapcar (lambda (env) (append env new-node-env)) envs-list)))))))))))))))))))

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
				   (let ((rule-edges-info (filter-new-node-pred-edges (get-preds))))
					 (let ((new-node-rule-edges (first rule-edges-info)))
					   (let ((other-rule-edges (second rule-edges-info)))
						 (let ((new-node-rule-edge-nodes (nodes new-node-rule-edges)))
						   (let ((other-rule-edge-nodes (nodes other-rule-edges)))
							 (set-subtract other-rule-edge-nodes new-node-rule-edge-nodes)))))))))
		r))

	;; For testing -- assessing numbers of vars in rules (in the preds)

	(defm get-vars ()
	  (let ((rule-edges-info (filter-new-node-pred-edges (get-preds))))
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
								   (let ((all-edges (second (filter-new-node-pred-edges (get-preds)))))
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
					   (let ((preds (second (! (g filter-new-node-pred-edges) (get-preds)))))
						 (dolist (pred preds)
						   (when (> (length (filter-vars pred)) 1)
							 (return-from b nil))))
					   t)))
			  (first has-single-const-preds-cache)))))

	;; This is also a candidate for caching, using the signature
	(defm has-rest-vars ()
	  (block b (mapc (lambda (pred) (when (! (g has-rest-var) pred) (return-from b t))) (get-preds)) nil))

	;; T if any pred has at least two consecutive vars
	(let ((has-seq-vars-cache :undefined))
	  (defm has-seq-vars ()
		(if (not (eq has-seq-vars-cache :undefined))
			has-seq-vars-cache
			(setq has-seq-vars-cache
				  (block b
					(defr
						(defl doloop (pred cnt)
						  (cond
							((= cnt 2)
							 (return-from b t))
							((null pred)
							 nil)
							((is-var-name (first pred))
							 (doloop (rest pred) (+ cnt 1)))
							(t
							 (doloop (rest pred) 0))))
						(let ()
						  (dolist (pred (get-preds ))
							(doloop pred 0))
						  nil)))))))

	(defm get-pred-consts ()
	  (let ((r nil))
		(dolist (pred (get-preds))
		  (when (not (eq (second pred) 'new-node))
			(dolist (node pred)
			  (when (not (is-var-name node))
				(setq r (cons node r))))))
		(dedup-list r)))
	
	))

(defc rulegraph-adds objgraph (graph rule-node)
  (let ()
	(defm init ()
	  (let ((add-edges (! ((! (graph get-rule-components) rule-node) adds))))
		;; auto-add-gnlrp
		(rem-edge '(global-node local-rule-pool local-rule-pool-node))
		;; (rem-edge '(global-node global-rule-pool-ref global-rule-pool-node))
		(dolist (add-edge add-edges)
		  (add-edge add-edge))))))


;; 5/9/23 Finally discovered a clisp method to define a test and hash fcn for a hash table. So we do that below and can
;; use CL hash tables. Though this does not seem to be in the CL std, sbcl has a variant (of the same name, slightly
;; different semantics), but abcl does not.
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
		(mod (+ (sxhash (env-trig-entry-rule-node e))
				(env-hash (env-trig-entry-env e))
				(env-trig-entry-adds-hash e))
			 most-positive-fixnum))
	  (let ((hash-test-name (hdefine-hash-table-test #'env-trig-entry-equal #'env-trig-entry-hash)))
		(let ((env-triggered-table (make-hash-table :test hash-test-name)))
		  (let ((lte (make-env-trig-entry)))
			(let ((graph nil))

			  (defm set-graph (g)
				(setq graph g))

			  (defm hash-adds (rule-node) ;; !!!!!!!!!!! NOP !!!!!!!!!!!
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
				(env-prune env (cons 't (! (std-vars var-cache)))))

			  (defm insert (rule-node env)
				(timer 'env-triggered-insert
				  (lambda ()
					(let ((env (env-remove-bindings env)))
					  (let ((te (lookup rule-node env)))
						(when (null te)
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
					  r))))
			
			  (defm as-list () ;; For debug only
				(list env-triggered-table))

			  (defm clear () ;; For debug only
				(clrhash env-triggered-table)
				nil)

			  (defm rule-has-been-triggered (rule-node env)
				(timer 'rule-has-been-triggered
				  (lambda ()
					(and (lookup rule-node env) t)))))))))))

;; General surjective map, for many-to-one mappings. Default test for
;; input and result sets is equal.

(defc sur-map nil (&key (input-test #'equal) (res-test #'equal) (input-size 1021) (res-size 17))
  (let ((input-hash (make-hash-table :test input-test :size input-size)))
	(defm lookup (input &key check-existence-only)
	  (let ((r (gethash input input-hash)))
		(or (and check-existence-only (not (null r)))
			(and r (hash-table-value-to-list r)))))
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
  (defun new-cross-aux2 (envs-list &key (record-lengths t) rule-trace-info)
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

(defc foundation base-graph (&key (nat 20))
  (let ()
	(defm init ()
	  (base-graph-init)
	  (add-natural-number-edges nat)
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

(defc fft-test foundation nil
  (let ()
	(defm init ()
	  (clear-counters)
	  (clear-perf-stats)
	  (foundation-init)
	  (read-rule-file "fft.lisp")
	  (read-rule-file "tree.lisp")
	  (read-rule-file "rule30.lisp")
	  (read-rule-file "fft-delta.lisp")
	  )
	(defm run (fft-n &key (rule-30-levels fft-n) (rule-mode :local-global))
	  (define-rule `(rule
					 (name init)
					 (attach-to global-node)
					 (pred
					  (global-node rule ?r)
					  (?r name init))
					 (add
					  (print init)
					  (r level ,rule-30-levels)
					  (r rule-30-top)
					  (tree-rule x ,fft-n)
					  (x fft-top)
					  (x fft xfft)
					  (x level ,fft-n)
					  (x color navajowhite)
					  (x rand r)
					  (x rule ,(! (g query) '((?x name fft-rule)) '?x))
					  (x local-rule-pool local-rule-pool-node)
					  (r local-rule-pool local-rule-pool-node)
					  (queue x r)
					  )
					 (del
					  (global-node rule ?this-rule))))
	  (add-natural-number-edges (max fft-n rule-30-levels))
	  (timer 'main
		(lambda ()
		  (execute-global-all-objs-loop))))))

;; Test of just fft -- no rule-30, deltas, etc.

(defc pure-fft-test foundation nil
  (let ()
	(defm init ()
	  (clear-counters)
	  (clear-perf-stats)
	  (foundation-init)
	  (read-rule-file "fft.lisp")
	  (read-rule-file "tree.lisp")
	  )
	(defm run (fft-n &key (rule-mode :local-global))
	  (define-rule `(rule
					 (name init)
					 (attach-to global-node)
					 (pred
					  (global-node rule ?r)
					  (?r name init))
					 (add
					  (print init)
					  (tree-rule x ,fft-n)
					  (x fft-top)
					  (x fft xfft)
					  (x level ,fft-n)
					  (x rule ,(! (g query) '((?x name fft-rule)) '?x))
					  (x local-rule-pool local-rule-pool-node)
					  (queue x)
					  )
					 (del
					  (global-node rule ?this-rule))))
	  (add-natural-number-edges fft-n)
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
;; eval: (emacs-file-locals)
;; End:


