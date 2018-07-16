(defsetf hgethash hputhash)		;; Needs to be defined early for macro expansion purposes

#|
(defc x-graph nil nil
  (let ((nodelist (make-sur-map)))))

(defc x-graph nil nil
  (let ((nodelist (make-x-sur-map)))))
|#

(defc graph x-graph nil
  (let (
		;; (nodelist (make-sur-map))
		(edgelist (make-hash-table :test #'equal :size 32768))
		(nodeposlist (make-hash-table :test #'equal :size 32768))

		(subqet-map (make-sur-map :res-size 16))
		(superqet-map (make-sur-map :res-size 16))

		(qu (make-qet-utils))
		)

	(defm add-edge (edge)
	  (graph-add-edge edge))

	(defm graph-add-edge (edge)
	  (let ((edge (or (gethash edge edgelist)
					  (setf (gethash edge edgelist) edge))))
		(dotimes (i (length edge))
		  (add-node (nth i edge) i edge))
		(add-subqets edge)
		edge))

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
			#|
			(let ((r (! (nodelist lookup) node))
				  (s (! (x-nodelist lookup) node)))
			  (when (not (set-equal r s))
				(print (list 'ne r s)))
			  r)
			|#
			(let ((r1 (! (nodelist lookup) node)))
			  ;; (let ((r2 (sort r1 #'g<)))
			  ;; r2))
			  r1)
			)))

	;; This is used with the cross-intersect evaluation model, which
	;; is slow but has nice theory. 

	(defm get-edges-from-subqet (subqet)
	  (timer 'get-edges-from-subqet
		(lambda ()
		  (let ((map (make-hash-table :test #'equal :size 256)))
			(defr
			  (defl g (qet)
				(when (edge-exists qet)
				  (setf (gethash qet map) qet))
				(let ((sup (superqets qet)))
				  (if (null sup)
					  nil
					  (dolist (qet sup)
						(g qet)))))
			  (g subqet))
			(let ((edges (hash-table-value-to-list map)))
			  edges)))))

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

	(defm all-subqets () ;; For test
	  (! (superqet-map as-list)))

	;; Private:

	(defm add-subqets (edge)
	  (defr
		(defl add-layer (qets sub-qets)
		  (when (not (null sub-qets))
			(when (not (null qets))
			  (dolist (qet qets)
				(dolist (sub-qet sub-qets)
				  (when (! (qu is-subqet) sub-qet qet)
					(add-subqet sub-qet qet)))))
			(add-layer sub-qets (! (qu subqets-length-n) edge (- (length (first sub-qets)) 1)))))
		(when t ;; (<= (length edge) 4)
		  (add-layer nil (list edge)))
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

	(defm dump-edges (&key (sort t) (dump-fcn #'print))
	  (if (not sort)
		  (maphash (lambda (k v)
					 (funcall dump-fcn v))
				   edgelist)
		  (let ((edges nil))
			(maphash (lambda (k v)
					   (setq edges (cons v edges)))
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

	(defm scan-edgelist (fcn)
	  (maphash (lambda (k v)
				 (funcall fcn v))
			   edgelist)
	  nil)

	(defm get-all-nodes ()
	  (! (nodelist inputs)))

	(defm get-all-edges ()
	  (let ((edge-hash edgelist))
		(let ((r nil))
		  (maphash (lambda (k v)
					 (setq r (cons k r)))
				   edge-hash)
		  r)))))

(defc xgraph graph nil
  (let ((a 1))
	(defm m1 (x) (+ a x))
	(defm m2 ())
	(defm get-all-nodes ()
	  "Ha ha!!!")))

(defc rule-components nil nil
  (let ((var-list nil)
		(pred-list nil)
		(del-list nil)
		(add-list nil)
		(not-list nil))
	(defm set-components (var pred del add not)
	  (setq var-list var)
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
	  (list var-list pred-list del-list add-list not-list))
	(defm vars ()
	  var-list)
	(defm preds ()
	  pred-list)
	(defm dels ()
	  del-list)
	(defm adds ()
	  add-list)
	(defm nots ()
	  not-list)))

(defstruct queue-entry
  (prev nil)		;; toward tail
  (next nil)		;; toward head
  (value nil))

(defc queue nil nil
  (let ((qhash  (make-hash-table :test #'equal))
		(head nil)
		(tail nil))

	;; public:

	(defm push-tail (x)
	  (let ((entry (make-queue-entry :value x))
			(entry-hash (gethash x qhash)))
		(when (null entry-hash)
		  (setq entry-hash (make-hash-table :test #'eq))
		  (setf (gethash x qhash) entry-hash))
		(setf (gethash entry entry-hash) entry)
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
		  (let ((entry-hash (gethash r qhash)))
			(remhash tail entry-hash)
			(remove-one tail))
		  r)))

	(defm push-head (x)
	  (let ((entry (make-queue-entry :value x))
			(entry-hash (gethash x qhash)))
		(when (null entry-hash)
		  (setq entry-hash (make-hash-table :test #'eq))
		  (setf (gethash x qhash) entry-hash))
		(setf (gethash entry entry-hash) entry)
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
		  (let ((entry-hash (gethash r qhash)))
			(remhash head entry-hash)
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
		  (clrhash entry-hash)))
	  nil)

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
	  (let ((entry-hash (gethash x qhash)))
		(and entry-hash (> (hash-table-count entry-hash) 0) t)))

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


(defc infer-root-var-cache nil nil
  (let ((cache (make-hash-table :test #'equal)))

	(defm get (rule-node)
	  (gethash rule-node cache))

	(defm insert (rule-node var-node)
	  (setf (gethash rule-node cache) var-node))))

(defc status-graph graph nil
  (let ()
	(defm add-edge (edge)
	  (status-graph-add-edge edge))
	(defm status-graph-add-edge (edge)
	  (graph-add-edge edge))))

(defc objgraph graph nil
  (let ((objnodeseq 0)
		(newpoolnodeseq 0)
		(obj-queue (make-queue))
		(global-node 'global-node)
		(global-rule-pool 'global-rule-pool-node)
		(local-rule-pool 'local-rule-pool-node)
		(env-triggered-table (make-env-triggered))
		(edge-cache (make-edge-cache))
		(infer-root-var-cache (make-infer-root-var-cache))
		(rule-status-seq-no 0)
		(rule-status-graph nil)		;; An objgraph, set later 
		(hget-key-rest1 (list nil nil))
		(hget-sup-rest1 (list nil nil)))
	(let ((hget-key-rest2 (rest hget-key-rest1))
		  (hget-sup-rest2 (rest hget-sup-rest1))
	  
		  #|
		  ;; ********************
		  
		  ;; This table is populated upon entry of an edge. We take
		  ;; subsets(edge) and map those to the pred, via subset-to-rule-edges,
		  ;; which is populated in define-rule

		  (edge-to-rule-edges (make-sur-map))
		  (subset-to-rule-edges (make-sur-map))

		  (rule-edge-to-rule-node (make-sur-map))


		  (rule-edge-to-raw-edge (make-sur-map))

		  (pred-edge-to-overlap-pred-edges (make-sur-map))
		  (pred-edge-to-cand-edges (make-sur-map)) ;; Full set of edges after filtering. More than one pred-edge can map to a set of cand-edges ; ; ; ;

		  (pred-edge-to-subset (make-sur-map)) ;; Just rem-var

		  ;; ********************
		  |#

		  (elem-attrs '(elem0 elem1 elem2 elem3 elem4 elem5 elem6 elem7 elem8 elem9
							  elem10 elem11 elem12 elem13 elem14 elem15 elem16 elem17 elem18 elem19)))

	  (defm objgraph-init ()
		(! (env-triggered-table set-graph) self)
		(addraw global-node 'local-rule-pool local-rule-pool)
		(addraw global-node 'global-rule-pool-ref global-rule-pool)
		nil)

	  (defm init ()
		(objgraph-init))

	  (defm get-elem-attrs ()
		elem-attrs)

	  (defm get-edge-cache ()
		edge-cache)

	  (defm get-rule-status-graph ()
		rule-status-graph)

	  (defm rule-status-add-edge (edge)		;; Adds edge for the case of this obj being a rule status graph
		(add-edge edge))

	  (defm rule-status-graph-add-edge (edge)		;; Adds edge to the rule-status-graph member
		(and rule-status-graph (! (rule-status-graph rule-status-add-edge) edge)))

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
			  (! (obj-queue push-tail) n)
			  (queue-info (list 'add n (hget n 'name) (get-queue))))
			(let ((e node-or-edge))
			  (let ((n (first e)))
				(add-edge e)
				(! (obj-queue push-tail) n)
				(queue-info (list 'add n (hget n 'name) (get-queue)))))))

	  (defm del (node-or-edge &optional a v)
		(if (is-node node-or-edge)
			(let ((n node-or-edge))
			  (delraw n a v)
			  (! (obj-queue push-tail) n)
			  (queue-info (list 'del (hget n 'name) (get-queue))))
			(let ((e node-or-edge))
			  (let ((n (first e)))
				(rem-edge e)
				(! (obj-queue push-tail) n)
				(queue-info (list 'del(hget n 'name) (get-queue)))))))

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

	  (defm define-obj (props &key (add-rule-link t) (new-pool 0))
		(let ((node (new-obj-node :new-pool new-pool)))
		  (dolist (prop props)
			(add-edge (cons node prop)))
		  (when add-rule-link
			(addraw node 'global-rule-pool global-rule-pool))
		  (addraw node 'local-rule-pool local-rule-pool)
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

	  (defm queue-node (n &key only-if-not-queued head)
		(if (and only-if-not-queued
				 (node-queued n))
			nil
			(let ()
			  (if head 
				  (! (obj-queue push-head) n)
				  (! (obj-queue push-tail) n))
			  (queue-info (list 'queue-node n (hget n 'name) (get-queue))))))

	  (defm queue-thunk (thunk)
		(! (obj-queue push-tail) thunk)
		(queue-info (list 'queue-thunk thunk (hget n 'name) (get-queue)))
		nil)

	  (defm get-queue () ;; debug fcn
		obj-queue)

	  (defm clear-queue ()
		(setq obj-queue (make-queue)))

	  (defm execute-queue (&key (once nil) (local-rules-only nil))
		(block exq
		  (loop
		   ;; Interesting experiment, i.e., executing as a stack instead
		   ;; of a queue.  FFT worked, even a bit faster. Number of
		   ;; exec-all passes 2 instead of three. However seemed to be
		   ;; more dup of rule execs.
		   ;; (let ((obj (! (obj-queue pop-tail))))
		   (let ((obj (! (obj-queue pop-head))))
			 (if obj
				 (if (functionp obj)
					 (funcall obj)
					 (let ((m (execute-obj obj :local-rules-only local-rules-only)))
					   (when once
						 (return-from exq nil))
					   (when m 
						 (queue-node obj))))
				 (return-from exq nil))))))

	  (defm execute-obj (node &key local-rules-only)
		(let ((r nil))
		  ;; (print `(*** begin exec ,node))
		  (let ((rules (hget-all node 'rule)))
			(dolist (rule rules)
			  (let ((m (match-and-execute-rule rule node)))
				(when m
				  (setq r t)))))
		  (when (not local-rules-only)
			(let ((global-rule-pool (hget node 'global-rule-pool)))
			  (when global-rule-pool
				(let ((global-rules (dedup-rules (hget-all global-rule-pool 'grp-rule))))
				  (dolist (rule global-rules)
					(let ((m (match-and-execute-rule rule node)))
					  (when m
						(setq r t))))))))
		  ;; (print `(*** end exec ,node))
		  r))

	  (defm execute-all-objs (&key local-rules-only)
		(timer 'execute-all-objs
		  (lambda ()
			(let ((nodes (get-all-nodes))
				  (r nil))
			  (dolist (node nodes)
				(let ((m (execute-obj node :local-rules-only local-rules-only)))
				  (when m
					(setq r t))))
			  r))))

	  ;; Repeated execute all objs, then execute the queue, until no obj runs in the all-obj scan

	  (defm execute-all-objs-loop (&key local-rules-only)
		(block xxx
		  (let ((i 0))
			(loop
			 (let ((nodes (get-all-nodes))
				   (r nil))
			   (print `(execute-all-objs-loop execute-queue start ,i))
			   (execute-queue :local-rules-only local-rules-only)
			   (print `(execute-all-objs-loop execute-queue end ,i))
			   (dolist (node nodes)
				 (let ((m (execute-obj node :local-rules-only local-rules-only)))
				   (when m
					 (setq r t))))
			   (setq i (+ i 1))
			   (when (not r)
				 (return-from xxx nil))))))
		nil)

	  ;; As above, but alternate between executing queue and executing the global-node

	  (defm execute-global-loop (&key local-rules-only)
		(block egl
		  (let ((i 0)
				(nedges 0)
				(prev-nedges 0))
			(loop
			 (execute-queue :local-rules-only local-rules-only)
			 (execute-obj 'global-node)
			 (setq nedges (length (get-all-edges)))
			 (when (= prev-nedges nedges)
			   (return-from egl nil))
			 (setq prev-nedges nedges))))
		nil)

	  (defm execute-global-all-objs-loop (&key local-rules-only)
		(block egl
		  (let ((i 0)
				(nedges 0)
				(prev-nedges 0))
			(defr
			  (defl log1 (name)
				(format t "~%***~a ~a~30t~a" name i (date-time)))
			  (loop
			   (log1 'start-queue)
			   (execute-queue :local-rules-only local-rules-only)
			   (log1 'end-queue)
			   (log1 'start-global-node)
			   (execute-obj 'global-node)
			   (log1 'end-global-node)
			   (setq nedges (length (get-all-edges)))
			   (print `(***nedges 1 ,nedges))
			   (when (= prev-nedges nedges)
				 (log1 'start-exec-all)
				 (execute-all-objs :local-rules-only #| t |# local-rules-only)  ;; !!!!!!!   Only does local rules 
				 (log1 'end-exec-all)
				 (setq nedges (length (get-all-edges)))
				 (print `(***nedges 2 ,nedges))
				 (when (= prev-nedges nedges)
				   (return-from egl nil)))
			   (print `(***nedges 3 ,nedges))
			   (setq prev-nedges nedges)
			   (setq i (+ i 1)))
			  nil))))

	  ;; If var is supplied, finds unique value in returned envs

	  (defm query (clauses &optional var)
		(let ((envslist (query1 clauses)))
		  (if var
			  (env-lookup var (first (first envslist)))
			  (dedup-list (mapcan (lambda (envlist)
									(mapcar (lambda (env)
											  (env-prune env '(?this-rule ?this-rule-name ?this-obj ?root-var)))
											envlist))
								  envslist)))))

	  (defm query1 (clauses)
		(let ((rule (first (define-rule `(rule (pred ,@clauses)) :local t :add-to-local-rule-pool nil))))
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
			  (! (og query2) clauses)))))

	  (defm query2 (clauses)
		(let ((rule (first (define-rule `(rule (pred ,@clauses)) :local t :add-to-local-rule-pool nil))))
		  (dedup-list (mapcar (lambda (node)
								(all-matches rule node))
							  (get-all-nodes)))))

	  ;; All variants of hget call the main hget-aux below

	  (defm hget (node attr)
		(hget-aux node attr nil nil nil))

	  ;; list of all values with edge label attr

	  (defm hget-all (node attr)
		(hget-aux node attr nil nil t))

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

	  #|
	  (defm hget-aux (node attr inverse edge all)		;; private ; ; ;
		(timer 'hget-aux
		  (lambda ()
			(block hg
			  (when node
				(let ((subqet (if inverse (list attr node) (list node attr))))
				  (let ((sups (superqets subqet)))
					(let ((r nil))
					  (dolist (sup sups)
						(when (and (edge-exists sup) 
								   (= (length sup) 3)
								   (equal subqet (if inverse 
													 (list (second sup) (third sup))
													 (list (first sup) (second sup)))))
						  (if (not all)
							  (return-from hg (if edge sup (third sup)))
							  (setq r (cons (if edge sup (third sup)) r)))))
					  r))))))))
	  |#

	(defm get-rule-components (rule-node)
	  (let ((vars-node (hget rule-node 'vars)))
		(let ((var-list (and vars-node (edge-elem-node-to-list vars-node)))
			  (pred-list (mapcar (lambda (node)
								   (edge-elem-node-to-list node))
								 (hget-all rule-node 'pred)))
			  (del-list (mapcar (lambda (node)
								  (edge-elem-node-to-list node))
								(hget-all rule-node 'del)))
			  (add-list (mapcar (lambda (node)
								  (edge-elem-node-to-list node))
								(hget-all rule-node 'add)))
			  (not-list (mapcar (lambda (node)
								  (edge-elem-node-to-list node))
								(hget-all rule-node 'not))))
		  (let ((rule-components (make-rule-components)))
			(! (rule-components set-components) var-list pred-list del-list add-list not-list)
			rule-components))))

	(defm has-rules (node)
	  (or (hget node 'global-rule-pool)
		  (hget node 'rule)))

	(defm dedup-rules (rules)
	  (dedup-list rules :id-func (lambda (rule)
								   (let ((name (hget rule 'name)))
									 (if name name rule)))))

	(defm matched-edges (rule-pred-edges env)
	  (let ((r nil))
		(dolist (rule-edge rule-pred-edges)
		  (when (not (eq (second rule-edge) 'new-node))
			(let ((e nil))
			  (dolist (rule-node rule-edge)
				(setq e (append e (list (env-lookup rule-node env)))))
			  (setq r (cons e r)))))
		r))

	;; Union of all edge sets produced by matched-edges in each env in envlist

	(defm matched-edges-union (rule-pred-edges envlist)
	  (let ((r nil))
		(dolist (env envlist)
		  (setq r (hunion r (matched-edges rule-pred-edges env))))
		r))

	;; Returns t if any new edges were created, else nil

	(defm add-consequent-edges (obj-node pred-edges edges not-edges rule-node envlist)
	  (timer 'add-consequent-edges
		(lambda ()
		  (let ((r nil))
			(dolist (env envlist)
			  (timer 'add-consequent-edges-per-env
				(lambda ()
				  (let ((all-node-hash (make-hash-table :test #'equal))
						(dont-queue-node-hash (make-hash-table :test #'equal))
						(dont-queue-all nil)
						(queue-node-hash (make-hash-table :test #'equal))
						(queue-tail-node-hash (make-hash-table :test #'equal))
						(matched-edges (matched-edges pred-edges env)))
					(block yyy
					  (let ()
						(if (! (env-triggered-table rule-has-been-triggered) rule-node env)
							(let ()
							  (gstat 'already-env-triggered-rules (lambda (x y) (+ x y)) (lambda () 1))
							  ;; (when (= (length envlist) 1)
							  ;;   (rem-edge `(,obj-node rule ,rule-node)))
							  (return-from yyy nil))
							(let ((te nil))
							  (when (not (edge-exists `(,rule-node no-triggered)))
								(setq te (! (env-triggered-table insert) rule-node env matched-edges)))
							  ;; (when (= (length envlist) 1)
							  ;;    (rem-edge `(,obj-node rule ,rule-node)))
							  (dolist (edge not-edges)
								(let ((not-edge 
									   (mapcar (lambda (node)
												 (env-lookup node env))
											   edge)))
								  (when (edge-exists not-edge)
									(return-from yyy nil))))
							  (let ((new-node-hash (make-hash-table :test #'equal)))
								(dolist (edge edges)
								  (block xxx
									(let ((new-edge nil))
									  (dolist (node edge)
										(let ((xnew-node (let ((sn (env-lookup node env)))
														   (if (and (is-var-name node)
																	(is-scoped-new-pool-node sn))
															   (let ((nn (gethash sn new-node-hash)))
																 (when (null nn)
																   (setq nn (setf (gethash sn new-node-hash) (define-obj nil 
																											   :new-pool 1
																											   :add-rule-link t)))) ;;; was nil
																 nn)
															   sn))))
										  (let ((new-node
												 (cond
												  ((and (is-new-pool-node xnew-node)
														(hget xnew-node 'next-new-node))
												   (hget xnew-node 'next-new-node))
												  ((is-new-pool-node xnew-node)
												   (let ((xxnew-node (gethash xnew-node new-node-hash)))
													 (when (null xxnew-node)
													   (setq xxnew-node (setf (gethash xnew-node new-node-hash) (define-obj nil :add-rule-link t)))) ;;; was nil
													 xxnew-node))
												  (t xnew-node))))
											(setq new-edge (append new-edge (list new-node))))))
									  (cond
									   ((eq (first new-edge) 'dont-queue)
										(if (null (rest new-edge))
											(setq dont-queue-all t)
											(let ((node (second new-edge)))
											  (setf (gethash node dont-queue-node-hash) node))))

									   ((eq (first new-edge) 'queue)
										(let ((node (second new-edge)))
										  (setf (gethash node queue-node-hash) node)))
									   ((eq (first new-edge) 'queue-head)
										(let ((node (second new-edge)))
										  (setf (gethash node queue-node-hash) node)
										  (setf (gethash node queue-tail-node-hash) node)))
									   ((eq (first new-edge) 'exec)
										(let ((rule (second new-edge))
											  (obj (third new-edge)))
										  (let ((thunk (lambda () (match-and-execute-rule rule obj))))
											(setf (gethash thunk queue-node-hash) thunk)))))
									  (when (not (edge-exists new-edge))
										(dolist (new-node new-edge)
										  (setf (gethash new-node all-node-hash) new-node))
										(add-edge new-edge)
										;; (! (edge-cache push) new-edge)
										(cond
										 ((eq (first new-edge) 'print) ;; Hack, mainly for debug -- if print is head node of an edge, print it
										  (print (rest new-edge))
										  )))
									  (when (edge-exists new-edge)
										;; (print (list (hget rule-node 'name) obj-node env 'old))
										)))))))))
					(when (not (= (hash-table-count all-node-hash) 0))
					  (setq r t)
					  ;; (! (edge-cache push-list) matched-edges)
					  ;; (add-consequent-edges-info (list env (hash-table-to-list all-node-hash)))
					  )
					(let ((nodes nil))
					  (maphash (lambda (k v)
								 (let ((node v))
								   (setq nodes (cons node nodes))))
							   (if (not (= (hash-table-count queue-node-hash) 0))
								   queue-node-hash
								   all-node-hash))
					  (dolist (node nodes)
						(when (and (or (functionp node) (has-rules node))
								   (not dont-queue-all)
								   (not (gethash node dont-queue-node-hash)))
						  (let ((head (gethash node queue-tail-node-hash)))
							(queue-node node :only-if-not-queued t :head head)))))))))
			r))))
	
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

	(defm rule-stats (&optional (sort-colno 1))
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
							(query '((?x type rule))))))
		  (let ((info (sort info (lambda (x y) (> (nth sort-colno x) (nth sort-colno y))))))
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
						(nth 6 x)))))))
	  nil)

	(defm update-tested (rule-node)
	  (gstat 'm-and-e-rules-tested (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((tested-count (or (hget rule-node 'tested) 0)))
		(rem-edge `(,rule-node tested ,tested-count))
		(add-edge `(,rule-node tested ,(+ tested-count 1))))
	  nil)

	(defm update-matched (rule-node)
	  (gstat 'm-and-e-rules-matched (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((matched-count (or (hget rule-node 'matched) 0)))
		(rem-edge `(,rule-node matched ,matched-count))
		(add-edge `(,rule-node matched ,(+ matched-count 1))))
	  nil)

	(defm update-failed (rule-node)
	  (gstat 'm-and-e-rules-failed (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((failed-count (or (hget rule-node 'failed) 0)))
		(rem-edge `(,rule-node failed ,failed-count))
		(add-edge `(,rule-node failed ,(+ failed-count 1))))
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
	  (gstat 'm-and-e-rules-new-edges (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((new-edges-count (or (hget rule-node 'new-edges) 0)))
		(rem-edge `(,rule-node new-edges ,new-edges-count))
		(add-edge `(,rule-node new-edges ,(+ new-edges-count 1))))
	  nil)

	(defm update-not-new-edges (rule-node)
	  (gstat 'm-and-e-rules-not-new-edges (lambda (x y) (+ x y)) (lambda () 1))
	  (let ((not-new-edges-count (or (hget rule-node 'not-new-edges) 0)))
		(rem-edge `(,rule-node not-new-edges ,not-new-edges-count))
		(add-edge `(,rule-node not-new-edges ,(+ not-new-edges-count 1))))
	  nil)

	(defm match-and-execute-rule (rule-node obj-node)
	  (timer 'match-and-execute-rule
		(lambda ()
		  (let ((r nil))
			(when (not (edge-exists `(,rule-node disabled)))
			  (let ((rule-name (hget rule-node 'name)))
				(when (edge-exists `(rule-trace ,rule-name))
				  (print `(match-and-execute-rule tested rule ,rule-node ,rule-name obj ,obj-node)))
				(let ((envlist (all-matches rule-node obj-node)))
				  (update-tested rule-node)
				  (if envlist
					  (let ((rule-comps (get-rule-components rule-node)))
						(let ((pred-list (! (rule-comps preds)))
							  (del-list (! (rule-comps dels)))
							  (add-list (! (rule-comps adds)))
							  (not-list (! (rule-comps nots))))
						  (update-matched rule-node)
						  (when (edge-exists `(rule-trace ,rule-name))
							(print `(match-and-execute-rule matched ,envlist)))
						  (del-consequent-edges del-list not-list envlist)
						  (let ((m (add-consequent-edges obj-node pred-list add-list not-list rule-node envlist)))
							(if m
								(let ()
								  (when (edge-exists `(success ,rule-node ,obj-node))
									(print `(dup (success ,rule-node ,obj-node) ,rule-name)))
								  (add-edge `(success ,rule-node ,obj-node))
								  (rule-status-graph-add-edge `(rule-stat success ,rule-node ,obj-node ,rule-status-seq-no))
								  (update-new-edges rule-node)
								  (setq r t))
								(let ()
								  (update-not-new-edges rule-node))))))
					  (let ()
						(update-failed rule-node)
						(update-not-new-edges rule-node)
						(add-edge `(failure ,rule-node ,obj-node))
						(rule-status-graph-add-edge `(rule-stat failure ,rule-node ,obj-node ,rule-status-seq-no))
						)))))
			(setq rule-status-seq-no (+ rule-status-seq-no 1))
			r))))

	;; Given a node with zero or more ELEM properties, return a list
	;; of nodes representing those elements as an edge

	(defm edge-elem-node-to-list (node)
	  (block el
		(let ((unordered (hget-all node 'elem)))
		  (or unordered
			  (let ((r nil))
				(dolist (e elem-attrs)
				  (let ((v (hget node e)))
					(if (null v)
						(return-from el r)
						(setq r (append r (list v)))))))))))

	;; Chains through all elem relations to find original edge

	(defm resolve-edge-elem-node-to-list (node)
	  (block rl
		(let ((edge (edge-elem-node-to-list node)))
		  (loop
		   (when (not (memq (second edge) elem-attrs))
			 (return-from rl edge))
		   (setq edge (edge-elem-node-to-list (third edge)))))))

	
	;; Given an edge as a list, creates a node with ELEM attrs to each
	;; element of the edge. If pushfcn is passed, it is called with each
	;; edge added as an ELEM attr. Returns the node

	(defm list-to-edge-elem-node (edge &key ordered pushfcn new-pool)
	  (if ordered
		  (let ((node (new-obj-node :new-pool new-pool))
				(i 0))
			(dolist (elem-node edge)
			  (let ((elem-edge (addraw node (intern (format nil "ELEM~a" i)) elem-node)))
				(when pushfcn
				  (funcall pushfcn elem-edge)))
			  (setq i (+ i 1)))
			node)
		  (let ((node (new-obj-node :new-pool new-pool)))
			(dolist (elem-node edge)
			  (let ((elem-edge (addraw node 'elem elem-node)))
				(when pushfcn
				  (funcall pushfcn elem-edge))))
			node)))

	;; Returns list of obj edges

	#| Test with matching 
	
	(defm expand-rule-obj-edges (rule-node obj-node rule-nodepos)
	  (timer 'expand-rule-obj-edges
		(lambda ()
		  (gstat 'expand-rule-obj-edges-len (lambda (v y) (+ (length v) y))
			(lambda ()
			  (defr
				(defl xvar-match-filter-edges (x y) y)
				(let ((traced (edge-exists `(rule-trace ,(hget rule-node 'name)))))
				  (let ((root-var (infer-root-rule-var rule-node obj-node))
						(obj-edges-result nil))
					(do ((rule-edge-list 
						  (bipartite-breadth-rule-walk-seq rule-node root-var)
						  (rest rule-edge-list))
						 (obj-edges
						  (expand-edges `((,obj-node)) rule-nodepos rule-node)
						  (expand-edges (var-match-filter-edges (first rule-edge-list) obj-edges) rule-nodepos rule-node))
						 (envs
						  nil
						  (match-pat-obj-edge-lists (first rule-edge-list) obj-edges root-var obj-node rule-node)))
						((null rule-edge-list) nil)
						(setq obj-edges-result (hunion obj-edges-result (var-match-filter-edges (first rule-edge-list) obj-edges)))
						(when traced
						  (print (list 'expand-rule-obj-edges (first rule-edge-list) envs obj-edges-result))))
					obj-edges-result))))))))
	|#

	(defm expand-rule-obj-edges (rule-node obj-node rule-nodepos)
	  (timer 'expand-rule-obj-edges
		(lambda ()
		  (gstat 'expand-rule-obj-edges-len (lambda (v y) (+ (length v) y))
			(lambda ()
			  (defr
				(defl xvar-match-filter-edges (x y) y)
				(let ((traced (edge-exists `(rule-trace ,(hget rule-node 'name)))))
				  (let ((root-var (infer-root-rule-var rule-node obj-node))
						(obj-edges-result nil))
					(do ((rule-edge-list 
						  (bipartite-breadth-rule-walk-seq rule-node root-var)
						  (rest rule-edge-list))
						 (obj-edges
						  (expand-edges `((,obj-node)) rule-nodepos rule-node)
						  (expand-edges (var-match-filter-edges (first rule-edge-list) obj-edges) rule-nodepos rule-node)))
						((null rule-edge-list) nil)
						(setq obj-edges-result (hunion obj-edges-result (var-match-filter-edges (first rule-edge-list) obj-edges)))
						(when traced
						  (print (list 'expand-rule-obj-edges (first rule-edge-list) obj-edges-result))))
					obj-edges-result))))))))

	#|
	(defm expand-rule-obj-edges (rule-node obj-node rule-nodepos)
	  (timer 'expand-rule-obj-edges
		(lambda ()
		  (gstat 'expand-rule-obj-edges-len (lambda (v y) (+ (length v) y))
			(lambda ()
			  (let ((expanded-edges
					 (let ((root-var (infer-root-rule-var rule-node obj-node))
						   (obj-edges-result nil))
					   (do ((rule-edge-stream 
							 (bipartite-breadth-rule-walk-stream rule-node root-var)
							 (stream-rest rule-edge-stream))
							(obj-edges
							 (expand-edges `((,obj-node)) rule-nodepos rule-node)
							 (expand-edges (var-match-filter-edges (stream-first rule-edge-stream) obj-edges) rule-nodepos rule-node)))
						   ((null rule-edge-stream) nil)
						   (setq obj-edges-result (hunion obj-edges-result (var-match-filter-edges (stream-first rule-edge-stream) obj-edges))))
					   obj-edges-result)))
				(dolist (edge expanded-edges)
				  (! (edge-cache push-head) edge)))
			  (! (edge-cache pop-top-n) 100))))))
	  |#

	;; Given a set of edges, gets all their nodes, and returns the edges of those nodes
	;; rule-node passed for tracing purposes only

	(defm expand-edges (edges rule-nodepos rule-node)
	  (timer 'expand-edges
		(lambda ()
		  (let ((visit-hash (make-hash-table :test #'equal)))
			(let ((nodes (nodes edges)))
			  (dolist (node nodes)
				(let ((child-edges (get-children node 5 rule-nodepos)))
				  (dolist (child-edge child-edges)
					(setf (gethash child-edge visit-hash) child-edge)))))
			(let ((r nil))
			  (maphash (lambda (k v)
						 (setq r (cons v r)))
					   visit-hash)
			  (when (edge-exists `(rule-trace ,(hget rule-node 'name)))
				(print (list 'expand-edges r)))
			  r)))))

	(defm get-children (bnode sigma-span rule-nodepos)
	  (cond
	   ((is-sigma-node bnode)
		(append (get-obj-edges bnode)
				(sigma-edges (hget-edge-inverse bnode 'sigma) sigma-span)))
	   ((and (is-node bnode)
			 (edge-exists `(,bnode prop)))
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

	(defm var-match-filter-edges (rule-edges obj-edges)
	  (timer 'var-match-filter-edges
		(lambda ()
		  (labels ((match-one-edge (pat-edge obj-edge)
								   (labels ((match-one-edge-aux (pat-edge obj-edge)
																(cond
																 ((null pat-edge)
																  t)
																 ((is-var-name (first pat-edge))
																  (match-one-edge-aux (rest pat-edge) (rest obj-edge)))
																 ((equal (first pat-edge) (first obj-edge))
																  (match-one-edge-aux (rest pat-edge) (rest obj-edge)))
																 (t
																  (return-from match-one-edge nil)))))
										   (match-one-edge-aux pat-edge obj-edge))))
				  (let ((edge-hash (make-hash-table :test #'equal :size 1024)))
					(dolist (rule-edge rule-edges)
					  (dolist (obj-edge obj-edges)
						(when (match-one-edge rule-edge obj-edge)
						  (setf (gethash obj-edge edge-hash) obj-edge))))
					(let ((r nil))
					  (maphash (lambda (k v)
								 (setq r (cons v r)))
							   edge-hash)
					  r))))))

	;; If any rule edge contains no common nodes, error, since would
	;; likely be missing many nodes which should match. This is really
	;; saying that we should not have a rule edge which is *all* vars.
	;;
	;; Also if common-nodes is empty, return nil. This seems drastic, but
	;; otherwise there is too much searching. So a rule must have at least
	;; one node in commmon with a matching object

	(defm check-rule-edges (rule-edges common-nodes)
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
		  nil))

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

	(defm edges-with-nodes (nodes edges)
	  (let ((edge-hash (make-hash-table :test #'equal)))
		(dolist (edge edges)
		  (dolist (node nodes)
			(when (member node edge :test #'equal)
			  (setf (gethash edge edge-hash) edge))))
		(let ((r nil))
		  (maphash (lambda (k v)
					 (setq r (cons v r)))
				   edge-hash)
		  r)))

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
	
	(defm all-matches (rule-node obj-node)
	  (timer 'all-matches
		(lambda ()
		  (let ((root-var (infer-root-rule-var rule-node obj-node)))
			(when (possible-match rule-node obj-node root-var)
			  (let ((envlist
					 (if t
						 (all-matches-aux rule-node obj-node root-var)
						 (e-all-matches-aux rule-node obj-node root-var)))) ;;; Experimental version
				(let ((result
					   (mapcar (lambda (env)
								 (append `((?this-rule ,rule-node)
										   (?this-rule-name ,(hget rule-node 'name))
										   (?this-obj ,obj-node)
										   (?root-var ,root-var))
										 env))
							   envlist)))
				  result)))))))

	(defm all-matches-aux (rule-node obj-node root-var)
	  (timer 'all-matches-aux
		(lambda ()
		  (block ama
			(let ((rule-edges (! ((get-rule-components rule-node) preds))))
			  (when rule-edges ;; If no preds, then it's a data rule, i.e., just adds. Should be executed once in the special loop, not done over and over.
				(let ((sig-span (sigma-span rule-node))
					  (rule-nodepos (nodepos rule-edges))
					  (rule-nodes (nodes rule-edges)))
				  (let ((obj-edges (let ()
									 (expand-rule-obj-edges rule-node obj-node rule-nodepos)
									 ;; (print (! (g hget) rule-node 'name))
									 ;; (h (second (filter-new-node-pred-edges rule-edges)))
									 )
									   ))
					(let ((obj-nodes (nodes obj-edges)))
					  (let ((common-nodes (intersect rule-nodes obj-nodes)))
						(if (not (check-rule-edges rule-edges common-nodes))
							(return-from ama nil)
							(let ((xobj-edges (edges-with-nodes common-nodes obj-edges)))
							  (let ((obj-edges (var-match-filter-edges rule-edges xobj-edges)))
								(if (not (check-obj-containment obj-node obj-edges))
									(return-from ama nil)
									(let ((envlist (match-pat-obj-edge-lists rule-edges obj-edges root-var obj-node rule-node)))
									  envlist)))))))))))))))

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

	(defm filter-new-node-pred-edges (patlist)
	  (let ((new-node-pred-edges nil)
			(remaining-pred-edges nil))
		(dolist (pat patlist)
		  (if (eq (second pat) 'new-node)
			  (setq new-node-pred-edges (cons pat new-node-pred-edges))
			  (setq remaining-pred-edges (cons pat remaining-pred-edges))))
		(list new-node-pred-edges remaining-pred-edges)))

	;; Returns a list of envs. Each env is the set of bindings for a
	;; particular patlist match.

	(defm match-pat-obj-edge-lists (patlist objlist root-rule-var obj-node rule-node)
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
			(let ((matches (x-match-all-pat-obj-edge-lists patlist objlist root-rule-var obj-node rule-node)))
			  (when matches
				(let ((matches
					   (if new-node-env
						   (cons (list new-node-env) matches)
						   matches)))
				  (multiple-value-bind (envs partial) (cross-aux2 matches)
					#|
					(when (null envs)
					  (print (list '*** (hget rule-node 'name) obj-node 
								   (mapcar (lambda (env) 
											 (let ((me (matched-edges patlist env)))
											   (let ((sme (set-subtract me objlist)))
												 (list (length patlist) (length sme) #| me sme |#))))
										   partial))))
					|#
					envs))))))))

	(defm patlist-equal (patlist1 patlist2)
	  (set-equal patlist1 patlist2))

	(defm any-binding-invalid (edge-env valid-bindings-hash)
	  (block abi
		(dolist (binding edge-env)
		  (unless (equal binding '(t t))
			(unless (gethash binding valid-bindings-hash)
			  (return-from abi t))))))

	;; Returns a list of env lists, each env list is the set of bindings from matching one pat to the objlist
	;; If any contain env list is nil, this fcn returns nil
	;; I.e., ((env env ..) (env env ...) ...)
	;;
	;; Validity checks:
	;;	- All bindings of an env produced by a single edge match must refer to the same obj edge

	(defm x-match-all-pat-obj-edge-lists (patlist objlist root-rule-var obj-node rule-node)
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
			(let ((env (match-one-edge pat obj root-rule-var obj-node rule-node)))
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

	(defm match-one-edge (pat-edge obj-edge root-rule-var obj-node rule-node)
	  (defr
		(defl match-one-edge1 (pat-edge obj-edge)
		  (defr
			(defl match-one-edge-aux (pat-edge obj-edge)
			  (cond
			   ((null pat-edge)
				'((t t))) ;; literal-match case; dummy binding
			   ;; If we have only one of the roots, then whole edge does not match
			   ((or (and (equal (first pat-edge) root-rule-var)
						 (not (equal (first obj-edge)obj-node)))
					(and (not (equal (first pat-edge) root-rule-var))
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
				(when r
				  ;; (edge-cache-add-matched-edge the-edge-cache rule-node obj-edge)
				  )
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

	(defm e-match-pat-obj-edge-lists (patlist objlist root-rule-var obj-node rule-node)
	  (let ((patlist-info (filter-new-node-pred-edges patlist)))
		(let ((new-node-pred-edges (first patlist-info))
			  (patlist (second patlist-info)))
		  (let ((new-node-env nil))
			(dolist (new-node-pred-edge new-node-pred-edges)
			  (setq new-node-env (cons (list (first new-node-pred-edge) (third new-node-pred-edge)) new-node-env)))
			(let ((matches (e-match-all-pat-obj-edge-lists patlist objlist root-rule-var obj-node rule-node)))
			  (when matches
				(if new-node-env
					(cross-aux2 (cons (list new-node-env) matches))
					(cross-aux2  matches))))))))

	;; Returns list of env lists

	(defm e-match-all-pat-obj-edge-lists (patlist objlist root-rule-var obj-node rule-node)
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
			(let ((env (match-one-edge pat obj root-rule-var obj-node rule-node)))
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
		(let ((rule-edges-seq (bipartite-breadth-rule-walk-seq rule-node root-var)))
		  (let ((rule-edges-list '(()))
				;; (obj-edges `((,obj-node)))
				(obj-edges (expand-rule-obj-edges rule-node obj-node rule-nodepos))
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
			(list renvs rule-edges-list)))))

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
	  (remove-named-rule rule-expr)
	  (let ((rule (define-obj nil :add-rule-link t :new-pool new-pool))	;; add-rule-link nil
			(edge-list nil)
			(add-to-node nil))
		(flet ((pushe (edge)
					  (setq edge-list (cons edge edge-list))))
			  (dolist (clause (rest rule-expr))
				(let ((clause-type (first clause)))
				  (cond
				   ((eq clause-type 'name)
					(let ((rule-name (second clause)))
					  (pushe (addraw rule 'name rule-name))))
				   ((eq clause-type 'root-var)
					(let ((root-var (second clause)))
					  (pushe (addraw rule 'root-var root-var))))
				   ((eq clause-type 'no-triggered)
					(pushe (add-edge `(,rule no-triggered))))
				   ((eq clause-type 'local)
					(setq local t)
					(pushe (add-edge `(,rule local))))
				   ((eq clause-type 'global-node)
					(setq add-to-global-node t))
				   ((eq clause-type 'attach-to)
					(setq add-to-node (second clause)))
				   ((eq clause-type 'vars)
					(let ((varlist-node (list-to-edge-elem-node (first (rest clause))
																:ordered nil
																:pushfcn (lambda (edge) (pushe edge))
																:new-pool new-pool)))
					  (pushe (addraw rule 'vars varlist-node))))
				   ((member clause-type '(add) :test #'eq)
					(dolist (edge (rest clause))
					  (cond
					   ((and (listp (third edge)) (eq (first (third edge)) 'rule))
						(let ((clause-node (new-obj-node)))
						  (let ((rule-values (define-rule (third edge) :local t :new-pool (+ new-pool 1))))
							(let ((rule-node (first rule-values))
								  (rule-edges (second rule-values)))
							  (let ((rule-add-edge-node (list-to-edge-elem-node (append (reverse (rest (reverse edge)))
																						(list rule-node))
																				:ordered t
																				:pushfcn (lambda (edge) (pushe edge))
																				:new-pool new-pool)))
								(pushe (addraw rule 'add rule-add-edge-node)))
							  (dolist (rule-edge rule-edges)
								(let ((node (list-to-edge-elem-node rule-edge
																	:ordered t
																	:pushfcn (lambda (edge) (pushe edge))
																	:new-pool new-pool)))
								  (pushe (addraw rule 'add node))))))))
					   ((and (not (null (third edge)))
							 (listp (third edge)))
						(print (list 'def edge))
						(let ((third-node-edges nil))
						  (let ((third-node (list-to-edge-elem-node (third edge)
																	:ordered t
																	:pushfcn (lambda (edge) (setq third-node-edges (cons edge third-node-edges)))
																	:new-pool (+ new-pool 1))))
							(let ((clause-node (list-to-edge-elem-node `(,(first edge) ,(second edge) ,third-node)
																	   :ordered t
																	   :pushfcn (lambda (edge) (pushe edge))
																	   :new-pool new-pool)))
							  (pushe (addraw rule clause-type clause-node))
							  (dolist (edge third-node-edges)
								(addraw rule clause-type
										(list-to-edge-elem-node edge
																:ordered t
																:pushfcn (lambda (edge) (pushe edge))
																:new-pool new-pool)))))))
					   (t
						(let ((clause-node (list-to-edge-elem-node edge
																   :ordered t
																   :pushfcn (lambda (edge) (pushe edge))
																   :new-pool new-pool)))
						  (pushe (addraw rule clause-type clause-node)))))))
				   ((member clause-type '(pred del not) :test #'eq)
					(dolist (edge (rest clause))
					  (let ((clause-node (list-to-edge-elem-node edge
																 :ordered t
																 :pushfcn (lambda (edge) (pushe edge))
																 :new-pool new-pool)))
						(pushe (addraw rule clause-type clause-node))))))))
			  (pushe (addraw rule 'type 'rule))
			  (if add-to-node
				  (addraw add-to-node 'rule rule)
				  (let ()
					(when (not local)
					  (addraw global-rule-pool 'grp-rule rule))
					(when add-to-local-rule-pool
					  (addraw local-rule-pool 'lrp-rule rule))))
			  (list rule edge-list))))

	(defm bipartite-breadth-rule-walk-seq (rule-node root-var)
	  (timer 'bipartite-breadth-rule-walk-seq
		(lambda ()
		  (defr
			(defl bw (rule-graph root-node depth prev)
			  (let ((r (bipartite-breadth-rule-walk rule-graph root-node :depth depth)))
				(if (equal r prev)
					nil
					(cons
					 (set-subtract r prev)
					 (bw rule-graph root-node (+ depth 2) r)))))
			(let ((rule-graph (make-objgraph))
				  (rule-edges (! ((get-rule-components rule-node) preds))))
			  (let ((root-node (if root-var root-var (first (first rule-edges)))))
				(dolist (rule-edge rule-edges)
				  (! (rule-graph add-edge) rule-edge)
				  (dolist (node rule-edge)
					(when (edge-exists `(,node prop))
					  (! (rule-graph add-edge) `(,node prop)))))
				(bw rule-graph root-node 0 nil)))))))

	(defm bipartite-breadth-rule-walk (rule-graph root-node &key (depth 10))
	  (let ((visit-hash (make-hash-table :test #'equal)))
		(defr
		  (defl bwb (bnodes depth)
			(if (= depth 0)
				(xmapcand (lambda (bnode)
							(if (gethash bnode visit-hash)
								nil
								(let ()
								  (setf (gethash bnode visit-hash) bnode)
								  (! (rule-graph get-rule-children) bnode))))
						  bnodes)
				(bwb (xmapcand (lambda (bnode)
								 (if (gethash bnode visit-hash)
									 nil
									 (let ()
									   (setf (gethash bnode visit-hash) bnode)
									   (! (rule-graph get-rule-children) bnode))))
							   bnodes)
					 (- depth 1))))
		  (setf (gethash root-node visit-hash) root-node)
		  (bwb (! (rule-graph get-rule-children) root-node) depth)
		  (let ((r nil))
			(maphash (lambda (k v)
					   (when (is-edge v)
						 (setq r (cons v r))))
					 visit-hash)
			r))))

	
	(defm get-rule-children (bnode)
	  (cond
	   ((and (is-node bnode)
			 (edge-exists `(,bnode prop)))
		nil)
	   ((is-node bnode)
		(get-edges bnode))
	   ((is-edge bnode)
		bnode)
	   (t nil)))


	(defm bipartite-depth-rule-walk (rule-node root-var)
	  (let ((rule-graph (make-objgraph))
			(rule-edges (! ((get-rule-components rule-node) preds))))
		(let ((root-node (if root-var root-var (first (first rule-edges)))))
		  (print rule-edges)
		  (dolist (rule-edge rule-edges)
			(! (rule-graph add-edge) rule-edge))
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
				  (let ((children (! (rule-graph get-rule-children) bnode)))
					(when (is-edge (first children))
					  (print (list 'children children)))
					(dolist (child children)
					  (bwb child (if (is-edge child) (cons child curlist) curlist)))))))
			  (defl bdrw (rule-graph root-node)
				(bwb root-node nil))
			  (bdrw rule-graph root-node))))))

	;; Fix me! Order-dependent!!!!!!

	(defm infer-root-rule-var (rule-node obj-node)
	  (timer 'infer-root-rule-var
		(lambda ()
		  (let ((r
				 (block iv
				   (let ((root-var (hget rule-node 'root-var)))
					 (or root-var
						 (let ((rule-edges (! ((get-rule-components rule-node) preds)))
							   (obj-edges (get-edges obj-node)))
						   (let ((visited-node-hash (make-hash-table :size 16 :test #'equal))
								 (unique-node-edge-hash (make-hash-table :size 16 :test #'equal)))
							 (dolist (rule-edge rule-edges)
							   (dolist (rule-node rule-edge)
								 (if (gethash rule-node visited-node-hash)
									 (remhash rule-node unique-node-edge-hash)
									 (let ()
									   (setf (gethash rule-node unique-node-edge-hash) rule-edge)
									   (setf (gethash rule-node visited-node-hash) rule-node)))))
							 (dolist (obj-edge obj-edges)
							   (dolist (obj-node obj-edge)
								 (let ((rule-edge (gethash obj-node unique-node-edge-hash)))
								   (when rule-edge
									 (return-from iv (first rule-edge)))))))))))))
			(when (edge-exists `(rule-trace ,(hget rule-node 'name)))
			  (print (list 'infer-root-rule-var r)))
			r))))

	(defm get-rule-consts (rule)
	  (let ((pred-list (! ((get-rule-components rule) preds))))
		(dedup-list (mapcan (lambda (edge)
							  (filter-vars edge))
							pred-list))))

	(defm get-rule-consts-pred (pred-list)
	  (dedup-list (mapcan (lambda (edge)
							(filter-vars edge))
						  pred-list)))

	(defm get-rule-preds-root (preds root-var)
	  (let ((r nil))
		(dolist (pred preds)
		  (when (member root-var pred :test #'equal)
			(setq r (cons pred r))))
		r))
	
	(defm possible-match (rule-node obj-node root-var)
	  (timer 'possible-match
		(lambda ()
		  (let ((rule-comps (get-rule-components rule-node)))
			(let ((preds (! (rule-comps preds))))
			  (let ((root-preds (get-rule-preds-root preds root-var)))
				(let ((root-pred-const-nodes (get-rule-consts-pred root-preds)))
				  (let ((pred-const-nodes (get-rule-consts-pred preds)))
					(let ((obj-edges (get-edges obj-node)))
					  (let ((obj-nodes (nodes obj-edges)))
						(and (intersect pred-const-nodes obj-nodes)
							 (= 
							  (length (intersect root-pred-const-nodes obj-nodes))
							  (length root-pred-const-nodes)))))))))))))

	(defm make-rule-graph (rule-node)
	  (let ((rule-graph (make-objgraph))
			(rule-edges (! ((get-rule-components rule-node) preds))))
		(dolist (rule-edge rule-edges)
		  (! (rule-graph add-edge) rule-edge))
		rule-graph))

	)))

(defstruct env-trig-entry
  (rule-node nil)
  (env nil))

(defc env-triggered nil nil
  (let ((env-triggered-table (make-hash-table :test #'equalp :size 32768))
		(edge-to-trig-entry (make-hash-table :test #'equal :size 32768))
		(lte (make-env-trig-entry))
		(graph nil))

	(defm set-graph (g)
	  (setq graph g))

	(defm insert (rule-node env matched-edges)
	  (timer 'env-triggered-insert
		(lambda ()
		  (let ((te (lookup rule-node env)))
			(when (null te)
			  (setq te (make-env-trig-entry :rule-node rule-node :env env))
			  (setf (gethash te env-triggered-table) te)
			  (dolist (matched-edge matched-edges)
				(insert-edge matched-edge te)))
			te))))

	(defm lookup (rule-node env)
	  (setf (env-trig-entry-rule-node lte) rule-node)
	  (setf (env-trig-entry-env lte) env)
	  (let ((te (gethash lte env-triggered-table)))
		(or te)))
	
	(defm insert-edge (edge trig-entry)
	  (setf (gethash edge edge-to-trig-entry) trig-entry)
	  nil)

	(defm removed-edge (edge)
	  (timer 'env-triggered-removed-edge
		(lambda ()
		  (let ((te (gethash edge edge-to-trig-entry)))
			(if te
				(let ()
				  (remhash te env-triggered-table)
				  t)
				nil)))))

	(defm as-list ()		;; For debug only
	  (list env-triggered-table edge-to-trig-entry))

	(defm rule-has-been-triggered (rule-node env)
	  (when am-traced
		(print (! (graph hget) rule-node 'name)))
	  (if (lookup rule-node env)
		  t
		  nil))))

(defc edge-cache nil nil
  (let ((cache (make-queue)))

	(defm push (edge)
	  (! (cache remove) edge)
	  (! (cache push-tail) edge)
	  nil)

	(defm push-head (edge)
	  (! (cache remove) edge)
	  (! (cache push-head) edge)
	  nil)

	(defm remove (edge)
	  (! (cache remove) edge))

	(defm push-list (edges)
	  (dolist (edge edges)
		(push edge))
	  nil)

	(defm pop-top-n (n)
	  (let ((r nil))
		(dotimes (i n)
		  (let ((e (! (cache pop-tail))))
			(if (null e)
				(return nil)
				(setq r (cons e r)))))
		r))

	(defm get-top-n (n)
	  (let ((r (pop-top-n n)))
		(dolist (x r)
		  (! (cache push-tail) x))
		r))

	(defm as-list ()
	  (! (cache as-list)))))


;; General surjective map, for many-to-one mappings. Default test for
;; input and result sets is equal.

(defc sur-map nil (&key (input-test #'equal) (res-test #'equal) (input-size 32768) (res-size 512))
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
		nil))
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
	(defm as-list ()
	  (let ((inputlist (hash-table-key-to-list input-hash)))
		(mapcar (lambda (input)
				  (list input (lookup input)))
				inputlist)))
	(defm map (fcn)		;; fcn is two args, (input reslist)
	  ())))

;; x-sur-map

(defc x-sur-map nil (&key (input-test #'equal) (res-test #'equal) (input-size 32768) (res-size 512))
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
				inputlist)))))

(defc dumper nil nil
  (let ((g nil)
		(elem-attrs nil))

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

	;; "c:\Program Files (x86)\Graphviz2.38\bin\dot.exe" -Tjpeg fe.gv -o fe.jpg
	;; (let ((d (make-dumper))) (! (d set-graph) g)(! (d dump-gv-edges) "yyy3.gv" '(sigma even-func)))

	;; attrs is a list of attrs to draw. If attrs = t, draw all
	;; rules is a list of rules to draw. If rules = t, draw all

	(defm dump-gv-edges (file &key (attrs t) (rules t))
	  (let ((omitted-attrs (append elem-attrs '(triggered add pred del binding left right))))
		(with-open-file (s file :direction :output)
		  (defr
			(defl dump-gv-node (n &key two-input-op as-prop rule-name)
			  (let ((fmt "\"~a~a\" [fontname=arial,label=\"~a\"~a];~%")) ;;; (fmt "\"~a~a\" [fontname=arial,style=bold,label=\"~a\"~a];~%")
				(format s fmt
						(if rule-name (format nil "~a-" rule-name) "")
						n
						n
						(if as-prop ",shape=none" ""))))
			(defl dump-gv-edges-data (v)
			  (when (not (and (eq (second v) 'type) (eq (third v) 'rule)))
				(when (and (not (intersect v omitted-attrs))
						   (or (eq attrs t)
							   (intersect v attrs)))
				  (when (and (>= (length v) 1)
							 (! (g hget) (first v) 'color))
					(format s "\"~a\" [color=~a,style=filled];~%" (first v) (! (g hget) (first v) 'color)))
				  (let ((l (length v)))
					(cond
					 ((and (= l 2)
						   (eq (second v) 'next))
					  (format s "\"~a\" -> \"~a\" [label=\"~a\",style=\"setlinewidth(1)\"];~%"
							  (first v) (first v) (second v)))
					 ((and (= l 3)
						   (eq (second v) 'fcn-color))
					  nil)
					 ((and (= l 3)
						   (eq (second v) 'rule30val))
					  (format s "\"~a\" [color=~a,style=filled];~%" (first v) (if (= (third v) 1) "blue" "pink")))
					 ((= l 3)
					  (let ((in-node (first v))
							(out-node (third v))
							(prop (second v)))
						(let ((in-node-color (! (g hget) prop 'in-node-color))
							  (out-node-color (! (g hget) prop 'out-node-color))
							  (edge-color (! (g hget) prop 'edge-color))
							  (edge-color-string ""))
						  (dump-gv-node in-node)
						  (dump-gv-node out-node)
						  (when in-node-color
							(format s "\"~a\" [color=~a,style=filled];~%" in-node in-node-color))
						  (when out-node-color
							(format s "\"~a\" [color=~a,style=filled];~%" out-node out-node-color))
						  (when edge-color
							(setq edge-color-string (format nil ",color=~a" edge-color)))
						  (format s "\"~a\" -> \"~a\" [fontname=arial,label=\"~a\",style=\"setlinewidth(1)\"~a];~%" in-node out-node prop edge-color-string))))
					 ((eq (second v) 'zero)
					  (format s "\"~a\" [fontname=arial,label=\"~a-z\"];~%" (first v) (first v)))
					 ((and (= l 4)
						   (! (g edge-exists) `(,(third v) two-input-op)))
					  (let ((i1 (first v))
							(i2 (second v))
							(fcn-name (third v))
							(o (fourth v)))
						(let ((fcn (format nil "~a-~a" fcn-name o))
							  (color (! (g hget) fcn-name 'color))
							  (in-node-color (! (g hget) fcn-name 'in-node-color))
							  (out-node-color (! (g hget) fcn-name 'out-node-color))
							  (fcn-color (! (g hget) o 'fcn-color)))
						  (when fcn-color
							(setq color fcn-color))
						  (dump-gv-node fcn :two-input-op t)
						  (dump-gv-node i1)
						  (dump-gv-node i2)
						  (dump-gv-node o)
						  (when in-node-color
							(format s "\"~a\" [color=~a,style=filled];~%" i1 in-node-color))
						  (when in-node-color
							(format s "\"~a\" [color=~a,style=filled];~%" i2 in-node-color))
						  (when out-node-color
							(format s "\"~a\" [color=~a,style=filled];~%" o out-node-color))
						  (when color
							(format s "\"~a\" [shape=~a,color=~a,style=filled];~%"
									fcn 
									"ellipse" ;; (if (= (random 5) 0) "invtrapezium" "none")
									color))
						  (format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i1 fcn)
						  (format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" i2 fcn)
						  (format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\"];~%" fcn o))))
					 ((= l 2)
					  (let ((n (first v))
							(p (second v)))
						(dump-gv-node n)
						(dump-gv-node p :as-prop t)
						(format s "\"~a\" -> \"~a\"[style=\"setlinewidth(1)\",label=\"\",arrowhead=none];~%" n p)))
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
							 (member (! (g hget) (first v) 'name) rules)))
				(dump-gv-rule (first v))))
			(defl dump-gv-rule (rule)
			  (let ((rule-components (! (g get-rule-components) rule))
					(rule-name (! (g hget) rule 'name)))
				(let ((pred-edges (! (rule-components preds)))
					  (del-edges (! (rule-components dels)))
					  (add-edges (! (rule-components adds)))
					  (not-edges (! (rule-components nots)))
					  (rule-nodes (! (rule-components all-nodes)))
					  (i 0))
				  (format s "digraph \"~a\" {~%" rule-name)
				  (format s "subgraph \"cluster\" {~%")
				  (format s "label = \"~a\";~%" rule-name)
				  ;; (format s "\"~a\" [label=\"~a\"];~%" rule rule-name)
				  ;; (format s "\"~a\" -> \"~a\" [label=\"~a\"];~%" "rules" rule-name  "")
				  (dolist (rule-edges (list pred-edges del-edges add-edges not-edges))
					(when (or (eq rule-edges pred-edges)
							  (eq rule-edges add-edges))
					  (setq i (+ i 1))
					  (dolist (rule-edge rule-edges)
						(when (not (and (eq rule-edges add-edges)
										(or (eq (first rule-edge) 'print)
											#| (eq (second rule-edge) 'rule) |# )))
						  (let ((l (length rule-edge)))
							(cond
							 ((and (= l 2)
								   (eq (second rule-edge) 'next))
							  (format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" rule-name (first rule-edge) (first rule-edge))
							  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a\",style=~a,fontname=arial,color=~a];~%"
									  rule-name (first rule-edge)
									  rule-name (first rule-edge)
									  (second rule-edge)
									  (if (eq rule-edges add-edges) "dashed" "solid")
									  (if (eq rule-edges add-edges) "red" "black")))
							 ((= l 2)
							  (let ((n (first rule-edge))
									(p (second rule-edge)))
								(dump-gv-node n :rule-name rule-name)
								(dump-gv-node p :as-prop t :rule-name rule-name)
								(format s "\"~a-~a\" -> \"~a-~a\" [label=\"\",style=~a,fontname=arial,color=~a,arrowhead=none];~%"
									  rule-name n
									  rule-name p
									  (if (eq rule-edges add-edges) "dashed" "solid")
									  (if (eq rule-edges add-edges) "red" "black"))))
							 ((= l 3)
							  (dump-gv-node (first rule-edge) :rule-name rule-name)
							  (dump-gv-node (third rule-edge) :rule-name rule-name)
							  (format s "\"~a-~a\" -> \"~a-~a\" [label=\"~a~a\",style=~a,fontname=arial,color=~a];~%"
									  rule-name (first rule-edge)
									  rule-name (third rule-edge)
									  (if (member rule-edge del-edges :test #'equal) "X " "")
									  (second rule-edge)
									  (if (eq rule-edges add-edges) "dashed" "solid")
									  (cond
									   ((eq rule-edges add-edges) "red")
									   ((member rule-edge del-edges :test #'equal) "blue")
									   (t "black"))))
							 ((and (= l 4)
								   (! (g edge-exists) `(,(third rule-edge) two-input-op)))
							  (let ((i1 (first rule-edge))
									(i2 (second rule-edge))
									(fcn-name (third rule-edge))
									(o (fourth rule-edge)))
								(let ((fcn (format nil "~a-~a-~a-~a-~a" rule-name fcn-name i1 i2 o))
									  (color (! (g hget) fcn-name 'color)))
								  (format s "\"~a\" [label=\"~a\",fontname=arial];~%" fcn fcn-name)
								  (when color
									(format s "\"~a\" [color=~a,style=filled]~%" fcn color))
								  (format s "\"~a-~a\" -> \"~a\"" rule-name i1 fcn)
								  (format s "[style=~a,color=~a];~%" 
										  (if (eq rule-edges add-edges) "dashed" "solid")
										  (if (eq rule-edges add-edges) "red" "black"))
								  (format s "\"~a-~a\" -> \"~a\"" rule-name i2 fcn)
								  (format s "[style=~a,color=~a];~%"
										  (if (eq rule-edges add-edges) "dashed" "solid")
										  (if (eq rule-edges add-edges) "red" "black"))
								  (format s "\"~a\" -> \"~a-~a\"" fcn rule-name o)
								  (format s "[style=~a,color=~a];~%"
										  (if (eq rule-edges add-edges) "dashed" "solid")
										  (if (eq rule-edges add-edges) "red" "black")))))
							 (t
							  (dotimes (i l)
								(let ((rule-node (nth i rule-edge)))
								  (format s "\"~a-~a\" [label=\"~a\",fontname=arial];~%" rule-name rule-node rule-node)))
							  (dotimes (i l)
								(let ((rule-node (nth i rule-edge)))
								  (format s "\"~a-~a\"" rule-name rule-node)
								  (when (< i (- l 1))
									(format s " -> "))))
							  (format s "[style=~a,fontname=arial,color=~a];~%"
									  (if (eq rule-edges add-edges) "dashed" "solid")
									  (if (eq rule-edges add-edges) "red" "black")))))))))
				  (format s "}~%")
				  (format s "}~%"))))
			(format s "digraph G {~%")
			(! (g dump-edges) :dump-fcn #'dump-gv-edges-data)
			(format s "}~%")
			(! (g dump-edges) :dump-fcn #'dump-gv-edges-rules)))))))

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

(defun env-prune (env varlist)		;; Remove all bindings from env which contain a member of varlist
  (mapcan (lambda (b)
			(if (intersect b varlist)
				nil
				(list b)))
		  env))

;; Returns a node which is the head of a graph representing the
;; env. The graph form is:
;; (env (binding (left x) (right y)) ...)

(defun env-to-node (env g)
  (let ((env-node (new-obj-node)))
	(dolist (binding env)
	  (let ((left (first binding))
			(right (second binding)))
		(let ((binding-node (new-obj-node)))
		  (addraw g binding-node 'left left)
		  (addraw g binding-node 'right right)
		  (addraw g env-node 'binding binding-node))))
	env-node))

;; Inverse of above. Returns an env.

(defun node-to-env (node g)
  (let ((env nil))
	(dolist (binding-node (hget-all g node 'binding))
	  (setq env (cons (list (hget g binding-node 'left)
							(hget g binding-node 'right))
					  env)))
	env))

;; Take a list of env lists (list of envs envs-list), each env list is
;; the set of envs from matching one pat against a list of obj-edges.
;; 									  
;; Do the cross product by building up a set of envs. Look for 
;; conflicts at each step, and toss those envs in conflict, so they
;; are not further expanded.		

(defun cross-aux2 (envs-list)
  (let ((lengths (list (length envs-list) (mapcar (lambda (x) (length x)) envs-list))))
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
		;; (cross-aux2-info (cons (length r) lengths))
		r))))

#|

;; mv variant return partial-match env as well as nil failed match ; ; ; ; ;

	  (defun cross-aux2 (envs-list)
(timer 'cross-aux2
(lambda ()
(let ((env-list-out '(()))
(r1 nil)
(found-nil nil))
(dolist (envs envs-list)
(if envs
(let ()
(dolist (p env-list-out)
(dolist (env envs)
(let ((new-env (env-no-conflict-dedup (append env p))))
(when new-env
(setq r1 (cons new-env r1))))))
(setq env-list-out r1)
(setq r1 nil))
(setq found-nil t)))
(if found-nil
(values nil env-list-out)
(values env-list-out env-list-out))))))
	  |#

(let ((bindinghash (make-hash-table :size 128 :test #'equal)))
  (defun env-no-conflict-dedup (env)
	(timer 'env-no-conflict-dedup
	  (lambda ()
		(block xxx
		  (clrhash bindinghash)
		  (dolist (binding env)
			(let ((var (first binding))
				  (val (second binding)))
			  (let ((hval (gethash var bindinghash)))
				(when (null hval)
				  (setf (gethash var bindinghash) val)
				  (setq hval val))
				(when (not (equal val hval))
				  (return-from xxx nil)))))
		  (let ((r nil))
			(maphash (lambda (k v)
					   (setq r (cons (list k v) r)))
					 bindinghash)
			r))))))

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

;; Three intersect versions below, intersect new-intersect, and
;; new-new-intersect. intersect uses a straight n^2 loop, while the
;; others use some form of hashing/caching. The plain loop is fastest,
;; probably due to the short lengths used mostly in the H machine.
;;
;; Note uses equal and test is not passed in

(defun intersect (l1 l2)
  (timer 'intersect
	(lambda ()
	  (cond
	   ((eq l1 t)
		l2)
	   ((eq l2 t)
		(l1))
	   (t
		(let ((r nil))
		  (dolist (x1 l1)
			(dolist (x2 l2)
			  (when (equal x1 x2)
				(setq r (cons x1 r)))))
		  r))))))

(defun intersect-with-test (l1 l2 &key (test #'equal))
  (timer 'intersect-with-test
	(lambda ()
	  (cond
	   ((eq l1 t)
		l2)
	   ((eq l2 t)
		(l1))
	   (t
		(let ((r nil))
		  (dolist (x1 l1)
			(dolist (x2 l2)
			  (when (funcall test x1 x2)
				(setq r (cons x1 r)))))
		  r))))))

(let ((test #'equal))
  (let ((h (make-hash-table :size 1024 :test test)))
	(defun new-intersect (l1 l2 &key (test #'equal))
	  (timer 'new-intersect
		(lambda ()
		  (cond
		   ((eq l1 t)
			l2)
		   ((eq l2 t)
			l1)
		   (t
			(clrhash h)
			(let ((r nil))
			  (dolist (x l1)
				(setf (gethash x h) x))
			  (dolist (x l2)
				(when (gethash x h)
				  (setq r (cons x r))))
			  r))))))))

(let ((test #'equal))
  (let ((c (make-hash-table :size 1024 :test test)))
	(defun new-new-intersect (l1 l2 &key (test #'equal))
	  (timer 'new-new-intersect
		(lambda ()
		  (let ((r (gethash (cons l1 l2) c)))
			(or r 
				(cond
				 ((eq l1 t)
				  l2)
				 ((eq l2 t)
				  (l1))
				 (t
				  (let ((r nil))
					(dolist (x1 l1)
					  (dolist (x2 l2)
						(when (funcall test x1 x2)
						  (setq r (cons x1 r)))))
					(setf (gethash (cons l1 l2) c) r)
					r))))))))))

(let ((test #'equal))
  (let ((h (make-hash-table :size 32768 :test test)))
	(defun hunion (l1 l2)
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
				(setf (gethash x h) x))
			  (dolist (x l2)
				(setf (gethash x h) x))
			  (let ((r nil))
				(maphash (lambda (k v)
						   (setq r (cons v r)))
						 h)
				r)))))))))

(let ((test #'eq))
  (let ((h (make-hash-table :size 32768 :test test)))
	(defun eq-hunion (l1 l2)
	  (timer 'eq-hunion
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
				(setf (gethash x h) x))
			  (dolist (x l2)
				(setf (gethash x h) x))
			  (let ((r nil))
				(maphash (lambda (k v)
						   (setq r (cons v r)))
						 h)
				r)))))))))

(defun set-equal (l1 l2 &key (test #'equal))
  (= (length l1)
	 (length l2)
	 (length (intersect-with-test l1 l2 :test test))))

;; l1 \ l2

(defun set-subtract (l1 l2 &key (test #'equal))
  (let ((i (intersect l1 l2))
		(r nil))
	(dolist (x1 l1)
	  (when (not (member x1 i :test test))
		(setq r (cons x1 r))))
	r))

;; s1 and s2 are each sets of sets. Each set from s1 is intersected
;; with each set from s2. If the result is non-empty, then s1 and s2
;; are unioned into the final result.

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

(defun mapcad (fcn list)
  (let ((r nil))
	(dolist (l list)
	  (let ((v (funcall fcn l)))
		(when v
		  (setq r (cons v r)))))
	r))

;; Suspect system bugs with built-in mapcan

(defun xmapcan (fcn list)
  (let ((r nil))
	(dolist (l list)
	  (let ((v (funcall fcn l)))
		(setq r (append r v))))
	r))

;; Like mapcan in that it appends the results of applying fcn to each
;; member of list, but in this cases depuplicates -- assures there is
;; no dup list member.

(defun xmapcand (fcn list)
  (let ((elem-hash (make-hash-table :size 512 :test #'equal)))
	(dolist (l list)
	  (let ((v (funcall fcn l)))
		(dolist (elem v)
		  (setf (gethash elem elem-hash) elem))))
	(let ((r nil))
	  (maphash (lambda (k v)
				 (setq r (cons v r)))
			   elem-hash)
	  r)))

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
	(let ((m 0))
	  (maphash (lambda (k v)
				 (let ((name k))
				   (let ((l (length (symbol-name name))))
					 (when (> l m)
					   (setq m l)))))
			   perf-hash)
	  (format t "~%~vtavg~vtcount~vtsum~%" (+ m 4)  (+ m 25) (+ m 40))
	  (maphash (lambda (k v)
				 (let ((name k)
					   (timerec v))
				   (let ((sum (timerec-sum timerec))
						 (count (timerec-count timerec)))
					 (let ((units (if (eq (timerec-type timerec) 'time)
									  internal-time-units-per-second
									  1)))
					   (let ((avg-time (/ (float (/ sum count)) units))
							 (sum-time (/ (float sum) units)))
						 (format t
								 "~a~vt~a~vt~a~vt~a~%"
								 name (+ m 4) avg-time (+ m 25) count (+ m 40) sum-time))))))
			   perf-hash)
	  (format t "~%")
	  nil))
  (defun clear-perf-stats ()
	(setq perf-hash (make-hash-table :test #'eq)))
  (defun get-perf-hash () perf-hash))

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
	  (let ((pm (> hour 12)))
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

;; Simple fixed-block non-expandable hash table. Not a defc for max performance

(defstruct hashtab
  (block nil)
  (size nil)
  (test nil)
  (count 0))

(defun hmake-hash-table (&key (size 16) (test #'equal))
  (let ((h (make-hashtab)))
	(setf (hashtab-block h) (make-array size))
	(setf (hashtab-size h) size)
	(setf (hashtab-test h) test)
	h))

(defun hgethash (key hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab))
		(test (hashtab-test hashtab)))
	(let ((hk (mod (sxhash key) s)))
	  (let ((l (svref b hk)))
		(dolist (e l)
		  (when (funcall test (first e) key)
			(return-from hgethash (rest e)))))))
  nil)

(defun hputhash (key hashtab value)
  (block b
	(let ((s (hashtab-size hashtab))
		  (b (hashtab-block hashtab))
		  (test (hashtab-test hashtab)))
	  (let ((hk (mod (sxhash key) s)))
		(let ((l (svref b hk)))
		  (dolist (e l)
			(when (funcall test (first e) key)
			  (setf (rest e) value)
			  (return-from b nil)))
		  (setf (hashtab-count hashtab) (+ (hashtab-count hashtab) 1))
		  (setf (svref b hk) (cons (cons key value) (svref b hk)))))))
  value)

(defun hremhash (key hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab))
		(test (hashtab-test hashtab)))
	(let ((hk (mod (sxhash key) s)))
	  (let ((l (svref b hk)))
		(cond 
		 ((null l)
		  (return-from hremhash nil))
		 ((funcall test (first (first l)) key)
		  (setf (svref b hk) (rest l))
		  (setf (hashtab-count hashtab) (- (hashtab-count hashtab) 1))
		  (return-from hremhash t))
		 (t
		  (loop
		   (when (null l)
			 (return-from hremhash nil))
		   (let ((e (first (rest l))))
			 (when (funcall test (first e) key)
			   (setf (rest l) (rest (rest l)))
			   (setf (hashtab-count hashtab) (- (hashtab-count hashtab) 1))
			   (return-from hremhash t))
			 (setq l (rest l)))))))))
  nil)

(defun hmaphash (fcn hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab)))
	(dotimes (i s)
	  (let ((l (svref b i)))
		(dolist (e l)
		  (funcall fcn (first e) (rest e))))))
  nil)

(defun hhash-table-count (hashtab)
  (hashtab-count hashtab))

(defun hhash-table-to-list (h)
  (let ((r nil))
	(hmaphash (lambda (k v)
			   (setq r (cons (list k v) r)))
			 h)
	r))

(defun hhash-table-key-to-list (h)
  (let ((r nil))
	(hmaphash (lambda (k v)
			   (setq r (cons k r)))
			 h)
	r))

(defun hhash-table-value-to-list (h)
  (let ((r nil))
	(hmaphash (lambda (k v)
			   (setq r (cons v r)))
			 h)
	r))

;; Global trace "info" section

(defun match-and-execute-info (info))

(defun match-and-execute-env-info (info))

(defun queue-info (info))

;;

(defc the-graph objgraph nil
  (let ()

	(defm add-natural-number-edges (n)
	  (dotimes (i n)
		(add-obj-edge `(,i sigma ,(+ i 1)) :add-rule-link t)))

	#|
	;; Below is a generator for the print-gc rules.
	;; The print gc rules look like this:

	(rule
	 (name print-gc1)
	 (no-triggered)
	 (local)
	 (pred
	  (print ?x1))
	 (del
	  (print ?x1)))

	;; Note this 2-arg rule explicitly contains a not clause to prevent
	;; deletion of the rule itself. This print-gc idea was the
	;; stimulus to create negation rules. rules to 2+ args do not
	;; contain a not clause.

	(rule
	 (name print-gc2)
	 (no-triggered)
	 (local)
	 (pred
	  (print ?x1 ?x2))
	 (not
	  (print rule ?x2))
	 (del
	  (print ?x1 ?x2)))

	...More print-gc rules...

	;; They are generated by a rule like this:

	(rule
	 (name print-gc-rule)
	 (pred 
	  (local-rule-pool lrp-rule ?pr1)
	  (local-rule-pool lrp-rule ?pr2)
	  ...
	  (?pr1 name print-gc1)
	  (?pr2 name print-gc2)
	  ...)
	 (add
	  (print rule ?pr1)
	  (print rule ?pr2)
	  ...))
	  |#

	(defm add-print-gc-rules ()
	  (let ((n 6))
		(defr
		  (defl gcname (n)
			(intern (format nil "PRINT-GC~a" n)))
		  (defl genvars (n)
			(let ((r nil))
			  (dotimes (j n)
				(let ((i (+ j 1)))
				  (setq r (append r (list (intern (format nil "?X~a" i)))))))
			  r))
		  (dotimes (j n)
			(let ((i (+ j 1)))
			  (define-rule
				`(rule
				  (name ,(gcname i))
				  (no-triggered)
				  (local)
				  (pred
				   (print ,@(genvars i)))
				  ,@(when (= i 2)
					  `((not
						 (print rule ?x2))))
				  (del
				   (print ,@(genvars i)))))))
		  (define-rule
			`(rule
			  (name print-gc-rule)
			  (attach-to global-node)
			  (pred
			   (global-node rule ?r)
			   (?r name print-gc-rule)
			   (global-node local-rule-pool ?p)
			   ,@(let ((r nil))
				   (dotimes (j n)
					 (let ((i (+ j 1)))
					   (setq r (append r `((?p lrp-rule ,(intern (format nil "?PR~a" i))))))))
				   r)
			   ,@(let ((r nil))
				   (dotimes (j n)
					 (let ((i (+ j 1)))
					   (setq r (append r `((,(intern (format nil "?PR~a" i)) name ,(intern (format nil "PRINT-GC~a" i))))))))
				   r))
			  (add
			   (print print-gc-rule)
			   ,@(let ((r nil))
				   (dotimes (j n)
					 (let ((i (+ j 1)))
					   (setq r (append r `((print rule ,(intern (format nil "?PR~a" i))))))))
				   r))
			  (del
			   (global-node rule ?this-rule)))))))

	(defm defg (rules)
	  (dolist (rule rules)
		(define-rule rule))
	  nil)

	(defm read-rule-file (file)
	  (with-open-file (s file :direction :input)
		(loop
		 (let ((r (read s nil nil)))
		   (if (null r)
			   (return nil)
			   (define-rule r))))
		nil))

	(defm init ()
	  (objgraph-init)
	  ;; (setq rule-status-graph (make-objgraph))
	  (clear-perf-stats)		;; Note the perf stats are global
	  (add-natural-number-edges 20)
	  (add-print-gc-rules)
	  (do-defg)
	  ;; (read-rule-file "tree.lisp")  ;;; !!!!!!!!! Needs work
	  ;; (read-rule-file "globaltree.lisp")
	  (read-rule-file "exectree.lisp")
	  (read-rule-file "rule30.lisp")
	  (read-rule-file "fft-delta.lisp")
	  ;; (read-rule-file "counter.lisp")
	  (execute-obj 'global-node)
	  (clear-queue))

	(defm do-defg ()
	  (defg
		'(
		  ;; Don't have a null add-rule-link capability for now -- so we always do

		  (rule
		   (name data)
		   (attach-to global-node)
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
			(rand randgen)
			(global-node next-color)
			;;;;;
			;; (aup prop)
			;; (adn prop)
			;; (tree-next prop)
			;; (next prop)
			;; (elem prop)
			;; (is-elem-of prop)
			;; (ref prop)
			;; (rule prop)
			;; (zero prop)
			;; (local-rule-pool prop)
			;; (name prop)
			;; (fft-comb prop)
			;; (value prop)
			;; (top prop)
			;; (l prop)
			;; (p prop)
			;;;;;
			(+ 1 2 3)
			(+ 2 2 4)
			(+ 2 3 5))
		   (del
			(global-node rule ?this-rule)))

		  (rule
		   (name add-parent)
		   ;; (local)
		   (pred
			(?a elem ?e))
		   (add
			(print add-parent ?e ?a)
			(?e is-elem-of ?a)))

#|
	  (rule
		  (name rand-rule)
		  (local)
		  (attach-to rand)
		  (pred
		  (?x rand ?r)
		  (rand randgen)
		  (randgen val ?v)
		  (?v center-up ?s)
		  (?v rule30val ?r))
		  (add
		  (randgen val ?s))
		  (del
		  (randgen val ?v)))
	  |#

		  (rule
		   (name ev-init)
		   (local)
		   (root-var ?e0) ;; was ?a
		   (pred
			(?a elem ?e0)
			(?e0 is-elem-of ?a)
			(?e0 zero)
			(?e0 local-rule-pool ?p)
			(?p lrp-rule ?od-next)
			(?od-next name od-next))
		   (add
			(print ev-init ?e0)
			(?e0 ev)
			(?e0 rule ?od-next))
		   (del
			(?this-obj rule ?this-rule)
			))

		  (rule
		   (name od-next)
		   (local)
		   (root-var ?e0)
		   (pred
			(?a elem ?e0)
			(?a elem ?e1)
			(?e0 is-elem-of ?a)
			(?e1 is-elem-of ?a)
			(?e0 next ?e1)
			(?e0 ev)
			(?e0 local-rule-pool ?p)
			(?p lrp-rule ?ev-next)
			(?ev-next name ev-next))
		   (add
			(print od-next ?e1 ?root-var)
			(?e1 od)
			(?e1 rule ?ev-next))
		   (del
			(?this-obj rule ?this-rule)
			))

		  (rule
		   (name ev-next)
		   (local)
		   (root-var ?e0)
		   (pred
			(?a elem ?e0)
			(?a elem ?e1)
			(?e0 is-elem-of ?a)
			(?e1 is-elem-of ?a)
			(?e0 next ?e1)
			(?e0 od)
			(?e0 local-rule-pool ?p)
			(?p lrp-rule ?od-next)
			(?od-next name od-next))
		   (add
			(print ev-next ?e1 ?root-var)
			(?e1 ev)
			(?e1 rule ?od-next))
		   (del
			(?this-obj rule ?this-rule)
			))

		  (rule
		   (name ev-od-obj-rule)
		   (attach-to global-node)
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

		  ;; Note copy-rule currently is limited in the edge size of what may be
		  ;; found in a rule component -- must be three, so e.g. larger prints
		  ;; are cut off

		  (rule
		   (name copy-rule-rule)
		   (pred
			(?r copy-rule ?y)
			(?r name ?name)
			(?r root-var ?x-root-var)
			(?nn1 new-node sn1))
		   (add
			(print copy-rule-rule ?r ?y ?r ?name ?x-root-var ?nn1)
			(?nn1 root-var ?x-root-var)
			(?nn1 name ?name)
			(?r rule (rule
					  (name copy-rule-rule-pred)
					  (pred
					   (?r pred ?p)
					   (?nn2 new-node sn2))
					  (add
					   (print copy-rule-rule-pred ?r ?p ?nn2)
					   (?r rule (rule
								 (name copy-rule-rule-pred-2)
								 (pred
								  (?r name ?name)
								  (?r pred ?p) ;; connected!
								  (?p elem0 ?pe0)
								  (?p elem1 ?pe1)
								  (?p elem2 ?pe2))
								 (add
								  (print copy-rule-rule-pred-2 ?r ?p ?nn1 ?nn2 ?pe0 ?pe1 ?pe2)
								  (?y rule ?nn1)
								  (?nn1 pred ?nn2)
								  (?nn2 elem0 ?pe0)
								  (?nn2 elem1 ?pe1)
								  (?nn2 elem2 ?pe2)))))))
			(?r rule (rule
					  (name copy-rule-rule-add)
					  (pred
					   (?r add ?a)
					   (?nn3 new-node sn3))
					  (add
					   (print copy-rule-rule-add ?r ?a ?nn3)
					   (?r rule (rule
								 (name copy-rule-rule-add-2)
								 (pred
								  (?r name ?name)
								  (?r add ?a) ;; connected!
								  (?a elem0 ?ae0)
								  (?a elem1 ?ae1)
								  (?a elem2 ?ae2))
								 (add
								  (print copy-rule-rule-add-2 ?r ?a ?nn1 ?nn3 ?ae0 ?ae1 ?ae2)
								  (?y rule ?nn1)
								  (?nn1 add ?nn3)
								  (?nn3 elem0 ?ae0)
								  (?nn3 elem1 ?ae1)
								  (?nn3 elem2 ?ae2)))))))))

		  (rule
		   (name fwd-fe-rule)
		   (root-var ?m)
		   (local)
		   (pred
			(?n sigma ?m)
			(?n even-func ?o)
			(?o sigma ?p)
			(?p sigma ?q))
		   (add
			(print fwd-fe-rule ?m even-func ?q)
			(?m even-func ?q)))

		  (rule
		   (name fwd-fe-rule-gen)
		   (root-var ?x)
		   (local)
		   (pred
			(?x sigma ?y)
			(?x even-func ?z)
			(?x fe ?t)
			(?x local-rule-pool ?p)
			(?p lrp-rule ?fwd-fe-rule)
			(?p lrp-rule ?copy-rule-rule)
			(?fwd-fe-rule name fwd-fe-rule)
			(?copy-rule-rule name copy-rule-rule))
		   (del
			(?x rule ?this-rule)
			(?x fe ?t))
		   (add
			(print fwd-fe-rule-gen ?x)
			(?fwd-fe-rule copy-rule ?y)
			(?fwd-fe-rule rule ?copy-rule-rule)
			(?y rule ?this-rule)))

		  (rule
		   (name fe-0-rule)
		   (local)
		   (pred
			(0 sigma 1)
			(0 local-rule-pool ?p)
			(?p lrp-rule ?fwd-fe-rule-gen)
			(?fwd-fe-rule-gen name fwd-fe-rule-gen))
		   (add
			(print fe-0-rule 0)
			(0 even-func 2)
			(0 rule ?fwd-fe-rule-gen)))

		  (rule
		   (name back-fe-rule-gen)
		   (root-var ?a)
		   (local)
		   (pred
			(?c sigma ?a)
			(?a local-rule-pool ?p)
			(?p lrp-rule ?fe-0-rule)
			(?fe-0-rule name fe-0-rule))
		   (add
			(print back-fe-rule-gen ?c)
			(?c fe ?x)
			(?c rule ?this-rule)
			(?c rule ?fe-0-rule))
		   (del
			(?a rule ?this-rule)))

		  (rule
		   (name back-fe-rule-gen1)
		   (root-var ?a)
		   (local)
		   (pred
			(?c sigma ?a))
		   (add
			(print back-fe-rule-gen1 ?c)
			(?c rule ?this-rule)
			(?c rule (rule
					  (name fwd-fe-rule1)
					  (root-var ?c)
					  (pred
					   (?c sigma ?d)
					   (?c even-func ?e)
					   (?e sigma ?f)
					   (?f sigma ?g))
					  (add
					   (print fwd-fe-rule1 ?d even-func ?g)
					   (?d even-func ?g)))))
		   (del
			(?a rule ?this-rule)))

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
		   (name is)
		   (root-var ?x)
		   (pred
			(?x is ?y)
			(?y rule ?r))
		   (del
			(?x is ?y))
		   (add
			(print is ?x ?y ?r)
			(dont-queue)
			(?x rule ?r)
			(?x from-is-rule ?r)))

		  (rule
		   (name xis)
		   (root-var ?x)
		   (pred
			(?x xis ?y)
			(?y xrule ?r))
		   (del
			(?x xis ?y))
		   (add
			(print xis ?this-obj ?x ?y ?r)
			;; (dont-queue)
			(?x rule ?r)
			(?x from-xis-rule ?r)))

		  #|
	  (rule
		  (name is-not)
		  (pred
		  (?x is-not ?y) ;; Put this...	; ; ; ; ;
		  (?x is ?y)	   ;; ...and detect this ; ; ; ; ;
		  (?y rule ?r)
		  (?x rule ?r))
		  (add
		  (print is-not ?x ?y ?r))
		  (del
		  (?x is ?y)
		  (?x is-not ?y)
		  (?x rule ?r)
		  (?this-obj rule ?this-rule)))
	  |#

		  ;; An array is a node denoted in a rule as ?a with a set of nodes
		  ;; denoted by property elem related by next

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
			(print even-zero ?a ?a1 ?ae0 ?e0)
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
			(print odd-zero ?a ?a1 ?ae0 ?e0)
			(?ae0 zero))
		   (del
			;; (?this-obj rule ?this-rule)
			))

		  (rule
		   (name self-cycle)
		   (local)
		   (root-var ?ae0)
		   (pred
			(?ae0 is-elem-of ?a1)
			(?ae0 ref ?e1)
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
			(?nn1 xis ev-od-obj)
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
			(print even-new ?a ?e0 ?nn1)
			(?nn1 is-elem-of ?a1) ;; Should have parent rule apply instead
			(?a1 elem ?nn1)
			(?nn1 value ?v)
			(?nn1 ref ?e0)
			(?nn1 xis ev-od-obj)
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
		   (name copy-array-struct-next)
		   (local)
		   (root-var ?ae0)
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
		   (root-var ?ae0)
		   (pred
			(?a copy-array-struct ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?e0 is-elem-of ?a)
			(?e0 next))
		   (add
			(print copy-array-struct-next-sing ?a ?a1 ?ae0 ?e0)
			(?ae0 next))
		   (del
			;; (?this-obj rule ?this-rule)  ;; Don't del
			))

		  (rule
		   (name copy-array-struct-zero)
		   ;; (local)		;;;;;;;;;;;;;;;
		   (root-var ?ae0) ;;;;;;;;;;;;
		   (pred
			(?a copy-array-struct ?a1)
			(?ae0 is-elem-of ?a1)
			(?ae0 ref ?e0)
			(?e0 zero))
		   (add
			(print copy-array-struct-zero ?this-obj ?a ?a1 ?ae0 ?e0)
			(?ae0 zero))
		   (del
			;; (?this-obj rule ?this-rule) ;; Don't del
			))

		  (rule
		   (name copy-array-struct-new)
		   (local)
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
			(?nn1 rule ?copy-array-struct-zero)
			(?nn1 rule ?copy-array-struct-next)
			(?nn1 rule ?copy-array-struct-next-sing))
		   (del
			;; (?this-obj rule ?this-rule) ;; Don't del
			))

		  (rule
		   (name fft-comb-rule-next-sing)
		   (local)
		   (root-var ?ey0)
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
			(?ef0 ?eg0 fft-hb ?ey0)
			(?ey0 local-rule-pool ?p)
			(?p lrp-rule ?fft-comb-rule-next-sing)
			(?fft-comb-rule-next-sing name fft-comb-rule-next-sing))
		   (add
			(print fft-comb-rule-next ?x0 ?x1 ?y ?ef0 ?ef1 ?eg0 ?eg1 ?ey0 ?ey1)
			;; (print fft-comb-rule-next ?x0 ?x1 ?y)
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
			))

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
			(?x randgen ?rg)
			(?rg val ?r)
			(?r center-up ?s)
			(?x local-rule-pool ?p)
			(?p lrp-rule ?copy-array-struct-new)
			(?p lrp-rule ?fft-comb-rule-zero)
			(?copy-array-struct-new name copy-array-struct-new)
			(?fft-comb-rule-zero name fft-comb-rule-zero))
		   (add
			(print fft-rule-zero ?x ?y ?r)
			(?x rand ?r)
			(?x copy-array-struct ?y)
			(?x d ?y)			;; display-connection
			(?y rand ?r)
			(?rg val ?s)
			(?x rule ?copy-array-struct-new)
			(?y rule ?fft-comb-rule-zero))
		   (del
			(?this-obj rule ?this-rule)
			(?rg val ?r)
			))

		  ;; This rule is global and bare-bones, with no rule-passing
		  ;; and related optimizations. It's modified explicitly by
		  ;; the optimizer in fft-delta.lisp.

		  (rule
		   (name fft-rule)
		   (pred
			(?x fft ?y)
			(?x level ?l)
			(?x randgen ?rg)
			(?rg val ?r)
			(?r center-up ?s)
			(?x color ?c)
			(?c left-color ?cl)
			(?c right-color ?cr)
			(?l1 sigma ?l)
			(?nn1 new-node sn1)
			(?nn2 new-node sn2)
			(?nn3 new-node sn3)
			(?nn4 new-node sn4))
		   (add
			(print fft-rule ?x ?y ?rg ?r ?s)
			(?x rand ?r)
			(?x even ?nn1)
			(?x odd ?nn2)
			(?nn1 oe ?x)
			(?nn2 oe ?x)
			(?nn1 oev 0)
			(?nn2 oev 1)
			(?x copy-array-struct ?y)
			(?nn1 fft ?nn3)
			(?nn2 fft ?nn4)
			(?nn3 ?nn4 fft-comb ?y)
			(?nn1 level ?l1)
			(?nn2 level ?l1)
			(?nn1 randgen ?rg)
			(?nn2 randgen ?rg)
			(?nn1 color ?cl)
			(?nn2 color ?cr)
			(?rg val ?s))
		   (del
			(?rg val ?r)
			(?this-obj rule ?this-rule)))

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
		   (name display-data)
		   (attach-to global-node)
		   (pred
			(global-node rule ?r)
			(?r name display-data))
		   (add
			(print display-data)
			(fft color green)
			(fft in-node-color springgreen)
			(fft out-node-color darkturquoise)
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
			(+ two-input-op))
		   (del
			(global-node rule ?this-rule)))

		  (rule
		   (name color-circle-data)
		   (attach-to global-node)
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
		   (name color-tree)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node next-color)
			(?x next-color ?y)
			(?y next-color ?z))
		   (add
			(print color-tree ?x ?y ?z)
			(?x left-color ?y)
			(?x right-color ?z)
			(?x color ?x))
		   (del
			(?this-obj rule ?this-rule)))

		  #|
		  (rule
		   (name add-color-perm)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node next-color)
			(?x next-color ?u)
			(?y next-color ?v))
		   (add
			(print add-color-perm ?x ?y)
			(?x perm ?y))
		   (del
			(?this-obj rule ?this-rule)))

		  (rule
		   (name prune-color-perm)
		   (attach-to global-node)
		   (root-var global-node)
		   (pred
			(global-node next-color)
			(?x next-color ?u)
			(?y next-color ?v)
			(?z next-color ?w)
			(?x perm ?y)
			(?x perm ?z))
		   (add
			(print prune-color-perm ?x ?y))
		   (del
			(?x perm ?z)))
		  |#

		  )))))
