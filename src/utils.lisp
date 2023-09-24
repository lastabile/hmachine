


;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental macros
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defr (&rest defls-and-forms)
  `(labels
	,(mapappend (lambda (defls)
				  (when (eq (first defls) 'defl)
					(list (rest defls))))
				defls-and-forms)
	,@(mapappend (lambda (forms)
				   (unless (eq (first forms) 'defl)
					 (list forms)))
				 defls-and-forms)))

(defmacro dolists (vars-lists &rest body)
  `(let ()
	 (mapc (lambda ,(first vars-lists) ,@body) ,@(second vars-lists))
	 nil))

(defmacro $comment (&rest e)
  nil)

(defmacro $nocomment (&rest e)
  `(let () ,@e))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine and system info
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-laptop ()
  (let ((mi (machine-instance)))
	(equal (subseq mi 0 (search " " mi)) "DESKTOP-0QT0FUH")))

;;;;;;;;;;;;;
;; Perf Stats
;;;;;;;;;;;;;

;; Was inside of perf-hash let but moved out in case performance will be improved

(defstruct timerec 
  (sum 0)
  (count 0)
  (max 0)
  (type 'time)) ;; { time, gen }

(defstruct logrec
  (name)
  (seqno 0)
  (value 0))

(let ((perf-hash (make-hash-table :test #'eq)))
  (let ((log-stat-list nil))
	(let ((seqno 0))
	  (let ((display-order ;; Any not in this list go on the end
			 '(
			   main
			   me-tested
			   me-matched
			   me-failed
			   me-matched-new-edges
			   me-matched-not-new-edges
			   me-efficiency
			   me-redundancy
			   me-failure
			   eo-tested
			   eo-matched
			   eo-failed
			   eo-matched-new-edges
			   eo-matched-not-new-edges
			   eo-efficiency
			   eo-redundancy
			   eo-failure
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
					(when (> deltat (timerec-max timerec))
					  (setf (timerec-max timerec) deltat))
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
					  (when (> deltat (timerec-max timerec))
						(setf (timerec-max timerec) deltat))
					  (setf (timerec-count timerec) (+ 1 (timerec-count timerec)))))))
			  v)))
		(defun gstat (name statfcn thunk)
		  (let ((v (funcall thunk)))
			(let ((timerec (gethash name perf-hash)))
			  (when (null timerec)
				(setq timerec (make-timerec :type 'gen))
				(setf (gethash name perf-hash) timerec))
			  (setf (timerec-sum timerec) (funcall statfcn v (timerec-sum timerec)))
			  (when (> v (timerec-max timerec))
				(setf (timerec-max timerec) v))
			  (setf (timerec-count timerec) (+ 1 (timerec-count timerec))))
			v))
		;;
		;; Log stats relative to a seqno in a form suitable for
		;; gnuplot. We need to think of good measures to capture. Tried
		;; with expand-len and expand-len-max, but it didn't seem to
		;; provide much info.  So disable for now.
		;;
		(defun log-stat (name value)
		  ($comment		;; The logging can be a little expensive so enable/disable as needed
		   (let ()
			 (setq log-stat-list (cons (make-logrec :name name :seqno seqno :value value) log-stat-list))
			 (setq seqno (+ seqno 1)))))
		(defun get-log-seqno ()
		  seqno)
		(defun get-log-stat-list  ()
		  log-stat-list)
		(defun write-log-stat-file (file)
		  (with-open-file (s file :direction :output)
			(let ((stat-hash (make-hash-table)))
			  (let ((stats (reverse (get-log-stat-list))))
				(let ((names '(index)))
				  (dolist (stat stats)
					(let ((name (logrec-name stat)))
					  (when (not (memq name names))
						(setq names (append names (list name))))))
				  (let ((n 1))
					(dolist (name names)
					  (format s "~(~a~)=~a " name n)		;; downcase the names
					  (setq n (+ n 1))))
				  (format s "~%")
				  (dolist (stat stats)
					(let ((stat-name (logrec-name stat)))
					  (let ((stat-value (logrec-value stat)))
						(let ((stat-seqno (logrec-seqno stat)))
						  (format s "~a " stat-seqno)
						  (dolist (name (rest names))
							(if (eq name stat-name)
								(format s "~a " stat-value)
								(format s "x ")))
						  (format s "~%"))))))))))

		(defun perf-stats (&key sort)
		  (defr
			(defl div (x y)
			  (if (= y 0) most-positive-single-float (/ x y)))
			(let ()
			  ;; me (match-end-execute) stats
			  (let ((eff (make-timerec :type 'gen))
					(red (make-timerec :type 'gen))
					(fail (make-timerec :type 'gen)))
				(let ((new-edges (timerec-sum (or (gethash 'me-matched-new-edges perf-hash) (make-timerec))))
					  (not-new-edges (timerec-sum (or (gethash 'me-matched-not-new-edges perf-hash) (make-timerec))))
					  (tested (timerec-sum (or (gethash 'me-tested perf-hash) (make-timerec))))
					  (matched (timerec-sum (or (gethash 'me-matched perf-hash) (make-timerec)))))
				  (setf (timerec-sum eff) (div (float new-edges) (float tested)))
				  (setf (timerec-count eff) 1)

				  ;; We used to calc it this way, and in some sense it's better than not-new-edges/tested, since it
				  ;; gives us a figure not dependent on the failure rate.  We're using not-new-edges/tested since we can
				  ;; then say eff%+redun%+fail%=1
				  ;;
				  ;; (setf (timerec-sum red) (div (float not-new-edges) (float matched)))

				  (setf (timerec-sum red) (div (float not-new-edges) (float tested)))
				  (setf (timerec-count red) 1)

				  (setf (timerec-sum fail) (div (float (- tested matched)) (float tested)))
				  (setf (timerec-count fail) 1)

				  (setf (gethash 'me-efficiency perf-hash) eff)
				  (setf (gethash 'me-redundancy perf-hash) red)
				  (setf (gethash 'me-failure perf-hash) fail)))

			  ;; eo (execute-obj) stats
			  (let ((eff (make-timerec :type 'gen))
					(red (make-timerec :type 'gen))
					(fail (make-timerec :type 'gen)))
				(let ((new-edges (timerec-sum (or (gethash 'eo-matched-new-edges perf-hash) (make-timerec))))
					  (not-new-edges (timerec-sum (or (gethash 'eo-matched-not-new-edges perf-hash) (make-timerec))))
					  (tested (timerec-sum (or (gethash 'eo-tested perf-hash) (make-timerec))))
					  (matched (timerec-sum (or (gethash 'eo-matched perf-hash) (make-timerec)))))
				  (setf (timerec-sum eff) (div (float new-edges) (float tested)))
				  (setf (timerec-count eff) 1)

				  (setf (timerec-sum red) (div (float not-new-edges) (float tested)))
				  (setf (timerec-count red) 1)

				  (setf (timerec-sum fail) (div (float (- tested matched)) (float tested)))
				  (setf (timerec-count fail) 1)

				  (setf (gethash 'eo-efficiency perf-hash) eff)
				  (setf (gethash 'eo-redundancy perf-hash) red)
				  (setf (gethash 'eo-failure perf-hash) fail)))

			  (let ((m 0)
					(d 14))
				(maphash (lambda (k v)
						   (let ((name k))
							 (let ((l (length (symbol-name name))))
							   (let ((l (+ l 2)))	;; Account for prefix marker
								 (when (> l m)
								   (setq m l))))))
						 perf-hash)
				(format t "~%~vtavg~vtmax~vtcount~vtsum~%" (+ m 5) (+ m 5 (* d 1)) (+ m 5 (* d 2)) (+ m 5 (* d 3)))
				(let ((extra-names nil))
				  (maphash (lambda (k v)
							 (let ((name k))
							   (when (not (member name display-order))
								 (setq extra-names (cons name extra-names)))))
						   perf-hash)
				  (let ((names (if sort
								   (sort-perf-stats)
								   (append display-order extra-names))))
					(dolist (name names)
					  (let ((timerec (gethash name perf-hash)))
						(when timerec
						  (let ((sum (timerec-sum timerec))
								(max (timerec-max timerec))
								(count (timerec-count timerec)))
							(let ((units (if (eq (timerec-type timerec) 'time)
											 internal-time-units-per-second
											 1)))
							  (let ((avg-time (/ (float (div (float sum) count)) units))
									(sum-time (/ (float sum) units))
									(max-time (/ (float max) units)))
								(format t
										"||~a~vt~a~vt~a~vt~a~vt~a~%"
										name (+ m 5) avg-time (+ m 5 (* d 1)) max-time (+ m 5 (* d 2)) count (+ m (* d 3)) sum-time)))))))))
				(format t "~%")
				nil))))
		(defun sort-perf-stats ()
		  (defr
			(defl get-sum (timerec)
			  (if (eq (timerec-type timerec) 'time)
				  (float (/ (timerec-sum timerec)  internal-time-units-per-second))
				  (timerec-sum timerec)))
			(let ((l nil))
			  (maphash (lambda (k v)
						 (let ((name k))
						   (let ((timerec v))
							 (setq l (cons (list name timerec) l)))))
					   perf-hash)
			  (mapcar (lambda (e) (first e)) (sort l (lambda (e1 e2) (> (get-sum (second e1)) (get-sum (second e2)))))))))
		(defun clear-perf-stats ()
		  (setq perf-hash (make-hash-table :test #'eq))
		  (setq log-stat-list nil)
		  (setq seqno 0))
		(defun get-perf-hash () perf-hash)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set functions
;;
;; The general rule used for these functions is that they preserve
;; order. Names have been changed wrt CL to avoid conflict.
;;
;;		CL					H				Ordering rule
;;	union				hunion					Elements of x are included in order, followed by elements of y in order,
;;													omitting elements of y that are already in x.
;;	intersection		intersect				Elements of x and not in y are included in order,
;;													followed by elements of y in order that are not in x.
;;	set-difference		set-subtract			Elements of x and not in y are included in order.
;;	set-exclusive-or
;;	subsetp
;;  adjoin
;;						set-equal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1/26/19 Modified these intersection and union operations to preserve order

;; See notes in previous file versions. This basic n^2 loop wins in
;; speed over other versions using hash tables.
;; 

(let ((p0 (list nil)))		;; Note uses equal and test is not passed in
  (defun intersect (l1 l2)
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
		r)))))

(let ((p0 (list nil)))
  (defun intersect-with-test (l1 l2 &key (test #'equal))
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
		r)))))

;; 10/9/22 Ran some tests against CL union w/ :test equal and hunion
;; does better on 100 element lists. Note C and Lisp source code
;; available for clisp. See eg c:/Users/lstabile/clisp-2.49/src/defs1.lisp or
;; c:/Users/lstabile/clisp-2.49/src/hashtabl.d

(let ((test #'equal))
  (let ((h (make-hash-table :size 1021 :test test))) ;; 32768
	(let ((p0 (list nil)))
	  (defun hunion (l1 l2)
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
			  r))))))))

(let ((test #'eq))
  (let ((h (make-hash-table :size 1021 :test test))) ;; 32768
	(let ((p0 (list nil)))
	  (defun eq-hunion (l1 l2)
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
			  r))))))))

(defun set-equal (l1 l2 &key (test #'equal))
  (= (length l1)
	 (length l2)
	 (length (intersect-with-test l1 l2 :test test))))

(defun set-hash (l)
  (defr
	(defl h (l)
	  (cond
	   ((null l)
		0)
	   ((not (listp l))
		(sxhash l))
	   (t
		(+ (h (first l)) (h (rest l))))))
	(h l)))

(define-hash-table-test set-hash-test set-equal set-hash)

;; l1 \ l2
;;
;; Uses CL set-difference, which retains ordering as desired.

(defun set-subtract (l1 l2 &key (test #'equal))
  (set-difference l1 l2 :test test))

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
	  (if (= (length lists) 1)
		  (xmapcand1 fcn (first lists) nil)
		  (xmapcand fcn lists nil)))
	(defun mapsunion (fcn &rest lists)
	  (if (= (length lists) 1)
		  (xmapcand1 fcn (first lists) t)
		  (xmapcand fcn lists t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum (from to fcn)	;; fcn is (lambda (i) ...)
  (let ((acc 0))
    (do ((i from (+ i 1)))
		((> i to) acc)
		(setq acc (+ acc (funcall fcn i))))
	acc))

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

;; This version using clrhash tested about twice as fast as the
;; version where the table is created each call

(let ((objhash (make-hash-table :size 1281 :test #'equal)))
  (defun dedup-list (list &key (id-func (lambda (x) x)))
	(clrhash objhash)
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

;; Like mapcar, but omits null results from the fcn call.
;;
;; Retains order of incoming list, i.e., (e1 e2 ...) => ((f e1) (f e2) ...)
;;
;; Note mapappend defined in hoss.lisp
;;

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

(defun memq (x l)
  (member x l :test #'eq))

(defun memqq (x l)
  (member x l :test (lambda (x y) (and (eq (first x) (first y)) (eq (second x) (second y))))))

;; Accepts an arbitrary number of args of any type and cats their
;; printed representations together into a single interned symbol.

(defun symcat (&rest l)
  (defr
	(defl format-string (n)
	  (let ((r ""))
		(dotimes (i n)
		  (setq r (concatenate 'string r "~a")))
		r))
	(let ((len (length l)))
	  (intern (apply #'format nil (format-string len) l)))))

;;;;;;;;;;;;;
;; Streams
;;;;;;;;;;;;;

(defun stream-first (stream)
  (first stream))

(defun stream-rest (stream)
  (funcall (second stream)))

(defun stream-to-list (stream)
  (when stream
	(cons (stream-first stream) (stream-to-list (stream-rest stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized depth- and breadth-first search
;;
;; node is any object, and the children fcn returns a list of node
;; objects, given the node. A children fcn returning nil is presumed
;; to terminate some part of the object structure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generalized depth-first search. Start with node, children-fcn
;; gets list of child nodes and level. It also does whatever node
;; processing is required. In this manner, if the node processing
;; so dictates, the function can return nil, thus pruning the
;; search.
;;
;; In this depth case, the children fcn should process node, and return the list of children 
;;
;; Returns list of all cycle heads
;;
;; (children-fcn node level)
;;
;; Example:
;;
;;     (! (g depth) 'init (lambda (node level) (print node) (mapcar (lambda (x) (first x)) (! (g query) `((rd 1 ,node "" "" ?n2)) '(?n2)))))

(defun depth (node children-fcn)
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

;; Generalized breadth-first search. Start with node, children-fcn
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

(defun breadth (node children-fcn)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some basic utils for hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gather
;;
;; (gather <list> <fcn>)
;;
;; <fcn> == (lambda (x) ...) => key, x element-of <list>
;;
;; <fcn> takes an element of <list> and returns a key represnting some common attribute of a subset of the elements of
;; <list>. It returns a list of pairs (<key> (x1 ... xn)), where xi is an element of <list>.
;;
;; Example: (gather '((a 1) (b 2) (a 2) (c 3) (b 4)) (lambda (x) (first x))) =>
;;						((a ((a 2) (a 1))) (b ((b 4) (b 2))) (c ((c 3))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gather (l f)
  (let ((r nil))
	(let ((h (make-hash-table :test #'equal)))
	  (dolist (x l)
		(setf (gethash (funcall f x) h) (cons x (gethash (funcall f x) h))))
	  (maphash (lambda (k v)
				 (setq r (cons (list k v) r)))
			   h)) 
	r))

(defun rgather (l)
  (defr
	(defl p (prefix-tree)
	  (let ((pt prefix-tree))
		(when pt
		  (print (first pt))
		  (dolist (x (rest pt))
			(p x)))))
	(defl r (l)
	  (when (not (null (mapcad (lambda (x) x) l)))
		(let ((r nil))
		  (let ((h (make-hash-table :test #'equal)))
			(dolist (x l)
			  (setf (gethash (first x) h) (cons (rest x) (gethash (first x) h))))
			(maphash (lambda (k v)
					   (print v)
					   (setq r (cons (list k (r v)) r)))
					 h)) 
		  r)))
	(let ((s (r l)))
	  ;; (p s)
	  s)))

;; Shell commands. It's maddening working around the crlf crap. The igncr bash arg lets script files work by ignoring
;; the \r. Also I don't completely understand why I need a separate version for script files but I'll live with it for
;; now.

(defun shell-script-file (script-file)
  (print script-file)
  (run-program "bash" :arguments (list "-o" "igncr" script-file)))		;; Takes a script file name

(defun shell-cmd (cmd)
  (print cmd)
  (run-program "bash" :arguments (list "-o" "igncr" "-c" cmd)))	;; Takes a command with args 

;;
;; Fcn == (lambda (stdout-stream) ...)

(defun with-redirected-stdout (file fcn)
  (with-open-file (s file :direction :output)
	  (let ((std *standard-output*))
		(let ((*standard-output* s))
		  (funcall fcn std)))))

;; Local Variables:
;; eval: (put 'execute-obj 'lisp-indent-function 'defun)
;; eval: (put 'add-consequent-edges 'lisp-indent-function 'defun)
;; eval: (put 'dolists 'lisp-indent-function 'defun)
;; eval: (put 'macrolet 'lisp-indent-function 'defun)
;; fill-column: 120
;; End:
