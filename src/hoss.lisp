;;
;; Hoss -- the H-Machine Object System
;;
;;		Like Hoss on Bonanza, it's simple but effective and sometimes elegant.
;;

(let ((class-info (make-hash-table :test #'equal))
	  (class-name-list-table (make-hash-table :test #'equal)))
  (defstruct class-entry
	(class-name)
	(superclass-name)
	(make-args)
	(body)
	(n-created 0))
  (defun add-class (class-name superclass-name make-args body)
	(setf (gethash class-name class-info) (make-class-entry :class-name class-name :superclass-name superclass-name :make-args make-args :body body)))
  (defun add-class-name-list (class-name-list)
	(setf (gethash class-name-list class-name-list-table) t))
  (defun class-name-list-added (class-name-list)
	(let ((r (eq (gethash class-name-list class-name-list-table) t)))
	  r))
  (defun clear-class-name-list-table ()
	(setq class-name-list-table (make-hash-table :test #'equal)))
  (defun get-make-args (class-name)
	(let ((info (gethash class-name class-info)))
	  (class-entry-make-args info)))
  (defun get-body (class-name)
	(let ((info (gethash class-name class-info)))
	  (class-entry-body info)))
  (defun get-superclass-name (class-name)
	(let ((info (gethash class-name class-info)))
	  (class-entry-superclass-name info)))
  (defun get-inheritance-chain (class-name)
	(if (null class-name)
		nil
	    (append (get-inheritance-chain (get-superclass-name class-name)) (list class-name))))
  (defun add-make-call (class-name)
	(let ((info (gethash class-name class-info)))
	  (setf (class-entry-n-created info) (+ (class-entry-n-created info) 1))
	  nil))
  (defun class-stats ()
	(let ((r nil))
	  (maphash (lambda (k v)
				 (let ((info v))
				   (setq r (cons (list (class-entry-class-name info)
									   (class-entry-n-created info))
								 r))
				   nil))
			   class-info)
	  r))
  (defun clear-class-stats ()
	(maphash (lambda (k v)
			   (let ((info v))
				 (setf (class-entry-n-created info) 0)))
			 class-info))
  (defun class-info ()		;; For debugging
	class-info))

;; If it's clisp, ! is pre-defined as factorial, and one must go
;; through package locking hackery. Package locking is not part of the
;; CL standard and clisp has it, also looks like Franz does as well.

(if (and (find-package 'cs-common-lisp)		;; This package looks like it's unique to clisp
		 (fboundp '!))
	(eval (read-from-string "(ext:without-package-lock (\"EXT\")
								   (defmacro ! (fcn &rest args)
									 `(funcall (funcall ,(first fcn) ',(second fcn)) (list ,@args))))))"))
	(defmacro ! (fcn &rest args)
	  `(funcall (funcall ,(first fcn) ',(second fcn)) (list ,@args))))

;; Experiment: 
;; Basic multi-value let that simply binds vars to successive members of a list.
;; Only supports a single binding clause.
;;
;; (mlet (((x y z) '(1 2 3)))
;;   (list x y z)) => (1 2 3)
;;

(defmacro mlet (clause &rest body)
  (let ((clause (first clause)))
	(let ((init-var (gensym)))
	  `(let ((,init-var ,(second clause)))
		 ,(mlet-fcn init-var (first clause) 0 body)))))

(defun mlet-fcn (init-var bound-vars index body)
  (if (null bound-vars)
	  `(progn ,@body)
		(let ((var (first bound-vars)))
		  `(let ((,var (nth ,index ,init-var)))
			 ,(mlet-fcn init-var (rest bound-vars) (+ index 1) body)))))

(defmacro defc (class-name superclass-name make-args body)
  (if (and (not (eq class-name 'top-obj))
		   (null superclass-name))
	  (defc-fcn class-name 'top-obj make-args body)
	  (defc-fcn class-name superclass-name make-args body)))

(defun defc-fcn (class-name superclass-name make-args body)
  (let ((method-name-table (make-hash-table :test #'eq)))
	(defr
	  (defl symbol-cat (x y z)
		(intern (concatenate 'string (symbol-name x) (symbol-name y) (symbol-name z))))
	  (defl get-inherited-make-args (class-name)
		(let ((class-name-list (reverse (get-inheritance-chain class-name)))) ;; Get bottom-up, left-to-right
		  (let ((req-args nil))
			(let ((key-args nil))
			  (dolist (class-name class-name-list)
				(let ((args (get-make-args class-name)))
				  (let ((key nil))
					(dolist (arg args)
					  (if (eq arg '&key)
						  (setq key t)
						  (if key
							  (setq key-args (append key-args (list arg)))
							  (setq req-args (append req-args (list arg)))))))))
			  (append req-args (when key-args '(&key)) key-args)))))
	  (defl process-class (class-name-list)
		(defr
		  (defl process-defms (defms)
			(let ((subexprs nil))
			  `((labels (,@(mapappend (lambda (d)
										(if (not (eq (first d) 'defm))
											(let ()
											  (setq subexprs (append subexprs (list d)))
											  nil)
											(let ((method-name (second d))
												  (args (third d))
												  (class-name (first class-name-list)))
											  (setf (gethash method-name method-name-table) t)
											  (let ((class-method-name (symbol-cat class-name '- method-name)))
												`(,@(when (eq method-name 'init)
													  `((,class-method-name (&rest all-args) (apply (function ,method-name) all-args))))
												  ,(if (traced method-name)
													   `(,method-name ,args
																	  (enter-trace ',method-name (list ,@(filter-arg-list args)))
																	  (let ((r (let ((am-traced t)) ,@(rest (rest (rest d))))))
																		(exit-trace ',method-name r)
																		r))
													   `(,method-name ,args
																	  (let ((r (let ((am-traced nil)) ,@(rest (rest (rest d))))))
																		r))))))))
									  defms))
						,@(mapcar (lambda (d)
									(if (not (eq (first d) 'defm))
										(let ()
										  nil)
										(let ((method-name (second d))
											  (class-name (first class-name-list)))
										  (let ((class-method-name (symbol-cat class-name '- method-name)))
											`(let ()
											   (setf (gethash ',method-name method-lookup-hash) (function ,method-name))
											   ;; 6/30/26 Removed this as it's very unlikely to be invoked from outside the class
											   #| (setf (gethash ',class-method-name method-lookup-hash) (function ,method-name)) |# )))))
								  defms)
						,@(mapcar (lambda (e) (process-body e)) subexprs)
						,(process-class (rest class-name-list))))))
		  (defl process-body (body)
			(let ((e body))
			  (cond ((and (listp e)
						  (listp (first e))
						  (eq (first (first e)) 'defm))
					 (process-defms e))
					((or (null e) (not (listp e)))
					 e)
					(t 
					 (cons (process-body (first e))
						   (process-body (rest e)))))))
		  (if (or (null class-name-list)
				  (class-name-list-added class-name-list))
			  nil
			  (let ((body (get-body (first class-name-list))))
				(add-class-name-list class-name-list)
				(process-body body)))))
	  (clear-class-name-list-table)
	  (add-class class-name superclass-name make-args body)								;; Need to add the class both at compile-time...
	  (let ((inherited-make-args (get-inherited-make-args class-name)))
		(let ((processed-class (process-class (get-inheritance-chain class-name))))
		  (let ((hash-size (nearest-prime-greater-than (* 3 (hash-table-count method-name-table)))))
			`(let ()
			   (add-class ',class-name ',superclass-name ',make-args ',body)			;; ...and run time. -- ?? Is this still useful?
			   (defun ,(intern (format nil "MAKE-~a" class-name)) (,@inherited-make-args)
				 (add-make-call ',class-name) ;; Activate this to record class stats
				 (let ((method-lookup-hash (make-hash-table :test #'eq :size ,hash-size)))
				   (declare (optimize (speed 3)))
				   (let ()
					 ,processed-class
					 (let ((self (lambda (mname)
								   (let ((m (gethash mname method-lookup-hash)))
									 (lambda (x)
									   (apply m x))))))
					   (! (self set-self) self)
					   (! (self init))
					   self)))))))))))

(defun filter-arg-list (arglist)
  (mapappend (lambda (arg)
			   (cond
				((member arg '(&key &rest &optional))
				 nil)
				((listp arg)
				 (list (first arg)))
				(t (list arg))))
			 arglist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((trace-level 0))
  (defun traced (fcn)
	(member fcn
			'(
			  ;; execute-obj
			  ;; execute-global-all-objs-loop
			  ;; match-and-execute-rule
			  ;; subst-match
			  ;; infer-root-rule-var check-rule-edges check-obj-containment
			  ;; all-matches
			  ;; all-matches-aux
			  ;; all-matches-aux2
			  ;; subst-match
			  ;; get-root-vars
			  ;; get-rule-components
			  ;; infer-root-var
			  ;; possible-match-fcn
			  ;; xcross-intersect-rule-edges
			  ;; e-match-pat-obj-edge-lists
			  ;; check-rule-edges
			  ;; var-match-filter-edges
			  ;; match-pat-obj-edge-lists
			  ;; x-match-all-pat-obj-edge-lists
			  ;; match-one-edge
			  ;; any-binding-invalid
			  ;; get-children
			  ;; get-rule-children
			  ;; bipartite-breadth-rule-walk
			  ;; bipartite-breadth-rule-walk-seq
			  ;; expand-edges
			  ;; expand-rule-obj-edges
			  ;; hget-all
			  ;; hget-inverse-all
			  ;; m1 m2
			  ;; add-consequent-edges
			  ;; del-consequent-edges
			  ;; rule-has-been-triggered
			  ;; env-remove-bindings
			  ;; insert
			  ;; lookup
			  ;; insert-triggered
			  ;; remove-edge
			  ;; edge-exists
			  ;; rem-edge
			  ;; add-edge
			  ;; rule-status-add-edge
			  ;; define-rule
			  ;; queued
			  ;; get-top-n
			  ;; cross-aux2 is defun'ed
			  ;; matched-edges
			  ;; e-filter-by-common-nodes
			  ;; rule-has-failed-enough
			  ;; queue-node
			  ;; push-tail
			  ;; pop-head
			  ;; push-head
			  ;; pop-tail
			  ;; remove
			  ;; query 
			  ;; query1 query2
			  ;; init
			  ;; objgraph-init
			  ;; rem-subset
			  ;; rem-subsets
			  ;; get-edges-from-subqet
			  ;; add-subqets
			  ;; add-subqet
			  ;; rem-subqets
			  ;; rem-subqet
			  ;; subqets
			  ;; superqets
			  ;; add-subset
			  ;; subqets-length-n
			  ;; is-subqet
			  ;; qet-exists
			  ;; list-to-edge-elem-node
			  ;; edge-elem-node-to-list
			  ;; get-rule-neighborhood
			  ;; get-entry 
			  ;; update-tested 
			  ;; update-matched 
			  ;; get-matched 
			  ;; update-failed 
			  ;; update-new-edges 
			  ;; update-not-new-edges 
			  ;; update-last-expand-len 
			  ;; update-new-edges-max-expand-len 
			  )))
    (defun incr-trace-level ()
	  (setq trace-level (+ trace-level 2)))
    (defun decr-trace-level ()
	  (setq trace-level (- trace-level 2)))
	(defun enter-trace (fcn args)
	  (format t "~%")
	  (dotimes (i trace-level)
		(format t " "))
	  (format t "-> ~a ~a" fcn args)
	  (incr-trace-level))
	(defun exit-trace (fcn args)
	  (decr-trace-level)
	  (format t "~%")
	  (dotimes (i trace-level)
		(format t " "))
	  (format t "<- ~a ~a" fcn args)))
