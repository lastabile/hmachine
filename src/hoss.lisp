
(let ((class-info (make-hash-table :test #'equal)))
  (defstruct class-entry
	(class-name)
	(body)
	(superclass-name)
	(n-created 0))
  (defun add-class (class-name superclass-name body)
	(setf (gethash class-name class-info) (make-class-entry :class-name class-name :superclass-name superclass-name :body body)))
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
	  r)))

(defmacro defr (&rest defls-and-forms)
  `(labels
	,(mapcan (lambda (defls)
			   (when (eq (first defls) 'defl)
				 (list (rest defls))))
			 defls-and-forms)
	,@(mapcan (lambda (forms)
			   (unless (eq (first forms) 'defl)
				 (list forms)))
			  defls-and-forms)))

(defmacro dolists (vars-lists &rest body)
  `(let ()
	 (mapc (lambda ,(first vars-lists) ,@body) ,@(second vars-lists))
	 nil))

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

(defmacro defc (class-name superclass-name make-args body)
  (if (and (not (eq class-name 'top-obj))
		   (null superclass-name))
	  (defc-fcn class-name 'top-obj make-args body)
	  (defc-fcn class-name superclass-name make-args body)))

(defun defc-fcn (class-name superclass-name make-args body)
  (defr
	(defl symbol-cat (x y z)
	  (intern (concatenate 'string (symbol-name x) (symbol-name y) (symbol-name z))))
	(defl process-class (class-name-list)
	  (defr
		(defl process-defms (defms)
		  `((labels (,@(mapcan (lambda (d)
								 (let ((method-name (second d))
									   (args (third d))
									   (class-name (first class-name-list)))
								   (let ((class-method-name (symbol-cat class-name '- method-name)))
									 `((,class-method-name (&rest all-args) (apply (function ,method-name) all-args))
									   ,(if (traced method-name)
											`(,method-name ,args
														   (enter-trace ',method-name (list ,@(filter-arg-list args)))
														   (let ((r (let ((am-traced t)) ,@(rest (rest (rest d))))))
															 (exit-trace ',method-name r)
															 r))
											`(,method-name ,args
														   (let ((r (let ((am-traced nil)) ,@(rest (rest (rest d))))))
															 r)))))))
							   defms))
					,@(mapcar (lambda (d)
								(let ((method-name (second d))
									  (class-name (first class-name-list)))
								  (let ((class-method-name (symbol-cat class-name '- method-name)))
									`(let ()
									   (setf (hgethash ',method-name method-lookup-hash) (function ,method-name))
									   (setf (hgethash ',class-method-name method-lookup-hash) (function ,method-name))))))
							  defms)
					,(process-class (rest class-name-list)))))
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
		(if (null class-name-list)
			`(lambda (mname)
			   (let ((m (hgethash mname method-lookup-hash)))
				 (lambda (x)
				   (apply m x))))
			(let ((body (get-body (first class-name-list))))
			  (process-body body)))))
	(add-class class-name superclass-name body)
	`(defun ,(intern (format nil "MAKE-~a" class-name)) (,@make-args)
	   ;; (add-make-call ',class-name)        ;; Activate this to record class stats
	   (let ((method-lookup-hash (hmake-hash-table :test #'eq :size 257)))
		 (let ((self ,(process-class (get-inheritance-chain class-name))))
		   (! (self set-self) self)
		   (! (self init))
		   self)))))

(defun filter-arg-list (arglist)
  (mapcan (lambda (arg)
			(cond
			 ((member arg '(&key &rest &opt))
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
			  ;; match-and-execute-rule
			  ;;  match-and-execute-rule-on-edges
			  ;; all-matches  all-matches-aux  infer-root-rule-var check-rule-edges check-obj-containment
			  ;; get-root-vars
			  ;; infer-root-var
			  ;; all-matches
			  ;; all-matches-aux
			  ;; possible-match-fcn
			  ;; xcross-intersect-rule-edges
			  ;; e-match-pat-obj-edge-lists
			  ;; check-rule-edges
			  ;; var-match-filter-edges
			  ;; match-pat-obj-edge-lists
			  ;; x-match-all-pat-obj-edge-lists
			  ;; match-one-edge
			  ;; get-children
			  ;; get-rule-children
			  ;; bipartite-breadth-rule-walk
			  ;; bipartite-breadth-rule-walk-seq
			  ;; expand-edges
			  ;; expand-rule-obj-edges
			  ;; hget-all
			  ;; m1 m2
			  ;; add-consequent-edges
			  ;; del-consequent-edges
			  ;; ;; rule-has-been-triggered
			  ;; ;; env-remove-bindings
			  ;; insert
			  ;; lookup
			  ;; insert-triggered
			  ;; remove-edge
			  ;; edge-exists
			  ;; rem-edge
			  ;; add-edge
			  ;; rule-status-add-edge
			  ;; define-rule
			  ;; is-queued
			  ;; get-top-n
			  ;; cross-aux2 is defun'ed
			  ;; matched-edges
			  ;; e-filter-by-common-nodes
			  ;; rule-has-failed-enough
			  ;; queue-node
			  ;; query 
			  ;; query1 query2
			  ;; init
			  ;; objgraph-init
			  ;; rem-subset
			  ;; rem-subsets
			  ;; get-edges-from-subqet
			  ;; add-subqets
			  ;; add-subset
			  ;; subqets-length-n
			  ;; qet-exists
			  ;; do-eval
			  ;; depth
			  ;; list-to-edge-elem-node
			  ;; edge-elem-node-to-list
			  ;; get-rule-neighborhood
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