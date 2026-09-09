

























;; Coding the defun such that the access to the value is local seems likely to be best perf.

(let ((local-cl-type *cl-type*))
  (defun cl-type ()
	local-cl-type))

(defmacro when-cl-type (cl-type form)
  (when (eq (cl-type) cl-type)
	form))

(defun mapappend (fcn l)
  (if (null l)
	  nil
	  (append (funcall fcn (first l)) (mapappend fcn (rest l)))))

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


;; Local Variables:
;; eval: (emacs-file-locals)
;; End:

