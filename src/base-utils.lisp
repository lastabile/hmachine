

























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

;; Local Variables:
;; eval: (emacs-file-locals)
;; End:

