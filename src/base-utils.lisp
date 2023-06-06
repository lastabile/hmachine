


(defun mapappend (fcn l)
  (if (null l)
	  nil
	  (append (funcall fcn (first l)) (mapappend fcn (rest l)))))