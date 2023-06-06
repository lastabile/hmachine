;;
;; This stuff just for command line
;;

;; (w "xxx" (lambda (s) (print x s)))

(defun w (filename fcn)
  (with-open-file (s filename :direction :output)
	(funcall fcn s)))

