
;; 6/30/26 Used to have get-self method, but there does not seem to be
;; a use for it, so got rid of it since it's just extra perf drag.

(defc top-obj nil nil
  (let ((self nil))
	(defm set-self (s)
	  (setq self s))
	(defm init ()
	  nil)))
