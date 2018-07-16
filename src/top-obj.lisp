(defc top-obj nil nil
  (let ((self nil))
	(defm set-self (s)
	  (setq self s))
	(defm get-self ()
	  self)
	(defm init ()
	  nil)))

  
