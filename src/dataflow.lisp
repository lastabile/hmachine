






(defc dataflow nil
  (let ((fcn nil)
		(ninputs 0)
		(ninputsmax 2)
		(output-set nil)
		(noutputsmax 2)
		(input-count 0))
	(let ((inputs (make-array ninputsmax))
		  (outputs (make-array noutputsmax)))		;; Each entry is of the form (dataflow inputno)

	  (defm set-input-value (inputvalue inputno)
		(let ()
		  (setf (aref inputs inputno) inputvalue)
		  (setq input-count (+ input-count 1))
		  (when (= input-count ninputs)
			(setq input-count 0)
			(let ((value (! (fcn execute) inputs)))
			  (if output-set
				  (dotimes (i noutputsmax)
					(let ((outrec (aref outputs i)))
					  (when outrec
						(let ((fcn (first outrec))
							  (inputno (second outrec)))
						  (! (fcn set-input-value) value inputno)))))
				  (print `(value ,value)))))
		  nil))

	  (defm set-output (dataflow inputno &optional (outputno 0))
		(setq output-set t)
		(setf (aref outputs outputno) (list dataflow inputno))
		nil)

	  (defm set-ninputs (n)
		(setq ninputs n)
		nil)

	  (defm set-fcn (f)
		(setq fcn f))

)))


(defc function nil
  (let ()
	(defm execute (args)
	  nil)))

(defc plus-fcn function
  (let ()
	(defm execute (args)
	  (+ (aref args 0) (aref args 1)))))

(defc square-fcn function
  (let ()
	(defm execute (args)
	  (* (aref args 0) (aref args 0)))))

(let ((plus (make-dataflow))
	  (plus-fcn (make-plus-fcn))
	  (square (make-dataflow))
	  (square-fcn (make-square-fcn)))
  (! (plus set-fcn) plus-fcn)
  (! (plus set-ninputs) 2)
  (! (square set-fcn) square-fcn)
  (! (square set-ninputs) 1)
  (! (plus set-output) square 0)
  (setq p plus))





#|

(f (g x1) (h x2 (a (b x3))))



(def-df-block (fcn nargs)
 


|#

