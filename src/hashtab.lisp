;; Simple fixed-block non-expandable hash table. Not a defc for max performance

(defsetf hgethash hputhash)		;; Needs to be defined early for macro expansion purposes

(defstruct hashtab
  (block nil)
  (size nil)
  (test nil)
  (count 0))

(defun hmake-hash-table (&key (size 17) (test #'equal))
  (let ((h (make-hashtab)))
	(setf (hashtab-block h) (make-array size :initial-element nil))
	(setf (hashtab-size h) size)
	(setf (hashtab-test h) test)
	h))

(defun hgethash (key hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab))
		(test (hashtab-test hashtab)))
	(let ((hk (mod (sxhash key) s)))
	  (let ((l (svref b hk)))
		(dolist (e l)
		  (when (funcall test (first e) key)
			(return-from hgethash (rest e)))))))
  nil)

(defun hputhash (key hashtab value)
  (block b
	(let ((s (hashtab-size hashtab))
		  (b (hashtab-block hashtab))
		  (test (hashtab-test hashtab)))
	  (let ((hk (mod (sxhash key) s)))
		(let ((l (svref b hk)))
		  (dolist (e l)
			(when (funcall test (first e) key)
			  (setf (rest e) value)
			  (return-from b nil)))
		  (setf (hashtab-count hashtab) (+ (hashtab-count hashtab) 1))
		  (setf (svref b hk) (cons (cons key value) (svref b hk)))))))
  value)

(defun hremhash (key hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab))
		(test (hashtab-test hashtab)))
	(let ((hk (mod (sxhash key) s)))
	  (let ((l (svref b hk)))
		(cond 
		 ((null l)
		  (return-from hremhash nil))
		 ((funcall test (first (first l)) key)
		  (setf (svref b hk) (rest l))
		  (setf (hashtab-count hashtab) (- (hashtab-count hashtab) 1))
		  (return-from hremhash t))
		 (t
		  (loop
		   (when (null l)
			 (return-from hremhash nil))
		   (let ((e (first (rest l))))
			 (when (funcall test (first e) key)
			   (setf (rest l) (rest (rest l)))
			   (setf (hashtab-count hashtab) (- (hashtab-count hashtab) 1))
			   (return-from hremhash t))
			 (setq l (rest l)))))))))
  nil)

(defun hclrhash (hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab)))
	(dotimes (i s)
	  (setf (aref b i) nil))
	hashtab))

(defun hmaphash (fcn hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab)))
	(dotimes (i s)
	  (let ((l (svref b i)))
		(dolist (e l)
		  (funcall fcn (first e) (rest e))))))
  nil)

(defun hhash-table-count (hashtab)
  (hashtab-count hashtab))

(defun hhash-table-to-list (h)
  (let ((r nil))
	(hmaphash (lambda (k v)
			   (setq r (cons (list k v) r)))
			 h)
	r))

(defun hhash-table-key-to-list (h)
  (let ((r nil))
	(hmaphash (lambda (k v)
			   (setq r (cons k r)))
			 h)
	r))

(defun hhash-table-value-to-list (h)
  (let ((r nil))
	(hmaphash (lambda (k v)
			   (setq r (cons v r)))
			 h)
	r))

;; Some baseic utils for CL hash tables too

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

(defun mapappend (fcn l)
  (if (null l)
	  nil
	  (append (funcall fcn (first l)) (mapappend fcn (rest l)))))

