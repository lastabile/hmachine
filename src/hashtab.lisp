;; Simple fixed-block non-expandable hash table. Not a defc for max performance
;; We don't want to load hoss before this, so we use the basics, e.g. labels, directly.
;;
;; 5/11/23 This file is not currently used, as we went to CL hash
;; tables with the define-hash-table-test clisp extension. But abcl
;; and others will break. So someday we might want to restore this.

(defsetf hgethash hputhash)		;; Needs to be defined early for macro expansion purposes

(defstruct hashtab
  (block nil)
  (size nil)
  (test nil)
  (hash nil)
  (count 0))

(defun hmake-hash-table (&key (size 17) (test #'equal) (hash #'sxhash))
  (let ((h (make-hashtab)))
	(setf (hashtab-block h) (make-array size :initial-element nil))
	(setf (hashtab-size h) size)
	(setf (hashtab-test h) test)
	(setf (hashtab-hash h) hash)
	h))

(defun hgethash (key hashtab)
  (let ((s (hashtab-size hashtab))
		(b (hashtab-block hashtab))
		(test (hashtab-test hashtab))
		(hash (hashtab-hash hashtab)))
	(let ((hk (mod (funcall hash key) s)))
	  (let ((l (svref b hk)))
		(dolist (e l)
		  (when (funcall test (first e) key)
			(return-from hgethash (rest e)))))))
  nil)

(defun hputhash (key hashtab value)
  (block b
	(let ((s (hashtab-size hashtab))
		  (b (hashtab-block hashtab))
		  (test (hashtab-test hashtab))
		  (hash (hashtab-hash hashtab)))
	  (let ((hk (mod (funcall hash key) s)))
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
		(test (hashtab-test hashtab))
		(hash (hashtab-hash hashtab)))
	(let ((hk (mod (funcall hash key) s)))
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

