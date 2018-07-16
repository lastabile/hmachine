
(cond
 ((find-package 'cs-common-lisp)		;; CLISP
  (let ()
	(load "ht-clisp.lisp")))
 ((find-package 'sb-int)				;; SBCL (Steel Bank Common Lisp)
  (let ()
	(load "ht-sbcl.lisp")))
 (t										;; Assume abcl until others come along
  (let ()
	(load "ht-abcl.lisp"))))

(defun hcompile (file-root &key (warnings nil))
  (version-suppressor warnings
    (lambda ()
	  (let ((file-ext (version-file-src-ext)))
		(let ((file (concatenate 'string file-root "." file-ext)))
		  (version-compile-file file warnings))))))

;; Load compiled

(defun hcload (file-root)
  (let ((file-ext (version-file-obj-ext)))
	(let ((file (concatenate 'string file-root "." file-ext)))
	  (load file))))

;; Load interpretive (src)

(defun hiload (file-root)
  (let ((file-ext (version-file-src-ext)))
	(let ((file (concatenate 'string file-root "." file-ext)))
	  (load file))))

(defun build (&key (warnings nil))
  (hcompile "hashtab" :warnings warnings)
  (hcload "hashtab")
  (hcompile "hoss" :warnings warnings)
  (hcload "hoss")
  (hcompile "top-obj" :warnings warnings)
  (hcload "top-obj")
  (hcompile "h" :warnings warnings)
  (hcload "h")
  nil)

(defun load-interp (&key (warnings nil))
  (hiload "hashtab")
  (hiload "hoss")
  (hiload "top-obj")
  (hiload "h")
  nil)

(defun load-compiled (&key (warnings nil))
  (hcload "hashtab")
  (hcload "hoss")
  (hcload "top-obj")
  (hcload "h")
  nil)
