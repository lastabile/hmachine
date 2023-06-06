
(cond
 ((find-package 'cs-common-lisp)		;; CLISP
  (let ()
	(load "ht-clisp.lisp")))
 ((find-package 'sb-int)				;; SBCL (Steel Bank Common Lisp)
  (let ()
	(load "ht-sbcl.lisp")))
 (t										;; Assume ABCL (Armed Bear Common Lisp) until others come along
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

  (hcompile "base-utils" :warnings warnings)
  (hcload "base-utils")
  (hcompile "utils" :warnings warnings)
  (hcload "utils")
  (hcompile "file-utils" :warnings warnings)
  (hcload "file-utils")
  (hcompile "hoss" :warnings warnings)
  (hcload "hoss")
  (hcompile "top-obj" :warnings warnings)
  (hcload "top-obj")
  (hcompile "h" :warnings warnings)
  (hcload "h")
  (hcompile "dumper" :warnings warnings)
  (hcload "dumper")
  nil)

(defun load-interp (&key (warnings nil))
  (hiload "base-utils")
  (hiload "utils")
  (hiload "file-utils")
  (hiload "hoss")
  (hiload "top-obj")
  (hiload "h")
  (hiload "dumper")
  nil)

(defun load-compiled (&key (warnings nil))
  (hcload "base-utils")
  (hcload "utils")
  (hcload "file-utils")
  (hcload "hoss")
  (hcload "top-obj")
  (hcload "h")
  (hcload "dumper")
  nil)
