
(sb-ext:unlock-package 'common-lisp)
(sb-ext:unlock-package 'sb-ext)

(defun version-file-src-ext ()
  "lisp")

(defun version-file-obj-ext ()
  "fasl")

(defun version-suppressor (warnings thunk)
  (let ((*suppress-compiler-warnings* (not warnings)))
	(funcall thunk)))

(defun version-compile-file (file warnings)
  (compile-file file))

(defvar *cl-type* :sbcl)
