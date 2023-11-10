
;; Not done!

(sb-ext:unlock-package 'common-lisp)
(sb-ext:unlock-package 'sb-ext)

(defun version-file-src-ext ()
  "lisp")

(defun version-file-obj-ext ()
  "abcl")

(defun version-compile-file (file warnings)
  (compile-file file))

(defvar *cl-type* :abcl)

(defun version-suppressor (warnings thunk)
  (funcall thunk))

(defun version-compile-file (file warnings)
  (compile-file file :warnings warnings))
