

(defun version-file-src-ext ()
  "lisp")

(defun version-file-obj-ext ()
  "abcl")

(defun version-suppressor (warnings thunk)
  (let ((*suppress-compiler-warnings* (not warnings)))
	(funcall thunk)))

(defun version-compile-file (file warnings)
  (compile-file file))

