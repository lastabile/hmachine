;; Dreadful mumbo-jumbo to redefine warning functions, since the
;; standard and advertised methods for controlling warnings don't work.
;;
;; Packages are evil.

(setf (ext:package-lock custom:*system-package-list*) nil)
(setq system::cur-pkg (package-name *package*))
(in-package "SYSTEM")
(defun c-style-warn (cstring &rest args)
  (when *compile-warnings*
	(apply 'c-warning 'sys::simple-style-warning cstring args)))
(defun c-warn (cstring &rest args)
  (when *compile-warnings*
	(apply 'c-warning 'sys::simple-warning cstring args)))
(eval `(in-package ,cur-pkg))

(defun version-file-src-ext ()
  "lisp")

(defun version-file-obj-ext ()
  "fas")

(defun version-suppressor (warnings thunk)
  (funcall thunk))

(defun version-compile-file (file warnings)
  (compile-file file :warnings warnings))
