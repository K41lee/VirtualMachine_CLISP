;;;; compare-double.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(defvar *double* '(defun double (n) (* n 2)))

(format t "~%═══ CODE ORIGINAL ═══~%~%")
(defvar *code-orig* (compile-lisp *double*))
(loop for inst in *code-orig* for i from 0
      do (format t "~2D: ~A~%" i inst))
