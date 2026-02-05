;;;; compare-original.lisp
(load "src/compiler.lisp")

(defvar *double* '(defun double (n) (* n 2)))

(format t "~%Code ORIGINAL pour double:~%~%")
(defvar *code* (compile-lisp *double*))
(loop for instr in *code* 
      for i from 1 to 20
      do (format t "~3D: ~A~%" i instr))
