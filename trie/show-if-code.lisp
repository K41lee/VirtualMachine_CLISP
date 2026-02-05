;;;; show-if-code.lisp
(load "src/compiler-simplified.lisp")

(defvar *test1* '(defun test1 (n) (if (< n 2) 100 200)))
(defvar *code* (compile-lisp-to-mips-simplified *test1*))
(loop for instr in *code* 
      for i from 1
      do (format t "~3D: ~A~%" i instr))
