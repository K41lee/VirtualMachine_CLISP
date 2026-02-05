;;;; show-fibo-code.lisp
(load "src/compiler-simplified.lisp")

(defvar *fibo* '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))

(format t "~%Code généré pour fibo:~%~%")
(defvar *code* (compile-lisp-to-mips-simplified *fibo*))
(loop for instr in *code* 
      for i from 1
      do (format t "~3D: ~A~%" i instr))

(format t "~%~%Analysons les instructions IF...~%")
(loop for instr in *code*
      when (or (eq (first instr) :BLT)
               (eq (first instr) :BGT)
               (eq (first instr) :BEQ)
               (eq (first instr) :BNE)
               (eq (first instr) :J)
               (eq (first instr) :JMP))
      do (format t "Branchement: ~A~%" instr))
