;;;; debug-labels.lisp
(load "src/compiler-simplified.lisp")

(defvar *test1* '(defun test1 (n) (if (< n 2) 100 200)))
(defvar *code* (compile-lisp-to-mips-simplified *test1*))

(format t "~%Labels dans le code:~%")
(loop for instr in *code*
      for i from 1
      when (and (listp instr) (eq (first instr) :LABEL))
      do (format t "~3D: ~A - type: ~A~%" 
                i instr (type-of (second instr))))

(format t "~%~%Références aux labels:~%")
(loop for instr in *code*
      for i from 1
      when (and (listp instr) 
                (member (first instr) '(:BLT :BGT :BEQ :BNE :JMP :JAL)))
      do (format t "~3D: ~A - dernier élément type: ~A~%" 
                 i instr (type-of (car (last instr)))))
