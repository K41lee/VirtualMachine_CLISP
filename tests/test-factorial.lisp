;;;; test-factorial.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(defvar *factorial* '(defun factorial (n)
                       (if (< n 2) 
                           1 
                           (* n (factorial (- n 1))))))

(defvar *code* (compile-lisp-to-mips-simplified *factorial*))

(format t "~%Code pour factorial:~%")
(loop for instr in *code* 
      for i from 1 to 30
      do (format t "~3D: ~A~%" i instr))

(format t "~%~%Test factorial(2):~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *code*)
(set-register *vm* :$A0 2)
(run-vm *vm* :max-instructions 500)
(format t "Résultat: ~A (attendu 2)~%" (get-register *vm* :$V0))
