;;;; debug-fibo2.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(defvar *fibo* '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))
(defvar *code* (compile-lisp-to-mips-simplified *fibo*))

(format t "~%Instructions pour fibo:~%")
(loop for instr in *code*
      for i from 0
      do (format t "~3D: ~A~%" i instr))

(format t "~%~%Test fibo(2) - devrait retourner 1:~%~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *code*)
(set-register *vm* :$A0 2)
(setf (vm-trace *vm*) t)
(run-vm *vm* :max-instructions 200)
(format t "~%Résultat: ~A (attendu 1)~%" (get-register *vm* :$V0))
