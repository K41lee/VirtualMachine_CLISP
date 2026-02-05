;;;; debug-fibo-original.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(defvar *fibo* '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))

(format t "~%═══ CODE FIBO ORIGINAL DÉTAILLÉ ═══~%~%")
(defvar *code* (compile-lisp *fibo*))

(loop for inst in *code* 
      for i from 0
      do (format t "~2D: ~A~%" i inst))

(format t "~%~%═══ TEST EXÉCUTION ═══~%~%")
(defvar *vm* (make-new-vm :verbose nil))
(load-code *vm* *code*)
(set-register *vm* :$A0 5)
(format t "Test fibo(5)...~%")
(run-vm *vm*)
(format t "Résultat: ~A (attendu: 5)~%~%" (get-register *vm* :$V0))
