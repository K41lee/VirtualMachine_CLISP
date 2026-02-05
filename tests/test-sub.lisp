;;;; test-sub.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(defvar *test-sub* '(defun test-sub (n) (- n 1)))
(defvar *code* (compile-lisp-to-mips-simplified *test-sub*))

(format t "~%Code pour (- n 1):~%")
(loop for instr in *code* 
      for i from 0 to 40
      do (format t "~3D: ~A~%" i instr))

(format t "~%~%Test (- 5 1):~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *code*)
(set-register *vm* :$A0 5)
(set-register *vm* :$RA 99999)  ; Adresse de retour bidon
(run-vm *vm* :max-instructions 100)
(format t "Résultat: ~A (attendu 4)~%" (get-register *vm* :$V0))
