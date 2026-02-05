;;;; manual-trace-fibo2.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(defvar *fibo* '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))
(defvar *code* (compile-lisp-to-mips-simplified *fibo*))
(defvar *vm* (make-new-vm))
(load-code *vm* *code*)
(set-register *vm* :$A0 2)

(format t "~%=== DEBUT: fibo(2) ===~%")
(format t "$A0 = ~A, $V0 = ~A, $S0 = ~A~%" 
        (get-register *vm* :$A0)
        (get-register *vm* :$V0)
        (get-register *vm* :$S0))

;; Exécuter 20 instructions
(dotimes (i 20)
  (let ((pc (get-register *vm* :$PC))
        (instr (get-vm-instruction-at *vm* pc)))
    (format t "~%[~2D] PC=~A: ~A~%" i pc instr)
    (execute-instruction *vm* instr)
    (format t "     $V0=~A, $S0=~A, $T0=~A, $SP=~A~%" 
            (get-register *vm* :$V0)
            (get-register *vm* :$S0)
            (get-register *vm* :$T0)
            (get-register *vm* :$SP))))

(format t "~%~%=== RESULTAT APRES 20 INSTRUCTIONS ===~%")
(format t "$V0 = ~A (attendu: 1)~%" (get-register *vm* :$V0))
