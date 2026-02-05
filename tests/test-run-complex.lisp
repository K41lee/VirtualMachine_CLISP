#!/usr/bin/env clisp
;;; Test d'exécution de fibo et ack dans la VM

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")

(initialize-compiler-symbols)
(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║         EXÉCUTION DE FIBO ET ACK DANS LA VM                    ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; Test 1: Fibonacci
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: Fibonacci~%")
(format t "════════════════════════════════════════════════════════════════~%")

(let* ((fibo-def '(defun fibo (n)
                    (if (< n 2)
                        n
                        (+ (fibo (- n 1)) (fibo (- n 2))))))
       (code (compile-lisp-to-mips-simplified fibo-def))
       (vm (make-new-vm)))
  
  (format t "~%Code compilé: ~A instructions~%" (length code))
  (load-code vm code)
  
  ;; Tester fibo(5)
  (format t "~%Calcul de fibo(5)...~%")
  (let* ((code-start (calculate-code-start vm))
         (fibo-addr (+ code-start 1)))  ; Après le J initial
    
    ;; Préparer l'appel
    (set-value vm :$a0 5)  ; n = 5
    (set-value vm :$pc fibo-addr)
    
    ;; Exécuter
    (setf (vm-state vm) :running)
    (let ((max-steps 10000)
          (step-count 0))
      (loop while (and (eq (vm-state vm) :running)
                      (< step-count max-steps))
            do (execute-step vm)
               (incf step-count))
      
      (format t "  Exécution terminée après ~A instructions~%" step-count)
      (format t "  État: ~A~%" (vm-state vm))
      (format t "  Résultat ($V0): ~A~%" (get-value vm :$v0))
      (format t "  Attendu: 5 (car fibo(5) = 5)~%")
      
      (if (= (get-value vm :$v0) 5)
          (format t "  ✓ CORRECT!~%")
          (format t "  ✗ INCORRECT~%")))))

;;; Test 2: Ackermann
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: Ackermann~%")
(format t "════════════════════════════════════════════════════════════════~%")

(let* ((ack-def '(defun ack (m n)
                   (cond
                     ((= m 0) (+ n 1))
                     ((= n 0) (ack (- m 1) 1))
                     (t (ack (- m 1) (ack m (- n 1)))))))
       (code (compile-lisp-to-mips-simplified ack-def))
       (vm (make-new-vm)))
  
  (format t "~%Code compilé: ~A instructions~%" (length code))
  (load-code vm code)
  
  ;; Tester ack(2, 1)
  (format t "~%Calcul de ack(2, 1)...~%")
  (let* ((code-start (calculate-code-start vm))
         (ack-addr (+ code-start 1)))
    
    (set-value vm :$a0 2)  ; m = 2
    (set-value vm :$a1 1)  ; n = 1
    (set-value vm :$pc ack-addr)
    
    (setf (vm-state vm) :running)
    (let ((max-steps 50000)
          (step-count 0))
      (loop while (and (eq (vm-state vm) :running)
                      (< step-count max-steps))
            do (execute-step vm)
               (incf step-count))
      
      (format t "  Exécution terminée après ~A instructions~%" step-count)
      (format t "  État: ~A~%" (vm-state vm))
      (format t "  Résultat ($V0): ~A~%" (get-value vm :$v0))
      (format t "  Attendu: 5 (car ack(2,1) = 5)~%")
      
      (if (= (get-value vm :$v0) 5)
          (format t "  ✓ CORRECT!~%")
          (format t "  ✗ INCORRECT~%")))))

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                   TESTS TERMINÉS!                              ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
