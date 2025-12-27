#!/usr/bin/env clisp
;; Vérifier résultat APPEND

(load "main.lisp")

(format t "~%=== TEST APPEND (cons 1 (cons 2 nil)) + (cons 3 nil) ===~%~%")

(let* ((vm (make-vm))
       (code (compile-lisp '(append (cons 1 (cons 2 nil)) (cons 3 nil)))))
  (load-code vm code)
  (run-vm vm)
  
  (let ((result (get-register vm *reg-v0*)))
    (format t "Résultat: adresse ~A~%~%" result)
    
    (when (and (numberp result) (> result 0))
      ;; Lire la liste résultante
      (let ((addr result))
        (format t "Liste résultante:~%")
        (loop for i from 0 to 5
              while (and addr (> addr 0))
              do (let ((car-val (mem-read vm addr))
                       (cdr-addr (mem-read vm (+ addr 1))))
                   (format t "  [~A] CAR=~A, CDR=~A~%" addr car-val cdr-addr)
                   (setf addr cdr-addr)))
        (format t "~%")
        
        (format t "Attendu: 1 → 2 → 3 → NIL~%")
        (format t "Test ~A~%" 
                (if (and (= (mem-read vm result) 1)
                         (let ((addr2 (mem-read vm (+ result 1))))
                           (and (> addr2 0)
                                (= (mem-read vm addr2) 2)
                                (let ((addr3 (mem-read vm (+ addr2 1))))
                                  (and (> addr3 0)
                                       (= (mem-read vm addr3) 3)
                                       (= (mem-read vm (+ addr3 1)) 0))))))
                    "✅ RÉUSSI"
                    "❌ ÉCHOUÉ"))))))
