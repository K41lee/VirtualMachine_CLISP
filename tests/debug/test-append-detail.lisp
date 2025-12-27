#!/usr/bin/env clisp
;; Test APPEND avec validation

(load "main.lisp")

(format t "~%=== TEST APPEND DÉTAILLÉ ===~%~%")

(let* ((vm (make-vm))
       (code (compile-lisp '(append (list 1) (list 2)))))
  (format t "Code: ~A instructions~%~%" (length code))
  
  ;; Charger et exécuter
  (load-code vm code)
  (format t "Avant exécution: $GP=~A~%~%" (get-register vm (get-reg :gp)))
  
  (handler-case
      (progn
        (run-vm vm)
        (let ((result (get-register vm *reg-v0*))
              (gp (get-register vm (get-reg :gp))))
          (format t "Après exécution:~%")
          (format t "  $V0 = ~A (résultat)~%" result)
          (format t "  $GP = ~A (heap pointer)~%~%" gp)
          
          ;; Si résultat > 0, c'est une adresse, lire la CONS
          (when (and (numberp result) (> result 0))
            (let ((car-val (mem-read vm result))
                  (cdr-addr (mem-read vm (+ result 1))))
              (format t "CONS à l'adresse ~A:~%" result)
              (format t "  CAR = ~A~%" car-val)
              (format t "  CDR = ~A~%" cdr-addr)
              
              (when (and (numberp cdr-addr) (> cdr-addr 0))
                (let ((car2 (mem-read vm cdr-addr))
                      (cdr2 (mem-read vm (+ cdr-addr 1))))
                  (format t "~%CONS à l'adresse ~A:~%" cdr-addr)
                  (format t "  CAR = ~A~%" car2)
                  (format t "  CDR = ~A~%" cdr2)))))))
    (error (e)
      (format t "ERREUR: ~A~%" e))))
