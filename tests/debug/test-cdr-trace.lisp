#!/usr/bin/env clisp
;; Test CDR avec traces

(load "main.lisp")

(format t "~%=== TEST CDR AVEC TRACES ===~%")

(let* ((compiled-code (compile-lisp '(cdr (cons 1 2))))
       (vm (make-vm)))
  (format t "Code chargé : ~A instructions~%" (length compiled-code))
  (load-code vm compiled-code)
  
  (format t "Avant exécution : $V0 = ~A, $GP = ~A~%" 
          (get-register vm *reg-v0*)
          (get-register vm (get-reg :gp)))
  
  ;; Exécuter avec limite d'instructions
  (let ((max-steps 50)
        (step 0))
    (loop while (and (< step max-steps) (not (zerop (get-register vm :$PC))))
          do (progn
               (when (< step 20)
                 (format t "Step ~A: PC=~A~%" step (get-register vm :$PC)))
               (execute-step vm)
               (incf step)))
    (format t "Arrêté après ~A steps~%" step))
  
  (format t "Après exécution : $V0 = ~A~%" (get-register vm *reg-v0*)))
