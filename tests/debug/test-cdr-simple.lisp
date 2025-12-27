#!/usr/bin/env clisp
;; Test simple de CDR

(load "main.lisp")

(format t "~%=== TEST CDR SIMPLE ===~%")

;; Test (cdr (cons 1 2))
(format t "~%Test: (cdr (cons 1 2))~%")
(let* ((compiled-code (compile-lisp '(cdr (cons 1 2))))
       (vm (make-vm))
       result)
  (format t "Code généré : ~A instructions~%" (length compiled-code))
  (load-code vm compiled-code)
  (execute vm)
  (setf result (get-register vm *reg-v0*))
  (format t "Résultat : $V0 = ~A~%" result)
  (format t "Attendu : 2~%")
  (format t "Test ~A~%~%" (if (= result 2) "✅ RÉUSSI" "❌ ÉCHOUÉ")))

;; Test (cdr '(10 20 30))
(format t "~%Test: (cdr '(10 20 30))~%")
(let* ((compiled-code (compile-lisp '(cdr '(10 20 30))))
       (vm (make-vm))
       result)
  (format t "Code généré : ~A instructions~%" (length compiled-code))
  (load-code vm compiled-code)
  (execute vm)
  (setf result (get-register vm *reg-v0*))
  (format t "Résultat : $V0 = ~A (adresse de la sous-liste)~%" result)
  (format t "Test ~A~%~%" (if (> result 0) "✅ RÉUSSI" "❌ ÉCHOUÉ")))

(format t "~%=== FIN DES TESTS ===~%")
