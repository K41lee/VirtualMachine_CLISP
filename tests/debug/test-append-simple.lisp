#!/usr/bin/env clisp
;; Test APPEND simple avec LIST

(load "main.lisp")

(format t "~%=== TEST APPEND ===~%~%")

;; Test simple : (append (list 1) (list 2))
(format t "Test: (append (list 1) (list 2))~%")
(handler-case
    (let ((result (compile-and-run '(append (list 1) (list 2)))))
      (format t "Résultat brut : ~A~%" result)
      (format t "Test ✅ RÉUSSI~%"))
  (error (e)
    (format t "Erreur : ~A~%"  e)
    (format t "Test ❌ ÉCHOUÉ~%")))

(format t "~%Fin des tests~%")
