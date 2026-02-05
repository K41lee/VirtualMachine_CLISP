#!/usr/bin/env clisp
;;; Test simple et sûr de l'instruction LIST

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

(format t "~%=== Test Simple LIST ===~%")

;; Test 1: Vérifier que LIST est dans *opcodes*
(format t "~%1. LIST dans *opcodes*: ~A~%" (member :LIST *opcodes*))

;; Test 2: Vérifier l'arité
(format t "2. Arité de LIST: ~A~%" (instruction-arity :LIST))

;; Test 3: Essayer de charger et exécuter un code simple avec LIST
(format t "~%3. Test chargement et exécution avec LIST:~%")
(let* ((test-code '(
  (:LI 42 :$V0)
  (:PUSH :$V0)
  (:LI 43 :$V0)
  (:PUSH :$V0)
  (:LIST 2)
  (:HALT)
))
       (vm (make-new-vm)))
  (format t "   Code source: ~A~%" test-code)
  (handler-case
      (progn
        (load-code vm test-code)
        (format t "   ✓ Chargement réussi~%")
        (format t "   Test exécution...~%")
        (run-vm vm)
        (format t "   ✓ Exécution terminée~%")
        (format t "   Résultat dans $V0 (handle): ~A~%" (get-value vm :$v0))
        (format t "   Liste créée: ~A~%" (gethash (get-value vm :$v0) *vm-lisp-objects*)))
    (error (e)
      (format t "   ✗ Erreur: ~A~%" e))))

(format t "~%Terminé.~%")
