;;;; test-simple-function.lisp
;;;; Test avec une fonction simple pour déboguer

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%═══════════════════════════════════════════════════════~%")
(format t "TEST FONCTION SIMPLE: (defun double (n) (* n 2))~%")
(format t "═══════════════════════════════════════════════════════~%~%")

(defvar *double-code* '(defun double (n) (* n 2)))

(format t "Compilation avec compilateur simplifié...~%")
(defvar *code* (compile-lisp-to-mips-simplified *double-code*))
(format t "✓ ~A instructions générées~%~%" (length *code*))

(format t "Code généré:~%")
(loop for inst in *code*
      for i from 0
      do (format t "~2D: ~A~%" i inst))

(format t "~%Test d'exécution avec double(10)...~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *code*)
(set-register *vm* :$A0 10)
(format t "  $A0 = 10~%~%")

(run-vm *vm*)

(defvar *result* (get-register *vm* :$V0))
(format t "~%Résultat: double(10) = ~A~%" *result*)
(format t "Attendu:  20~%")

(if (= *result* 20)
    (format t "~%✅ SUCCÈS !~%~%")
    (format t "~%❌ ÉCHEC ! (obtenu ~A)~%~%" *result*))
