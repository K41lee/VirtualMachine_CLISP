#!/usr/bin/env clisp
;;; Test simple: tester GLOBAL-GET + LIST

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")

(initialize-compiler-symbols)

(format t "~%Test 1: Vérifier que les variables globales sont définies~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(format t "*instr-li* dans *vm-globals*: ~A~%" (gethash '*instr-li* *vm-globals*))
(format t "*reg-v0* dans *vm-globals*: ~A~%" (gethash '*reg-v0* *vm-globals*))
(format t "*instr-list* dans *vm-globals*: ~A~%" (gethash '*instr-list* *vm-globals*))

(format t "~%Test 2: Code MIPS manuel pour construire (1 42 25)~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(let ((vm (make-new-vm)))
  (setf (vm-verbose vm) t)
  
  ;; Code MIPS qui devrait construire ((1 42 25)):
  ;; 1. GLOBAL-GET *instr-li* → $V0 = 1
  ;; 2. SW $V0, $SP, 0 → empiler 1
  ;; 3. ADDI $SP, -4, $SP
  ;; 4. LI 42, $V0 → charger 42
  ;; 5. SW $V0, $SP, 0 → empiler 42
  ;; 6. ADDI $SP, -4, $SP
  ;; 7. GLOBAL-GET *reg-v0* → $V0 = 25
  ;; 8. SW $V0, $SP, 0 → empiler 25
  ;; 9. ADDI $SP, -4, $SP
  ;; 10. LIST 3 → crée (1 42 25), retourne handle dans $V0
  ;; 11. SW $V0, $SP, 0 → empiler handle
  ;; 12. ADDI $SP, -4, $SP
  ;; 13. LIST 1 → crée ((1 42 25)), retourne handle dans $V0
  ;; 14. HALT
  
  (let ((code '(
    (:LABEL "__start")
    (:J "MAIN")
    (:LABEL "MAIN")
    (:GLOBAL-GET *instr-li*)
    (:SW :$V0 :$SP 0)
    (:ADDI :$SP -4 :$SP)
    (:LI 42 :$V0)
    (:SW :$V0 :$SP 0)
    (:ADDI :$SP -4 :$SP)
    (:GLOBAL-GET *reg-v0*)
    (:SW :$V0 :$SP 0)
    (:ADDI :$SP -4 :$SP)
    (:LIST 3)
    (:SW :$V0 :$SP 0)
    (:ADDI :$SP -4 :$SP)
    (:LIST 1)
    (:HALT))))
    
    (format t "Chargement du code...~%")
    (load-code vm code)
    
    (format t "~%Exécution...~%~%")
    (run-vm vm)
    
    (format t "~%~%Résultat final:~%")
    (format t "  $V0 = ~A~%" (get-value vm :$v0))
    (let ((result (gethash (get-value vm :$v0) *vm-lisp-objects*)))
      (format t "  Objet dans *vm-lisp-objects*: ~A~%" result))))
