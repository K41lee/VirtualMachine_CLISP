#!/usr/bin/env clisp
;;; Test debug pour comprendre pourquoi 42 → 0

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")

(initialize-compiler-symbols)
(load "src/compiler-simplified.lisp")

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "DEBUG: Pourquoi 42 devient 0 dans compile-constant-simplified?~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

;; Compiler compile-constant-simplified
(format t "1. Compilation de compile-constant-simplified...~%")
(let ((code (compile-lisp-to-mips-simplified 
              '(defun compile-constant-simplified (value env)
                 (list (list *instr-li* value *reg-v0*))))))
  
  (format t "   Code compilé: ~A instructions~%~%" (length code))
  
  ;; Créer la VM
  (let ((vm (make-new-vm)))
    (setf (vm-verbose vm) t)
    (load-code vm code)
    
    (format t "~%2. Test avec la valeur 42...~%")
    (format t "════════════════════════════════════════════════════════════════~%~%")
    
    ;; Créer un wrapper qui appelle la fonction avec 42
    (let* ((code-start (calculate-code-start vm))
           (fn-addr (+ code-start 1))
           (wrapper `(
             (:LI 42 :$A0)
             (:LI 0 :$A1)
             (:JAL ,fn-addr)
             (:HALT)))
           (wrapper-start (+ code-start (length code))))
      
      ;; Charger le wrapper
      (dotimes (i (length wrapper))
        (mem-write vm (+ wrapper-start i) (nth i wrapper)))
      
      (format t "Wrapper à l'adresse ~A:~%" wrapper-start)
      (format t "  LI 42, $A0~%")
      (format t "  LI 0, $A1~%")
      (format t "  JAL ~A~%~%" fn-addr)
      
      ;; Exécuter
      (set-register vm (get-reg :pc) wrapper-start)
      (run-vm vm)
      
      ;; Récupérer le résultat
      (let* ((result-handle (get-register vm (get-reg :v0)))
             (result (gethash result-handle *vm-lisp-objects*)))
        (format t "~%~%════════════════════════════════════════════════════════════════~%")
        (format t "RÉSULTAT FINAL:~%")
        (format t "════════════════════════════════════════════════════════════════~%~%")
        (format t "  Handle: ~A~%" result-handle)
        (format t "  Code brut: ~A~%" result)
        
        ;; Analyser chaque élément
        (when (and result (listp result))
          (let ((instr (first result)))
            (format t "~%  Instruction: ~A~%" instr)
            (when (listp instr)
              (format t "    [0] = ~A (devrait être 1 = LI)~%" (first instr))
              (format t "    [1] = ~A (devrait être 42)~%" (second instr))
              (format t "    [2] = ~A (devrait être 27 = $V0)~%" (third instr))
              
              ;; Vérifier si ce sont des IDs
              (format t "~%  Vérification IDs:~%")
              (format t "    [0] symbol-name: ~A~%" (symbol-name-from-id (first instr)))
              (format t "    [1] symbol-name: ~A~%" (symbol-name-from-id (second instr)))
              (format t "    [2] symbol-name: ~A~%" (symbol-name-from-id (third instr))))))))))
