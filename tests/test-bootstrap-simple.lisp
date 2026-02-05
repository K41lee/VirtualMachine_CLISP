#!/usr/bin/env clisp
;;;; ============================================================================
;;;; TEST: Bootstrap avec constantes simples
;;;; Validation que le système fonctionne end-to-end
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(initialize-compiler-symbols)
(load "src/compiler-simplified.lisp")
(load "utils-bootstrap.lisp")

(format t "╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   BOOTSTRAP: Test avec constantes simples                    ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;; Compiler compile-constant-simplified
(format t "~%Compilation de compile-constant-simplified...~%")
(defparameter *code* (compile-lisp-to-mips-simplified 
  '(defun compile-constant-simplified (expr env)
     (list (global-get *instr-li*) expr (global-get *reg-v0*)))))

(format t "✓ ~A instructions générées~%" (length *code*))

;; Charger dans la VM
(format t "~%Chargement dans la VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))
(load-code *vm* *code*)
(format t "✓ Chargé~%")

;; Trouver l'adresse de la fonction
(defparameter *fn-addr* 
  (let ((code-start (calculate-code-start *vm*))
        (addr 0))
    (dolist (instr *code*)
      (when (and (listp instr) (eq (first instr) :LABEL))
        (return (+ code-start addr)))
      (incf addr))))

(format t "✓ Fonction à l'adresse: ~A~%"  *fn-addr*)

;; Test avec des constantes
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Tests de compilation~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *test-values* '(42 123 -5 0 999))

(dolist (val *test-values*)
  (format t "~%Test: ~A~%" val)
  
  ;; Compilation native
  (let* ((native-result (compile-constant-simplified val nil))
         (exec-vm (make-new-vm :verbose nil)))
    
    (format t "  Natif: ~A~%" native-result)
    
    ;; Charger le compilateur dans la VM
    (load-code exec-vm *code*)
    
    ;; Créer le wrapper pour appeler la fonction
    (let* ((code-start (calculate-code-start exec-vm))
           (wrapper `(
             (:LI ,val :$A0)        ; $A0 = valeur
             (:LI 0 :$A1)           ; $A1 = env (NIL)
             (:JAL ,*fn-addr*)
             (:HALT)))
           (wrapper-start (+ code-start (length *code*))))
      
      ;; Charger le wrapper
      (dotimes (i (length wrapper))
        (mem-write exec-vm (+ wrapper-start i) (nth i wrapper)))
      
      ;; Exécuter
      (set-register exec-vm (get-reg :pc) wrapper-start)
      (run-vm exec-vm)
      
      ;; Récupérer le résultat
      (let* ((result-handle (get-register exec-vm (get-reg :v0)))
             (vm-result (gethash result-handle *vm-lisp-objects*)))
        
        (format t "  VM:    ~A~%" vm-result)
        
        (if (equal native-result vm-result)
            (format t "  ✅ IDENTIQUE~%")
            (format t "  ❌ DIFFÉRENT~%"))))))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "✅ Tests terminés~%")
(format t "════════════════════════════════════════════════════════════════~%")
