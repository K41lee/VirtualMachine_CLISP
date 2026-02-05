#!/usr/bin/env clisp
;;; Test de bootstrap SIMPLIFIÉ avec VERBOSE pour debugging

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles AVANT de charger le compilateur
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "src/compiler-bootstrap-ids.lisp")

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST DEBUG: Compilation de compile-constant-simplified~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

;; Lire le fichier compiler-simplified.lisp
(with-open-file (stream "src/compiler-simplified.lisp" :direction :input)
  (let ((all-code nil)
        (selected-functions nil))
    ;; Lire toutes les formes
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          do (push form all-code))
    (setq all-code (nreverse all-code))
    
    ;; Trouver compile-constant-simplified
    (dolist (form all-code)
      (when (and (listp form)
                 (eq (first form) 'defun)
                 (eq (second form) 'compile-constant-simplified))
        (push form selected-functions)))
    
    (format t "Fonction trouvée: ~A~%~%" (second (first selected-functions)))
    (format t "Définition: ~A~%~%" (first selected-functions))
    
    ;; Compiler la fonction
    (let ((compiled-code (compile-lisp-with-ids (first selected-functions))))
      (format t "~%Code compilé: ~A instructions~%" (length compiled-code))
      (format t "~%Premières 10 instructions:~%")
      (loop for instr in (subseq compiled-code 0 (min 10 (length compiled-code)))
            for i from 0
            do (format t "  [~2D] ~A~%" i instr))
      
      ;; Créer la VM et charger
      (format t "~%~%Chargement dans la VM...~%")
      (let ((vm (make-new-vm)))
        (setf (vm-verbose vm) t)  ; MODE VERBOSE
        (load-code vm compiled-code)
        
        (format t "~%~%Exécution de compile-constant-simplified(42, NIL)...~%")
        (format t "════════════════════════════════════════════════════════════════~%~%")
        
        ;; Préparer les arguments
        (set-value vm :$a0 42)     ; value = 42
        (set-value vm :$a1 0)      ; env = NIL = 0
        
        ;; Trouver l'adresse de la fonction
        (let ((func-addr (+ (calculate-code-start vm 0) 1)))  ; +1 pour sauter le J __start
          (format t "Appel de la fonction à l'adresse ~A~%~%" func-addr)
          (set-value vm :$pc func-addr)
          (setf (vm-state vm) :running)
          
          ;; Exécuter (max 100 instructions pour debug)
          (dotimes (i 100)
            (when (eq (vm-state vm) :halted)
              (format t "~%VM arrêtée après ~A instructions~%" i)
              (return))
            (when (eq (vm-state vm) :error)
              (format t "~%Erreur VM après ~A instructions~%" i)
              (return))
            (execute-step vm))
          
          (format t "~%~%Résultat final:~%")
          (format t "  $V0 = ~A~%" (get-value vm :$v0))
          (let ((result (gethash (get-value vm :$v0) *vm-lisp-objects*)))
            (format t "  Objet dans *vm-lisp-objects*: ~A~%" result)))))))
