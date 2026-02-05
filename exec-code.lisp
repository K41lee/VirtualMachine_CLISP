#!/usr/bin/env clisp
;;;; ============================================================================
;;;; EXEC-CODE - Compile et exécute du code Lisp dans la VM
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║            COMPILATION ET EXÉCUTION DE CODE LISP               ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;; Fonction à compiler et exécuter
(defparameter *function-to-compile*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2))))))

;; Arguments pour l'exécution
(defparameter *function-name* 'FIBO)
(defparameter *function-args* '(20))  ; fibo(20)
(defparameter *expected-result* 6765)

;;; ============================================================================
;;; ÉTAPE 1: Affichage du code source
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 1: Code source à compiler~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Fonction: ~A~%" (second *function-to-compile*))
(format t "~%Code Lisp:~%")
(format t "────────────────────────────────────────────────────────────────~%")
(format t "(defun fibo (n)~%")
(format t "  (if (< n 2)~%")
(format t "      n~%")
(format t "      (+ (fibo (- n 1))~%")
(format t "         (fibo (- n 2)))))~%")
(format t "────────────────────────────────────────────────────────────────~%")

;;; ============================================================================
;;; ÉTAPE 2: Compilation
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 2: Compilation~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Compilation en cours...~%")
(defparameter *compiled-code* 
  (compile-lisp-to-mips-simplified *function-to-compile*))

(format t "✓ Compilation réussie!~%")
(format t "✓ ~A instructions MIPS générées~%" (length *compiled-code*))

;;; ============================================================================
;;; ÉTAPE 3: Chargement dans la VM
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 3: Chargement dans la VM~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Création d'une nouvelle VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))

(format t "Chargement du code compilé...~%")
(load-code *vm* *compiled-code*)
(format t "✓ Code chargé dans la VM~%")

;;; ============================================================================
;;; ÉTAPE 4: Localisation de la fonction
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4: Localisation de la fonction~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *function-address* nil)
(let ((code-start (calculate-code-start *vm*))
      (addr 0))
  (dolist (instr *compiled-code*)
    (when (and (listp instr)
               (eq (first instr) :LABEL))
      (let ((label-str (if (symbolp (second instr))
                          (symbol-name (second instr))
                          (second instr))))
        (when (string= label-str (symbol-name *function-name*))
          (setf *function-address* (+ code-start addr))
          (format t "~%✓ Fonction ~A trouvée à l'adresse: ~A~%" 
                  *function-name* *function-address*)
          (return))))
    (incf addr)))

(unless *function-address*
  (format t "~%✗ ERREUR: Impossible de trouver la fonction ~A~%" *function-name*)
  (quit))

;;; ============================================================================
;;; ÉTAPE 5: Préparation de l'exécution
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 5: Préparation de l'exécution~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Configuration des registres...~%")

;; Placer les arguments dans les registres appropriés
;; $A0 = premier argument, $A1 = deuxième argument, etc.
(let ((arg-regs '(:a0 :a1 :a2 :a3)))
  (loop for arg in *function-args*
        for reg in arg-regs
        do (progn
             (set-register *vm* (get-reg reg) arg)
             (format t "  $~A = ~A~%" (string-upcase (symbol-name reg)) arg))))

;; Configurer PC et RA
(set-register *vm* (get-reg :pc) *function-address*)
(set-register *vm* (get-reg :ra) 0)  ; Adresse de retour = HALT

(format t "  $PC = ~A (adresse de la fonction)~%" *function-address*)
(format t "  $RA = 0 (HALT après exécution)~%")

(format t "~%✓ Registres configurés~%")

;;; ============================================================================
;;; ÉTAPE 6: Exécution
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 6: Exécution~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Appel: ~A(~{~A~^, ~})~%" 
        (string-downcase (symbol-name *function-name*))
        *function-args*)
(format t "~%Exécution en cours...")

;; Capturer le temps d'exécution
(defparameter *start-time* (get-internal-real-time))

;; Exécuter (capture les erreurs attendues)
(handler-case
    (run-vm *vm*)
  (error (e)
    ;; L'erreur "Adresse mémoire hors limites: 0" est normale (retour à RA=0)
    (let ((err-msg (format nil "~A" e)))
      (unless (search "Adresse mémoire hors limites: 0" err-msg)
        (format t "~%⚠ Erreur pendant l'exécution: ~A~%" e)))))

(defparameter *end-time* (get-internal-real-time))
(defparameter *execution-time* 
  (/ (- *end-time* *start-time*) internal-time-units-per-second))

(format t " terminée!~%")
(format t "Temps d'exécution: ~,3F secondes~%" *execution-time*)

;;; ============================================================================
;;; ÉTAPE 7: Résultat
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 7: Résultat~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *result* (get-register *vm* (get-reg :v0)))

(format t "~%Résultat: ~A(~{~A~^, ~}) = ~A~%"
        (string-downcase (symbol-name *function-name*))
        *function-args*
        *result*)
