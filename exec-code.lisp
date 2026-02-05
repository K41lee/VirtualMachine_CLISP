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

;;; ============================================================================
;;; ÉTAPE 1: Compilation
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 1: Compilation~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Compilation en cours...~%")
(defparameter *compiled-code* 
  (compile-lisp-to-mips-simplified *function-to-compile*))

(format t "✓ Compilation réussie!~%")
(format t "✓ ~A instructions MIPS générées~%" (length *compiled-code*))

;;; ============================================================================
;;; ÉTAPE 2: Chargement dans la VM
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 2: Chargement dans la VM~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Création d'une nouvelle VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))

(format t "Chargement du code compilé...~%")
(load-code *vm* *compiled-code*)
(format t "✓ Code chargé dans la VM~%")

;;; ============================================================================
;;; ÉTAPE 3: Exécution directe avec call-function
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 3: Exécution avec call-function~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Appel: ~A(~{~A~^, ~})~%" 
        (string-downcase (symbol-name *function-name*))
        *function-args*)

;; Capturer le temps d'exécution
(defparameter *start-time* (get-internal-real-time))

;; Appel direct - tout est automatique!
(defparameter *result* 
  (apply #'call-function *vm* *function-name* *function-args*))

(defparameter *end-time* (get-internal-real-time))
(defparameter *execution-time* 
  (/ (- *end-time* *start-time*) internal-time-units-per-second))

(format t "~%✓ Exécution terminée en ~,3F secondes~%" *execution-time*)

;;; ============================================================================
;;; ÉTAPE 4: Résultat
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4: Résultat~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Résultat: ~A(~{~A~^, ~}) = ~A~%"
        (string-downcase (symbol-name *function-name*))
        *function-args*
        *result*)
