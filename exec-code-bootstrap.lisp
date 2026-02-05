#!/usr/bin/env clisp
;;;; ============================================================================
;;;; EXEC-CODE-BOOTSTRAP - Compile avec le compilateur BOOTSTRAPPÉ dans la VM
;;;; Utilise la délégation CLISP pour les fonctions de base non implémentées
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "src/utils-bootstrap.lisp")

;; Charger le code depuis code.lisp
(load "code.lisp")

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║     COMPILATION BOOTSTRAPPÉE AVEC DÉLÉGATION CLISP             ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;; Activer la délégation CLISP pour les fonctions de base
(setf *vm-delegate-to-lisp* t)

;;; ============================================================================
;;; ÉTAPE 1: Enregistrer les fonctions CLISP déléguées
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 1: Configuration de la délégation CLISP~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Enregistrement des fonctions CLISP déléguées...~%")

;; Fonctions arithmétiques de base
(vm-register-delegate "+" #'+)
(vm-register-delegate "-" #'-)
(vm-register-delegate "*" #'*)
(vm-register-delegate "/" #'/)
(vm-register-delegate "<" #'<)
(vm-register-delegate ">" #'>)
(vm-register-delegate "<=" #'<=)
(vm-register-delegate ">=" #'>=)
(vm-register-delegate "=" #'=)

;; Fonctions de listes
(vm-register-delegate "CAR" #'car)
(vm-register-delegate "CDR" #'cdr)
(vm-register-delegate "CONS" #'cons)
(vm-register-delegate "LIST" #'list)
(vm-register-delegate "LENGTH" #'length)
(vm-register-delegate "APPEND" #'append)

;; Fonctions de test
(vm-register-delegate "NULL" #'null)
(vm-register-delegate "ATOM" #'atom)
(vm-register-delegate "LISTP" #'listp)
(vm-register-delegate "NUMBERP" #'numberp)

;; Fonctions d'affichage
(vm-register-delegate "PRINT" #'print)
(vm-register-delegate "FORMAT" #'format)

;; Fonctions du compilateur (pour bootstrap)
(vm-register-delegate "READ-EXPRESSION-FROM-VM" #'read-expression-from-vm)
(vm-register-delegate "CONVERT-KEYWORDS-TO-SYMBOLS" #'convert-keywords-to-symbols)
(vm-register-delegate "COMPILE-LISP-TO-MIPS-SIMPLIFIED" #'compile-lisp-to-mips-simplified)

(format t "✓ ~A fonctions CLISP enregistrées pour délégation~%" 
        (hash-table-count *vm-delegated-functions*))

;;; ============================================================================
;;; ÉTAPE 2: Compiler le COMPILATEUR (une seule fois)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 2: Compilation du COMPILATEUR en MIPS~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Compilation du compilateur avec CLISP...~%")

(defparameter *compiler-code*
  (list
    ;; On compile juste compile-from-handle qui délègue aux autres fonctions
    (compile-lisp-to-mips-simplified
      '(defun compile-from-handle (handle)
         (let* ((expr (read-expression-from-vm handle))
                (lisp-expr (convert-keywords-to-symbols expr)))
           (compile-lisp-to-mips-simplified lisp-expr))))))

;; Aplatir le code (si nécessaire)
(defparameter *compiler-code-flat*
  (if (and (listp (first *compiler-code*))
           (listp (first (first *compiler-code*))))
      (first *compiler-code*)
      *compiler-code*))

(format t "✓ Compilateur compilé: ~A instructions MIPS~%" 
        (length *compiler-code-flat*))

;;; ============================================================================
;;; ÉTAPE 3: Charger le COMPILATEUR dans une VM
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 3: Chargement du COMPILATEUR dans la VM~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Création de la VM du compilateur...~%")
(defparameter *compiler-vm* (make-new-vm :verbose nil))

(format t "Chargement du compilateur compilé dans la VM...~%")
(load-code *compiler-vm* *compiler-code-flat*)

(format t "✓ Compilateur chargé et prêt à compiler dans la VM!~%")

;;; ============================================================================
;;; ÉTAPE 4: Utiliser le compilateur BOOTSTRAPPÉ pour compiler notre fonction
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4: Compilation de ~A avec le compilateur BOOTSTRAPPÉ~%"
        (second *function-definition*))
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Construction de l'expression en mémoire VM...~%")
(defparameter *expr-handle* 
  (build-expression-in-vm *function-definition*))

(format t "✓ Expression construite, handle: ~A~%" *expr-handle*)

(format t "~%Appel du compilateur BOOTSTRAPPÉ dans la VM...~%")
(format t "(Avec délégation CLISP pour les fonctions de base)~%")

;; ATTENTION: Pour l'instant, on utilise encore le compilateur natif
;; car la délégation JAL nécessite que les arguments soient correctement passés
;; et que le résultat soit une liste d'instructions MIPS (pas un entier dans $V0)

(format t "~%NOTE: Utilisation du compilateur natif pour l'instant~%")
(format t "      (La délégation complète nécessite un marshalling sophistiqué)~%")

(defparameter *compiled-code* 
  (compile-from-handle *expr-handle*))

(format t "~%✓ Compilation terminée: ~A instructions MIPS~%" 
        (length *compiled-code*))

;;; ============================================================================
;;; ÉTAPE 5: Charger et exécuter le code compilé
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 5: Exécution du code compilé~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Création d'une VM d'exécution...~%")
(defparameter *exec-vm* (make-new-vm :verbose nil))

(format t "Chargement du code compilé...~%")
(load-code *exec-vm* *compiled-code*)

(format t "✓ Code chargé dans la VM d'exécution~%")

(format t "~%Appel: ~A(~{~A~^, ~})~%" 
        (string-downcase (symbol-name *function-name*))
        *function-args*)

;; Exécution
(defparameter *result* 
  (apply #'call-function *exec-vm* *function-name* *function-args*))

(format t "~%✓ Exécution terminée~%")

;;; ============================================================================
;;; ÉTAPE 6: Vérification et statistiques
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 6: Résultat et statistiques~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Résultat: ~A(~{~A~^, ~}) = ~A~%"
        (string-downcase (symbol-name *function-name*))
        *function-args*
        *result*)

(if (= *result* *expected-result*)
    (format t "✓ RÉSULTAT CORRECT! (attendu: ~A)~%" *expected-result*)
    (format t "✗ ERREUR: attendu ~A, obtenu ~A~%" *expected-result* *result*))

;; Afficher les statistiques de délégation
(vm-show-delegation-stats)
