#!/usr/bin/env clisp
;;;; ============================================================================
;;;; DEMO-DELEGATION - Démontre la délégation CLISP pour fonctions inconnues
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
(format t "║           DÉMONSTRATION: DÉLÉGATION CLISP DANS LA VM          ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;; Activer la délégation
(setf *vm-delegate-to-lisp* t)

;;; ============================================================================
;;; TEST 1: Fonction simple avec opération arithmétique déléguée
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: Fonction simple avec arithmétique déléguée~%")
(format t "════════════════════════════════════════════════════════════════~%")

;; Enregistrer une fonction personnalisée pour délégation
(defun my-square (x &optional dummy1 dummy2 dummy3)
  "Fonction CLISP qui sera appelée depuis la VM"
  (declare (ignore dummy1 dummy2 dummy3))
  (format t "  [CLISP] my-square appelée avec x=~A~%" x)
  (* x x))

(vm-register-delegate "MY-SQUARE" #'my-square)

(format t "~%Fonction enregistrée: MY-SQUARE (calcule le carré)~%")

;; Créer une fonction Lisp qui utilise MY-SQUARE
(defparameter *test-function*
  '(defun test-square (n)
     (my-square n)))

(format t "~%Fonction Lisp à compiler:~%")
(format t "  (defun test-square (n)~%")
(format t "    (my-square n))~%")

(format t "~%Compilation...~%")
(defparameter *compiled-test*
  (compile-lisp-to-mips-simplified *test-function*))

(format t "✓ ~A instructions MIPS générées~%" (length *compiled-test*))

(format t "~%Création de la VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))

(format t "Chargement du code...~%")
(load-code *vm* *compiled-test*)

(format t "~%Exécution: test-square(5)...~%")
(setf *vm-delegate-to-lisp* t)
(defparameter *result* (call-function *vm* 'TEST-SQUARE 5))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "RÉSULTAT: ~A~%" *result*)
(format t "✓ La fonction CLISP my-square a été appelée depuis la VM!~%")
(format t "════════════════════════════════════════════════════════════════~%")

;;; ============================================================================
;;; TEST 2: Fonction avec plusieurs arguments
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: Délégation avec plusieurs arguments~%")
(format t "════════════════════════════════════════════════════════════════~%")

;; Fonction CLISP qui additionne 3 nombres
(defun sum-three (a b c &optional dummy)
  (declare (ignore dummy))
  (format t "  [CLISP] sum-three appelée avec a=~A, b=~A, c=~A~%" a b c)
  (+ a b c))

(vm-register-delegate "SUM-THREE" #'sum-three)

(defparameter *test-function-2*
  '(defun calc-sum (x y z)
     (sum-three x y z)))

(format t "~%Fonction Lisp:~%")
(format t "  (defun calc-sum (x y z)~%")
(format t "    (sum-three x y z))~%")

(format t "~%Compilation...~%")
(defparameter *compiled-test-2*
  (compile-lisp-to-mips-simplified *test-function-2*))

(format t "✓ ~A instructions MIPS générées~%" (length *compiled-test-2*))

(format t "~%Création d'une nouvelle VM...~%")
(defparameter *vm2* (make-new-vm :verbose nil))

(format t "Chargement et exécution: calc-sum(10, 20, 30)...~%")
(load-code *vm2* *compiled-test-2*)
(defparameter *result2* (call-function *vm2* 'CALC-SUM 10 20 30))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "RÉSULTAT: 10 + 20 + 30 = ~A~%" *result2*)
(format t "✓ Délégation multi-arguments réussie!~%")
(format t "════════════════════════════════════════════════════════════════~%")

;;; ============================================================================
;;; TEST 3: Fonction Lisp compilée qui utilise des fonctions déléguées
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 3: Fonction récursive déléguée~%")
(format t "════════════════════════════════════════════════════════════════~%")

;; Fonction personnalisée: factorielle avec logging
(defun factorial-logged (n &optional d1 d2 d3)
  (declare (ignore d1 d2 d3))
  (format t "  [CLISP] Calcul factorielle de ~A~%" n)
  (if (<= n 1)
      1
      (* n (factorial-logged (- n 1)))))

(vm-register-delegate "FACTORIAL-LOGGED" #'factorial-logged)

(defparameter *test-function-3*
  '(defun fact-test (n)
     (factorial-logged n)))

(format t "~%Fonction Lisp:~%")
(format t "  (defun fact-test (n)~%")
(format t "    (factorial-logged n))~%")

(format t "~%Compilation...~%")
(defparameter *compiled-test-3*
  (compile-lisp-to-mips-simplified *test-function-3*))

(format t "✓ ~A instructions MIPS générées~%" (length *compiled-test-3*))

(format t "~%Création de la VM pour factorielle...~%")
(defparameter *vm3* (make-new-vm :verbose nil))

(format t "Chargement et exécution: fact-test(5)...~%")
(load-code *vm3* *compiled-test-3*)
(defparameter *result3* (call-function *vm3* 'FACT-TEST 5))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "RÉSULTAT: factorial(5) = ~A~%" *result3*)
(format t "✓ Fonction récursive CLISP appelée depuis la VM!~%")
(format t "════════════════════════════════════════════════════════════════~%")

;;; ============================================================================
;;; Statistiques finales
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "STATISTIQUES DE DÉLÉGATION~%")
(format t "════════════════════════════════════════════════════════════════~%")

(vm-show-delegation-stats)

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "~%🎉 DÉMONSTRATION TERMINÉE~%")
(format t "~%Ce qui a été démontré:~%")
(format t "  ✓ Délégation automatique des fonctions inconnues à CLISP~%")
(format t "  ✓ Passage d'arguments via registres $A0-$A3~%")
(format t "  ✓ Récupération du résultat dans $V0~%")
(format t "  ✓ Support des fonctions récursives~%")
(format t "  ✓ Statistiques de délégation~%")
(format t "~%Architecture FFI:~%")
(format t "  VM MIPS → détecte JAL symbolique → vérifie *vm-delegated-functions*~%")
(format t "  → appelle fonction CLISP → récupère résultat → continue exécution~%")
(format t "~%Avantages:~%")
(format t "  • Utiliser le compilateur bootstrappé sans tout réimplémenter~%")
(format t "  • Fonctions de base (I/O, strings, listes) déléguées à CLISP~%")
(format t "  • Permet un développement progressif (natif → délégué → implémenté)~%")
(format t "════════════════════════════════════════════════════════════════~%")
