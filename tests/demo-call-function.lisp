#!/usr/bin/env clisp
;;;; ============================================================================
;;;; DEMO CALL-FUNCTION - Démonstration de l'appel direct de fonctions
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║       DÉMONSTRATION: APPEL DIRECT DE FONCTIONS (VM)           ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; EXEMPLE 1: Fibonacci
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "EXEMPLE 1: Fibonacci~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Code Lisp:~%")
(format t "(defun fibo (n)~%")
(format t "  (if (< n 2) n~%")
(format t "      (+ (fibo (- n 1)) (fibo (- n 2)))))~%")

;; Compiler
(format t "~%Compilation...~%")
(defparameter *fibo-code*
  (compile-lisp-to-mips-simplified
    '(defun fibo (n)
       (if (< n 2)
           n
           (+ (fibo (- n 1)) (fibo (- n 2)))))))
(format t "✓ ~A instructions générées~%" (length *fibo-code*))

;; Créer VM et charger
(format t "~%Création de la VM et chargement...~%")
(defparameter *vm1* (make-new-vm))
(load-code *vm1* *fibo-code*)
(format t "✓ Code chargé~%")

;; APPEL DIRECT - Tout est automatique!
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "APPELS DIRECTS avec call-function:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Test 1: fibo(5)~%")
(defparameter *result1* (call-function *vm1* 'FIBO 5))
(format t "→ Résultat: ~A (attendu: 5)~%" *result1*)
(format t "   ~A~%" (if (= *result1* 5) "✅ CORRECT" "❌ INCORRECT"))

(format t "~%Test 2: fibo(10)~%")
(setf (vm-state *vm1*) :ready)  ; Réinitialiser l'état
(defparameter *result2* (call-function *vm1* 'FIBO 10))
(format t "→ Résultat: ~A (attendu: 55)~%" *result2*)
(format t "   ~A~%" (if (= *result2* 55) "✅ CORRECT" "❌ INCORRECT"))

(format t "~%Test 3: fibo(15)~%")
(setf (vm-state *vm1*) :ready)
(defparameter *result3* (call-function *vm1* 'FIBO 15))
(format t "→ Résultat: ~A (attendu: 610)~%" *result3*)
(format t "   ~A~%" (if (= *result3* 610) "✅ CORRECT" "❌ INCORRECT"))

;;; ============================================================================
;;; EXEMPLE 2: Factorielle
;;; ============================================================================

(format t "~%~%════════════════════════════════════════════════════════════════~%")
(format t "EXEMPLE 2: Factorielle~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Code Lisp:~%")
(format t "(defun fact (n)~%")
(format t "  (if (< n 2) 1~%")
(format t "      (* n (fact (- n 1)))))~%")

;; Compiler
(format t "~%Compilation...~%")
(defparameter *fact-code*
  (compile-lisp-to-mips-simplified
    '(defun fact (n)
       (if (< n 2)
           1
           (* n (fact (- n 1)))))))
(format t "✓ ~A instructions générées~%" (length *fact-code*))

;; Créer VM et charger
(format t "~%Création de la VM et chargement...~%")
(defparameter *vm2* (make-new-vm))
(load-code *vm2* *fact-code*)
(format t "✓ Code chargé~%")

;; APPELS DIRECTS
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "APPELS DIRECTS avec call-function:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Test 1: fact(5)~%")
(defparameter *result4* (call-function *vm2* 'FACT 5))
(format t "→ Résultat: ~A (attendu: 120)~%" *result4*)
(format t "   ~A~%" (if (= *result4* 120) "✅ CORRECT" "❌ INCORRECT"))

(format t "~%Test 2: fact(7)~%")
(setf (vm-state *vm2*) :ready)
(defparameter *result5* (call-function *vm2* 'FACT 7))
(format t "→ Résultat: ~A (attendu: 5040)~%" *result5*)
(format t "   ~A~%" (if (= *result5* 5040) "✅ CORRECT" "❌ INCORRECT"))

(format t "~%Test 3: fact(10)~%")
(setf (vm-state *vm2*) :ready)
(defparameter *result6* (call-function *vm2* 'FACT 10))
(format t "→ Résultat: ~A (attendu: 3628800)~%" *result6*)
(format t "   ~A~%" (if (= *result6* 3628800) "✅ CORRECT" "❌ INCORRECT"))

;;; ============================================================================
;;; EXEMPLE 3: Ackermann (fonction à 2 arguments)
;;; ============================================================================

(format t "~%~%════════════════════════════════════════════════════════════════~%")
(format t "EXEMPLE 3: Ackermann (2 arguments)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Code Lisp:~%")
(format t "(defun ack (m n)~%")
(format t "  (cond~%")
(format t "    ((= m 0) (+ n 1))~%")
(format t "    ((= n 0) (ack (- m 1) 1))~%")
(format t "    (t (ack (- m 1) (ack m (- n 1))))))~%")

;; Compiler
(format t "~%Compilation...~%")
(defparameter *ack-code*
  (compile-lisp-to-mips-simplified
    '(defun ack (m n)
       (cond
         ((= m 0) (+ n 1))
         ((= n 0) (ack (- m 1) 1))
         (t (ack (- m 1) (ack m (- n 1))))))))
(format t "✓ ~A instructions générées~%" (length *ack-code*))

;; Créer VM et charger
(format t "~%Création de la VM et chargement...~%")
(defparameter *vm3* (make-new-vm))
(load-code *vm3* *ack-code*)
(format t "✓ Code chargé~%")

;; APPELS DIRECTS avec 2 arguments
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "APPELS DIRECTS avec call-function (2 arguments):~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Test 1: ack(2, 3)~%")
(defparameter *result7* (call-function *vm3* 'ACK 2 3))
(format t "→ Résultat: ~A (attendu: 9)~%" *result7*)
(format t "   ~A~%" (if (= *result7* 9) "✅ CORRECT" "❌ INCORRECT"))

(format t "~%Test 2: ack(3, 2)~%")
(setf (vm-state *vm3*) :ready)
(defparameter *result8* (call-function *vm3* 'ACK 3 2))
(format t "→ Résultat: ~A (attendu: 29)~%" *result8*)
(format t "   ~A~%" (if (= *result8* 29) "✅ CORRECT" "❌ INCORRECT"))

(format t "~%Test 3: ack(3, 4)~%")
(setf (vm-state *vm3*) :ready)
(defparameter *result9* (call-function *vm3* 'ACK 3 4))
(format t "→ Résultat: ~A (attendu: 125)~%" *result9*)
(format t "   ~A~%" (if (= *result9* 125) "✅ CORRECT" "❌ INCORRECT"))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                          RÉSUMÉ                                ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defparameter *total-tests* 9)
(defparameter *passed-tests*
  (+ (if (= *result1* 5) 1 0)
     (if (= *result2* 55) 1 0)
     (if (= *result3* 610) 1 0)
     (if (= *result4* 120) 1 0)
     (if (= *result5* 5040) 1 0)
     (if (= *result6* 3628800) 1 0)
     (if (= *result7* 9) 1 0)
     (if (= *result8* 29) 1 0)
     (if (= *result9* 125) 1 0)))

(format t "~%Tests réussis: ~A/~A~%" *passed-tests* *total-tests*)

(if (= *passed-tests* *total-tests*)
    (format t "~%✅ TOUS LES TESTS RÉUSSIS!~%")
    (format t "~%⚠ ~A test(s) échoué(s)~%" (- *total-tests* *passed-tests*)))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "~%AVANTAGES DE call-function:~%")
(format t "  • Pas besoin de localiser manuellement la fonction~%")
(format t "  • Pas besoin de configurer les registres~%")
(format t "  • Gestion automatique des erreurs~%")
(format t "  • Supporte jusqu'à 4 arguments ($a0-$a3)~%")
(format t "  • Utilisation simple: (call-function vm 'FONCTION arg1 arg2...)~%")
(format t "~%════════════════════════════════════════════════════════════════~%~%")
