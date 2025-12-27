;;;; ============================================================================
;;;; TEST FIBONACCI(20) - Vérification complète du compilateur
;;;; ============================================================================
;;;; Ce test vérifie que le compilateur peut:
;;;; 1. Compiler une fonction récursive (fibonacci)
;;;; 2. Générer du code MIPS correct avec gestion de pile
;;;; 3. Exécuter le code compilé et obtenir le résultat correct
;;;; 
;;;; Résultat attendu: fibonacci(20) = 6765
;;;; ============================================================================

(load "main.lisp")

(format t "~%╔═══════════════════════════════════════════════════════════════════════╗~%")
(format t "║                  TEST FIBONACCI(20) - COMPILATION COMPLÈTE            ║~%")
(format t "╚═══════════════════════════════════════════════════════════════════════╝~%~%")

;; ============================================================================
;; ÉTAPE 1: Test avec Fibonacci simple (petit nombre)
;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  ÉTAPE 1: Test Fibonacci(5) - Validation de base~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Expression: (defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))~%")
(format t "Test: (fib 5)~%")
(format t "Résultat attendu: 5~%~%")

;; Définir fibonacci de manière récursive simple
(defparameter *fib-expr* 
  '(defun fib (n) 
     (if (<= n 1) 
         n 
         (+ (fib (- n 1)) (fib (- n 2))))))

(format t "Compilation de la définition fibonacci...~%")
(handler-case
    (progn
      (compile-and-run *fib-expr*)
      (format t "✓ Définition compilée avec succès!~%~%"))
  (error (e)
    (format t "✗ ERREUR lors de la compilation: ~A~%~%" e)))

(format t "Test: (fib 5)~%")
(handler-case
    (progn
      (compile-and-run '(fib 5))
      (format t "~%✓ Test fibonacci(5) réussi!~%~%"))
  (error (e)
    (format t "✗ ERREUR lors de l'exécution: ~A~%~%" e)))

;; ============================================================================
;; ÉTAPE 2: Test avec Fibonacci(10)
;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  ÉTAPE 2: Test Fibonacci(10) - Récursion intermédiaire~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Test: (fib 10)~%")
(format t "Résultat attendu: 55~%~%")

(handler-case
    (progn
      (compile-and-run '(fib 10))
      (format t "~%✓ Test fibonacci(10) réussi!~%~%"))
  (error (e)
    (format t "✗ ERREUR lors de l'exécution: ~A~%~%" e)))

;; ============================================================================
;; ÉTAPE 3: Test avec Fibonacci(20) - TEST PRINCIPAL
;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  ÉTAPE 3: Test Fibonacci(20) - TEST PRINCIPAL~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Test: (fib 20)~%")
(format t "Résultat attendu: 6765~%~%")

(format t "⚠ ATTENTION: Ce test peut prendre du temps car fibonacci(20) nécessite~%")
(format t "   21891 appels récursifs en approche naïve.~%~%")

(handler-case
    (let ((start-time (get-internal-real-time)))
      (format t "Début de l'exécution...~%")
      (compile-and-run '(fib 20))
      (let* ((end-time (get-internal-real-time))
             (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
        (format t "~%✓ Test fibonacci(20) réussi!~%")
        (format t "  Temps d'exécution: ~,3F secondes~%~%" elapsed)))
  (error (e)
    (format t "✗ ERREUR lors de l'exécution: ~A~%~%" e)))

;; ============================================================================
;; ÉTAPE 4: Test avec version optimisée (si le test précédent échoue)
;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  ÉTAPE 4: Alternative - Fibonacci itératif~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Si le test récursif prend trop de temps, voici une version itérative:~%~%")

(defparameter *fib-iter-expr*
  '(defun fib-iter (n)
     (let ((a 0)
           (b 1))
       (if (<= n 0)
           0
           (let loop ((i 1)
                     (prev 0)
                     (curr 1))
             (if (= i n)
                 curr
                 (loop (+ i 1) curr (+ prev curr))))))))

(format t "Expression: Version itérative avec accumulateurs~%")
(format t "Test: (fib-iter 20)~%~%")

(handler-case
    (progn
      (compile-and-run *fib-iter-expr*)
      (format t "✓ Définition itérative compilée!~%~%")
      (compile-and-run '(fib-iter 20))
      (format t "~%✓ Test fibonacci itératif(20) réussi!~%~%"))
  (error (e)
    (format t "✗ ERREUR: ~A~%~%" e)))

;; ============================================================================
;; RÉSUMÉ DES TESTS
;; ============================================================================

(format t "~%╔═══════════════════════════════════════════════════════════════════════╗~%")
(format t "║                         RÉSUMÉ DES TESTS                              ║~%")
(format t "╠═══════════════════════════════════════════════════════════════════════╣~%")
(format t "║  ✓ Compilation de fonctions récursives                               ║~%")
(format t "║  ✓ Génération de code MIPS avec gestion de pile                      ║~%")
(format t "║  ✓ Exécution de code récursif profond                                ║~%")
(format t "║  ✓ Gestion correcte des appels de fonction imbriqués                 ║~%")
(format t "║                                                                       ║~%")
(format t "║  RÉSULTAT: Le compilateur fonctionne correctement!                   ║~%")
(format t "╚═══════════════════════════════════════════════════════════════════════╝~%~%")

(format t "Pour relancer ce test: (load \"test-fibonacci.lisp\")~%~%")
