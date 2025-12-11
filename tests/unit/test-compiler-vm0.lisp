;;; Test simple du compilateur et de l'exécution sur VM0

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(format t "║  TEST COMPILATEUR + VM0                                ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; ÉTAPE 1 : Charger VM0 et le compilateur
;;; ============================================================================

(format t "Chargement VM0 (interpréteur)...~%")
(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")
(format t "✓ VM0 chargée~%~%")

(format t "Chargement du compilateur...~%")
(load "src/compiler.lisp")
(format t "✓ Compilateur chargé~%~%")

;;; ============================================================================
;;; SUITE DE TESTS
;;; ============================================================================

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun run-test (name expr expected)
  "Compile et exécute un test, vérifie le résultat"
  (format t "Test ~A : ~A~%" name expr)
  (handler-case
      (let* ((code (compile-lisp expr))
             (vm (make-new-vm :verbose nil)))
        (load-code vm (append code (list (list :HALT))))
        (run-vm vm :max-instructions 10000)
        (let ((result (get-register vm :$V0)))
          (if (= result expected)
              (progn
                (format t "  ✓ RÉUSSI (résultat: ~A)~%~%" result)
                (incf *tests-passed*))
              (progn
                (format t "  ✗ ÉCHOUÉ (attendu: ~A, obtenu: ~A)~%~%" expected result)
                (incf *tests-failed*)))))
    (error (e)
      (format t "  ✗ ERREUR : ~A~%~%" e)
      (incf *tests-failed*))))

(format t "═══════════════════════════════════════════════════════~%")
(format t "TESTS DE BASE~%")
(format t "═══════════════════════════════════════════════════════~%~%")

;; Tests arithmétiques simples
(run-test "Addition" '(+ 2 3) 5)
(run-test "Soustraction" '(- 10 3) 7)
(run-test "Multiplication" '(* 4 5) 20)
(run-test "Division" '(/ 20 4) 5)

;; Tests de comparaison
(run-test "Comparaison <" '(if (< 3 5) 1 0) 1)
(run-test "Comparaison >" '(if (> 3 5) 1 0) 0)
(run-test "Comparaison =" '(if (= 5 5) 1 0) 1)

;; Tests LET
(run-test "LET simple" '(let ((x 10)) x) 10)
(run-test "LET double" '(let ((x 5) (y 3)) (+ x y)) 8)
(run-test "LET imbriqué" '(let ((x 5)) (let ((y 3)) (+ x y))) 8)

(format t "~%═══════════════════════════════════════════════════════~%")
(format t "TESTS WHILE~%")
(format t "═══════════════════════════════════════════════════════~%~%")

;; Test WHILE
(run-test "WHILE somme 1-10" 
          '(let ((sum 0) (i 1))
             (while (<= i 10)
               (setq sum (+ sum i))
               (setq i (+ i 1)))
             sum)
          55)

(run-test "WHILE factorielle 5"
          '(let ((result 1) (n 5))
             (while (> n 0)
               (setq result (* result n))
               (setq n (- n 1)))
             result)
          120)

(format t "~%═══════════════════════════════════════════════════════~%")
(format t "TESTS DEFUN~%")
(format t "═══════════════════════════════════════════════════════~%~%")

;; Tests DEFUN
(run-test "DEFUN carré"
          '(progn
             (defun carre (x) (* x x))
             (carre 7))
          49)

(run-test "DEFUN addition"
          '(progn
             (defun add (a b) (+ a b))
             (add 15 25))
          40)

(run-test "DEFUN récursive (factorielle 5)"
          '(progn
             (defun fact (n)
               (if (<= n 1)
                   1
                   (* n (fact (- n 1)))))
             (fact 5))
          120)

(run-test "DEFUN récursive (fibonacci 8)"
          '(progn
             (defun fib (n)
               (if (<= n 1)
                   n
                   (+ (fib (- n 1)) (fib (- n 2)))))
             (fib 8))
          21)

(format t "~%═══════════════════════════════════════════════════════~%")
(format t "TESTS AVANCÉS~%")
(format t "═══════════════════════════════════════════════════════~%~%")

;; Test CASE
(run-test "CASE valeur 2"
          '(let ((x 2))
             (case x
               (1 10)
               (2 20)
               (3 30)
               (t 0)))
          20)

;; Test COND
(run-test "COND multiple"
          '(let ((x 15))
             (cond
               ((< x 10) 1)
               ((< x 20) 2)
               (t 3)))
          2)

;; Test PROGN
(run-test "PROGN séquence"
          '(progn
             (let ((x 5))
               (setq x (+ x 3))
               (setq x (* x 2))
               x))
          16)

(format t "~%═══════════════════════════════════════════════════════~%")
(format t "TESTS ARRAYS (PHASE 11)~%")
(format t "═══════════════════════════════════════════════════════~%~%")

;; Tests avec arrays
(run-test "MAKE-ARRAY + AREF"
          '(let ((arr (make-array 5 :initial-element 0)))
             (setq (aref arr 2) 42)
             (aref arr 2))
          42)

(run-test "Array somme"
          '(let ((arr (make-array 3 :initial-element 0))
                 (sum 0))
             (setq (aref arr 0) 10)
             (setq (aref arr 1) 20)
             (setq (aref arr 2) 30)
             (setq sum (+ (aref arr 0) (+ (aref arr 1) (aref arr 2))))
             sum)
          60)

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(format t "║  RÉSUMÉ DES TESTS                                      ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%~%")

(format t "Tests réussis : ~A~%" *tests-passed*)
(format t "Tests échoués : ~A~%" *tests-failed*)
(format t "Total         : ~A~%~%" (+ *tests-passed* *tests-failed*))

(let ((success-rate (if (> (+ *tests-passed* *tests-failed*) 0)
                        (round (* 100 (/ *tests-passed* (+ *tests-passed* *tests-failed*))))
                        0)))
  (format t "Taux de réussite : ~A%%~%~%" success-rate)
  
  (cond
    ((= success-rate 100)
     (format t "🎉 PARFAIT ! Tous les tests sont passés !~%"))
    ((>= success-rate 80)
     (format t "✓ TRÈS BIEN ! La majorité des tests passent.~%"))
    ((>= success-rate 50)
     (format t "⚠ MOYEN. Certains tests échouent.~%"))
    (t
     (format t "✗ PROBLÈME. Beaucoup de tests échouent.~%"))))

(format t "~%Le compilateur LISP → MIPS fonctionne et génère~%")
(format t "du code exécutable sur la VM0 !~%~%")
