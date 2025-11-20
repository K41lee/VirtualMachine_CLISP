;;;; test-let.lisp
;;;; Tests pour la structure LET

(load "compiler.lisp")

(format t "~%~%")
(format t "================================================================================~%")
(format t "                         TESTS LET - PHASE 7a~%")
(format t "================================================================================~%")
(format t "~%")

;;; ============================================================================
;;; TEST 1 : LET SIMPLE
;;; ============================================================================

(defun test-let-simple ()
  "Test : (let ((x 5)) x) → 5"
  (format t "TEST 1 : LET simple~%")
  (format t "-----------------~%")
  (format t "Expression : (let ((x 5)) x)~%")
  
  (let ((expr '(let ((x 5)) x))
        (expected 5))
    (handler-case
        (let* ((vm (compile-and-run expr))
               (result (get-register vm *reg-v0*)))
          (format t "Résultat : ~A~%" result)
          (if (= result expected)
              (format t "✓ TEST 1 RÉUSSI~%~%")
              (format t "✗ TEST 1 ÉCHOUÉ (attendu: ~A, obtenu: ~A)~%~%" expected result)))
      (error (e)
        (format t "✗ TEST 1 ERREUR : ~A~%~%" e)))))

;;; ============================================================================
;;; TEST 2 : LET MULTIPLE VARIABLES
;;; ============================================================================

(defun test-let-multiple ()
  "Test : (let ((x 3) (y 4)) (+ x y)) → 7"
  (format t "TEST 2 : LET avec multiples variables~%")
  (format t "--------------------------------------~%")
  (format t "Expression : (let ((x 3) (y 4)) (+ x y))~%")
  
  (let ((expr '(let ((x 3) (y 4)) (+ x y)))
        (expected 7))
    (handler-case
        (let* ((vm (compile-and-run expr))
               (result (get-register vm *reg-v0*)))
          (format t "Résultat : ~A~%" result)
          (if (= result expected)
              (format t "✓ TEST 2 RÉUSSI~%~%")
              (format t "✗ TEST 2 ÉCHOUÉ (attendu: ~A, obtenu: ~A)~%~%" expected result)))
      (error (e)
        (format t "✗ TEST 2 ERREUR : ~A~%~%" e)))))

;;; ============================================================================
;;; TEST 3 : LET SHADOWING
;;; ============================================================================

(defun test-let-shadowing ()
  "Test : (let ((x 1)) (let ((x 2)) x)) → 2"
  (format t "TEST 3 : LET avec shadowing~%")
  (format t "---------------------------~%")
  (format t "Expression : (let ((x 1)) (let ((x 2)) x))~%")
  
  (let ((expr '(let ((x 1)) (let ((x 2)) x)))
        (expected 2))
    (handler-case
        (let* ((vm (compile-and-run expr))
               (result (get-register vm *reg-v0*)))
          (format t "Résultat : ~A~%" result)
          (if (= result expected)
              (format t "✓ TEST 3 RÉUSSI~%~%")
              (format t "✗ TEST 3 ÉCHOUÉ (attendu: ~A, obtenu: ~A)~%~%" expected result)))
      (error (e)
        (format t "✗ TEST 3 ERREUR : ~A~%~%" e)))))

;;; ============================================================================
;;; TEST 4 : LET IMBRIQUÉ (accès variable externe)
;;; ============================================================================

(defun test-let-nested ()
  "Test : (let ((x 1)) (let ((y 2)) (+ x y))) → 3"
  (format t "TEST 4 : LET imbriqué avec accès variable externe~%")
  (format t "------------------------------------------------~%")
  (format t "Expression : (let ((x 1)) (let ((y 2)) (+ x y)))~%")
  
  (let ((expr '(let ((x 1)) (let ((y 2)) (+ x y))))
        (expected 3))
    (handler-case
        (let* ((vm (compile-and-run expr))
               (result (get-register vm *reg-v0*)))
          (format t "Résultat : ~A~%" result)
          (if (= result expected)
              (format t "✓ TEST 4 RÉUSSI~%~%")
              (format t "✗ TEST 4 ÉCHOUÉ (attendu: ~A, obtenu: ~A)~%~%" expected result)))
      (error (e)
        (format t "✗ TEST 4 ERREUR : ~A~%~%" e)))))

;;; ============================================================================
;;; TEST 5 : LET AVEC IF
;;; ============================================================================

(defun test-let-with-if ()
  "Test : (let ((x 10)) (if (< x 20) x 0)) → 10"
  (format t "TEST 5 : LET avec IF~%")
  (format t "--------------------~%")
  (format t "Expression : (let ((x 10)) (if (< x 20) x 0))~%")
  
  (let ((expr '(let ((x 10)) (if (< x 20) x 0)))
        (expected 10))
    (handler-case
        (let* ((vm (compile-and-run expr))
               (result (get-register vm *reg-v0*)))
          (format t "Résultat : ~A~%" result)
          (if (= result expected)
              (format t "✓ TEST 5 RÉUSSI~%~%")
              (format t "✗ TEST 5 ÉCHOUÉ (attendu: ~A, obtenu: ~A)~%~%" expected result)))
      (error (e)
        (format t "✗ TEST 5 ERREUR : ~A~%~%" e)))))

;;; ============================================================================
;;; TEST 6 : LET AVEC EXPRESSION COMPLEXE
;;; ============================================================================

(defun test-let-complex ()
  "Test : (let ((a 2) (b 3)) (* (+ a b) (- a b))) → 5 * (-1) = -5"
  (format t "TEST 6 : LET avec expression complexe~%")
  (format t "--------------------------------------~%")
  (format t "Expression : (let ((a 2) (b 3)) (* (+ a b) (- a b)))~%")
  
  (let ((expr '(let ((a 2) (b 3)) (* (+ a b) (- a b))))
        (expected -5))
    (handler-case
        (let* ((vm (compile-and-run expr))
               (result (get-register vm *reg-v0*)))
          (format t "Résultat : ~A~%" result)
          (if (= result expected)
              (format t "✓ TEST 6 RÉUSSI~%~%")
              (format t "✗ TEST 6 ÉCHOUÉ (attendu: ~A, obtenu: ~A)~%~%" expected result)))
      (error (e)
        (format t "✗ TEST 6 ERREUR : ~A~%~%" e)))))

;;; ============================================================================
;;; SUITE DE TESTS COMPLÈTE
;;; ============================================================================

(defun run-all-let-tests ()
  "Exécute tous les tests LET"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                         SUITE DE TESTS LET~%")
  (format t "================================================================================~%")
  (format t "~%")
  
  (let ((start-time (get-internal-real-time)))
    (test-let-simple)
    (test-let-multiple)
    (test-let-shadowing)
    (test-let-nested)
    (test-let-with-if)
    (test-let-complex)
    
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "================================================================================~%")
      (format t "Tests terminés en ~,3F secondes~%" elapsed)
      (format t "================================================================================~%")
      (format t "~%"))))

;;; ============================================================================
;;; MESSAGE D'AIDE
;;; ============================================================================

(format t "~%Tests LET chargés.~%")
(format t "Fonctions disponibles:~%")
(format t "  - (test-let-simple)     : Test LET simple~%")
(format t "  - (test-let-multiple)   : Test LET avec multiples variables~%")
(format t "  - (test-let-shadowing)  : Test LET avec shadowing~%")
(format t "  - (test-let-nested)     : Test LET imbriqué~%")
(format t "  - (test-let-with-if)    : Test LET avec IF~%")
(format t "  - (test-let-complex)    : Test LET avec expression complexe~%")
(format t "  - (run-all-let-tests)   : Exécuter tous les tests~%")
(format t "~%")
