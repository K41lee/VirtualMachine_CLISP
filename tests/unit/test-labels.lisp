;;;; test-labels.lisp
;;;; Tests pour la forme spéciale LABELS (Phase 8)

(load "compiler.lisp")

(format t "~%================================================================================~%")
(format t "                         TESTS LABELS - PHASE 8~%")
(format t "================================================================================~%~%")

;;; ============================================================================
;;; TEST 1: LABELS simple non récursif
;;; ============================================================================

(defun test-labels-simple ()
  (format t "~%TEST 1 : LABELS simple non récursif~%")
  (format t "---------------------------------~%")
  (format t "Expression : (labels ((double (x) (* x 2)))~%")
  (format t "               (double 5))~%~%")
  
  (let* ((expr '(labels ((double (x) (* x 2)))
                  (double 5)))
         (asm-code (compile-lisp expr))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
    (dolist (instr asm-code)
      (format t "~A~%" instr))
    
    (format t "~%=== EXÉCUTION ===~%")
    (format t "Résultat dans $v0: ~A~%" result)
    (format t "Résultat : ~A~%" result)
    (if (= result 10)
        (format t "✓ TEST 1 RÉUSSI~%~%")
        (format t "✗ TEST 1 ÉCHOUÉ (attendu: 10, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 2: LABELS récursif simple (factorielle)
;;; ============================================================================

(defun test-labels-recursive ()
  (format t "~%TEST 2 : LABELS récursif (factorielle)~%")
  (format t "---------------------------------------~%")
  (format t "Expression : (labels ((fact (n)~%")
  (format t "                        (if (<= n 1)~%")
  (format t "                          1~%")
  (format t "                          (* n (fact (- n 1))))))~%")
  (format t "               (fact 5))~%")
  (format t "Attendu : 120~%~%")
  
  (let* ((expr '(labels ((fact (n)
                           (if (<= n 1)
                             1
                             (* n (fact (- n 1))))))
                  (fact 5)))
         (asm-code (compile-lisp expr))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
    (format t "Nombre d'instructions: ~A~%~%" (length asm-code))
    
    (format t "=== EXÉCUTION ===~%")
    (format t "Résultat dans $v0: ~A~%" result)
    (format t "Résultat : ~A~%" result)
    (if (= result 120)
        (format t "✓ TEST 2 RÉUSSI~%~%")
        (format t "✗ TEST 2 ÉCHOUÉ (attendu: 120, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 3: Récursion mutuelle (pair/impair)
;;; ============================================================================

(defun test-labels-mutual ()
  (format t "~%TEST 3 : Récursion mutuelle (pair/impair)~%")
  (format t "------------------------------------------~%")
  (format t "Expression : (labels ((even? (n)~%")
  (format t "                        (if (= n 0) 1 (odd? (- n 1))))~%")
  (format t "                      (odd? (n)~%")
  (format t "                        (if (= n 0) 0 (even? (- n 1)))))~%")
  (format t "               (even? 10))~%")
  (format t "Attendu : 1 (vrai)~%~%")
  
  (let* ((expr '(labels ((even? (n)
                           (if (= n 0)
                             1
                             (odd? (- n 1))))
                         (odd? (n)
                           (if (= n 0)
                             0
                             (even? (- n 1)))))
                  (even? 10)))
         (asm-code (compile-lisp expr))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
    (format t "Nombre d'instructions: ~A~%~%" (length asm-code))
    
    (format t "=== EXÉCUTION ===~%")
    (format t "Résultat dans $v0: ~A~%" result)
    (format t "Résultat : ~A~%" result)
    (if (= result 1)
        (format t "✓ TEST 3 RÉUSSI~%~%")
        (format t "✗ TEST 3 ÉCHOUÉ (attendu: 1, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 4: LABELS avec LET (capture variables)
;;; ============================================================================

(defun test-labels-with-let ()
  (format t "~%TEST 4 : LABELS avec LET~%")
  (format t "-------------------------~%")
  (format t "Expression : (let ((x 10))~%")
  (format t "               (labels ((add-x (y) (+ x y)))~%")
  (format t "                 (add-x 5)))~%")
  (format t "Attendu : 15~%~%")
  
  (let* ((expr '(let ((x 10))
                  (labels ((add-x (y) (+ x y)))
                    (add-x 5))))
         (asm-code (compile-lisp expr))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
    (format t "Nombre d'instructions: ~A~%~%" (length asm-code))
    
    (format t "=== EXÉCUTION ===~%")
    (format t "Résultat dans $v0: ~A~%" result)
    (format t "Résultat : ~A~%" result)
    (if (= result 15)
        (format t "✓ TEST 4 RÉUSSI~%~%")
        (format t "✗ TEST 4 ÉCHOUÉ (attendu: 15, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 5: Plusieurs fonctions non récursives
;;; ============================================================================

(defun test-labels-multiple ()
  (format t "~%TEST 5 : Plusieurs fonctions~%")
  (format t "-----------------------------~%")
  (format t "Expression : (labels ((double (x) (* x 2))~%")
  (format t "                      (triple (x) (* x 3))~%")
  (format t "                      (sum-both (x) (+ (double x) (triple x))))~%")
  (format t "               (sum-both 4))~%")
  (format t "Attendu : 8 + 12 = 20~%~%")
  
  (let* ((expr '(labels ((double (x) (* x 2))
                         (triple (x) (* x 3))
                         (sum-both (x) (+ (double x) (triple x))))
                  (sum-both 4)))
         (asm-code (compile-lisp expr))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
    (format t "Nombre d'instructions: ~A~%~%" (length asm-code))
    
    (format t "=== EXÉCUTION ===~%")
    (format t "Résultat dans $v0: ~A~%" result)
    (format t "Résultat : ~A~%" result)
    (if (= result 20)
        (format t "✓ TEST 5 RÉUSSI~%~%")
        (format t "✗ TEST 5 ÉCHOUÉ (attendu: 20, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 6: LABELS imbriqué
;;; ============================================================================

(defun test-labels-nested ()
  (format t "~%TEST 6 : LABELS imbriqué~%")
  (format t "------------------------~%")
  (format t "Expression : (labels ((outer (x)~%")
  (format t "                        (labels ((inner (y)~%")
  (format t "                                   (* x y)))~%")
  (format t "                          (inner 3))))~%")
  (format t "               (outer 5))~%")
  (format t "Attendu : 15~%~%")
  
  (let* ((expr '(labels ((outer (x)
                           (labels ((inner (y)
                                      (* x y)))
                             (inner 3))))
                  (outer 5)))
         (asm-code (compile-lisp expr))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
    (format t "Nombre d'instructions: ~A~%~%" (length asm-code))
    
    (format t "=== EXÉCUTION ===~%")
    (format t "Résultat dans $v0: ~A~%" result)
    (format t "Résultat : ~A~%" result)
    (if (= result 15)
        (format t "✓ TEST 6 RÉUSSI~%~%")
        (format t "✗ TEST 6 ÉCHOUÉ (attendu: 15, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; Suite de tests
;;; ============================================================================

(defun run-all-labels-tests ()
  (format t "~%================================================================================~%")
  (format t "                         SUITE DE TESTS LABELS~%")
  (format t "================================================================================~%")
  
  (let ((start-time (get-internal-real-time)))
    (test-labels-simple)
    (test-labels-recursive)
    (test-labels-mutual)
    (test-labels-with-let)
    (test-labels-multiple)
    (test-labels-nested)
    
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "~%================================================================================~%")
      (format t "Tests terminés en ~,3F secondes~%" elapsed)
      (format t "================================================================================~%~%"))))

(format t "~%~%Tests LABELS chargés.~%")
(format t "Fonctions disponibles:~%")
(format t "  - (test-labels-simple)     : Test LABELS non récursif~%")
(format t "  - (test-labels-recursive)  : Test LABELS récursif~%")
(format t "  - (test-labels-mutual)     : Test récursion mutuelle~%")
(format t "  - (test-labels-with-let)   : Test LABELS avec LET~%")
(format t "  - (test-labels-multiple)   : Test plusieurs fonctions~%")
(format t "  - (test-labels-nested)     : Test LABELS imbriqué~%")
(format t "  - (run-all-labels-tests)   : Exécuter tous les tests~%~%")
