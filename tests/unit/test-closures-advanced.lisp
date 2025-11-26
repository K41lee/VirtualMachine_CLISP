;;;; test-closures-advanced.lisp
;;;; Tests avancés pour les closures (Phase 10)

(load "compiler.lisp")

(format t "~%================================================================================~%")
(format t "                   TESTS CLOSURES AVANCÉS - PHASE 10~%")
(format t "================================================================================~%~%")

;;; ============================================================================
;;; TEST 1: 3 niveaux d'imbrication
;;; ============================================================================

(defun test-closure-3-levels ()
  (format t "~%TEST 1 : 3 niveaux d'imbrication~%")
  (format t "---------------------------------~%")
  (format t "Expression : (labels ((level1 (a)~%")
  (format t "                        (labels ((level2 (b)~%")
  (format t "                                   (labels ((level3 (c)~%")
  (format t "                                              (+ a (+ b c))))~%")
  (format t "                                     (level3 30))))~%")
  (format t "                          (level2 20))))~%")
  (format t "               (level1 10))~%")
  (format t "Attendu : 10 + 20 + 30 = 60~%~%")
  
  (let* ((expr '(labels ((level1 (a)
                           (labels ((level2 (b)
                                      (labels ((level3 (c)
                                                 (+ a (+ b c))))
                                        (level3 30))))
                             (level2 20))))
                  (level1 10)))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "Résultat : ~A~%" result)
    (if (= result 60)
        (format t "✓ TEST 1 RÉUSSI~%~%")
        (format t "✗ TEST 1 ÉCHOUÉ (attendu: 60, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 2: Multiples variables capturées
;;; ============================================================================

(defun test-closure-multiple-vars ()
  (format t "~%TEST 2 : Multiples variables capturées~%")
  (format t "----------------------------------------~%")
  (format t "Expression : (labels ((outer (x y z)~%")
  (format t "                        (labels ((inner (w)~%")
  (format t "                                   (+ x (+ y (+ z w)))))~%")
  (format t "                          (inner 4))))~%")
  (format t "               (outer 1 2 3))~%")
  (format t "Attendu : 1 + 2 + 3 + 4 = 10~%~%")
  
  (let* ((expr '(labels ((outer (x y z)
                           (labels ((inner (w)
                                      (+ x (+ y (+ z w)))))
                             (inner 4))))
                  (outer 1 2 3)))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "Résultat : ~A~%" result)
    (if (= result 10)
        (format t "✓ TEST 2 RÉUSSI~%~%")
        (format t "✗ TEST 2 ÉCHOUÉ (attendu: 10, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 3: Closure avec récursion utilisant variable capturée
;;; ============================================================================

(defun test-closure-recursive-capture ()
  (format t "~%TEST 3 : Closure récursive avec capture~%")
  (format t "----------------------------------------~%")
  (format t "Expression : (labels ((outer (x)~%")
  (format t "                        (labels ((countdown (n)~%")
  (format t "                                   (if (<= n 0)~%")
  (format t "                                     x~%")
  (format t "                                     (countdown (- n 1)))))~%")
  (format t "                          (countdown 5))))~%")
  (format t "               (outer 42))~%")
  (format t "Attendu : 42 (après 5 itérations)~%~%")
  
  (let* ((expr '(labels ((outer (x)
                           (labels ((countdown (n)
                                      (if (<= n 0)
                                        x
                                        (countdown (- n 1)))))
                             (countdown 5))))
                  (outer 42)))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "Résultat : ~A~%" result)
    (if (= result 42)
        (format t "✓ TEST 3 RÉUSSI~%~%")
        (format t "✗ TEST 3 ÉCHOUÉ (attendu: 42, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 4: Closure modifiant variable capturée via SETQ
;;; ============================================================================

(defun test-closure-setq ()
  (format t "~%TEST 4 : Closure avec SETQ sur variable capturée~%")
  (format t "-------------------------------------------------~%")
  (format t "Expression : (labels ((outer (x)~%")
  (format t "                        (labels ((increment ()~%")
  (format t "                                   (setq x (+ x 1))~%")
  (format t "                                   x))~%")
  (format t "                          (increment))))~%")
  (format t "               (outer 10))~%")
  (format t "Attendu : 11~%~%")
  
  (let* ((expr '(labels ((outer (x)
                           (labels ((increment ()
                                      (setq x (+ x 1))
                                      x))
                             (increment))))
                  (outer 10)))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "Résultat : ~A~%" result)
    (if (= result 11)
        (format t "✓ TEST 4 RÉUSSI~%~%")
        (format t "✗ TEST 4 ÉCHOUÉ (attendu: 11, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 5: Closure avec opérations complexes
;;; ============================================================================

(defun test-closure-complex ()
  (format t "~%TEST 5 : Closure avec opérations complexes~%")
  (format t "--------------------------------------------~%")
  (format t "Expression : (labels ((make-multiplier (factor)~%")
  (format t "                        (labels ((multiply (n)~%")
  (format t "                                   (* factor n))~%")
  (format t "                                 (apply-twice (n)~%")
  (format t "                                   (multiply (multiply n))))~%")
  (format t "                          (apply-twice 3))))~%")
  (format t "               (make-multiplier 2))~%")
  (format t "Attendu : 2 * (2 * 3) = 12~%~%")
  
  (let* ((expr '(labels ((make-multiplier (factor)
                           (labels ((multiply (n)
                                      (* factor n))
                                    (apply-twice (n)
                                      (multiply (multiply n))))
                             (apply-twice 3))))
                  (make-multiplier 2)))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "Résultat : ~A~%" result)
    (if (= result 12)
        (format t "✓ TEST 5 RÉUSSI~%~%")
        (format t "✗ TEST 5 ÉCHOUÉ (attendu: 12, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; TEST 6: Closure avec LET et multiples captures
;;; ============================================================================

(defun test-closure-let-multiple ()
  (format t "~%TEST 6 : Closure avec LET et multiples captures~%")
  (format t "------------------------------------------------~%")
  (format t "Expression : (let ((x 5) (y 10))~%")
  (format t "               (labels ((compute (z)~%")
  (format t "                          (+ x (+ y z))))~%")
  (format t "                 (compute 15)))~%")
  (format t "Attendu : 5 + 10 + 15 = 30~%~%")
  
  (let* ((expr '(let ((x 5) (y 10))
                  (labels ((compute (z)
                             (+ x (+ y z))))
                    (compute 15))))
         (vm (compile-and-run expr))
         (result (get-register vm *reg-v0*)))
    
    (format t "Résultat : ~A~%" result)
    (if (= result 30)
        (format t "✓ TEST 6 RÉUSSI~%~%")
        (format t "✗ TEST 6 ÉCHOUÉ (attendu: 30, obtenu: ~A)~%~%" result))
    result))

;;; ============================================================================
;;; Suite de tests
;;; ============================================================================

(defun run-all-advanced-closure-tests ()
  (format t "~%================================================================================~%")
  (format t "                   SUITE DE TESTS CLOSURES AVANCÉS~%")
  (format t "================================================================================~%")
  
  (let ((start-time (get-internal-real-time))
        (passed 0)
        (failed 0))
    
    (handler-case (if (test-closure-3-levels) (incf passed) (incf failed))
      (error (e) (format t "✗ TEST 1 ERREUR: ~A~%~%" e) (incf failed)))
    
    (handler-case (if (test-closure-multiple-vars) (incf passed) (incf failed))
      (error (e) (format t "✗ TEST 2 ERREUR: ~A~%~%" e) (incf failed)))
    
    (handler-case (if (test-closure-recursive-capture) (incf passed) (incf failed))
      (error (e) (format t "✗ TEST 3 ERREUR: ~A~%~%" e) (incf failed)))
    
    (handler-case (if (test-closure-setq) (incf passed) (incf failed))
      (error (e) (format t "✗ TEST 4 ERREUR: ~A~%~%" e) (incf failed)))
    
    (handler-case (if (test-closure-complex) (incf passed) (incf failed))
      (error (e) (format t "✗ TEST 5 ERREUR: ~A~%~%" e) (incf failed)))
    
    (handler-case (if (test-closure-let-multiple) (incf passed) (incf failed))
      (error (e) (format t "✗ TEST 6 ERREUR: ~A~%~%" e) (incf failed)))
    
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "~%================================================================================~%")
      (format t "RÉSUMÉ: ~A/6 tests réussis~%" passed)
      (format t "Tests terminés en ~,3F secondes~%" elapsed)
      (format t "================================================================================~%~%"))))

(format t "~%~%Tests closures avancés chargés.~%")
(format t "Fonctions disponibles:~%")
(format t "  - (test-closure-3-levels)           : Test 3 niveaux imbrication~%")
(format t "  - (test-closure-multiple-vars)      : Test multiples variables~%")
(format t "  - (test-closure-recursive-capture)  : Test récursion avec capture~%")
(format t "  - (test-closure-setq)               : Test SETQ sur variable capturée~%")
(format t "  - (test-closure-complex)            : Test opérations complexes~%")
(format t "  - (test-closure-let-multiple)       : Test LET multiples captures~%")
(format t "  - (run-all-advanced-closure-tests)  : Exécuter tous les tests~%~%")
