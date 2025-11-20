;;;; test-loop.lisp
;;;; Tests pour la structure LOOP

(load "compiler.lisp")

(format t "~%~%")
(format t "================================================================================~%")
(format t "                         TESTS LOOP - PHASE 7b~%")
(format t "================================================================================~%")
(format t "~%")

;;; ============================================================================
;;; TEST 1 : LOOP SIMPLE (compteur)
;;; ============================================================================

(defun test-loop-simple ()
  "Test : Boucle simple qui compte de 0 à 5"
  (format t "TEST 1 : LOOP simple avec compteur~%")
  (format t "---------------------------------~%")
  (format t "Expression : (let ((x 0))~%")
  (format t "               (loop while (< x 5) do (setq x (+ x 1)))~%")
  (format t "               x)~%")
  
  (let ((expr '(let ((x 0))
                 (loop while (< x 5) do (setq x (+ x 1)))
                 x))
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
;;; TEST 2 : LOOP AVEC CALCUL
;;; ============================================================================

(defun test-loop-calcul ()
  "Test : Boucle qui calcule une somme"
  (format t "TEST 2 : LOOP avec accumulation~%")
  (format t "-------------------------------~%")
  (format t "Expression : (let ((i 1) (sum 0))~%")
  (format t "               (loop while (< i 6) do~%")
  (format t "                 (setq sum (+ sum i))~%")
  (format t "                 (setq i (+ i 1)))~%")
  (format t "               sum)~%")
  (format t "Attendu : 1+2+3+4+5 = 15~%")
  
  (let ((expr '(let ((i 1) (sum 0))
                 (loop while (< i 6) do
                   (setq sum (+ sum i))
                   (setq i (+ i 1)))
                 sum))
        (expected 15))
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
;;; TEST 3 : LOOP AVEC CONDITION COMPLEXE
;;; ============================================================================

(defun test-loop-condition ()
  "Test : Boucle avec condition complexe"
  (format t "TEST 3 : LOOP avec condition multiple~%")
  (format t "-------------------------------------~%")
  (format t "Expression : (let ((x 0) (y 0))~%")
  (format t "               (loop while (< x 10) do~%")
  (format t "                 (setq y (+ y x))~%")
  (format t "                 (setq x (+ x 2)))~%")
  (format t "               y)~%")
  (format t "Attendu : 0+2+4+6+8 = 20~%")
  
  (let ((expr '(let ((x 0) (y 0))
                 (loop while (< x 10) do
                   (setq y (+ y x))
                   (setq x (+ x 2)))
                 y))
        (expected 20))
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
;;; TEST 4 : LOOP IMBRIQUÉ
;;; ============================================================================

(defun test-loop-nested ()
  "Test : Boucles imbriquées"
  (format t "TEST 4 : LOOP imbriqué~%")
  (format t "----------------------~%")
  (format t "Expression : (let ((i 0) (j 0) (total 0))~%")
  (format t "               (loop while (< i 3) do~%")
  (format t "                 (setq j 0)~%")
  (format t "                 (loop while (< j 3) do~%")
  (format t "                   (setq total (+ total 1))~%")
  (format t "                   (setq j (+ j 1)))~%")
  (format t "                 (setq i (+ i 1)))~%")
  (format t "               total)~%")
  (format t "Attendu : 3×3 = 9~%")
  
  (let ((expr '(let ((i 0) (j 0) (total 0))
                 (loop while (< i 3) do
                   (setq j 0)
                   (loop while (< j 3) do
                     (setq total (+ total 1))
                     (setq j (+ j 1)))
                   (setq i (+ i 1)))
                 total))
        (expected 9))
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
;;; TEST 5 : LOOP AVEC IF
;;; ============================================================================

(defun test-loop-with-if ()
  "Test : Boucle avec condition IF interne"
  (format t "TEST 5 : LOOP avec IF interne~%")
  (format t "-----------------------------~%")
  (format t "Expression : (let ((i 0) (even 0) (odd 0))~%")
  (format t "               (loop while (< i 10) do~%")
  (format t "                 (if (= (mod i 2) 0)~%")
  (format t "                   (setq even (+ even 1))~%")
  (format t "                   (setq odd (+ odd 1)))~%")
  (format t "                 (setq i (+ i 1)))~%")
  (format t "               even)~%")
  (format t "Attendu : 5 nombres pairs (0,2,4,6,8)~%")
  
  (let ((expr '(let ((i 0) (even 0) (odd 0))
                 (loop while (< i 10) do
                   (if (= (mod i 2) 0)
                     (setq even (+ even 1))
                     (setq odd (+ odd 1)))
                   (setq i (+ i 1)))
                 even))
        (expected 5))
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
;;; SUITE DE TESTS COMPLÈTE
;;; ============================================================================

(defun run-all-loop-tests ()
  "Exécute tous les tests LOOP"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                         SUITE DE TESTS LOOP~%")
  (format t "================================================================================~%")
  (format t "~%")
  
  (let ((start-time (get-internal-real-time)))
    (test-loop-simple)
    (test-loop-calcul)
    (test-loop-condition)
    (test-loop-nested)
    (test-loop-with-if)
    
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "================================================================================~%")
      (format t "Tests terminés en ~,3F secondes~%" elapsed)
      (format t "================================================================================~%")
      (format t "~%"))))

;;; ============================================================================
;;; MESSAGE D'AIDE
;;; ============================================================================

(format t "~%Tests LOOP chargés.~%")
(format t "Fonctions disponibles:~%")
(format t "  - (test-loop-simple)     : Test LOOP simple~%")
(format t "  - (test-loop-calcul)     : Test LOOP avec accumulation~%")
(format t "  - (test-loop-condition)  : Test LOOP avec condition~%")
(format t "  - (test-loop-nested)     : Test LOOP imbriqué~%")
(format t "  - (test-loop-with-if)    : Test LOOP avec IF~%")
(format t "  - (run-all-loop-tests)   : Exécuter tous les tests~%")
(format t "~%")
