;;;; test-while.lisp
;;;; Tests unitaires pour WHILE loops
;;;; Phase 11 - Compilation de la VM

(load "main.lisp")

(format t "~%~%")
(format t "╔══════════════════════════════════════════════════════════════╗~%")
(format t "║         TESTS WHILE LOOPS - PHASE 11                        ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%")
(format t "~%")

(defparameter *test-count* 0)
(defparameter *test-passed* 0)

(defun test-while (name expr expected)
  "Teste une expression WHILE et vérifie le résultat"
  (incf *test-count*)
  (format t "Test ~A: ~A~%" *test-count* name)
  (format t "  Expression: ~S~%" expr)
  (format t "  Attendu: ~A~%" expected)
  
  (handler-case
      (let* ((vm (make-new-vm))
             (code (compile-lisp expr))
             (asm-with-halt (append code (list (list :HALT)))))
        (load-and-run vm asm-with-halt)
        (let ((result (get-register vm *reg-v0*)))
          (format t "  Résultat: ~A~%" result)
          (if (= result expected)
              (progn
                (incf *test-passed*)
                (format t "  ✅ PASS~%~%"))
              (progn
                (format t "  ❌ FAIL (attendu ~A, obtenu ~A)~%~%" expected result)))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%~%" e))))

;;; ============================================================================
;;; TESTS WHILE
;;; ============================================================================

(format t "═══ Test 1: WHILE simple compteur ═══~%")
(test-while "Boucle simple 0 à 5"
            '(let ((x 0))
               (while (< x 5)
                 (setq x (+ x 1)))
               x)
            5)

(format t "═══ Test 2: WHILE avec accumulation ═══~%")
(test-while "Somme 1 à 10"
            '(let ((sum 0)
                   (i 1))
               (while (<= i 10)
                 (progn
                   (setq sum (+ sum i))
                   (setq i (+ i 1))))
               sum)
            55)

(format t "═══ Test 3: WHILE condition fausse initiale ═══~%")
(test-while "Boucle jamais exécutée"
            '(let ((x 10))
               (while (< x 5)
                 (setq x (+ x 1)))
               x)
            10)

(format t "═══ Test 4: WHILE avec condition nil ═══~%")
(test-while "Condition constante nil"
            '(let ((x 0))
               (while nil
                 (setq x 99))
               x)
            0)

(format t "═══ Test 5: WHILE avec plusieurs instructions ═══~%")
(test-while "Body multiple"
            '(let ((x 0)
                   (y 0))
               (while (< x 3)
                 (progn
                   (setq y (+ y x))
                   (setq x (+ x 1))))
               y)
            3)  ; 0 + 1 + 2 = 3

(format t "═══ Test 6: WHILE imbriqué ═══~%")
(test-while "Boucles imbriquées"
            '(let ((i 0)
                   (j 0)
                   (sum 0))
               (while (< i 3)
                 (progn
                   (setq j 0)
                   (while (< j 3)
                     (progn
                       (setq sum (+ sum 1))
                       (setq j (+ j 1))))
                   (setq i (+ i 1))))
               sum)
            9)  ; 3 x 3 = 9

(format t "═══ Test 7: WHILE avec opération complexe ═══~%")
(test-while "Factorielle 5 (itérative)"
            '(let ((n 5)
                   (result 1))
               (while (> n 0)
                 (progn
                   (setq result (* result n))
                   (setq n (- n 1))))
               result)
            120)  ; 5! = 120

(format t "═══ Test 8: WHILE avec condition AND ═══~%")
(test-while "Condition composée"
            '(let ((x 0)
                   (y 10))
               (while (and (< x 5) (> y 5))
                 (progn
                   (setq x (+ x 1))
                   (setq y (- y 1))))
               x)
            5)

(format t "═══ Test 9: WHILE avec comparaison multiple ═══~%")
(test-while "Compteur double condition"
            '(let ((x 0))
               (while (<= x 10)
                 (setq x (+ x 2)))
               x)
            12)  ; 0, 2, 4, 6, 8, 10, 12 (sort à 12)

(format t "═══ Test 10: WHILE résultat nil ═══~%")
(test-while "Vérifier résultat WHILE = nil"
            '(let ((x 0))
               (let ((result (while (< x 3)
                               (setq x (+ x 1)))))
                 result))
            0)  ; WHILE retourne nil (0)

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%")
(format t "╔══════════════════════════════════════════════════════════════╗~%")
(format t "║                      RÉSUMÉ DES TESTS                        ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "Tests exécutés: ~A~%" *test-count*)
(format t "Tests réussis:  ~A~%" *test-passed*)
(format t "Tests échoués:  ~A~%" (- *test-count* *test-passed*))
(format t "~%")

(if (= *test-passed* *test-count*)
    (format t "✅ TOUS LES TESTS WHILE PASSENT ! 🎉~%~%")
    (format t "❌ CERTAINS TESTS ONT ÉCHOUÉ~%~%"))

;; Retourner le statut pour les scripts
(if (= *test-passed* *test-count*)
    (quit 0)
    (quit 1))
