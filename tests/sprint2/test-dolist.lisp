;;;; ============================================================================
;;;; TEST SUITE - DOLIST (Sprint 2.2)
;;;; ============================================================================
;;;; Tests pour itération sur listes avec DOLIST
;;;; Phase 11 - Option A - Sprint 2

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

;;; Utilitaire de test
(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defun test-compile-run (name expr expected)
  "Teste la compilation et l'exécution d'une expression"
  (incf *test-count*)
  (handler-case
      (let* ((code (compile-lisp expr))
             (vm (make-new-vm :verbose nil)))
        (load-code vm (append code (list (list :HALT))))
        (run-vm vm :max-instructions 50000)
        (let ((result (get-register vm :$V0)))
          (if (= result expected)
              (progn
                (incf *test-passed*)
                (format t "✓ ~A: résultat = ~A~%" name result)
                t)
              (progn
                (incf *test-failed*)
                (format t "✗ ~A: résultat = ~A (attendu: ~A)~%" name result expected)
                nil))))
    (error (e)
      (incf *test-failed*)
      (format t "✗ ~A: ERREUR - ~A~%" name e)
      nil)))

(format t "~%╔═══════════════════════════════════════════════════════════╗~%")
(format t "║  TESTS SPRINT 2.2 : DOLIST                                ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; TESTS DOLIST DE BASE
;;; ============================================================================

(format t "~%=== TESTS DOLIST DE BASE ===~%~%")

;; Test 1: DOLIST sur liste vide (NIL)
(test-compile-run "dolist-empty"
                  '(let ((sum 0))
                     (dolist (x nil)
                       (incf sum x))
                     sum)
                  0)

;; Test 2: DOLIST sur liste à 1 élément
(test-compile-run "dolist-one-element"
                  '(let ((sum 0))
                     (dolist (x (cons 5 nil))
                       (incf sum x))
                     sum)
                  5)

;; Test 3: DOLIST sur liste à 3 éléments (somme)
(test-compile-run "dolist-sum-three"
                  '(let ((sum 0))
                     (dolist (x (cons 1 (cons 2 (cons 3 nil))))
                       (incf sum x))
                     sum)
                  6)

;; Test 4: DOLIST compteur d'éléments
(test-compile-run "dolist-count"
                  '(let ((count 0))
                     (dolist (x (cons 10 (cons 20 (cons 30 nil))))
                       (incf count))
                     count)
                  3)

;; Test 5: DOLIST avec produit
(test-compile-run "dolist-product"
                  '(let ((prod 1))
                     (dolist (x (cons 2 (cons 3 (cons 4 nil))))
                       (setq prod (* prod x)))
                     prod)
                  24)

;;; ============================================================================
;;; TESTS DOLIST AVEC CONDITIONS
;;; ============================================================================

(format t "~%=== TESTS DOLIST AVEC CONDITIONS ===~%~%")

;; Test 6: DOLIST avec IF (compter éléments > 5)
(test-compile-run "dolist-if-count"
                  '(let ((count 0))
                     (dolist (x (cons 3 (cons 7 (cons 2 (cons 9 nil)))))
                       (when (> x 5)
                         (incf count)))
                     count)
                  2)

;; Test 7: DOLIST avec WHEN (somme conditionnelle)
(test-compile-run "dolist-when-sum"
                  '(let ((sum 0))
                     (dolist (x (cons 1 (cons 2 (cons 3 (cons 4 nil)))))
                       (when (> x 2)
                         (incf sum x)))
                     sum)
                  7)

;; Test 8: DOLIST avec UNLESS
(test-compile-run "dolist-unless"
                  '(let ((sum 0))
                     (dolist (x (cons 5 (cons 0 (cons 10 nil))))
                       (unless (null x)
                         (incf sum x)))
                     sum)
                  15)

;;; ============================================================================
;;; TESTS DOLIST IMBRIQUÉS
;;; ============================================================================

(format t "~%=== TESTS DOLIST IMBRIQUÉS ===~%~%")

;; Test 9: DOLIST imbriqués (2 niveaux)
(test-compile-run "dolist-nested-2"
                  '(let ((sum 0))
                     (dolist (x (cons 1 (cons 2 nil)))
                       (dolist (y (cons 10 (cons 20 nil)))
                         (incf sum (+ x y))))
                     sum)
                  66)

;; Test 10: DOLIST avec construction de liste
(test-compile-run "dolist-build-list"
                  '(let ((result nil))
                     (dolist (x (cons 1 (cons 2 (cons 3 nil))))
                       (setq result (cons (* x 2) result)))
                     (car result))
                  6)

;;; ============================================================================
;;; TESTS DOLIST AVEC VARIABLES LOCALES
;;; ============================================================================

(format t "~%=== TESTS AVEC VARIABLES LOCALES ===~%~%")

;; Test 11: DOLIST avec LET interne
(test-compile-run "dolist-let-internal"
                  '(let ((sum 0))
                     (dolist (x (cons 5 (cons 10 nil)))
                       (let ((double (* x 2)))
                         (incf sum double)))
                     sum)
                  30)

;; Test 12: Variable d'itération accessible
(test-compile-run "dolist-var-accessible"
                  '(let ((last 0))
                     (dolist (x (cons 7 (cons 8 (cons 9 nil))))
                       (setq last x))
                     last)
                  9)

;;; ============================================================================
;;; TESTS EDGE CASES
;;; ============================================================================

(format t "~%=== TESTS EDGE CASES ===~%~%")

;; Test 13: Longue liste (10 éléments)
(test-compile-run "dolist-long-list"
                  '(let ((sum 0))
                     (dolist (x (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 
                                (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 nil)))))))))))
                       (incf sum x))
                     sum)
                  55)

;; Test 14: DOLIST avec liste construite dynamiquement
(test-compile-run "dolist-dynamic-list"
                  '(let ((lst (cons 100 (cons 200 nil)))
                         (sum 0))
                     (dolist (x lst)
                       (incf sum x))
                     sum)
                  300)

;; Test 15: DOLIST retourne NIL
(test-compile-run "dolist-returns-nil"
                  '(let ((result (dolist (x (cons 1 (cons 2 nil)))
                                   (+ x 10))))
                     (if (null result) 1 0))
                  1)

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%~%=== RÉSUMÉ DES TESTS ===~%")
(format t "Tests réussis: ~A/~A (~A%)~%" 
        *test-passed* 
        *test-count*
        (if (> *test-count* 0)
            (round (* 100 (/ *test-passed* *test-count*)))
            0))
(format t "Tests échoués: ~A~%" *test-failed*)

(if (= *test-passed* *test-count*)
    (format t "~%✅ TOUS LES TESTS PASSENT!~%")
    (format t "~%⚠️  Certains tests ont échoué.~%"))
