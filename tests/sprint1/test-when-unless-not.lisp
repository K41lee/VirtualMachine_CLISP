;;;; ============================================================================
;;;; TESTS - Sprint 1.1 : WHEN/UNLESS/NOT
;;;; Phase 11 - Option A
;;;; ============================================================================

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(defun test-compile-run (name expr expected)
  "Teste la compilation et l'exécution d'une expression"
  (format t "~%Test: ~A~%" name)
  (format t "  Expression: ~A~%" expr)
  (handler-case
      (let* ((code (compile-lisp expr))
             (vm (make-new-vm :verbose nil)))
        (load-code vm (append code (list (list :HALT))))
        (run-vm vm :max-instructions 10000)
        (let ((result (get-register vm :$V0)))
          (if (= result expected)
              (format t "  ✓ RÉUSSI (résultat: ~A)~%" result)
              (format t "  ✗ ÉCHEC (attendu: ~A, obtenu: ~A)~%" expected result))
          (= result expected)))
    (error (e)
      (format t "  ✗ ERREUR: ~A~%" e)
      nil)))

(format t "~%╔═══════════════════════════════════════════════════════════╗~%")
(format t "║  TESTS SPRINT 1.1 : WHEN/UNLESS/NOT                      ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; TESTS NOT
;;; ============================================================================

(format t "~%─────────────────────────────────────────────────────────────~%")
(format t "Tests NOT~%")
(format t "─────────────────────────────────────────────────────────────~%")

(defvar *test-results* '())

;; Test 1: NOT sur valeur fausse (0)
(push (test-compile-run "NOT 0 (faux)"
                        '(not 0)
                        1)
      *test-results*)

;; Test 2: NOT sur valeur vraie (non-zero)
(push (test-compile-run "NOT 42 (vrai)"
                        '(not 42)
                        0)
      *test-results*)

;; Test 3: NOT sur expression arithmétique vraie
(push (test-compile-run "NOT (> 5 3)"
                        '(not (> 5 3))
                        0)
      *test-results*)

;; Test 4: NOT sur expression arithmétique fausse
(push (test-compile-run "NOT (< 5 3)"
                        '(not (< 5 3))
                        1)
      *test-results*)

;; Test 5: Double négation
(push (test-compile-run "NOT (NOT 42)"
                        '(not (not 42))
                        1)
      *test-results*)

;;; ============================================================================
;;; TESTS WHEN
;;; ============================================================================

(format t "~%─────────────────────────────────────────────────────────────~%")
(format t "Tests WHEN~%")
(format t "─────────────────────────────────────────────────────────────~%")

;; Test 6: WHEN avec condition vraie
(push (test-compile-run "WHEN condition vraie"
                        '(when 1 42)
                        42)
      *test-results*)

;; Test 7: WHEN avec condition fausse
(push (test-compile-run "WHEN condition fausse"
                        '(when 0 42)
                        0)  ; retourne 0 (dernière valeur avant WHEN)
      *test-results*)

;; Test 8: WHEN avec condition arithmétique vraie
(push (test-compile-run "WHEN (> 10 5)"
                        '(when (> 10 5) 100)
                        100)
      *test-results*)

;; Test 9: WHEN avec condition arithmétique fausse
(push (test-compile-run "WHEN (< 10 5)"
                        '(when (< 10 5) 100)
                        0)
      *test-results*)

;; Test 10: WHEN avec body multiple
(push (test-compile-run "WHEN body multiple"
                        '(when 1 (+ 2 3) (* 4 5))
                        20)  ; dernière expression
      *test-results*)

;; Test 11: WHEN dans LET
(push (test-compile-run "WHEN dans LET"
                        '(let ((x 10))
                           (when (> x 5)
                             (* x 2)))
                        20)
      *test-results*)

;;; ============================================================================
;;; TESTS UNLESS
;;; ============================================================================

(format t "~%─────────────────────────────────────────────────────────────~%")
(format t "Tests UNLESS~%")
(format t "─────────────────────────────────────────────────────────────~%")

;; Test 12: UNLESS avec condition fausse (body exécuté)
(push (test-compile-run "UNLESS condition fausse"
                        '(unless 0 42)
                        42)
      *test-results*)

;; Test 13: UNLESS avec condition vraie (body non exécuté)
(push (test-compile-run "UNLESS condition vraie"
                        '(unless 1 42)
                        0)
      *test-results*)

;; Test 14: UNLESS avec condition arithmétique fausse
(push (test-compile-run "UNLESS (< 10 5)"
                        '(unless (< 10 5) 100)
                        100)
      *test-results*)

;; Test 15: UNLESS avec condition arithmétique vraie
(push (test-compile-run "UNLESS (> 10 5)"
                        '(unless (> 10 5) 100)
                        0)
      *test-results*)

;; Test 16: UNLESS dans LET
(push (test-compile-run "UNLESS dans LET"
                        '(let ((x 3))
                           (unless (> x 5)
                             (* x 10)))
                        30)
      *test-results*)

;;; ============================================================================
;;; TESTS COMBINÉS
;;; ============================================================================

(format t "~%─────────────────────────────────────────────────────────────~%")
(format t "Tests Combinés~%")
(format t "─────────────────────────────────────────────────────────────~%")

;; Test 17: WHEN avec NOT
(push (test-compile-run "WHEN avec NOT"
                        '(when (not 0) 42)
                        42)
      *test-results*)

;; Test 18: UNLESS équivaut à WHEN NOT
(push (test-compile-run "UNLESS = WHEN NOT"
                        '(let ((x 10))
                           (unless (< x 5)
                             100))
                        100)
      *test-results*)

;; Test 19: NOT dans condition complexe
(push (test-compile-run "NOT dans condition"
                        '(let ((x 5))
                           (when (not (= x 0))
                             (* x 2)))
                        10)
      *test-results*)

;; Test 20: WHEN/UNLESS imbriqués
(push (test-compile-run "WHEN/UNLESS imbriqués"
                        '(let ((x 10))
                           (when (> x 5)
                             (unless (> x 20)
                               (* x 3))))
                        30)
      *test-results*)

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%╔═══════════════════════════════════════════════════════════╗~%")
(format t "║  RÉSUMÉ DES TESTS                                         ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")

(let ((total (length *test-results*))
      (passed (count t *test-results*)))
  (format t "~%Tests réussis: ~A/~A (~A%)~%"
          passed total
          (if (> total 0) (round (* 100 (/ passed total))) 0))
  (format t "~%Constructions testées:~%")
  (format t "  • NOT   : 5 tests~%")
  (format t "  • WHEN  : 6 tests~%")
  (format t "  • UNLESS: 5 tests~%")
  (format t "  • Combinés: 4 tests~%")
  (if (= passed total)
      (format t "~%✓ Tous les tests passent! WHEN/UNLESS/NOT sont fonctionnels.~%")
      (format t "~%✗ ~A test(s) ont échoué.~%" (- total passed))))
