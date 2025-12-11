;;;; ============================================================================
;;;; TESTS - Sprint 1.2 : INCF/DECF
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
(format t "║  TESTS SPRINT 1.2 : INCF/DECF                            ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; TESTS INCF
;;; ============================================================================

(format t "~%─────────────────────────────────────────────────────────────~%")
(format t "Tests INCF~%")
(format t "─────────────────────────────────────────────────────────────~%")

(defvar *test-results* '())

;; Test 1: INCF sans delta (incrément de 1)
(push (test-compile-run "INCF x (sans delta)"
                        '(let ((x 10))
                           (incf x)
                           x)
                        11)
      *test-results*)

;; Test 2: INCF avec delta positif
(push (test-compile-run "INCF x 5"
                        '(let ((x 10))
                           (incf x 5)
                           x)
                        15)
      *test-results*)

;; Test 3: INCF avec delta négatif (équivalent à DECF)
(push (test-compile-run "INCF x -3"
                        '(let ((x 10))
                           (incf x -3)
                           x)
                        7)
      *test-results*)

;; Test 4: INCF multiple fois
(push (test-compile-run "INCF multiple"
                        '(let ((x 0))
                           (incf x)
                           (incf x)
                           (incf x)
                           x)
                        3)
      *test-results*)

;; Test 5: INCF retourne la nouvelle valeur
(push (test-compile-run "INCF retourne valeur"
                        '(let ((x 10))
                           (incf x 5))
                        15)
      *test-results*)

;; Test 6: INCF dans une boucle WHILE
(push (test-compile-run "INCF dans WHILE"
                        '(let ((x 0)
                               (i 0))
                           (while (< i 5)
                             (incf x 2)
                             (incf i))
                           x)
                        10)
      *test-results*)

;;; ============================================================================
;;; TESTS DECF
;;; ============================================================================

(format t "~%─────────────────────────────────────────────────────────────~%")
(format t "Tests DECF~%")
(format t "─────────────────────────────────────────────────────────────~%")

;; Test 7: DECF sans delta (décrément de 1)
(push (test-compile-run "DECF x (sans delta)"
                        '(let ((x 10))
                           (decf x)
                           x)
                        9)
      *test-results*)

;; Test 8: DECF avec delta positif
(push (test-compile-run "DECF x 3"
                        '(let ((x 10))
                           (decf x 3)
                           x)
                        7)
      *test-results*)

;; Test 9: DECF avec delta négatif (équivalent à INCF)
(push (test-compile-run "DECF x -5"
                        '(let ((x 10))
                           (decf x -5)
                           x)
                        15)
      *test-results*)

;; Test 10: DECF retourne la nouvelle valeur
(push (test-compile-run "DECF retourne valeur"
                        '(let ((x 20))
                           (decf x 7))
                        13)
      *test-results*)

;; Test 11: DECF dans compteur décroissant
(push (test-compile-run "DECF compteur"
                        '(let ((count 10)
                               (sum 0))
                           (while (> count 0)
                             (incf sum count)
                             (decf count))
                           sum)
                        55)  ; 10+9+8+...+1
      *test-results*)

;;; ============================================================================
;;; TESTS COMBINÉS
;;; ============================================================================

(format t "~%─────────────────────────────────────────────────────────────~%")
(format t "Tests Combinés INCF/DECF~%")
(format t "─────────────────────────────────────────────────────────────~%")

;; Test 12: INCF et DECF alternés
(push (test-compile-run "INCF/DECF alternés"
                        '(let ((x 10))
                           (incf x 5)
                           (decf x 3)
                           (incf x 2)
                           x)
                        14)
      *test-results*)

;; Test 13: INCF/DECF sur multiple variables
(push (test-compile-run "Multiple variables"
                        '(let ((a 5)
                               (b 10))
                           (incf a 3)
                           (decf b 2)
                           (+ a b))
                        16)  ; (5+3) + (10-2) = 8 + 8
      *test-results*)

;; Test 14: INCF dans condition WHEN
(push (test-compile-run "INCF dans WHEN"
                        '(let ((x 0)
                               (flag 1))
                           (when flag
                             (incf x 10))
                           x)
                        10)
      *test-results*)

;; Test 15: DECF dans condition UNLESS
(push (test-compile-run "DECF dans UNLESS"
                        '(let ((x 20)
                               (flag 0))
                           (unless flag
                             (decf x 5))
                           x)
                        15)
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
  (format t "  • INCF : 6 tests~%")
  (format t "  • DECF : 5 tests~%")
  (format t "  • Combinés: 4 tests~%")
  (if (= passed total)
      (format t "~%✓ Tous les tests passent! INCF/DECF sont fonctionnels.~%")
      (format t "~%✗ ~A test(s) ont échoué.~%" (- total passed))))
