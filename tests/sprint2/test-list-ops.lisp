;;;; ============================================================================
;;;; TEST SUITE - LIST OPERATIONS (Sprint 2.1)
;;;; ============================================================================
;;;; Tests complets pour CONS, CAR, CDR, NULL
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
        (run-vm vm :max-instructions 10000)
        (let ((result (get-register vm :$V0)))
          (if (= result expected)
              (progn
                (incf *test-passed*)
                (format t "✓ ~A: ~A → ~A~%" name expr result)
                t)
              (progn
                (incf *test-failed*)
                (format t "✗ ~A: ~A → ~A (attendu: ~A)~%" name expr result expected)
                nil))))
    (error (e)
      (incf *test-failed*)
      (format t "✗ ~A: ERREUR - ~A~%" name e)
      nil)))

(format t "~%╔═══════════════════════════════════════════════════════════╗~%")
(format t "║  TESTS SPRINT 2.1 : LIST OPERATIONS                      ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; TESTS NULL
;;; ============================================================================

(format t "~%=== TESTS NULL ===~%~%")

;; Test 1: NULL de NIL
(test-compile-run "null-nil" '(null nil) 1)

;; Test 2: NULL d'un nombre non-zéro
(test-compile-run "null-number" '(null 42) 0)

;; Test 3: NULL de 0
(test-compile-run "null-zero" '(null 0) 1)

;; Test 4: NULL d'une expression qui retourne NIL
(test-compile-run "null-if-nil" '(null (if nil 1 nil)) 1)

;; Test 5: NULL d'une variable
(test-compile-run "null-var" '(let ((x nil)) (null x)) 1)

;;; ============================================================================
;;; TESTS CONS
;;; ============================================================================

(format t "~%=== TESTS CONS ===~%~%")

;; Test 6: CONS simple (1 . 2)
;; Retourne l'adresse de la cons cell (doit être > 0)
(test-compile-run "cons-simple" '(let ((c (cons 1 2))) (if (> c 0) 1 0)) 1)

;; Test 7: CONS avec NIL comme CDR (liste à 1 élément)
(test-compile-run "cons-nil-cdr" '(let ((c (cons 1 nil))) (if (> c 0) 1 0)) 1)

;; Test 8: CONS avec NIL comme CAR
(test-compile-run "cons-nil-car" '(let ((c (cons nil 2))) (if (> c 0) 1 0)) 1)

;; Test 9: CONS avec deux NIL
(test-compile-run "cons-nil-nil" '(let ((c (cons nil nil))) (if (> c 0) 1 0)) 1)

;; Test 10: CONS imbriqués (cons 1 (cons 2 nil))
(test-compile-run "cons-nested" '(let ((c (cons 1 (cons 2 nil)))) (if (> c 0) 1 0)) 1)

;;; ============================================================================
;;; TESTS CAR
;;; ============================================================================

(format t "~%=== TESTS CAR ===~%~%")

;; Test 11: CAR de NIL
(test-compile-run "car-nil" '(car nil) 0)

;; Test 12: CAR d'un cons simple
(test-compile-run "car-cons-simple" '(car (cons 42 17)) 42)

;; Test 13: CAR avec variable
(test-compile-run "car-var" '(let ((c (cons 100 200))) (car c)) 100)

;; Test 14: CAR d'un cons avec NIL comme CAR
(test-compile-run "car-nil-car" '(car (cons 0 999)) 0)

;; Test 15: CAR d'un cons imbriqué
(test-compile-run "car-nested" '(car (cons 55 (cons 66 nil))) 55)

;;; ============================================================================
;;; TESTS CDR
;;; ============================================================================

(format t "~%=== TESTS CDR ===~%~%")

;; Test 16: CDR de NIL
(test-compile-run "cdr-nil" '(cdr nil) 0)

;; Test 17: CDR d'un cons simple
(test-compile-run "cdr-cons-simple" '(cdr (cons 42 17)) 17)

;; Test 18: CDR avec variable
(test-compile-run "cdr-var" '(let ((c (cons 100 200))) (cdr c)) 200)

;; Test 19: CDR d'un cons avec NIL comme CDR
(test-compile-run "cdr-nil-cdr" '(cdr (cons 999 0)) 0)

;; Test 20: CDR d'un cons imbriqué (liste à 2 éléments)
(test-compile-run "cdr-nested" '(let ((c (cons 1 (cons 2 nil)))) 
                             (if (> (cdr c) 0) 1 0)) 1)

;;; ============================================================================
;;; TESTS CAR/CDR COMBINÉS
;;; ============================================================================

(format t "~%=== TESTS CAR/CDR COMBINÉS ===~%~%")

;; Test 21: CAR puis CDR
(test-compile-run "car-cdr-sequence" 
            '(let ((c (cons 10 20)))
               (+ (car c) (cdr c)))
            30)

;; Test 22: CDR puis CAR (liste à 2 éléments)
(test-compile-run "cdr-car-list"
            '(let ((c (cons 5 (cons 15 nil))))
               (car (cdr c)))
            15)

;; Test 23: Construction de liste à 3 éléments puis navigation
(test-compile-run "three-element-list"
            '(let ((c (cons 1 (cons 2 (cons 3 nil)))))
               (+ (car c) (car (cdr c))))
            3)

;; Test 24: CADR (car (cdr x))
(test-compile-run "cadr"
            '(let ((lst (cons 10 (cons 20 (cons 30 nil)))))
               (car (cdr lst)))
            20)

;; Test 25: CADDR (car (cdr (cdr x)))
(test-compile-run "caddr"
            '(let ((lst (cons 10 (cons 20 (cons 30 nil)))))
               (car (cdr (cdr lst))))
            30)

;;; ============================================================================
;;; TESTS NULL AVEC CONS/CAR/CDR
;;; ============================================================================

(format t "~%=== TESTS NULL AVEC LISTES ===~%~%")

;; Test 26: NULL du CDR d'une liste à 1 élément
(test-compile-run "null-cdr-one-element"
            '(null (cdr (cons 42 nil)))
            1)

;; Test 27: NULL du CDR d'une liste à 2 éléments
(test-compile-run "null-cdr-two-elements"
            '(null (cdr (cons 1 (cons 2 nil))))
            0)

;; Test 28: Test de liste vide avec NULL
(test-compile-run "null-empty-list"
            '(let ((empty nil))
               (if (null empty) 1 0))
            1)

;; Test 29: Test de liste non-vide avec NULL
(test-compile-run "null-non-empty-list"
            '(let ((lst (cons 1 nil)))
               (if (null lst) 0 1))
            1)

;; Test 30: NULL imbriqué
(test-compile-run "null-nested"
            '(if (null (if t nil (cons 1 2))) 1 0)
            1)

;;; ============================================================================
;;; TESTS MODIFICATION DE LISTES
;;; ============================================================================

(format t "~%=== TESTS MODIFICATION ===~%~%")

;; Test 31: Construction progressive d'une liste
(test-compile-run "progressive-list"
            '(let ((lst nil))
               (setq lst (cons 1 lst))
               (setq lst (cons 2 lst))
               (car lst))
            2)

;; Test 32: Vérification de la queue après ajout
(test-compile-run "check-tail"
            '(let ((lst nil))
               (setq lst (cons 1 lst))
               (setq lst (cons 2 lst))
               (car (cdr lst)))
            1)

;; Test 33: Longueur de liste (compte d'éléments)
(test-compile-run "list-length-3"
            '(let ((lst (cons 1 (cons 2 (cons 3 nil)))))
               (let ((count 0))
                 (when (> lst 0) (incf count))
                 (when (> (cdr lst) 0) (incf count))
                 (when (> (cdr (cdr lst)) 0) (incf count))
                 count))
            3)

;;; ============================================================================
;;; TESTS STRESS / EDGE CASES
;;; ============================================================================

(format t "~%=== TESTS STRESS ===~%~%")

;; Test 34: Longue liste (10 éléments)
(test-compile-run "long-list"
            '(let ((lst (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 
                        (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 nil))))))))))))
               (car lst))
            1)

;; Test 35: Navigation profonde
(test-compile-run "deep-navigation"
            '(let ((lst (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))))
               (car (cdr (cdr (cdr (cdr lst))))))
            5)

;; Test 36: Multiples variables
(test-compile-run "multiple-vars"
            '(let ((a (cons 10 20))
                   (b (cons 30 40)))
               (+ (car a) (cdr b)))
            50)

;; Test 37: Cons dans condition IF
(test-compile-run "cons-in-if"
            '(if t
                 (car (cons 100 200))
                 (car (cons 300 400)))
            100)

;; Test 38: Expression complexe
(test-compile-run "complex-expr"
            '(let ((x 5))
               (let ((lst (cons (* x 2) (cons (+ x 3) nil))))
                 (+ (car lst) (car (cdr lst)))))
            18)

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

;; Exit avec code de retour approprié
(if (= *test-passed* *test-count*)
    (quit :unix-status 0)
    (quit :unix-status 1))
