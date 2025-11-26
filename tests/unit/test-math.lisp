;;;; ============================================================================
;;;; TESTS - FONCTIONS MATHÉMATIQUES (ABS, MAX, MIN)
;;;; ============================================================================

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(defvar *test-count* 0)
(defvar *test-passed* 0)

(defun test-math (name lisp-code expected)
  "Teste une expression mathématique et vérifie le résultat"
  (setf *test-count* (+ *test-count* 1))
  (format t "~%Test ~A: ~A~%" *test-count* name)
  (format t "Expression: ~A~%" lisp-code)
  (format t "Attendu: ~A~%" expected)
  
  (handler-case
      (let* ((vm (compile-and-run lisp-code))
             (result (get-register vm *reg-v0*)))
        (format t "Résultat: ~A~%" result)
        (if (= result expected)
            (progn
              (setf *test-passed* (+ *test-passed* 1))
              (format t "✓ PASS~%"))
            (format t "✗ FAIL~%")))
    (error (e)
      (format t "✗ ERREUR: ~A~%" e))))

(format t "~%========================================~%")
(format t "TESTS - FONCTIONS MATHÉMATIQUES~%")
(format t "========================================~%")

;;; ============================================================================
;;; TESTS ABS (valeur absolue)
;;; ============================================================================

(format t "~%--- Tests ABS ---~%")

;; Test 1: ABS d'un nombre positif
(test-math "ABS positif"
           '(abs 5)
           5)

;; Test 2: ABS d'un nombre négatif
(test-math "ABS négatif"
           '(abs -8)
           8)

;; Test 3: ABS de zéro
(test-math "ABS zéro"
           '(abs 0)
           0)

;; Test 4: ABS avec expression
(test-math "ABS expression"
           '(abs (- 3 10))
           7)

;; Test 5: ABS dans calcul
(test-math "ABS dans calcul"
           '(+ (abs -5) (abs 3))
           8)

;;; ============================================================================
;;; TESTS MAX (maximum)
;;; ============================================================================

(format t "~%--- Tests MAX ---~%")

;; Test 6: MAX avec x > y
(test-math "MAX x > y"
           '(max 10 5)
           10)

;; Test 7: MAX avec x < y
(test-math "MAX x < y"
           '(max 3 8)
           8)

;; Test 8: MAX avec valeurs égales
(test-math "MAX égales"
           '(max 7 7)
           7)

;; Test 9: MAX avec négatifs
(test-math "MAX négatifs"
           '(max -3 -8)
           -3)

;; Test 10: MAX avec expressions
(test-math "MAX expressions"
           '(max (+ 2 3) (* 2 2))
           5)

;; Test 11: MAX avec zéro
(test-math "MAX avec zéro"
           '(max -5 0)
           0)

;;; ============================================================================
;;; TESTS MIN (minimum)
;;; ============================================================================

(format t "~%--- Tests MIN ---~%")

;; Test 12: MIN avec x < y
(test-math "MIN x < y"
           '(min 3 8)
           3)

;; Test 13: MIN avec x > y
(test-math "MIN x > y"
           '(min 10 5)
           5)

;; Test 14: MIN avec valeurs égales
(test-math "MIN égales"
           '(min 7 7)
           7)

;; Test 15: MIN avec négatifs
(test-math "MIN négatifs"
           '(min -3 -8)
           -8)

;; Test 16: MIN avec expressions
(test-math "MIN expressions"
           '(min (+ 2 3) (* 2 2))
           4)

;; Test 17: MIN avec zéro
(test-math "MIN avec zéro"
           '(min -5 0)
           -5)

;;; ============================================================================
;;; TESTS COMBINÉS
;;; ============================================================================

(format t "~%--- Tests combinés ---~%")

;; Test 18: Combinaison ABS et MAX
(test-math "ABS + MAX"
           '(max (abs -10) (abs 5))
           10)

;; Test 19: Combinaison ABS et MIN
(test-math "ABS + MIN"
           '(min (abs -3) (abs -7))
           3)

;; Test 20: Combinaison MAX et MIN
(test-math "MAX + MIN"
           '(min (max 5 10) (max 3 8))
           8)

;; Test 21: Expression complexe
(test-math "Expression complexe"
           '(+ (abs (- 5 10)) (max 2 (min 8 5)))
           10)

;;; ============================================================================
;;; RÉSUMÉ DES TESTS
;;; ============================================================================

(format t "~%========================================~%")
(format t "RÉSUMÉ: ~A/~A tests réussis (~A%)~%" 
        *test-passed* 
        *test-count*
        (if (> *test-count* 0)
            (round (* 100 (/ *test-passed* *test-count*)))
            0))
(format t "========================================~%")

;; Retourner un code de sortie approprié
(if (= *test-passed* *test-count*)
    (format t "~%✓ Tous les tests sont passés!~%")
    (format t "~%✗ Certains tests ont échoué.~%"))
