;;;; test-free-variables.lisp
;;;; Tests unitaires pour l'analyse des variables libres (PHASE 9 - CLOSURES)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

;;; ============================================================================
;;; TESTS DE BASE
;;; ============================================================================

(defun test-fv-constant ()
  "Test: constantes n'ont pas de variables libres"
  (format t "~%=== TEST FREE-VARIABLES: Constante ===~%")
  (let ((result (free-variables 42)))
    (format t "Expression: 42~%")
    (format t "Variables libres: ~A~%" result)
    (if (null result)
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu '(), reçu ~A~%" result))))

(defun test-fv-variable-free ()
  "Test: variable simple est libre si non liée"
  (format t "~%=== TEST FREE-VARIABLES: Variable libre ===~%")
  (let ((result (free-variables 'x)))
    (format t "Expression: x~%")
    (format t "Variables libres: ~A~%" result)
    (if (equal result '(x))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (x), reçu ~A~%" result))))

(defun test-fv-variable-bound ()
  "Test: variable liée n'est pas libre"
  (format t "~%=== TEST FREE-VARIABLES: Variable liée ===~%")
  (let ((result (free-variables 'x '(x y))))
    (format t "Expression: x, bound: (x y)~%")
    (format t "Variables libres: ~A~%" result)
    (if (null result)
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu '(), reçu ~A~%" result))))

;;; ============================================================================
;;; TESTS LAMBDA
;;; ============================================================================

(defun test-fv-lambda-simple ()
  "Test: lambda avec variable libre"
  (format t "~%=== TEST FREE-VARIABLES: Lambda simple ===~%")
  (let ((expr '(lambda (y) (+ x y)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    (if (equal result '(x))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (x), reçu ~A~%" result))))

(defun test-fv-lambda-no-free ()
  "Test: lambda sans variable libre"
  (format t "~%=== TEST FREE-VARIABLES: Lambda sans variables libres ===~%")
  (let ((expr '(lambda (x y) (+ x y)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    (if (null result)
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu '(), reçu ~A~%" result))))

(defun test-fv-lambda-nested ()
  "Test: lambda imbriquée avec capture"
  (format t "~%=== TEST FREE-VARIABLES: Lambda imbriquée ===~%")
  (let ((expr '(lambda (x) (lambda (y) (+ x y z))))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; x est lié dans lambda externe, y dans interne, z est libre
    (if (equal result '(z))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (z), reçu ~A~%" result))))

(defun test-fv-lambda-multiple-free ()
  "Test: lambda avec plusieurs variables libres"
  (format t "~%=== TEST FREE-VARIABLES: Lambda avec plusieurs vars libres ===~%")
  (let ((expr '(lambda (x) (+ x y z)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; y et z sont libres
    (if (and (member 'y result) (member 'z result) (= (length result) 2))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu {y, z}, reçu ~A~%" result))))

;;; ============================================================================
;;; TESTS LET
;;; ============================================================================

(defun test-fv-let-simple ()
  "Test: let avec variable libre"
  (format t "~%=== TEST FREE-VARIABLES: Let simple ===~%")
  (let ((expr '(let ((x 1)) (+ x y)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; x est lié par let, y est libre
    (if (equal result '(y))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (y), reçu ~A~%" result))))

(defun test-fv-let-value-free ()
  "Test: let - variable libre dans valeur d'initialisation"
  (format t "~%=== TEST FREE-VARIABLES: Let avec var libre dans init ===~%")
  (let ((expr '(let ((x y)) (+ x 1)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; y est libre (utilisé pour initialiser x)
    (if (equal result '(y))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (y), reçu ~A~%" result))))

(defun test-fv-let-nested ()
  "Test: let imbriqués"
  (format t "~%=== TEST FREE-VARIABLES: Let imbriqués ===~%")
  (let ((expr '(let ((x a))
                 (let ((y b))
                   (+ x y c))))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; a, b, c sont libres; x et y sont liés
    (if (and (member 'a result) (member 'b result) (member 'c result)
             (= (length result) 3))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu {a, b, c}, reçu ~A~%" result))))

;;; ============================================================================
;;; TESTS LABELS
;;; ============================================================================

(defun test-fv-labels-simple ()
  "Test: labels avec variable libre"
  (format t "~%=== TEST FREE-VARIABLES: Labels simple ===~%")
  (let ((expr '(labels ((f (x) (+ x y)))
                 (f 10)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; y est libre (utilisé dans le corps de f)
    (if (equal result '(y))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (y), reçu ~A~%" result))))

(defun test-fv-labels-recursive ()
  "Test: labels avec fonction récursive"
  (format t "~%=== TEST FREE-VARIABLES: Labels récursif ===~%")
  (let ((expr '(labels ((fact (n)
                          (if (= n 0)
                              1
                              (* n (fact (- n 1))))))
                 (fact x)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; x est libre (argument de fact dans le corps)
    (if (equal result '(x))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (x), reçu ~A~%" result))))

(defun test-fv-labels-mutual ()
  "Test: labels avec fonctions mutuellement récursives"
  (format t "~%=== TEST FREE-VARIABLES: Labels mutuellement récursif ===~%")
  (let ((expr '(labels ((even (n) (if (= n 0) t (odd (- n 1))))
                        (odd (n) (if (= n 0) nil (even (- n 1)))))
                 (even x)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; x est libre
    (if (equal result '(x))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu (x), reçu ~A~%" result))))

;;; ============================================================================
;;; TESTS COMPLEXES
;;; ============================================================================

(defun test-fv-closure-factory ()
  "Test: fabrique de closures"
  (format t "~%=== TEST FREE-VARIABLES: Fabrique de closures ===~%")
  (let ((expr '(lambda (x)
                 (lambda (y)
                   (+ x y))))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; Pas de variables libres (x et y sont tous liés)
    (if (null result)
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu '(), reçu ~A~%" result))))

(defun test-fv-complex-nested ()
  "Test: expression complexe imbriquée"
  (format t "~%=== TEST FREE-VARIABLES: Expression complexe ===~%")
  (let ((expr '(let ((x 10))
                 (labels ((f (y)
                            (lambda (z)
                              (+ x y z w))))
                   (f a))))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; w et a sont libres; x, y, z, f sont liés
    (if (and (member 'w result) (member 'a result) (= (length result) 2))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu {w, a}, reçu ~A~%" result))))

(defun test-fv-dotimes ()
  "Test: dotimes avec variable de boucle"
  (format t "~%=== TEST FREE-VARIABLES: Dotimes ===~%")
  (let ((expr '(dotimes (i n)
                 (+ i x)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; n et x sont libres; i est lié
    (if (and (member 'n result) (member 'x result) (= (length result) 2))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu {n, x}, reçu ~A~%" result))))

;;; ============================================================================
;;; TESTS SETQ
;;; ============================================================================

(defun test-fv-setq ()
  "Test: setq avec variable libre"
  (format t "~%=== TEST FREE-VARIABLES: Setq ===~%")
  (let ((expr '(setq x (+ y 1)))
        (result nil))
    (setq result (free-variables expr))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" result)
    ;; x et y sont libres
    (if (and (member 'x result) (member 'y result) (= (length result) 2))
        (format t "✓ TEST RÉUSSI~%")
        (format t "✗ TEST ÉCHEC: attendu {x, y}, reçu ~A~%" result))))

;;; ============================================================================
;;; LANCEUR DE TESTS
;;; ============================================================================

(defun run-free-variables-tests ()
  "Lance tous les tests des variables libres"
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  TESTS UNITAIRES - VARIABLES LIBRES (PHASE 9)    ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")
  
  (let ((tests '(test-fv-constant
                 test-fv-variable-free
                 test-fv-variable-bound
                 test-fv-lambda-simple
                 test-fv-lambda-no-free
                 test-fv-lambda-nested
                 test-fv-lambda-multiple-free
                 test-fv-let-simple
                 test-fv-let-value-free
                 test-fv-let-nested
                 test-fv-labels-simple
                 test-fv-labels-recursive
                 test-fv-labels-mutual
                 test-fv-closure-factory
                 test-fv-complex-nested
                 test-fv-dotimes
                 test-fv-setq)))
    (let ((passed 0)
          (failed 0))
      (dolist (test tests)
        (handler-case
            (progn
              (funcall test)
              (incf passed))
          (error (e)
            (format t "~%✗ Erreur dans ~A: ~A~%" test e)
            (incf failed))))
      
      (format t "~%╔════════════════════════════════════════════════════╗~%")
      (format t "║  RÉSULTATS: ~A/~A tests réussis~A║~%"
              passed (+ passed failed)
              (make-string (max 0 (- 26 (length (format nil "~A/~A tests réussis" 
                                                         passed (+ passed failed)))))
                          :initial-element #\Space))
      (format t "╚════════════════════════════════════════════════════╝~%"))))

;;; Lancer les tests
(run-free-variables-tests)
