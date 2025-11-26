;;;; test-closure-call.lisp
;;;; Tests pour l'appel de closures (PHASE 9 - Étape 5)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "~%╔════════════════════════════════════════════════════╗~%")
(format t "║  TESTS APPEL DE CLOSURES (PHASE 9)                ║~%")
(format t "╚════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; TEST 1: Appel simple de closure sans capture
;;; ============================================================================

(defun test-closure-call-no-capture ()
  "Test: Appeler une closure simple sans variables capturées"
  (format t "~%=== TEST 1: APPEL CLOSURE SANS CAPTURE ===~%")
  (let ((expr '(let ((f (lambda (x) (+ x 1))))
                 (f 5))))
    (format t "Expression: ~A~%" expr)
    (handler-case
        (progn
          (format t "Compilation...~%")
          (let ((result (compile-and-run expr)))
            (format t "Résultat: ~A~%" result)
            (if (= result 6)
                (format t "✓ Test réussi! (5 + 1 = 6)~%")
                (format t "✗ Échec: attendu 6, obtenu ~A~%" result))))
      (error (e)
        (format t "✗ Erreur: ~A~%" e)))))

;;; ============================================================================
;;; TEST 2: Appel de closure avec capture simple
;;; ============================================================================

(defun test-closure-call-with-capture ()
  "Test: Appeler une closure qui capture une variable"
  (format t "~%=== TEST 2: APPEL CLOSURE AVEC CAPTURE ===~%")
  (let ((expr '(let ((y 10))
                 (let ((f (lambda (x) (+ x y))))
                   (f 5)))))
    (format t "Expression: ~A~%" expr)
    (handler-case
        (progn
          (format t "Compilation...~%")
          (let ((result (compile-and-run expr)))
            (format t "Résultat: ~A~%" result)
            (if (= result 15)
                (format t "✓ Test réussi! (5 + 10 = 15)~%")
                (format t "✗ Échec: attendu 15, obtenu ~A~%" result))))
      (error (e)
        (format t "✗ Erreur: ~A~%" e)))))

;;; ============================================================================
;;; TEST 3: Closure retournée par une fonction
;;; ============================================================================

(defun test-closure-returned ()
  "Test: Utiliser une closure retournée par une fonction"
  (format t "~%=== TEST 3: CLOSURE RETOURNÉE ===~%")
  (let ((expr '(let ((make-adder (lambda (n) (lambda (x) (+ x n)))))
                 (let ((add5 (make-adder 5)))
                   (add5 3)))))
    (format t "Expression: ~A~%" expr)
    (handler-case
        (progn
          (format t "Compilation...~%")
          (let ((result (compile-and-run expr)))
            (format t "Résultat: ~A~%" result)
            (if (= result 8)
                (format t "✓ Test réussi! (3 + 5 = 8)~%")
                (format t "✗ Échec: attendu 8, obtenu ~A~%" result))))
      (error (e)
        (format t "✗ Erreur: ~A~%" e)))))

;;; ============================================================================
;;; TEST 4: Multiple captures
;;; ============================================================================

(defun test-closure-multiple-captures ()
  "Test: Closure qui capture plusieurs variables"
  (format t "~%=== TEST 4: CAPTURES MULTIPLES ===~%")
  (let ((expr '(let ((x 5))
                 (let ((y 10))
                   (let ((f (lambda (z) (+ (+ x y) z))))
                     (f 3))))))
    (format t "Expression: ~A~%" expr)
    (handler-case
        (progn
          (format t "Compilation...~%")
          (let ((result (compile-and-run expr)))
            (format t "Résultat: ~A~%" result)
            (if (= result 18)
                (format t "✓ Test réussi! (5 + 10 + 3 = 18)~%")
                (format t "✗ Échec: attendu 18, obtenu ~A~%" result))))
      (error (e)
        (format t "✗ Erreur: ~A~%" e)))))

;;; ============================================================================
;;; TEST 5: Closure appelée plusieurs fois
;;; ============================================================================

(defun test-closure-multiple-calls ()
  "Test: Appeler la même closure plusieurs fois"
  (format t "~%=== TEST 5: APPELS MULTIPLES ===~%")
  (let ((expr '(let ((y 10))
                 (let ((f (lambda (x) (+ x y))))
                   (+ (f 1) (f 2))))))
    (format t "Expression: ~A~%" expr)
    (handler-case
        (progn
          (format t "Compilation...~%")
          (let ((result (compile-and-run expr)))
            (format t "Résultat: ~A~%" result)
            (if (= result 23)
                (format t "✓ Test réussi! ((1+10) + (2+10) = 23)~%")
                (format t "✗ Échec: attendu 23, obtenu ~A~%" result))))
      (error (e)
        (format t "✗ Erreur: ~A~%" e)))))

;;; ============================================================================
;;; Exécution des tests
;;; ============================================================================

(test-closure-call-no-capture)
(test-closure-call-with-capture)
(test-closure-returned)
(test-closure-multiple-captures)
(test-closure-multiple-calls)

(format t "~%╔════════════════════════════════════════════════════╗~%")
(format t "║          TESTS TERMINÉS                            ║~%")
(format t "╚════════════════════════════════════════════════════╝~%")
