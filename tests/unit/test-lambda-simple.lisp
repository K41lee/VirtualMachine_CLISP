;;;; test-lambda-simple.lisp
;;;; Test simple pour la compilation de LAMBDA (PHASE 9 - CLOSURES)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

;;; ============================================================================
;;; TEST 1: Lambda sans capture (fonction simple)
;;; ============================================================================

(defun test-lambda-no-capture ()
  "Test: lambda sans variables libres"
  (format t "~%=== TEST LAMBDA SANS CAPTURE ===~%")
  (let ((expr '(lambda (x) (+ x 1))))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" (free-variables expr))
    (handler-case
        (let ((asm-code (compile-lisp expr)))
          (format t "~%Code assembleur généré:~%")
          (dolist (instr asm-code)
            (format t "  ~A~%" instr))
          (format t "~%✓ Compilation réussie~%"))
      (error (e)
        (format t "~%✗ Erreur de compilation: ~A~%" e)))))

;;; ============================================================================
;;; TEST 2: Lambda avec capture simple
;;; ============================================================================

(defun test-lambda-capture-simple ()
  "Test: lambda avec une variable libre"
  (format t "~%=== TEST LAMBDA AVEC CAPTURE SIMPLE ===~%")
  (let ((expr '(lambda (x) (+ x y))))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" (free-variables expr))
    (handler-case
        (let ((asm-code (compile-lisp expr)))
          (format t "~%Code assembleur généré (~A instructions):~%" (length asm-code))
          (format t "✓ Compilation réussie~%"))
      (error (e)
        (format t "~%✗ Erreur de compilation: ~A~%" e)))))

;;; ============================================================================
;;; TEST 3: Lambda dans LET (avec contexte)
;;; ============================================================================

(defun test-lambda-in-let ()
  "Test: lambda définie dans un LET"
  (format t "~%=== TEST LAMBDA DANS LET ===~%")
  (let ((expr '(let ((y 10))
                 (lambda (x) (+ x y)))))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres du lambda: ~A~%" (free-variables '(lambda (x) (+ x y))))
    (handler-case
        (let ((asm-code (compile-lisp expr)))
          (format t "~%Code assembleur généré (~A instructions):~%" (length asm-code))
          (format t "✓ Compilation réussie~%"))
      (error (e)
        (format t "~%✗ Erreur de compilation: ~A~%" e)))))

;;; ============================================================================
;;; TEST 4: Lambda imbriquée
;;; ============================================================================

(defun test-lambda-nested ()
  "Test: lambda qui retourne une lambda"
  (format t "~%=== TEST LAMBDA IMBRIQUÉE ===~%")
  (let ((expr '(lambda (x) (lambda (y) (+ x y)))))
    (format t "Expression: ~A~%" expr)
    (format t "Variables libres: ~A~%" (free-variables expr))
    (handler-case
        (let ((asm-code (compile-lisp expr)))
          (format t "~%Code assembleur généré (~A instructions):~%" (length asm-code))
          (format t "✓ Compilation réussie~%"))
      (error (e)
        (format t "~%✗ Erreur de compilation: ~A~%" e)))))

;;; ============================================================================
;;; LANCEUR DE TESTS
;;; ============================================================================

(defun run-lambda-compilation-tests ()
  "Lance tous les tests de compilation LAMBDA"
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║  TESTS COMPILATION LAMBDA (PHASE 9)               ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%")
  
  (let ((tests '(test-lambda-no-capture
                 test-lambda-capture-simple
                 test-lambda-in-let
                 test-lambda-nested)))
    (dolist (test tests)
      (handler-case
          (funcall test)
        (error (e)
          (format t "~%✗ Erreur fatale dans ~A: ~A~%" test e)))))
  
  (format t "~%╔════════════════════════════════════════════════════╗~%")
  (format t "║          TESTS TERMINÉS                            ║~%")
  (format t "╚════════════════════════════════════════════════════╝~%"))

;;; Lancer les tests
(run-lambda-compilation-tests)
