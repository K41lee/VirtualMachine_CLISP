#!/usr/bin/env clisp
;; Test APPEND fonctionnel

(load "main.lisp")

(format t "~%╔═══════════════════════════════════════════════════════════════════╗~%")
(format t "║  TEST APPEND - Implémentation fonctionnelle                       ║~%")
(format t "╚═══════════════════════════════════════════════════════════════════╝~%~%")

(defun test-append-case (description expr)
  "Teste un cas APPEND"
  (format t "Test: ~A~%" description)
  (format t "  Expression: ~A~%" expr)
  (handler-case
      (let ((result (compile-and-run expr)))
        (format t "  Résultat VM: ~A~%" result)
        (format t "  ✅ Compilation et exécution réussies~%~%")
        t)
    (error (e)
      (format t "  ❌ ERREUR: ~A~%~%" e)
      nil)))

;; Tests
(let ((pass 0) (total 0))
  
  ;; Test 1: Append simple
  (incf total)
  (when (test-append-case 
         "(append (cons 1 nil) (cons 2 nil))"
         '(append (cons 1 nil) (cons 2 nil)))
    (incf pass))
  
  ;; Test 2: Append avec NIL
  (incf total)
  (when (test-append-case
         "(append nil (cons 1 nil))"
         '(append nil (cons 1 nil)))
    (incf pass))
  
  ;; Test 3: Append deux listes
  (incf total)
  (when (test-append-case
         "(append (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil)))"
         '(append (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil))))
    (incf pass))
  
  ;; Test 4: Append NIL NIL
  (incf total)
  (when (test-append-case
         "(append nil nil)"
         '(append nil nil))
    (incf pass))
  
  (format t "~%═══════════════════════════════════════════════════════════════════~%")
  (format t "Résultat: ~A / ~A tests réussis~%" pass total)
  (format t "═══════════════════════════════════════════════════════════════════~%~%"))
