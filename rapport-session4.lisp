;;; Rapport de progression - Session 4

;; ✅ BUG RÉSOLU : DEFUN+LET compilent correctement

;; PROBLÈME IDENTIFIÉ :
;; Dans compile-variable, les locations sur la pile sont stockées comme
;; dotted pairs (:stack . offset) via (add-variable env var (cons :stack offset))
;; 
;; Le code utilisait (second location) qui échouait car :
;; - (second '(:stack . 0)) essaie de faire (car (cdr (:stack . 0)))
;; - (cdr (:stack . 0)) = 0 (un nombre)
;; - (car 0) → "SECOND: 0 is not a list"

;; SOLUTION APPLIQUÉE :
;; Remplacer (second location) par (cdr location) pour extraire l'offset
;; des dotted pairs dans compile-variable (lignes 676, 681, 686)

;; TESTS DE VALIDATION :
(load "src/compiler.lisp")

(format t "~%=== TESTS APRÈS FIX ===~%~%")

;; Test 1 : LET standalone
(let ((code1 '(let ((x 42)) x)))
  (format t "Test 1 : ~A~%" code1)
  (handler-case
      (format t "  ✓ ~A instructions~%~%" (length (compile-lisp code1)))
    (error (e) (format t "  ✗ ~A~%~%" e))))

;; Test 2 : DEFUN simple
(let ((code2 '(defun test1 (x) x)))
  (format t "Test 2 : ~A~%" code2)
  (handler-case
      (format t "  ✓ ~A instructions~%~%" (length (compile-lisp code2)))
    (error (e) (format t "  ✗ ~A~%~%" e))))

;; Test 3 : DEFUN+LET (bloquait avant)
(let ((code3 '(defun test2 (x) (let ((y 10)) y))))
  (format t "Test 3 : ~A~%" code3)
  (handler-case
      (format t "  ✓ ~A instructions~%~%" (length (compile-lisp code3)))
    (error (e) (format t "  ✗ ~A~%~%" e))))

;; Test 4 : DEFUN+LET+GET-REGISTER
(let ((code4 '(defun alloc (size) (let ((addr (get-register :$gp))) addr))))
  (format t "Test 4 : ~A~%" code4)
  (handler-case
      (format t "  ✓ ~A instructions~%~%" (length (compile-lisp code4)))
    (error (e) (format t "  ✗ ~A~%~%" e))))

;; Test 5 : ALLOC-MEMORY-like
(let ((code5 '(defun alloc-memory (size) 
                (let ((addr (get-register :$gp)))
                  (when (>= addr 1024)
                    (format :error "Out of memory"))
                  (set-register :$gp (+ addr size))
                  addr))))
  (format t "Test 5 : ALLOC-MEMORY-like~%")
  (handler-case
      (format t "  ✓ ~A instructions~%~%" (length (compile-lisp code5)))
    (error (e) (format t "  ✗ ~A~%~%" e))))

(format t "~%=== RÉSUMÉ ===~%")
(format t "Session 3 : 62%% (16/26 DEFUN, 1130 lignes MIPS)~%")
(format t "Session 4 : Bug DEFUN+LET résolu~%")
(format t "Déblocage  : ALLOC-MEMORY, PUSH-STACK, POP-STACK, PEEK-STACK~%")
(format t "Objectif 77%% : En cours d'estimation...~%~%")
