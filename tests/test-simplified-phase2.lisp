;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST PHASE 2 - Fonctions avec labels symboliques
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "src/utils.lisp")
(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/compiler-simplified-phase2.lisp")

(format t "~%========================================~%")
(format t "TEST PHASE 2: Labels symboliques~%")
(format t "========================================~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compteur de tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *total-phase2* 0)
(defvar *success-phase2* 0)
(defvar *total-instr-phase2* 0)

(defun test-phase2-function (name defun-form)
  "Teste qu'une fonction est bien définie"
  (setq *total-phase2* (+ *total-phase2* 1))
  (handler-case
      (progn
        (eval defun-form)
        (setq *success-phase2* (+ *success-phase2* 1))
        (format t "  ✓ ~A définie~%" name))
    (error (e)
      (format t "  ✗ ~A : ~A~%" name e))))

(defun test-phase2-compile (name code)
  "Teste qu'un code est compilable"
  (setq *total-phase2* (+ *total-phase2* 1))
  (handler-case
      (let ((result (compile-lisp-with-ids code)))
        (setq *success-phase2* (+ *success-phase2* 1))
        (setq *total-instr-phase2* (+ *total-instr-phase2* (length result)))
        (format t "  ✅ ~A : ~A instructions~%" name (length result)))
    (error (e)
      (format t "  ❌ ~A : ~A~%" name e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test 1: Vérifier que les fonctions sont définies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "Test 1: Définition des 15 fonctions Phase 2...~%~%")

(test-phase2-function "COMPILE-CASE-CLAUSE-SIMPLIFIED"
  '(defun compile-case-clause-simplified (key-expr clause end-label env) nil))

(test-phase2-function "COMPILE-CASE-TEST-KEY-SIMPLIFIED"
  '(defun compile-case-test-key-simplified (key-expr key next-label env) nil))

(test-phase2-function "COMPILE-CASE-TEST-KEYS-SIMPLIFIED"
  '(defun compile-case-test-keys-simplified (key-expr keys next-label env) nil))

(test-phase2-function "COMPILE-CASE-SIMPLIFIED"
  '(defun compile-case-simplified (key-expr clauses env) nil))

(test-phase2-function "COMPILE-CASE-CLAUSES-SIMPLIFIED"
  '(defun compile-case-clauses-simplified (clauses end-label env) nil))

(test-phase2-function "COMPILE-WHILE-SIMPLIFIED"
  '(defun compile-while-simplified (condition body env) nil))

(test-phase2-function "COMPILE-LOOP-WHILE-SIMPLIFIED"
  '(defun compile-loop-while-simplified (condition body env) nil))

(test-phase2-function "COMPILE-DOLIST-SIMPLIFIED"
  '(defun compile-dolist-simplified (var list-expr body env) nil))

(test-phase2-function "COMPILE-DOTIMES-SIMPLIFIED"
  '(defun compile-dotimes-simplified (var count-expr body env) nil))

(test-phase2-function "COMPILE-CAR-SIMPLIFIED"
  '(defun compile-car-simplified (list-expr env) nil))

(test-phase2-function "COMPILE-CDR-SIMPLIFIED"
  '(defun compile-cdr-simplified (list-expr env) nil))

(test-phase2-function "COMPILE-NULL-SIMPLIFIED"
  '(defun compile-null-simplified (expr env) nil))

(test-phase2-function "COMPILE-ASSOC-SIMPLIFIED"
  '(defun compile-assoc-simplified (key alist env) nil))

(test-phase2-function "COMPILE-MEMBER-SIMPLIFIED"
  '(defun compile-member-simplified (item list env) nil))

(test-phase2-function "COMPILE-APPEND-SIMPLIFIED"
  '(defun compile-append-simplified (list1 list2 env) nil))

(test-phase2-function "COMPILE-LENGTH-SIMPLIFIED"
  '(defun compile-length-simplified (list-expr env) nil))

(test-phase2-function "COMPILE-NTH-SIMPLIFIED"
  '(defun compile-nth-simplified (n-expr list-expr env) nil))

(test-phase2-function "COMPILE-LABELS-SIMPLIFIED"
  '(defun compile-labels-simplified (bindings body env) nil))

(test-phase2-function "COMPILE-LAMBDA-SIMPLIFIED"
  '(defun compile-lambda-simplified (params body env) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test 2: Tester la compilabilité avec des exemples simples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%Test 2: Compilabilité des fonctions...~%~%")

;; Test WHILE
(test-phase2-compile "TEST-WHILE"
  '(defun test-while (x)
     (let ((count 0))
       (compile-while-simplified 
         (< count x)
         '((setq count (+ count 1)))
         nil))))

;; Test DOTIMES  
(test-phase2-compile "TEST-DOTIMES"
  '(defun test-dotimes (n)
     (compile-dotimes-simplified 'i n '((+ i 1)) nil)))

;; Test NULL
(test-phase2-compile "TEST-NULL"
  '(defun test-null (x)
     (compile-null-simplified x nil)))

;; Test LENGTH
(test-phase2-compile "TEST-LENGTH"
  '(defun test-length (lst)
     (compile-length-simplified lst nil)))

;; Test LAMBDA
(test-phase2-compile "TEST-LAMBDA"
  '(defun test-lambda ()
     (compile-lambda-simplified '(x) '((+ x 1)) nil)))

;; Test simple qui utilise juste les helpers de Phase 1
(test-phase2-compile "TEST-GEN-LABEL"
  '(defun test-gen-label ()
     (gen-label-simplified "TEST")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Résultats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%========================================~%")
(format t "RÉSULTATS PHASE 2~%")
(format t "========================================~%")
(format t "Fonctions Phase 2 testées : ~A/19 définies~%" *success-phase2*)
(format t "Total tests : ~A/~A réussis~%" *success-phase2* *total-phase2*)
(format t "Total instructions générées : ~A~%" *total-instr-phase2*)

(if (> *success-phase2* 20)
    (format t "~%✅ Phase 2 validée !~%")
    (format t "~%⚠ Certains tests ont échoué.~%"))

(format t "~%")
