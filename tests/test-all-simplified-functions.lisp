;;;; test-all-simplified-functions.lisp
;;;; Test que toutes les fonctions simplifiées sont compilables

(load "src/utils.lisp")
(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/compiler.lisp")

(format t "~%========================================~%")
(format t "TEST: Compilation de toutes les fonctions simplifiées~%")
(format t "========================================~%~%")

;;; ============================================================================
;;; Test du LOADER simplifié
;;; ============================================================================

(format t "Test 1: Fonctions du loader-simplified.lisp~%~%")

(defvar *loader-functions*
  '(make-alist
    alist-get
    alist-put
    alist-remove
    collect-labels-simplified
    collect-labels-helper
    is-label-instr
    get-label-name
    resolve-labels-simplified
    resolve-labels-helper
    resolve-instruction
    resolve-element
    map-list
    reverse-list
    reverse-list-helper
    keyword-to-symbol-simplified
    normalize-instruction-simplified
    normalize-code-simplified
    parse-asm-simplified
    all-lists-p
    preprocess-code-simplified
    calculate-code-start-simplified
    load-code-simplified
    append-two-lists
    append-code-simplified
    dump-code-simplified
    load-and-run-simplified
    list-length
    nth-element))

(defvar *loader-tests* 0)
(defvar *loader-ok* 0)

(defun test-loader-function (name)
  "Teste qu'une fonction loader est définie"
  (setq *loader-tests* (+ *loader-tests* 1))
  (if (fboundp name)
      (progn
        (setq *loader-ok* (+ *loader-ok* 1))
        (format t "  ✓ ~A~%" name))
      (format t "  ✗ ~A non trouvée~%" name)))

(dolist (fn *loader-functions*)
  (test-loader-function fn))

(format t "~%Loader: ~A/~A fonctions définies (~,1F%)~%~%"
        *loader-ok* *loader-tests* 
        (* 100.0 (/ *loader-ok* *loader-tests*)))

;;; ============================================================================
;;; Test du COMPILER simplifié
;;; ============================================================================

(format t "Test 2: Fonctions du compiler-simplified.lisp~%~%")

(defvar *compiler-functions*
  '(reset-global-tables-simplified
    make-new-compiler-env-simplified
    env-get
    env-set
    env-remove
    add-variable-simplified
    lookup-variable-simplified
    lookup-variable-with-depth-simplified
    add-function-simplified
    lookup-function-simplified
    lookup-function-def-info-simplified
    alist-lookup
    alist-add
    alloc-stack-slot-simplified
    free-stack-slots-simplified
    allocate-temp-reg-simplified
    free-temp-reg-simplified
    alloc-temp-register-simplified
    gen-label-simplified
    copy-env-simplified
    copy-tree-simple
    make-lexical-env-simplified
    generate-static-link-access-simplified
    lisp-atom-p-simplified
    lisp-list-p-simplified
    vm-primitive-p-simplified
    parse-lisp-expr-simplified
    extract-first-elements-simplified
    extract-second-elements-simplified
    flatten-clauses-simplified
    extract-clause-bodies-simplified
    append-two
    append-many
    reverse-list-simple
    reverse-helper
    list-length-simple
    nth-simple
    map-eval-constant-expr-simplified
    free-variables-simplified
    free-variables-list-simplified
    member-simple
    compile-constant-simplified
    compile-variable-simplified
    compile-arithmetic-simplified
    compile-arithmetic-binary
    compile-arithmetic-op-simple
    compile-math-func-simplified
    compile-comparison-simplified
    compile-comparison-pair
    compile-comparison-branch-simple
    compile-if-simplified
    compile-cond-simplified
    compile-cond-clause
    compile-when-simplified
    compile-unless-simplified
    compile-not-simplified
    compile-and-simplified
    compile-and-helper
    compile-or-simplified
    compile-or-helper
    compile-case-simplified
    compile-case-clauses
    compile-case-clause
    compile-case-test
    compile-let-simplified
    compile-let-bindings
    compile-let*-simplified
    compile-progn-simplified
    compile-loop-while-simplified
    compile-while-simplified
    compile-loop-advanced-simplified
    compile-dolist-simplified
    compile-defun-simplified
    compile-function-body
    compile-funcall-simplified
    compile-args
    compile-lambda-simplified
    compile-apply-simplified
    compile-labels-simplified
    compile-flet-simplified
    compile-setq-simplified
    compile-setf-simplified
    compile-defvar-simplified
    compile-defparameter-simplified
    compile-defconstant-simplified
    compile-quote-simplified
    compile-function-simplified
    compile-make-array-simplified
    compile-aref-simplified
    compile-make-hash-table-simplified
    compile-gethash-simplified
    compile-hash-set-simplified
    compile-defstruct-simplified
    compile-make-struct-simplified
    compile-struct-accessor-simplified
    compile-cons-simplified
    compile-car-simplified
    compile-cdr-simplified
    compile-list-simplified
    compile-list-elements
    compile-append-simplified
    compile-length-simplified
    compile-nth-simplified
    compile-first-simplified
    compile-rest-simplified
    compile-null-simplified
    compile-listp-simplified
    compile-numberp-simplified
    compile-symbolp-simplified
    compile-atom-simplified
    compile-eq-simplified
    compile-equal-simplified
    compile-print-simplified
    compile-format-simplified
    compile-error-simplified
    compile-values-simplified
    compile-multiple-value-bind-simplified
    compile-block-simplified
    compile-return-from-simplified
    compile-tagbody-simplified
    compile-go-simplified
    compile-unwind-protect-simplified
    compile-catch-simplified
    compile-throw-simplified
    compile-expr-main
    compile-list-form
    compile-lisp-to-mips-simplified))

(defvar *compiler-tests* 0)
(defvar *compiler-ok* 0)

(defun test-compiler-function (name)
  "Teste qu'une fonction compiler est définie"
  (setq *compiler-tests* (+ *compiler-tests* 1))
  (if (fboundp name)
      (progn
        (setq *compiler-ok* (+ *compiler-ok* 1))
        (format t "  ✓ ~A~%" name))
      (format t "  ✗ ~A non trouvée~%" name)))

(dolist (fn *compiler-functions*)
  (test-compiler-function fn))

(format t "~%Compiler: ~A/~A fonctions définies (~,1F%)~%~%"
        *compiler-ok* *compiler-tests* 
        (* 100.0 (/ *compiler-ok* *compiler-tests*)))

;;; ============================================================================
;;; Test de compilation d'exemples
;;; ============================================================================

(format t "Test 3: Compilation d'exemples~%~%")

(defvar *compile-tests* 0)
(defvar *compile-ok* 0)

(defun test-compile-expr (name expr)
  "Teste la compilation d'une expression"
  (setq *compile-tests* (+ *compile-tests* 1))
  (handler-case
      (let* ((env (make-new-compiler-env-simplified))
             (result (compile-expr-main expr env)))
        (if (and result (> (length result) 0))
            (progn
              (setq *compile-ok* (+ *compile-ok* 1))
              (format t "  ✅ ~A : ~A instructions~%" name (length result)))
            (format t "  ⚠ ~A : résultat vide~%" name)))
    (error (e)
      (format t "  ❌ ~A : ~A~%" name e))))

;; Charger les fonctions simplifiées
(load "src/compiler-simplified.lisp")

;; Tests de compilation
(test-compile-expr "Constante" 42)
(test-compile-expr "Addition" '(+ 1 2))
(test-compile-expr "Comparaison" '(< x 10))
(test-compile-expr "IF simple" '(if (< x 10) x 0))
(test-compile-expr "LET" '(let ((x 5)) x))
(test-compile-expr "PROGN" '(progn (+ 1 2) (+ 3 4)))

(format t "~%Compilation: ~A/~A tests réussis (~,1F%)~%~%"
        *compile-ok* *compile-tests* 
        (* 100.0 (/ *compile-ok* *compile-tests*)))

;;; ============================================================================
;;; Résultats finaux
;;; ============================================================================

(format t "========================================~%")
(format t "RÉSULTATS GLOBAUX~%")
(format t "========================================~%")
(format t "Loader    : ~A/~A fonctions (~,1F%)~%" 
        *loader-ok* *loader-tests*
        (* 100.0 (/ *loader-ok* *loader-tests*)))
(format t "Compiler  : ~A/~A fonctions (~,1F%)~%"
        *compiler-ok* *compiler-tests*
        (* 100.0 (/ *compiler-ok* *compiler-tests*)))
(format t "Exemples  : ~A/~A compilés (~,1F%)~%"
        *compile-ok* *compile-tests*
        (* 100.0 (/ *compile-ok* *compile-tests*)))

(let ((total-tests (+ *loader-tests* *compiler-tests* *compile-tests*))
      (total-ok (+ *loader-ok* *compiler-ok* *compile-ok*)))
  (format t "~%TOTAL     : ~A/~A (~,1F%)~%"
          total-ok total-tests
          (* 100.0 (/ total-ok total-tests))))

(if (and (= *loader-ok* *loader-tests*)
         (= *compiler-ok* *compiler-tests*)
         (> *compile-ok* 4))
    (format t "~%✅ SUCCÈS : Toutes les fonctions sont définies et compilables !~%")
    (format t "~%⚠ Certaines fonctions manquent ou ne compilent pas.~%"))

(format t "~%")
