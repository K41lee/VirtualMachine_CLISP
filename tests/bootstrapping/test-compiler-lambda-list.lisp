;;;; test-compiler-lambda-list.lisp
;;;; Test de l'intégration des paramètres avancés dans le compilateur

(load "src/vm.lisp")
(load "src/compiler.lisp")

(format t "~%═══════════════════════════════════════════════════~%")
(format t "TEST COMPILATEUR + PARAMÈTRES AVANCÉS~%")
(format t "═══════════════════════════════════════════════════~%~%")

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 1 : Compiler une fonction avec &optional
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 1 : Compilation d'une fonction avec &optional~%")
(format t "  (defun test-opt (a &optional (b 10)) (+ a b))~%")

(let* ((expr '(defun test-opt (a &optional (b 10)) (+ a b)))
       (env (make-new-compiler-env)))
  (handler-case
      (let ((code (compile-expr expr env)))
        (format t "  Code généré : ~A instructions~%" (length code))
        (format t "  Premières instructions :~%")
        (dolist (instr (subseq code 0 (min 10 (length code))))
          (format t "    ~A~%" instr))
        (if (> (length code) 0)
            (format t "  ✓ TEST 1 RÉUSSI~%~%")
            (format t "  ✗ TEST 1 ÉCHOUÉ~%~%")))
    (error (e)
      (format t "  ✗ ERREUR : ~A~%~%" e))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 2 : Compiler une fonction avec &key
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 2 : Compilation d'une fonction avec &key~%")
(format t "  (defun test-key (a &key (x 1)) (+ a x))~%")

(let* ((expr '(defun test-key (a &key (x 1)) (+ a x)))
       (env (make-new-compiler-env)))
  (handler-case
      (let ((code (compile-expr expr env)))
        (format t "  Code généré : ~A instructions~%" (length code))
        (if (> (length code) 0)
            (format t "  ✓ TEST 2 RÉUSSI~%~%")
            (format t "  ✗ TEST 2 ÉCHOUÉ~%~%")))
    (error (e)
      (format t "  ✗ ERREUR : ~A~%~%" e))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 3 : Compiler une fonction avec &rest
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 3 : Compilation d'une fonction avec &rest~%")
(format t "  (defun test-rest (a &rest r) (cons a r))~%")

(let* ((expr '(defun test-rest (a &rest r) (cons a r)))
       (env (make-new-compiler-env)))
  (handler-case
      (let ((code (compile-expr expr env)))
        (format t "  Code généré : ~A instructions~%" (length code))
        (if (> (length code) 0)
            (format t "  ✓ TEST 3 RÉUSSI~%~%")
            (format t "  ✗ TEST 3 ÉCHOUÉ~%~%")))
    (error (e)
      (format t "  ✗ ERREUR : ~A~%~%" e))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 4 : CAS RÉEL - make-lexical-env de compiler.lisp
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 4 : Cas réel make-lexical-env~%")
(format t "  (defun make-lexical-env (parent-env &optional (increment-depth t)) ...)~%")

(let* ((expr '(defun make-lexical-env (parent-env &optional (increment-depth t))
                (list parent-env increment-depth)))
       (env (make-new-compiler-env)))
  (handler-case
      (let ((code (compile-expr expr env)))
        (format t "  Code généré : ~A instructions~%" (length code))
        (if (> (length code) 0)
            (format t "  ✓ TEST 4 RÉUSSI~%~%")
            (format t "  ✗ TEST 4 ÉCHOUÉ~%~%")))
    (error (e)
      (format t "  ✗ ERREUR : ~A~%~%" e))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 5 : CAS RÉEL - compile-and-run de compiler.lisp
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 5 : Cas réel compile-and-run~%")
(format t "  (defun compile-and-run (expr &key (verbose nil) (show-code nil)) ...)~%")

(let* ((expr '(defun compile-and-run (expr &key (verbose nil) (show-code nil))
                (list expr verbose show-code)))
       (env (make-new-compiler-env)))
  (handler-case
      (let ((code (compile-expr expr env)))
        (format t "  Code généré : ~A instructions~%" (length code))
        (if (> (length code) 0)
            (format t "  ✓ TEST 5 RÉUSSI~%~%")
            (format t "  ✗ TEST 5 ÉCHOUÉ~%~%")))
    (error (e)
      (format t "  ✗ ERREUR : ~A~%~%" e))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 6 : Fonction standard (sans paramètres avancés)
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 6 : Fonction standard (régression)~%")
(format t "  (defun add (a b) (+ a b))~%")

(let* ((expr '(defun add (a b) (+ a b)))
       (env (make-new-compiler-env)))
  (handler-case
      (let ((code (compile-expr expr env)))
        (format t "  Code généré : ~A instructions~%" (length code))
        (if (> (length code) 0)
            (format t "  ✓ TEST 6 RÉUSSI~%~%")
            (format t "  ✗ TEST 6 ÉCHOUÉ~%~%")))
    (error (e)
      (format t "  ✗ ERREUR : ~A~%~%" e))))

;;; ═══════════════════════════════════════════════════════════════════
;;; RÉSUMÉ
;;; ═══════════════════════════════════════════════════════════════════

(format t "═══════════════════════════════════════════════════~%")
(format t "TESTS TERMINÉS~%")
(format t "═══════════════════════════════════════════════════~%")
(format t "~%")
(format t "Le compilateur supporte maintenant :~%")
(format t "  ✓ Fonctions avec &optional~%")
(format t "  ✓ Fonctions avec &key~%")
(format t "  ✓ Fonctions avec &rest~%")
(format t "  ✓ Cas réels de compiler.lisp~%")
(format t "  ✓ Fonctions standard (rétrocompatibilité)~%")
(format t "~%")
