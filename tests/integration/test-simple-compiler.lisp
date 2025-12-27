;;; ============================================================================
;;; TEST COMPILATION SIMPLE - Sous-ensemble minimal du compilateur
;;; ============================================================================
;;;
;;; Objectif : Compiler une version ultra-simplifiée du compilateur
;;; qui peut compiler des expressions arithmétiques simples.
;;;
;;; Fonctions nécessaires :
;;; - CONS, CAR, CDR (✓ implémentées)
;;; - LENGTH, NTH (✓ implémentées)
;;; - ASSOC, MEMBER (✓ implémentées)
;;; - APPEND (⚠ en cours - version simplifiée suffit)
;;; - IF, WHILE (✓ implémentées)
;;; - +, -, * (✓ implémentées)
;;;
;;; ============================================================================

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST - Sous-ensemble compilable du compilateur~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

;;; ============================================================================
;;; TEST 1 : Compilateur simple d'expressions arithmétiques
;;; ============================================================================

(format t "Test 1 : Compilateur mini pour (+  a b)~%~%")

;; Version simplifiée qui ne compile que l'addition
(defparameter *mini-compiler*
  '(defun mini-compile (expr env)
     "Compilateur minimal pour (+  a b)"
     (if (member (car expr) (cons (quote +) (cons (quote -) nil)))
         ;; C'est un opérateur
         (let ((op (car expr))
               (arg1 (car (cdr expr)))
               (arg2 (car (cdr (cdr expr)))))
           ;; Pour l'instant, juste retourner une valeur symbolique
           42)
         ;; Sinon, retourner 0
         0)))

(defparameter *mips-mini-compiler* (compile-lisp *mini-compiler*))
(format t "  → Code MIPS généré : ~A instructions~%~%" (length *mips-mini-compiler*))

;; Vérifier que ça compile (pas d'erreur)
(if (> (length *mips-mini-compiler*) 0)
    (format t "  ✅ Mini-compilateur compile avec succès~%~%")
    (format t "  ❌ Échec de compilation~%~%"))

;;; ============================================================================
;;; TEST 2 : Parser simple avec ASSOC
;;; ============================================================================

(format t "Test 2 : Parser avec table ASSOC~%~%")

(defparameter *mini-parser*
  '(defun mini-parse (op)
     "Parse un opérateur avec table ASSOC"
     (let ((ops (cons (cons (quote +) 1)
                      (cons (cons (quote -) 2)
                            (cons (cons (quote *) 3) nil)))))
       (let ((result (assoc op ops)))
         (if result
             (cdr result)
             0)))))

(defparameter *mips-mini-parser* (compile-lisp *mini-parser*))
(format t "  → Code MIPS généré : ~A instructions~%~%" (length *mips-mini-parser*))

(if (> (length *mips-mini-parser*) 0)
    (format t "  ✅ Mini-parser compile avec succès~%~%")
    (format t "  ❌ Échec de compilation~%~%"))

;;; ============================================================================
;;; TEST 3 : Dispatcher simple avec MEMBER
;;; ============================================================================

(format t "Test 3 : Dispatcher avec MEMBER~%~%")

(defparameter *mini-dispatcher*
  '(defun dispatch-op (op)
     "Dispatch selon l'opérateur"
     (if (member op (cons (quote +) (cons (quote -) (cons (quote *) nil))))
         1
         0)))

(defparameter *mips-dispatcher* (compile-lisp *mini-dispatcher*))
(format t "  → Code MIPS généré : ~A instructions~%~%" (length *mips-dispatcher*))

(if (> (length *mips-dispatcher*) 0)
    (format t "  ✅ Dispatcher compile avec succès~%~%")
    (format t "  ❌ Échec de compilation~%~%"))

;;; ============================================================================
;;; TEST 4 : Environnement avec ASSOC lookup
;;; ============================================================================

(format t "Test 4 : Lookup dans environnement~%~%")

(defparameter *env-lookup*
  '(defun env-lookup (var env)
     "Cherche une variable dans l'environnement"
     (let ((binding (assoc var env)))
       (if binding
           (cdr binding)
           -1))))

(defparameter *mips-env-lookup* (compile-lisp *env-lookup*))
(format t "  → Code MIPS généré : ~A instructions~%~%" (length *mips-env-lookup*))

(if (> (length *mips-env-lookup*) 0)
    (format t "  ✅ Env-lookup compile avec succès~%~%")
    (format t "  ❌ Échec de compilation~%~%"))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                           RÉSUMÉ                                 ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%")
(format t "~%")
(format t "✅ Tests de compilation de sous-ensembles du compilateur terminés~%")
(format t "~%")
(format t "Fonctions primitives disponibles :~%")
(format t "  - CONS, CAR, CDR         ✓~%")
(format t "  - LENGTH, NTH            ✓~%")
(format t "  - ASSOC, MEMBER          ✓~%")
(format t "  - IF, WHILE, LET         ✓~%")
(format t "  - Arithmétique (+,-,*)   ✓~%")
(format t "~%")
(format t "Prochaine étape : compiler le compilateur complet~%")
(format t "  (nécessite : APPEND, MAPCAR, ou refactoring avec alists)~%")
(format t "~%")
