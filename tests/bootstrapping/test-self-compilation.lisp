#!/usr/bin/env clisp
;;; =============================================================================
;;; TEST DE BOOTSTRAPPING - AUTO-COMPILATION DU COMPILATEUR
;;; =============================================================================
;;;
;;; Ce test vérifie que le compilateur peut compiler des parties de lui-même.
;;; Stratégie : commencer par les fonctions simples, puis augmenter la complexité.
;;;
;;; Phase 1 : Fonctions helpers (récursion simple)
;;; Phase 2 : Fonctions de parsing (manipulations de listes)
;;; Phase 3 : Fonctions de compilation (complexes avec environnements)
;;; Phase 4 : Compilation complète du compilateur
;;;
;;; =============================================================================

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(defvar *test-count* 0)
(defvar *passed-count* 0)

(defun test-compilation (name source-code)
  "Test qu'une fonction peut être compilée et exécutée"
  (incf *test-count*)
  (format t "~%~%========================================~%")
  (format t "Test ~A: ~A~%" *test-count* name)
  (format t "========================================~%")
  
  (handler-case
      (let* ((env (make-hash-table :test 'equal))
             (parsed (parse-lisp-expr source-code))
             (compiled (compile-expr parsed env))
             (mips-code (get-hash env 'mips-code)))
        
        (format t "✓ Parsing réussi~%")
        (format t "✓ Compilation réussie~%")
        (format t "Code généré: ~A instructions MIPS~%" 
                (length mips-code))
        
        ;; Vérifier que du code a été généré
        (if (> (length mips-code) 0)
            (progn
              (incf *passed-count*)
              (format t "✅ TEST RÉUSSI~%")
              t)
            (progn
              (format t "❌ TEST ÉCHOUÉ: Aucun code généré~%")
              nil)))
    
    (error (e)
      (format t "❌ TEST ÉCHOUÉ: ~A~%" e)
      nil)))

(defun test-compilation-and-execution (name source-code test-call expected-result)
  "Test qu'une fonction peut être compilée ET exécutée correctement"
  (incf *test-count*)
  (format t "~%~%========================================~%")
  (format t "Test ~A: ~A~%" *test-count* name)
  (format t "========================================~%")
  
  (handler-case
      (let* ((combined-expr (list 'progn source-code test-call))
             (vm (compile-and-run combined-expr))
             (result (get-register vm :$V0)))
        
        (format t "Résultat attendu: ~A~%" expected-result)
        (format t "Résultat obtenu:  ~A~%" result)
        
        (if (equal result expected-result)
            (progn
              (incf *passed-count*)
              (format t "✅ TEST RÉUSSI~%")
              t)
            (progn
              (format t "❌ TEST ÉCHOUÉ~%")
              nil)))
    
    (error (e)
      (format t "❌ TEST ÉCHOUÉ: ~A~%" e)
      nil)))

;;; =============================================================================
;;; PHASE 1 : FONCTIONS HELPERS (BOOTSTRAPPING)
;;; =============================================================================

(format t "~%~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "PHASE 1 : COMPILATION DES FONCTIONS HELPERS~%")
(format t "═══════════════════════════════════════════════════════════════~%")

;;; Test 1 : extract-first-elements
(test-compilation-and-execution
 "extract-first-elements"
 '(defun extract-first-elements (list-of-pairs)
    (if (null list-of-pairs)
        nil
        (cons (first (first list-of-pairs))
              (extract-first-elements (rest list-of-pairs)))))
 '(extract-first-elements '((a 1) (b 2) (c 3)))
 '(a b c))

;;; Test 2 : extract-second-elements
(test-compilation-and-execution
 "extract-second-elements"
 '(defun extract-second-elements (list-of-pairs)
    (if (null list-of-pairs)
        nil
        (cons (second (first list-of-pairs))
              (extract-second-elements (rest list-of-pairs)))))
 '(extract-second-elements '((a 1) (b 2) (c 3)))
 '(1 2 3))

;;; Test 3 : flatten-clauses
(test-compilation-and-execution
 "flatten-clauses"
 '(defun flatten-clauses (clauses)
    (if (null clauses)
        nil
        (append (first clauses)
                (flatten-clauses (rest clauses)))))
 '(flatten-clauses '((a b) (c d) (e f)))
 '(a b c d e f))

;;; Test 4 : extract-clause-bodies
(test-compilation-and-execution
 "extract-clause-bodies"
 '(defun extract-clause-bodies (clauses)
    (if (null clauses)
        nil
        (append (rest (first clauses))
                (extract-clause-bodies (rest clauses)))))
 '(extract-clause-bodies '((cond1 body1) (cond2 body2 body3) (cond3 body4)))
 '(body1 body2 body3 body4))

;;; =============================================================================
;;; PHASE 2 : FONCTIONS DE MANIPULATION DE LISTES
;;; =============================================================================

(format t "~%~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "PHASE 2 : FONCTIONS DE MANIPULATION DE LISTES~%")
(format t "═══════════════════════════════════════════════════════════════~%")

;;; Test 5 : Fonction de recherche simple
(test-compilation-and-execution
 "lookup-simple"
 '(defun lookup-simple (key alist)
    (if (null alist)
        nil
        (if (equal key (first (first alist)))
            (second (first alist))
            (lookup-simple key (rest alist)))))
 '(lookup-simple 'b '((a 1) (b 2) (c 3)))
 2)

;;; Test 6 : Fonction de filtrage
(test-compilation-and-execution
 "filter-numbers"
 '(defun filter-numbers (lst)
    (if (null lst)
        nil
        (if (numberp (first lst))
            (cons (first lst) (filter-numbers (rest lst)))
            (filter-numbers (rest lst)))))
 '(filter-numbers '(1 a 2 b 3 c))
 '(1 2 3))

;;; Test 7 : Fonction de comptage
(test-compilation-and-execution
 "count-elements"
 '(defun count-elements (lst)
    (if (null lst)
        0
        (+ 1 (count-elements (rest lst)))))
 '(count-elements '(a b c d e))
 5)

;;; =============================================================================
;;; PHASE 3 : FONCTIONS AVEC STRUCTURES DE DONNÉES
;;; =============================================================================

(format t "~%~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "PHASE 3 : FONCTIONS AVEC STRUCTURES COMPLEXES~%")
(format t "═══════════════════════════════════════════════════════════════~%")

;;; Test 8 : get-required-params (du parser)
(test-compilation-and-execution
 "get-required-params"
 '(defun get-required-params (parsed-params)
    (getf parsed-params :required))
 '(get-required-params '(:required (a b) :optional ((c 10))))
 '(a b))

;;; Test 9 : has-optional-params-p (du parser)
(test-compilation-and-execution
 "has-optional-params-p"
 '(defun has-optional-params-p (parsed-params)
    (not (null (getf parsed-params :optional))))
 '(has-optional-params-p '(:required (a) :optional ((b 10))))
 t)

;;; Test 10 : Fonction avec COND et cas multiples
(test-compilation-and-execution
 "classify-number"
 '(defun classify-number (n)
    (cond
      ((< n 0) 'negative)
      ((= n 0) 'zero)
      ((< n 10) 'small)
      ((< n 100) 'medium)
      (t 'large)))
 '(classify-number 42)
 'medium)

;;; =============================================================================
;;; PHASE 4 : FONCTIONS DE PARSING (SIMPLIFIÉES)
;;; =============================================================================

(format t "~%~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "PHASE 4 : FONCTIONS DE PARSING SIMPLIFIÉES~%")
(format t "═══════════════════════════════════════════════════════════════~%")

;;; Test 11 : Parser simple pour expressions constantes
(test-compilation-and-execution
 "parse-constant"
 '(defun parse-constant (expr)
    (cond
      ((numberp expr) (list :constant expr))
      ((symbolp expr) (list :variable expr))
      (t (list :unknown expr))))
 '(parse-constant 42)
 '(:constant 42))

;;; Test 12 : Parser pour opérations binaires simples
(test-compilation-and-execution
 "parse-binary-op"
 '(defun parse-binary-op (expr)
    (if (and (consp expr) (= (length expr) 3))
        (list :op (first expr) :left (second expr) :right (third expr))
        (list :error expr)))
 '(parse-binary-op '(+ 1 2))
 '(:op + :left 1 :right 2))

;;; =============================================================================
;;; RÉSUMÉ DES TESTS
;;; =============================================================================

(format t "~%~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ DES TESTS DE BOOTSTRAPPING~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "~%Tests exécutés: ~A~%" *test-count*)
(format t "Tests réussis:  ~A~%" *passed-count*)
(format t "Tests échoués:  ~A~%" (- *test-count* *passed-count*))
(format t "Taux de réussite: ~,1F%~%" 
        (* 100.0 (/ *passed-count* *test-count*)))

(if (= *passed-count* *test-count*)
    (progn
      (format t "~%✅ TOUS LES TESTS SONT PASSÉS!~%")
      (format t "~%Le compilateur peut compiler ses propres fonctions helpers.~%")
      (format t "Prêt pour la compilation complète du compilateur.~%"))
    (progn
      (format t "~%⚠️  CERTAINS TESTS ONT ÉCHOUÉ~%")
      (format t "~%Des problèmes subsistent dans l'auto-compilation.~%")))

(format t "═══════════════════════════════════════════════════════════════~%")
(format t "~%")

;;; Sortir avec code approprié
(if (= *passed-count* *test-count*)
    (ext:exit 0)
    (ext:exit 1))
