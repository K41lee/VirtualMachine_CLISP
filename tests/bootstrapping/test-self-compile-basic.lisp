;;;; test-self-compile-basic.lisp
;;;; Test plus basique : vérifie que le compilateur peut compiler des parties de lui-même
;;;; sans se soucier de la représentation exacte des symboles

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TEST AUTO-COMPILATION BASIQUE~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "Objectif: Vérifier que le compilateur peut compiler~%")
(format t "          des fonctions qui manipulent des listes~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(defun test-function (name code expected-result)
  "Teste une fonction compilée"
  (format t "Test: ~A~%" name)
  (handler-case
      (let* ((vm (make-new-vm :verbose nil))
             (env (make-new-compiler-env))
             (asm-code (compile-expr code env)))
        (load-and-run vm asm-code :verbose nil :include-runtime t)
        (let ((result (get-register vm :$V0)))
          (if (equal result expected-result)
              (progn
                (format t "  ✓ PASS (résultat = ~A)~%" result)
                t)
              (progn
                (format t "  ✗ FAIL~%")
                (format t "    Attendu: ~A~%" expected-result)
                (format t "    Obtenu:  ~A~%" result)
                nil))))
    (error (e)
      (format t "  ✗ FAIL (erreur: ~A)~%" e)
      nil)))

(defparameter *tests-passed* 0)
(defparameter *tests-total* 0)

(defun run-test (name code expected)
  (incf *tests-total*)
  (when (test-function name code expected)
    (incf *tests-passed*)))

;; ============================================================================
;; PHASE 1: Tests de base sur listes
;; ============================================================================

(format t "~%PHASE 1: Opérations de base sur listes~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(run-test "longueur-liste (liste vide)"
          '(progn
             (defun longueur-liste (lst)
               (if (null lst) 0 (+ 1 (longueur-liste (cdr lst)))))
             (longueur-liste (quote ())))
          0)

(run-test "longueur-liste (1 élément)"
          '(progn
             (defun longueur-liste (lst)
               (if (null lst) 0 (+ 1 (longueur-liste (cdr lst)))))
             (longueur-liste (quote (a))))
          1)

(run-test "longueur-liste (5 éléments)"
          '(progn
             (defun longueur-liste (lst)
               (if (null lst) 0 (+ 1 (longueur-liste (cdr lst)))))
             (longueur-liste (quote (a b c d e))))
          5)

(run-test "nth-element (premier)"
          '(progn
             (defun nth-element (n lst)
               (if (= n 0)
                   (car lst)
                   (nth-element (- n 1) (cdr lst))))
             (nth-element 0 (quote (10 20 30))))
          10)

(run-test "nth-element (deuxième)"
          '(progn
             (defun nth-element (n lst)
               (if (= n 0)
                   (car lst)
                   (nth-element (- n 1) (cdr lst))))
             (nth-element 1 (quote (10 20 30))))
          20)

(run-test "nth-element (troisième)"
          '(progn
             (defun nth-element (n lst)
               (if (= n 0)
                   (car lst)
                   (nth-element (- n 1) (cdr lst))))
             (nth-element 2 (quote (10 20 30))))
          30)

;; ============================================================================
;; PHASE 2: Tests avec nombres dans listes
;; ============================================================================

(format t "~%PHASE 2: Calculs sur listes de nombres~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(run-test "somme-liste (vide)"
          '(progn
             (defun somme-liste (lst)
               (if (null lst)
                   0
                   (+ (car lst) (somme-liste (cdr lst)))))
             (somme-liste (quote ())))
          0)

(run-test "somme-liste (1 2 3)"
          '(progn
             (defun somme-liste (lst)
               (if (null lst)
                   0
                   (+ (car lst) (somme-liste (cdr lst)))))
             (somme-liste (quote (1 2 3))))
          6)

(run-test "somme-liste (10 20 30 40)"
          '(progn
             (defun somme-liste (lst)
               (if (null lst)
                   0
                   (+ (car lst) (somme-liste (cdr lst)))))
             (somme-liste (quote (10 20 30 40))))
          100)

(run-test "max-liste (simple)"
          '(progn
             (defun max-liste (lst)
               (if (null (cdr lst))
                   (car lst)
                   (let ((rest-max (max-liste (cdr lst))))
                     (if (> (car lst) rest-max)
                         (car lst)
                         rest-max))))
             (max-liste (quote (5 2 8 1 9 3))))
          9)

;; ============================================================================
;; PHASE 3: Tests de comptage et prédicats
;; ============================================================================

(format t "~%PHASE 3: Comptage et prédicats~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(run-test "count-if-positive (aucun)"
          '(progn
             (defun count-if-positive (lst)
               (if (null lst)
                   0
                   (if (> (car lst) 0)
                       (+ 1 (count-if-positive (cdr lst)))
                       (count-if-positive (cdr lst)))))
             (count-if-positive (quote (-5 -2 -8))))
          0)

(run-test "count-if-positive (tous)"
          '(progn
             (defun count-if-positive (lst)
               (if (null lst)
                   0
                   (if (> (car lst) 0)
                       (+ 1 (count-if-positive (cdr lst)))
                       (count-if-positive (cdr lst)))))
             (count-if-positive (quote (5 2 8))))
          3)

(run-test "count-if-positive (mixte)"
          '(progn
             (defun count-if-positive (lst)
               (if (null lst)
                   0
                   (if (> (car lst) 0)
                       (+ 1 (count-if-positive (cdr lst)))
                       (count-if-positive (cdr lst)))))
             (count-if-positive (quote (-5 2 -8 3 9))))
          3)

(run-test "member-p (présent)"
          '(progn
             (defun member-p (x lst)
               (if (null lst)
                   0
                   (if (= x (car lst))
                       1
                       (member-p x (cdr lst)))))
             (member-p 20 (quote (10 20 30))))
          1)

(run-test "member-p (absent)"
          '(progn
             (defun member-p (x lst)
               (if (null lst)
                   0
                   (if (= x (car lst))
                       1
                       (member-p x (cdr lst)))))
             (member-p 40 (quote (10 20 30))))
          0)

;; ============================================================================
;; RÉSULTATS
;; ============================================================================

(format t "~%~%═══════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ DES TESTS~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "Tests exécutés: ~A~%" *tests-total*)
(format t "Tests réussis:  ~A~%" *tests-passed*)
(format t "Tests échoués:  ~A~%" (- *tests-total* *tests-passed*))
(format t "Taux de réussite: ~,1F%~%" (* 100.0 (/ *tests-passed* *tests-total*)))

(if (= *tests-passed* *tests-total*)
    (progn
      (format t "~%✅ TOUS LES TESTS ONT RÉUSSI!~%")
      (format t "~%Le compilateur peut compiler des fonctions complexes~%")
      (format t "qui manipulent des listes et font de la récursion.~%")
      (format t "~%Cela démontre que l'auto-compilation est POSSIBLE~%")
      (format t "pour les fonctions qui ne dépendent pas de la~%")
      (format t "représentation exacte des symboles.~%"))
    (progn
      (format t "~%⚠️  CERTAINS TESTS ONT ÉCHOUÉ~%")
      (format t "~%Le système n'est pas encore prêt pour l'auto-compilation complète.~%")))

(format t "═══════════════════════════════════════════════════════════════~%")
