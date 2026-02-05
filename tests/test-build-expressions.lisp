#!/usr/bin/env clisp
;;;; ============================================================================
;;;; TEST: Construction d'expressions Lisp dans la VM
;;;; Vérifie que les fonctions de utils-bootstrap.lisp fonctionnent correctement
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")

;; Initialiser les symboles AVANT de charger les utils
(initialize-compiler-symbols)

(load "utils-bootstrap.lisp")

(format t "╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST: Construction d'expressions Lisp dans la VM            ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-expression (description expr)
  "Teste la construction et la lecture d'une expression"
  (incf *test-count*)
  (format t "~%TEST ~A: ~A~%" *test-count* description)
  (format t "  Expression: ~A~%" expr)
  (handler-case
      (let* ((handle (build-expression-in-vm expr))
             (reconstructed (read-expression-from-vm handle)))
        (format t "  → Handle: ~A~%" handle)
        (format t "  → Reconstruit: ~A~%" reconstructed)
        ;; Vérifier que la reconstruction correspond (avec symboles → keywords)
        (let ((expected (convert-symbols-to-keywords expr)))
          (if (equal reconstructed expected)
              (progn
                (format t "  ✅ SUCCÈS~%")
                (incf *pass-count*))
              (progn
                (format t "  ❌ ÉCHEC: attendu ~A~%" expected)
                (incf *fail-count*)))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *fail-count*))))

;;; ============================================================================
;;; SÉRIE DE TESTS
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CATÉGORIE 1: Atomes simples~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-expression "Nombre entier" 42)
(test-expression "Nombre négatif" -10)
(test-expression "Zéro" 0)
(test-expression "Symbole simple" 'FIBO)
(test-expression "Symbole +" '+)
(test-expression "Symbole IF" 'IF)
(test-expression "NIL" nil)

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CATÉGORIE 2: Listes simples~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-expression "Liste de nombres" '(1 2 3))
(test-expression "Addition simple" '(+ 1 2))
(test-expression "Soustraction" '(- 10 5))
(test-expression "Comparaison" '(< n 2))
(test-expression "Liste de symboles" '(A B C))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CATÉGORIE 3: Expressions imbriquées~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-expression "Addition imbriquée" '(+ (+ 1 2) 3))
(test-expression "Expression IF simple" '(if t 1 0))
(test-expression "IF avec comparaison" '(if (< n 2) n 0))
(test-expression "Expression complexe" '(+ (- 5 2) (* 3 4)))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CATÉGORIE 4: Expressions de fonction~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-expression "Liste de paramètres" '(n))
(test-expression "Liste multi-paramètres" '(m n))
(test-expression "LAMBDA simple" '(lambda (x) (+ x 1)))
(test-expression "DEFUN simple" '(defun increment (x) (+ x 1)))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CATÉGORIE 5: Fibonacci et Ackermann~%")
(format t "════════════════════════════════════════════════════════════════~%")

;; FIBONACCI complet
(format t "~%TEST ~A: Fibonacci complet~%" (incf *test-count*))
(defparameter *fibo-expr*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) 
            (fibo (- n 2))))))

(format t "  Expression:~%    ~A~%" *fibo-expr*)
(handler-case
    (let* ((handle (build-expression-in-vm *fibo-expr*))
           (reconstructed (read-expression-from-vm handle)))
      (format t "  → Handle: ~A~%" handle)
      (format t "  → Reconstruit:~%")
      (format t "    ~A~%" reconstructed)
      (let ((expected (convert-symbols-to-keywords *fibo-expr*)))
        (if (equal reconstructed expected)
            (progn
              (format t "  ✅ SUCCÈS: Structure complète de FIBO reconstruite~%")
              (incf *pass-count*))
            (progn
              (format t "  ❌ ÉCHEC~%")
              (format t "     Attendu: ~A~%" expected)
              (incf *fail-count*)))))
  (error (e)
    (format t "  ❌ ERREUR: ~A~%" e)
    (incf *fail-count*)))

;; ACKERMANN complet
(format t "~%TEST ~A: Ackermann complet~%" (incf *test-count*))
(defparameter *ack-expr*
  '(defun ack (m n)
     (cond
       ((= m 0) (+ n 1))
       ((= n 0) (ack (- m 1) 1))
       (t (ack (- m 1) (ack m (- n 1)))))))

(format t "  Expression:~%    ~A~%" *ack-expr*)
(handler-case
    (let* ((handle (build-expression-in-vm *ack-expr*))
           (reconstructed (read-expression-from-vm handle)))
      (format t "  → Handle: ~A~%" handle)
      (format t "  → Reconstruit:~%")
      (format t "    ~A~%" reconstructed)
      (let ((expected (convert-symbols-to-keywords *ack-expr*)))
        (if (equal reconstructed expected)
            (progn
              (format t "  ✅ SUCCÈS: Structure complète de ACK reconstruite~%")
              (incf *pass-count*))
            (progn
              (format t "  ❌ ÉCHEC~%")
              (format t "     Attendu: ~A~%" expected)
              (incf *fail-count*)))))
  (error (e)
    (format t "  ❌ ERREUR: ~A~%" e)
    (incf *fail-count*)))

;;; ============================================================================
;;; TESTS DE VÉRIFICATION STRUCTURELLE
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CATÉGORIE 6: Vérification structurelle avec LIST-CAR/CDR~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%TEST ~A: Déconstruction de (+ 1 2)~%" (incf *test-count*))
(let* ((expr '(+ 1 2))
       (handle (build-expression-in-vm expr)))
  (format t "  Construction: ~A → handle ~A~%" expr handle)
  
  ;; Simuler LIST-CAR/CDR (accès direct pour le test)
  (let* ((cons-cell (gethash handle *vm-lisp-objects*))
         (car-val (car cons-cell))
         (cdr-handle (cdr cons-cell))
         (cdr-cell (gethash cdr-handle *vm-lisp-objects*)))
    (format t "  CAR (symbole +): ~A (ID: ~A, nom: ~A)~%" 
            (symbol-id-to-keyword car-val)
            car-val
            (symbol-name-from-id car-val))
    (format t "  CDR: handle ~A → ~A~%" cdr-handle cdr-cell)
    (format t "  CDR CAR (1): ~A~%" (car cdr-cell))
    (format t "  CDR CDR: handle ~A~%" (cdr cdr-cell))
    (if (and (= car-val (build-atom-in-vm '+))
             (= (car cdr-cell) 1))
        (progn
          (format t "  ✅ Structure correcte~%")
          (incf *pass-count*))
        (progn
          (format t "  ❌ Structure incorrecte~%")
          (incf *fail-count*)))))

;;; ============================================================================
;;; STATISTIQUES
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "STATISTIQUES~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Objets créés dans *vm-lisp-objects*: ~A~%" (count-vm-objects))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                        RÉSULTATS FINAUX                        ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(format t "~%Tests exécutés: ~A~%" *test-count*)
(format t "Tests réussis:  ~A ✅~%" *pass-count*)
(format t "Tests échoués:  ~A ❌~%" *fail-count*)

(if (= *fail-count* 0)
    (format t "~%🎉 TOUS LES TESTS PASSENT! 🎉~%")
    (format t "~%⚠️  Certains tests ont échoué~%"))

(format t "~%Taux de réussite: ~A%~%" 
        (if (> *test-count* 0)
            (round (* 100 (/ *pass-count* *test-count*)))
            0))

(format t "~%════════════════════════════════════════════════════════════════~%")
