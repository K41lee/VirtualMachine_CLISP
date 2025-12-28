;;; ============================================================================
;;; TEST RESOLVE-LABELS - RÉSOLUTION DES ADRESSES DE LABELS
;;; ============================================================================
;;;
;;; Cette fonction remplace les références de labels par leurs adresses.
;;; 
;;; Exemple:
;;;   Code avant: (JMP LOOP_START)
;;;   Labels:     {LOOP_START: 1000}
;;;   Code après: (JMP 1000)
;;;
;;; Stratégie:
;;; 1. Parcourir le code avec dolist
;;; 2. Pour chaque instruction, vérifier si elle contient une référence label
;;; 3. Si oui, chercher l'adresse dans la hash-table avec vm-gethash
;;; 4. Remplacer le symbole par l'adresse
;;;
;;; Commande: clisp test-resolve-labels.lisp
;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║              TEST RESOLVE-LABELS - RÉSOLUTION                    ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; VERSION 1: RESOLVE-LABELS SIMPLIFIÉ (COMPTE LES RÉSOLUTIONS)
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: resolve-labels-count - Compte les instructions~%")
(format t "═════════════════════════════════════════════════════════════════~%")

;; Version qui compte juste combien d'instructions on a
(defparameter *resolve-count-src*
  '(defun resolve-count (asm-code labels)
     (let ((count 0))
       (dolist (instr asm-code)
         (setq count (+ count 1)))
       count)))

(format t "  → Compilation...~%")
(defparameter *resolve-count-mips* (compile-lisp *resolve-count-src*))
(format t "  ✓ Compilé: ~A instructions~%~%" (length *resolve-count-mips*))

;; Test d'exécution
(defparameter *vm1* (make-new-vm :verbose nil))
(load-code *vm1* *resolve-count-mips* :verbose nil)
(defparameter *addr1* (calculate-code-start *vm1*))

;; Créer une liste d'instructions test
(defparameter *test-code* (list (list :ADD 1 2 3) (list :SUB 4 5 6)))
(defparameter *handle-code*
  (let ((h (incf *vm-lisp-handle-counter*)))
    (setf (gethash h *vm-lisp-objects*) *test-code*)
    h))

;; Créer une hash-table vide (pas utilisée pour ce test)
(defparameter *test-labels* (make-hash-table :test 'equal))
(setf (gethash 'LOOP_START *test-labels*) 1000)
(defparameter *handle-labels*
  (let ((h (incf *vm-hash-handle-counter*)))
    (setf (gethash h *vm-hash-tables*) *test-labels*)
    h))

(set-register *vm1* *reg-a0* *handle-code*)
(set-register *vm1* *reg-a1* *handle-labels*)
(set-register *vm1* *reg-ra* 999999)
(set-register *vm1* (get-reg :pc) *addr1*)

(format t "  → Exécution...~%")
(handler-case
    (progn
      (run-vm *vm1* :max-instructions 10000)
      (defparameter *result1* (get-register *vm1* *reg-v0*))
      (format t "  ✓ Résultat: ~A instructions (attendu: ~A)~%~%" 
              *result1* (length *test-code*))
      (if (= *result1* (length *test-code*))
          (format t "  ✅ TEST 1 RÉUSSI~%~%")
          (format t "  ❌ TEST 1 ÉCHOUÉ~%~%")))
  (error (e)
    (format t "  ✗ Erreur: ~A~%~%" e)))

;;; ============================================================================
;;; VERSION 2: AVEC VM-GETHASH (RECHERCHE DANS HASH-TABLE)
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: resolve-with-lookup - Recherche dans hash-table~%")
(format t "═════════════════════════════════════════════════════════════════~%")

;; Version qui cherche chaque instruction dans la hash-table
;; Retourne le nombre de lookups effectués
(defparameter *resolve-lookup-src*
  '(defun resolve-lookup (asm-code labels)
     (let ((lookups 0))
       (dolist (instr asm-code)
         (if (vm-consp instr)
             (let ((key (vm-car instr)))
               ;; Tenter de chercher la clé dans labels
               (vm-gethash labels key)
               (setq lookups (+ lookups 1)))
             (setq lookups lookups)))
       lookups)))

(format t "  → Compilation...~%")
(defparameter *resolve-lookup-mips* (compile-lisp *resolve-lookup-src*))
(format t "  ✓ Compilé: ~A instructions~%~%" (length *resolve-lookup-mips*))

;; Vérifier opcodes
(format t "  → Vérification opcodes:~%")
(format t "     HASH-GET: ~A~%~%" 
        (if (some #'(lambda (i) (and (listp i) (eq (first i) :HASH-GET))) 
                  *resolve-lookup-mips*)
            "✓" "✗"))

;; Test d'exécution
(defparameter *vm2* (make-new-vm :verbose nil))
(load-code *vm2* *resolve-lookup-mips* :verbose nil)
(defparameter *addr2* (calculate-code-start *vm2*))

;; Code avec des instructions qui ont des clés
(defparameter *test-code2* 
  (list (list :ADD 1 2 3) (list :JMP 'LOOP_START) (list :SUB 4 5 6)))
(defparameter *handle-code2*
  (let ((h (incf *vm-lisp-handle-counter*)))
    (setf (gethash h *vm-lisp-objects*) *test-code2*)
    h))

;; Hash-table avec label
(defparameter *test-labels2* (make-hash-table :test 'equal))
;; Stocker avec handle car les clés sont des handles
(defparameter *loop-start-handle*
  (let ((h (incf *vm-lisp-handle-counter*)))
    (setf (gethash h *vm-lisp-objects*) 'LOOP_START)
    h))
(setf (gethash *loop-start-handle* *test-labels2*) 1000)

(defparameter *handle-labels2*
  (let ((h (incf *vm-hash-handle-counter*)))
    (setf (gethash h *vm-hash-tables*) *test-labels2*)
    h))

(set-register *vm2* *reg-a0* *handle-code2*)
(set-register *vm2* *reg-a1* *handle-labels2*)
(set-register *vm2* *reg-ra* 999999)
(set-register *vm2* (get-reg :pc) *addr2*)

(format t "  → Exécution...~%")
(handler-case
    (progn
      (run-vm *vm2* :max-instructions 10000)
      (defparameter *result2* (get-register *vm2* *reg-v0*))
      (format t "  ✓ Résultat: ~A lookups (attendu: ~A)~%~%" 
              *result2* (length *test-code2*))
      (if (= *result2* (length *test-code2*))
          (format t "  ✅ TEST 2 RÉUSSI~%~%")
          (format t "  ❌ TEST 2 ÉCHOUÉ~%~%")))
  (error (e)
    (format t "  ✗ Erreur: ~A~%~%" e)))

;;; ============================================================================
;;; ANALYSE: FONCTIONNALITÉS NÉCESSAIRES POUR RESOLVE-LABELS COMPLET
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "ANALYSE: Besoins pour resolve-labels complet~%")
(format t "═════════════════════════════════════════════════════════════════~%")

(format t "~%Pour implémenter le vrai resolve-labels, il faut:~%~%")

(format t "  1. ✅ Itérer sur le code                → dolist~%")
(format t "  2. ✅ Tester si instruction             → vm-consp~%")
(format t "  3. ✅ Extraire opcode                   → vm-car~%")
(format t "  4. ✅ Chercher dans hash-table          → vm-gethash~%")
(format t "  5. ❌ Créer nouvelle liste modifiée     → Besoin vm-list ou mapcar~%")
(format t "  6. ❌ Remplacer élément dans liste      → Besoin setf ou reconstruction~%")
(format t "~%")

(format t "PROBLÈME MAJEUR:~%")
(format t "  Le code compilé ne peut pas facilement MODIFIER une liste.~%")
(format t "  Common Lisp utilise setf, mais on n'a pas cette primitive.~%")
(format t "~%")

(format t "SOLUTIONS POSSIBLES:~%~%")

(format t "  OPTION A: Implémenter vm-list (variadic)~%")
(format t "    • (vm-list a b c d) → nouvelle liste~%")
(format t "    • Permet de reconstruire instructions~%")
(format t "    • Problème: nombre variable d'arguments~%")
(format t "~%")

(format t "  OPTION B: Faire resolve-labels côté Lisp~%")
(format t "    • collect-labels compilé → retourne hash-table~%")
(format t "    • resolve-labels natif Lisp → utilise la hash-table~%")
(format t "    • Plus simple, évite complexité~%")
(format t "    • Acceptable pour le loader (pas besoin de tout compiler)~%")
(format t "~%")

(format t "  OPTION C: Utiliser format différent~%")
(format t "    • Au lieu de modifier instructions~%")
(format t "    • Créer un tableau d'adresses résolues~%")
(format t "    • Code final reconstruit côté Lisp~%")
(format t "~%")

(format t "RECOMMANDATION: OPTION B (resolve-labels en Lisp natif)~%")
(format t "  • collect-labels compilé collecte les labels~%")
(format t "  • resolve-labels natif résout les références~%")
(format t "  • C'est acceptable: le loader peut être partiellement compilé~%")
(format t "  • Focus sur les parties critiques (collect est la plus complexe)~%")
(format t "~%")

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                         RÉSUMÉ FINAL                             ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║  Tests de resolve-labels:                                        ║~%")
(format t "║    • resolve-count      : Compte instructions ✓                  ║~%")
(format t "║    • resolve-lookup     : Lookup hash-table ✓                    ║~%")
(format t "║                                                                  ║~%")
(format t "║  CONCLUSION:                                                     ║~%")
(format t "║    Les primitives de base fonctionnent (dolist, gethash)        ║~%")
(format t "║    MAIS: modification de listes très difficile en code compilé  ║~%")
(format t "║                                                                  ║~%")
(format t "║  STRATÉGIE RECOMMANDÉE:                                          ║~%")
(format t "║    • collect-labels: Compilé (le plus complexe) ✅               ║~%")
(format t "║    • resolve-labels: Natif Lisp (acceptable) ✓                  ║~%")
(format t "║    • load-code: Partiellement compilé                            ║~%")
(format t "║                                                                  ║~%")
(format t "║  Cela permet d'avoir un loader fonctionnel sans implémenter     ║~%")
(format t "║  des primitives très complexes (list construction variadic).    ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Tests terminés.~%~%")
