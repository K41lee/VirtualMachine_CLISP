;;; ============================================================================
;;; TEST COLLECT-LABELS - FORMAT RÉEL D'INSTRUCTIONS MIPS
;;; ============================================================================
;;;
;;; Ce test vérifie collect-labels avec le format réel utilisé par le loader:
;;;   Instructions: (list :ADD $T0 $T1 $T2)
;;;   Labels:       (list :LABEL 'LOOP_START)
;;;
;;; Le vrai collect-labels doit:
;;; 1. Détecter si l'instruction commence par :LABEL
;;; 2. Extraire le nom du label (second élément)
;;; 3. Associer label → position dans le hash-table
;;;
;;; Problème à résoudre: comparer keywords (:LABEL) nécessite eq, pas equal
;;;
;;; Commande: clisp test-collect-labels-real-format.lisp
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST COLLECT-LABELS - FORMAT RÉEL D'INSTRUCTIONS MIPS         ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

;;; ============================================================================
;;; APPROCHE 1: UTILISER EQUAL POUR :LABEL (DEVRAIT FONCTIONNER)
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: collect-labels-v2 avec equal pour comparer :LABEL~%")
(format t "═════════════════════════════════════════════════════════════════~%")

;; Version qui utilise equal au lieu de eq
;; equal fonctionne avec keywords dans Common Lisp
(defparameter *collect-labels-v2-src*
  '(defun collect-labels-v2 (asm-code code-start)
     (let ((labels (vm-make-hash-table :test 'equal))
           (position 0))
       (dolist (instr asm-code)
         (if (vm-consp instr)
             (let ((first-elem (vm-car instr)))
               ;; Problème: on ne peut pas faire (equal first-elem :LABEL)
               ;; car equal n'est pas une primitive VM
               ;; Pour l'instant, on stocke tout ce qui est cons
               (vm-hash-set labels first-elem (+ code-start position))
               (setq position (+ position 1)))
             (setq position (+ position 1))))
       labels)))

(format t "  → Compilation...~%")
(handler-case
    (progn
      (defparameter *collect-labels-v2-mips* (compile-lisp *collect-labels-v2-src*))
      (format t "  ✓ Compilé : ~A instructions~%~%" (length *collect-labels-v2-mips*)))
  (error (e)
    (format t "  ✗ ERREUR: ~A~%~%" e)
    (quit)))

;; Test d'exécution
(format t "  → Création de la VM...~%")
(defparameter *vm1* (make-new-vm :verbose nil))
(load-code *vm1* *collect-labels-v2-mips* :verbose nil)
(defparameter *func-addr1* (calculate-code-start *vm1*))
(format t "  ✓ Code chargé à l'adresse ~A~%~%" *func-addr1*)

;; Créer une liste d'instructions avec format réel
(format t "  → Création d'une liste d'instructions avec labels~%")
(format t "     Format: (:LABEL nom) et (:ADD reg1 reg2 reg3)~%~%")

(defparameter *test-instructions*
  (list
   (list :LABEL 'LOOP_START)
   (list :ADD 1 2 3)
   (list :SUB 4 5 6)
   (list :LABEL 'LOOP_END)
   (list :JR 7)))

(format t "  Instructions test:~%")
(dolist (instr *test-instructions*)
  (format t "    ~A~%" instr))
(format t "~%")

;; Enregistrer la liste
(defparameter *list-handle1* 
  (let ((handle (incf *vm-lisp-handle-counter*)))
    (setf (gethash handle *vm-lisp-objects*) *test-instructions*)
    handle))
(format t "  ✓ Liste enregistrée avec handle ~A~%~%" *list-handle1*)

;; Configurer et exécuter
(set-register *vm1* *reg-a0* *list-handle1*)
(set-register *vm1* *reg-a1* 1000)
(set-register *vm1* *reg-ra* 999999)
(set-register *vm1* (get-reg :pc) *func-addr1*)

(format t "  → Exécution...~%")
(handler-case
    (progn
      (run-vm *vm1* :max-instructions 10000)
      (defparameter *result1* (get-register *vm1* *reg-v0*))
      (format t "  ✓ Résultat: handle ~A~%~%" *result1*)
      
      ;; Récupérer le hash-table
      (defparameter *result-hash1* (gethash *result1* *vm-hash-tables*))
      (if *result-hash1*
          (progn
            (format t "  ✓ Hash-table récupéré: ~A entrées~%~%" 
                    (hash-table-count *result-hash1*))
            (format t "  → Contenu (clés stockées comme handles):~%")
            (maphash #'(lambda (k v)
                        (let ((key-obj (gethash k *vm-lisp-objects* k)))
                          (format t "     ~A (handle ~A) → ~A~%" key-obj k v)))
                     *result-hash1*)
            (format t "~%")
            
            ;; Vérifier qu'on a bien 5 entrées (une par instruction)
            (if (= (hash-table-count *result-hash1*) (length *test-instructions*))
                (format t "  ✅ TEST RÉUSSI: ~A entrées collectées~%~%" 
                        (hash-table-count *result-hash1*))
                (format t "  ⚠️  TEST PARTIEL: ~A entrées (attendu: ~A)~%~%" 
                        (hash-table-count *result-hash1*) 
                        (length *test-instructions*))))
          (format t "  ✗ Hash-table non trouvé~%~%")))
  (error (e)
    (format t "  ✗ ERREUR: ~A~%~%" e)))

;;; ============================================================================
;;; APPROCHE 2: VERSION SIMPLIFIÉE QUI COMPTE SEULEMENT LES :LABEL
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: count-labels - compte uniquement les :LABEL~%")
(format t "═════════════════════════════════════════════════════════════════~%")

;; Version qui compte juste les labels sans comparaison
;; Stratégie: si c'est un cons avec 2 éléments, c'est potentiellement un label
(defparameter *count-labels-src*
  '(defun count-labels (asm-code)
     (let ((count 0))
       (dolist (instr asm-code)
         (if (vm-consp instr)
             (setq count (+ count 1))
             (setq count count)))
       count)))

(format t "  → Compilation...~%")
(defparameter *count-labels-mips* (compile-lisp *count-labels-src*))
(format t "  ✓ Compilé : ~A instructions~%~%" (length *count-labels-mips*))

;; Test d'exécution
(defparameter *vm2* (make-new-vm :verbose nil))
(load-code *vm2* *count-labels-mips* :verbose nil)
(defparameter *func-addr2* (calculate-code-start *vm2*))

(defparameter *list-handle2* 
  (let ((handle (incf *vm-lisp-handle-counter*)))
    (setf (gethash handle *vm-lisp-objects*) *test-instructions*)
    handle))

(set-register *vm2* *reg-a0* *list-handle2*)
(set-register *vm2* *reg-ra* 999999)
(set-register *vm2* (get-reg :pc) *func-addr2*)

(format t "  → Exécution...~%")
(run-vm *vm2* :max-instructions 10000)
(defparameter *result2* (get-register *vm2* *reg-v0*))
(format t "  ✓ Résultat: ~A instructions cons détectées~%~%" *result2*)

(if (= *result2* (length *test-instructions*))
    (format t "  ✅ TEST RÉUSSI: ~A/~A instructions~%~%" 
            *result2* (length *test-instructions*))
    (format t "  ❌ TEST ÉCHOUÉ: ~A détectées, attendu ~A~%~%" 
            *result2* (length *test-instructions*)))

;;; ============================================================================
;;; DIAGNOSTIC: COMPARER LA STRUCTURE DES INSTRUCTIONS
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "DIAGNOSTIC: Structure des instructions test~%")
(format t "═════════════════════════════════════════════════════════════════~%")

(format t "~%  Instructions créées en Lisp:~%")
(dolist (instr *test-instructions*)
  (format t "    ~A~%" instr)
  (format t "      Type: ~A~%" (type-of instr))
  (format t "      Consp: ~A~%" (consp instr))
  (format t "      Car: ~A (type: ~A)~%" (car instr) (type-of (car instr)))
  (when (cdr instr)
    (format t "      Cdr: ~A (type: ~A)~%" (cdr instr) (type-of (cdr instr))))
  (format t "~%"))

;;; ============================================================================
;;; ANALYSE: QU'EST-CE QUI MANQUE?
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "ANALYSE: Fonctionnalités manquantes~%")
(format t "═════════════════════════════════════════════════════════════════~%")

(format t "~%Pour implémenter le vrai collect-labels, il faut pouvoir:~%~%")

(format t "  1. ✅ Créer une hash-table        → vm-make-hash-table~%")
(format t "  2. ✅ Itérer sur une liste        → dolist~%")
(format t "  3. ✅ Tester si cons               → vm-consp~%")
(format t "  4. ✅ Extraire le CAR              → vm-car~%")
(format t "  5. ✅ Stocker dans hash-table     → vm-hash-set~%")
(format t "  6. ❌ Comparer avec :LABEL        → Manque vm-equal ou vm-eq~%")
(format t "  7. ❌ Extraire 2ème élément       → Manque vm-cadr ou vm-second~%")
(format t "~%")

(format t "Solutions possibles:~%~%")

(format t "  OPTION A: Implémenter vm-equal~%")
(format t "    • Opcode :EQUAL pour comparer deux valeurs~%")
(format t "    • Déréférence handles automatiquement~%")
(format t "    • Peut comparer keywords, symboles, nombres~%")
(format t "~%")

(format t "  OPTION B: Implémenter vm-second (ou vm-cadr)~%")
(format t "    • Opcode :LIST-CADR ou utiliser (car (cdr ...))~%")
(format t "    • Permet d'extraire le nom du label~%")
(format t "    • Plus flexible pour manipuler listes~%")
(format t "~%")

(format t "  OPTION C: Stratégie alternative sans comparaison~%")
(format t "    • Stocker TOUS les premiers éléments de cons~%")
(format t "    • Filtrer les labels côté Lisp après compilation~%")
(format t "    • Moins élégant mais fonctionne~%")
(format t "~%")

(format t "RECOMMANDATION: Implémenter vm-equal (OPTION A)~%")
(format t "  C'est la primitive la plus utile et réutilisable.~%")
(format t "  Elle permettra aussi de comparer d'autres valeurs.~%~%")

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                         RÉSUMÉ FINAL                             ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║  Test 1: collect-labels-v2   : Stocke tous les CAR              ║~%")
(format t "║  Test 2: count-labels        : Compte tous les cons             ║~%")
(format t "║                                                                  ║~%")
(format t "║  CONCLUSION:                                                     ║~%")
(format t "║  • La structure fonctionne (dolist + hash-table + car)          ║~%")
(format t "║  • Il manque vm-equal pour filtrer les :LABEL                   ║~%")
(format t "║  • Il manque vm-second pour extraire le nom du label            ║~%")
(format t "║                                                                  ║~%")
(format t "║  PROCHAINE ÉTAPE:                                                ║~%")
(format t "║  → Implémenter vm-equal et vm-second                             ║~%")
(format t "║  → Recompiler collect-labels avec ces primitives                ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Tests terminés.~%~%")
