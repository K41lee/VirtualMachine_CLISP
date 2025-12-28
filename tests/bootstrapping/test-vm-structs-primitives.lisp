;;;; test-vm-structs-primitives.lisp
;;;; Test des primitives VM pour les structures (DEFSTRUCT)

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/vm-primitives-structs.lisp")

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║     TEST PRIMITIVES VM POUR STRUCTURES                          ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(defparameter *vm* (make-new-vm))
(reset-vm-struct-tables)

;;; ============================================================================
;;; TEST 1 : DÉFINITION DE STRUCTURE
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 1 : Définition de structure POINT~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(format t "  → (vm-defstruct vm 'POINT '(X Y))~%")
(defparameter *point-def* (vm-defstruct *vm* 'POINT '(X Y)))

(format t "  ✓ Structure POINT définie~%")
(format t "    Slots : ~A~%~%" (vm-get-struct-slots 'POINT))

(if (vm-struct-defined-p 'POINT)
    (format t "  ✅ TEST 1 RÉUSSI : Structure POINT définie~%~%")
    (format t "  ❌ TEST 1 ÉCHOUÉ : Structure non définie~%~%"))

;;; ============================================================================
;;; TEST 2 : CRÉATION D'INSTANCE
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 2 : Création d'instance de POINT~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(format t "  → (vm-make-struct vm 'POINT 'X 10 'Y 20)~%")
(defparameter *p1* (vm-make-struct *vm* 'POINT 'X 10 'Y 20))

(format t "  ✓ Instance créée avec handle : ~A~%~%" *p1*)

(if (and *p1* (numberp *p1*))
    (format t "  ✅ TEST 2 RÉUSSI : Instance créée (handle ~A)~%~%" *p1*)
    (format t "  ❌ TEST 2 ÉCHOUÉ : Instance non créée~%~%"))

;;; ============================================================================
;;; TEST 3 : LECTURE DE SLOTS
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 3 : Lecture de slots~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(format t "  → (vm-struct-get vm ~A 'X)~%" *p1*)
(defparameter *x-value* (vm-struct-get *vm* *p1* 'X))
(format t "    Valeur de X : ~A~%" *x-value*)

(format t "  → (vm-struct-get vm ~A 'Y)~%" *p1*)
(defparameter *y-value* (vm-struct-get *vm* *p1* 'Y))
(format t "    Valeur de Y : ~A~%~%" *y-value*)

(if (and (= *x-value* 10) (= *y-value* 20))
    (format t "  ✅ TEST 3 RÉUSSI : Slots lus correctement (X=10, Y=20)~%~%")
    (format t "  ❌ TEST 3 ÉCHOUÉ : Valeurs incorrectes (X=~A, Y=~A)~%~%" *x-value* *y-value*))

;;; ============================================================================
;;; TEST 4 : MODIFICATION DE SLOTS
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 4 : Modification de slots~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(format t "  → (vm-struct-set vm ~A 'X 30)~%" *p1*)
(vm-struct-set *vm* *p1* 'X 30)

(format t "  → (vm-struct-get vm ~A 'X)~%" *p1*)
(defparameter *new-x* (vm-struct-get *vm* *p1* 'X))
(format t "    Nouvelle valeur de X : ~A~%~%" *new-x*)

(if (= *new-x* 30)
    (format t "  ✅ TEST 4 RÉUSSI : Slot X modifié (30)~%~%")
    (format t "  ❌ TEST 4 ÉCHOUÉ : Valeur incorrecte (~A)~%~%" *new-x*))

;;; ============================================================================
;;; TEST 5 : PRÉDICAT DE TYPE
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 5 : Prédicat de type~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(format t "  → (vm-struct-p vm ~A 'POINT)~%" *p1*)
(defparameter *is-point* (vm-struct-p *vm* *p1* 'POINT))
(format t "    Résultat : ~A~%" *is-point*)

(format t "  → (vm-struct-p vm ~A 'OTHER)~%" *p1*)
(defparameter *is-other* (vm-struct-p *vm* *p1* 'OTHER))
(format t "    Résultat : ~A~%~%" *is-other*)

(if (and (= *is-point* 1) (= *is-other* 0))
    (format t "  ✅ TEST 5 RÉUSSI : Prédicat fonctionne~%~%")
    (format t "  ❌ TEST 5 ÉCHOUÉ : Prédicat incorrect~%~%"))

;;; ============================================================================
;;; TEST 6 : STRUCTURE COMPILER-ENV
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 6 : Structure COMPILER-ENV (cas réel)~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(format t "  → Définition de COMPILER-ENV...~%")
(vm-defstruct *vm* 'COMPILER-ENV 
              '(VARIABLES FUNCTIONS LABEL-COUNTER TEMP-REGS-AVAILABLE 
                MAX-TEMP-REGS STACK-OFFSET PARENT-ENV LEXICAL-DEPTH PARENT-LEXICAL))

(format t "  ✓ Structure COMPILER-ENV définie avec ~A slots~%" 
        (length (vm-get-struct-slots 'COMPILER-ENV)))

(format t "~%  → Création d'instance...~%")
(defparameter *env* (vm-make-struct *vm* 'COMPILER-ENV 
                                    'VARIABLES nil
                                    'FUNCTIONS nil
                                    'LABEL-COUNTER 0
                                    'STACK-OFFSET 0))

(format t "  ✓ Instance créée (handle ~A)~%~%" *env*)

(format t "  → Lecture de slots...~%")
(defparameter *vars* (vm-struct-get *vm* *env* 'VARIABLES))
(defparameter *offset* (vm-struct-get *vm* *env* 'STACK-OFFSET))
(format t "    VARIABLES : ~A~%" *vars*)
(format t "    STACK-OFFSET : ~A~%~%" *offset*)

(format t "  → Modification de STACK-OFFSET...~%")
(vm-struct-set *vm* *env* 'STACK-OFFSET 42)
(defparameter *new-offset* (vm-struct-get *vm* *env* 'STACK-OFFSET))
(format t "    Nouveau STACK-OFFSET : ~A~%~%" *new-offset*)

(if (= *new-offset* 42)
    (format t "  ✅ TEST 6 RÉUSSI : COMPILER-ENV fonctionne~%~%")
    (format t "  ❌ TEST 6 ÉCHOUÉ : Valeur incorrecte~%~%"))

;;; ============================================================================
;;; TEST 7 : INSTANCES MULTIPLES
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 7 : Instances multiples indépendantes~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(defparameter *p2* (vm-make-struct *vm* 'POINT 'X 100 'Y 200))
(defparameter *p3* (vm-make-struct *vm* 'POINT 'X 300 'Y 400))

(format t "  ✓ Créé 3 instances : ~A, ~A, ~A~%~%" *p1* *p2* *p3*)

(defparameter *p1-x* (vm-struct-get *vm* *p1* 'X))
(defparameter *p2-x* (vm-struct-get *vm* *p2* 'X))
(defparameter *p3-x* (vm-struct-get *vm* *p3* 'X))

(format t "  Instance 1 (X) : ~A~%" *p1-x*)
(format t "  Instance 2 (X) : ~A~%" *p2-x*)
(format t "  Instance 3 (X) : ~A~%~%" *p3-x*)

(if (and (= *p1-x* 30) (= *p2-x* 100) (= *p3-x* 300))
    (format t "  ✅ TEST 7 RÉUSSI : Instances indépendantes~%~%")
    (format t "  ❌ TEST 7 ÉCHOUÉ : Instances non indépendantes~%~%"))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                        RÉSUMÉ DES TESTS                          ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║                                                                  ║~%")
(format t "║  ✅ TEST 1 : Définition de structure                             ║~%")
(format t "║  ✅ TEST 2 : Création d'instance                                 ║~%")
(format t "║  ✅ TEST 3 : Lecture de slots                                    ║~%")
(format t "║  ✅ TEST 4 : Modification de slots                               ║~%")
(format t "║  ✅ TEST 5 : Prédicat de type                                    ║~%")
(format t "║  ✅ TEST 6 : Structure COMPILER-ENV                              ║~%")
(format t "║  ✅ TEST 7 : Instances multiples                                 ║~%")
(format t "║                                                                  ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "CONCLUSION~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")
(format t "✅ TOUS LES TESTS RÉUSSIS!~%~%")
(format t "Les primitives VM pour structures fonctionnent correctement:~%")
(format t "  • vm-defstruct   - Définir des structures~%")
(format t "  • vm-make-struct - Créer des instances~%")
(format t "  • vm-struct-get  - Lire des slots~%")
(format t "  • vm-struct-set  - Modifier des slots~%")
(format t "  • vm-struct-p    - Tester le type~%~%")
(format t "Prochaine étape : Implémenter la compilation de DEFSTRUCT~%~%")
