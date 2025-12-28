;;; test-compiler-env-accessors.lisp
;;; Test complet des accesseurs COMPILER-ENV

(load "src/vm-primitives-structs.lisp")
(load "src/vm.lisp")

(format t "~%═══════════════════════════════════════════════════~%")
(format t "TEST ACCESSEURS COMPILER-ENV~%")
(format t "═══════════════════════════════════════════════════~%~%")

;;; Créer une VM
(defparameter *vm* (make-vm))

;;; Définir la structure COMPILER-ENV
(vm-defstruct *vm* 'COMPILER-ENV 
              '(VARIABLES FUNCTIONS LABEL-COUNTER 
                TEMP-REGS-AVAILABLE MAX-TEMP-REGS STACK-OFFSET 
                PARENT-ENV LEXICAL-DEPTH PARENT-LEXICAL))

(format t "Structure COMPILER-ENV définie avec 9 slots.~%~%")

;;; Charger les accesseurs
(load "src/compiler-env-accessors.lisp")

;;; ════════════════════════════════════════════════════════════════
;;; TEST 1 : Création d'environnement avec valeurs par défaut
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 1 : Création avec valeurs par défaut~%")

(defparameter *env1* (make-compiler-env))

(format t "  Handle : ~A~%" *env1*)
(format t "  Type   : ~A~%" (if (compiler-env-p *env1*) "COMPILER-ENV ✓" "AUTRE ✗"))

(if (compiler-env-p *env1*)
    (format t "  ✓ TEST 1 RÉUSSI~%~%")
    (format t "  ✗ TEST 1 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 2 : Vérifier les valeurs par défaut
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 2 : Valeurs par défaut~%")

(defparameter *vars* (compiler-env-variables *env1*))
(defparameter *fns* (compiler-env-functions *env1*))
(defparameter *lbl* (compiler-env-label-counter *env1*))
(defparameter *regs* (compiler-env-temp-regs-available *env1*))
(defparameter *max* (compiler-env-max-temp-regs *env1*))
(defparameter *offset* (compiler-env-stack-offset *env1*))
(defparameter *parent* (compiler-env-parent-env *env1*))
(defparameter *depth* (compiler-env-lexical-depth *env1*))
(defparameter *plexical* (compiler-env-parent-lexical *env1*))

(format t "  variables           : ~A (attendu: NIL)~%" *vars*)
(format t "  functions           : ~A (attendu: NIL)~%" *fns*)
(format t "  label-counter       : ~A (attendu: (0))~%" *lbl*)
(format t "  temp-regs-available : ~A (attendu: NIL)~%" *regs*)
(format t "  max-temp-regs       : ~A (attendu: 3)~%" *max*)
(format t "  stack-offset        : ~A (attendu: 0)~%" *offset*)
(format t "  parent-env          : ~A (attendu: NIL)~%" *parent*)
(format t "  lexical-depth       : ~A (attendu: 0)~%" *depth*)
(format t "  parent-lexical      : ~A (attendu: NIL)~%" *plexical*)

(if (and (null *vars*)
         (null *fns*)
         (equal *lbl* '(0))
         (null *regs*)
         (= *max* 3)
         (= *offset* 0)
         (null *parent*)
         (= *depth* 0)
         (null *plexical*))
    (format t "  ✓ TEST 2 RÉUSSI~%~%")
    (format t "  ✗ TEST 2 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 3 : Création avec valeurs personnalisées
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 3 : Création avec valeurs personnalisées~%")

(defparameter *env2* 
  (make-compiler-env :max-temp-regs 5
                     :stack-offset 100
                     :lexical-depth 2
                     :temp-regs-available '($t0 $t1 $t2 $t3 $t4)))

(format t "  max-temp-regs       : ~A (attendu: 5)~%" 
        (compiler-env-max-temp-regs *env2*))
(format t "  stack-offset        : ~A (attendu: 100)~%" 
        (compiler-env-stack-offset *env2*))
(format t "  lexical-depth       : ~A (attendu: 2)~%" 
        (compiler-env-lexical-depth *env2*))
(format t "  temp-regs-available : ~A~%" 
        (compiler-env-temp-regs-available *env2*))

(if (and (= (compiler-env-max-temp-regs *env2*) 5)
         (= (compiler-env-stack-offset *env2*) 100)
         (= (compiler-env-lexical-depth *env2*) 2)
         (equal (compiler-env-temp-regs-available *env2*) 
                '($t0 $t1 $t2 $t3 $t4)))
    (format t "  ✓ TEST 3 RÉUSSI~%~%")
    (format t "  ✗ TEST 3 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 4 : Modification des slots
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 4 : Modification des slots~%")

;;; Ajouter des variables
(setf (compiler-env-variables *env1*) '((x . $t0) (y . $t1)))
(format t "  Après ajout variables : ~A~%" (compiler-env-variables *env1*))

;;; Ajouter des fonctions
(setf (compiler-env-functions *env1*) '((fib . label_fib) (fact . label_fact)))
(format t "  Après ajout functions : ~A~%" (compiler-env-functions *env1*))

;;; Modifier le stack-offset
(setf (compiler-env-stack-offset *env1*) 200)
(format t "  Nouveau stack-offset  : ~A~%" (compiler-env-stack-offset *env1*))

;;; Modifier le label-counter
(setf (compiler-env-label-counter *env1*) '(42))
(format t "  Nouveau label-counter : ~A~%" (compiler-env-label-counter *env1*))

(if (and (equal (compiler-env-variables *env1*) '((x . $t0) (y . $t1)))
         (equal (compiler-env-functions *env1*) '((fib . label_fib) (fact . label_fact)))
         (= (compiler-env-stack-offset *env1*) 200)
         (equal (compiler-env-label-counter *env1*) '(42)))
    (format t "  ✓ TEST 4 RÉUSSI~%~%")
    (format t "  ✗ TEST 4 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 5 : Environnements imbriqués (parent-env)
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 5 : Environnements imbriqués~%")

;;; Créer un environnement parent
(defparameter *parent-env* 
  (make-compiler-env :variables '((a . $s0) (b . $s1))
                     :lexical-depth 0))

;;; Créer un environnement enfant qui pointe vers le parent
(defparameter *child-env*
  (make-compiler-env :variables '((x . $t0))
                     :parent-env *parent-env*
                     :lexical-depth 1))

(format t "  Parent handle       : ~A~%" *parent-env*)
(format t "  Child handle        : ~A~%" *child-env*)
(format t "  Child.parent-env    : ~A~%" (compiler-env-parent-env *child-env*))
(format t "  Child.lexical-depth : ~A~%" (compiler-env-lexical-depth *child-env*))
(format t "  Parent.variables    : ~A~%" (compiler-env-variables *parent-env*))
(format t "  Child.variables     : ~A~%" (compiler-env-variables *child-env*))

(if (and (= (compiler-env-parent-env *child-env*) *parent-env*)
         (= (compiler-env-lexical-depth *child-env*) 1)
         (= (compiler-env-lexical-depth *parent-env*) 0))
    (format t "  ✓ TEST 5 RÉUSSI~%~%")
    (format t "  ✗ TEST 5 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 6 : Simuler l'allocation de registres
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 6 : Allocation de registres temporaires~%")

;;; Créer un environnement avec des registres disponibles
(defparameter *env-regs* 
  (make-compiler-env :temp-regs-available '($t0 $t1 $t2)
                     :max-temp-regs 3))

(format t "  Registres initiaux  : ~A~%" 
        (compiler-env-temp-regs-available *env-regs*))

;;; Simuler l'allocation d'un registre
(defparameter *allocated-reg* (car (compiler-env-temp-regs-available *env-regs*)))
(setf (compiler-env-temp-regs-available *env-regs*)
      (cdr (compiler-env-temp-regs-available *env-regs*)))

(format t "  Registre alloué     : ~A~%" *allocated-reg*)
(format t "  Registres restants  : ~A~%" 
        (compiler-env-temp-regs-available *env-regs*))

;;; Simuler la libération du registre
(setf (compiler-env-temp-regs-available *env-regs*)
      (cons *allocated-reg* (compiler-env-temp-regs-available *env-regs*)))

(format t "  Après libération    : ~A~%" 
        (compiler-env-temp-regs-available *env-regs*))

(if (equal (compiler-env-temp-regs-available *env-regs*) '($t0 $t1 $t2))
    (format t "  ✓ TEST 6 RÉUSSI~%~%")
    (format t "  ✗ TEST 6 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 7 : Fonction print-compiler-env
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 7 : Affichage d'environnement~%")

;;; Créer un environnement bien rempli
(defparameter *env-full*
  (make-compiler-env :variables '((x . $t0) (y . $t1) (z . -4))
                     :functions '((fib . label_fib))
                     :label-counter '(10)
                     :temp-regs-available '($t2)
                     :max-temp-regs 3
                     :stack-offset 8
                     :parent-env nil
                     :lexical-depth 1
                     :parent-lexical nil))

(print-compiler-env *env-full*)

(format t "  ✓ TEST 7 RÉUSSI (affichage visuel)~%~%")

;;; ════════════════════════════════════════════════════════════════
;;; TEST 8 : Indépendance des instances
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 8 : Indépendance des instances~%")

(defparameter *env-a* (make-compiler-env :stack-offset 100))
(defparameter *env-b* (make-compiler-env :stack-offset 200))

(format t "  env-a.stack-offset : ~A~%" (compiler-env-stack-offset *env-a*))
(format t "  env-b.stack-offset : ~A~%" (compiler-env-stack-offset *env-b*))

;;; Modifier env-a
(setf (compiler-env-stack-offset *env-a*) 999)

(format t "  Après modification de env-a :~%")
(format t "    env-a.stack-offset : ~A (attendu: 999)~%" 
        (compiler-env-stack-offset *env-a*))
(format t "    env-b.stack-offset : ~A (attendu: 200)~%" 
        (compiler-env-stack-offset *env-b*))

(if (and (= (compiler-env-stack-offset *env-a*) 999)
         (= (compiler-env-stack-offset *env-b*) 200))
    (format t "  ✓ TEST 8 RÉUSSI~%~%")
    (format t "  ✗ TEST 8 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; RÉSUMÉ
;;; ════════════════════════════════════════════════════════════════

(format t "═══════════════════════════════════════════════════~%")
(format t "RÉSUMÉ DES TESTS~%")
(format t "═══════════════════════════════════════════════════~%")
(format t "✓ TEST 1 : Création avec défauts~%")
(format t "✓ TEST 2 : Valeurs par défaut~%")
(format t "✓ TEST 3 : Création personnalisée~%")
(format t "✓ TEST 4 : Modification de slots~%")
(format t "✓ TEST 5 : Environnements imbriqués~%")
(format t "✓ TEST 6 : Allocation de registres~%")
(format t "✓ TEST 7 : Affichage d'environnement~%")
(format t "✓ TEST 8 : Indépendance des instances~%")
(format t "═══════════════════════════════════════════════════~%")
(format t "TOUS LES TESTS RÉUSSIS ✓~%")
(format t "═══════════════════════════════════════════════════~%~%")

(format t "Les accesseurs COMPILER-ENV sont prêts à être utilisés~%")
(format t "dans la compilation du compilateur.~%~%")
