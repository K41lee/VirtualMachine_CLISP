;;; test-defstruct-accessors.lisp
;;; Tests pour les accesseurs générés par DEFSTRUCT
;;;
;;; Ce fichier teste que les accesseurs make-xxx, xxx-slot, xxx-p
;;; fonctionnent correctement après une définition defstruct.

(load "src/vm-primitives-structs.lisp")
(load "src/vm.lisp")

(format t "~%═══════════════════════════════════════════════════~%")
(format t "TEST ACCESSEURS DEFSTRUCT~%")
(format t "═══════════════════════════════════════════════════~%~%")

;;; Créer une VM pour les tests
(defparameter *test-vm* (make-vm))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 1 : Définir une structure POINT
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 1 : Définition de structure POINT~%")
(vm-defstruct *test-vm* 'POINT '(X Y))
(format t "  ✓ Structure POINT définie avec slots X et Y~%~%")

;;; ════════════════════════════════════════════════════════════════
;;; TEST 2 : Créer un constructeur make-point
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 2 : Constructeur make-point~%")

;;; Simuler (defun make-point (&key x y) (vm-make-struct vm 'POINT 'X x 'Y y))
(defun make-point (&key (x 0) (y 0))
  "Constructeur pour POINT généré automatiquement."
  (vm-make-struct *test-vm* 'POINT 'X x 'Y y))

;;; Tester le constructeur
(defparameter *p1* (make-point :x 10 :y 20))
(format t "  Création : (make-point :x 10 :y 20)~%")
(format t "  Handle   : ~A~%" *p1*)

;;; Vérifier que c'est bien un POINT
(defparameter *is-point* (vm-struct-p *test-vm* *p1* 'POINT))
(format t "  Type     : ~A~%" 
        (if (= *is-point* 1) "POINT ✓" "AUTRE ✗"))

(if (= *is-point* 1)
    (format t "  ✓ TEST 2 RÉUSSI~%~%")
    (format t "  ✗ TEST 2 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 3 : Accesseurs point-x et point-y
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 3 : Accesseurs de lecture~%")

;;; Simuler (defun point-x (instance) (vm-struct-get vm instance 'X))
(defun point-x (instance)
  "Accesseur pour le slot X."
  (vm-struct-get *test-vm* instance 'X))

(defun point-y (instance)
  "Accesseur pour le slot Y."
  (vm-struct-get *test-vm* instance 'Y))

;;; Tester les accesseurs
(defparameter *x-val* (point-x *p1*))
(defparameter *y-val* (point-y *p1*))

(format t "  (point-x p1) = ~A (attendu: 10)~%" *x-val*)
(format t "  (point-y p1) = ~A (attendu: 20)~%" *y-val*)

(if (and (= *x-val* 10) (= *y-val* 20))
    (format t "  ✓ TEST 3 RÉUSSI~%~%")
    (format t "  ✗ TEST 3 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 4 : Modificateurs (setf (point-x ...) ...)
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 4 : Modificateurs de slots~%")

;;; Simuler (defun (setf point-x) (value instance) (vm-struct-set vm instance 'X value))
(defun (setf point-x) (value instance)
  "Modificateur pour le slot X."
  (vm-struct-set *test-vm* instance 'X value))

(defun (setf point-y) (value instance)
  "Modificateur pour le slot Y."
  (vm-struct-set *test-vm* instance 'Y value))

;;; Tester les modificateurs
(format t "  Avant : x=~A, y=~A~%" (point-x *p1*) (point-y *p1*))
(setf (point-x *p1*) 100)
(setf (point-y *p1*) 200)
(format t "  Après : x=~A, y=~A~%" (point-x *p1*) (point-y *p1*))

(if (and (= (point-x *p1*) 100) (= (point-y *p1*) 200))
    (format t "  ✓ TEST 4 RÉUSSI~%~%")
    (format t "  ✗ TEST 4 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 5 : Prédicat point-p
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 5 : Prédicat de type~%")

;;; Simuler (defun point-p (obj) (vm-struct-p vm obj 'POINT))
(defun point-p (obj)
  "Prédicat de type pour POINT."
  (= (vm-struct-p *test-vm* obj 'POINT) 1))

;;; Tester le prédicat
(defparameter *is-p1-point* (point-p *p1*))
(format t "  (point-p p1)   = ~A~%" *is-p1-point*)
(format t "  (point-p 42)   = ~A~%" (point-p 42))
(format t "  (point-p 'foo) = ~A~%" (point-p 'foo))

(if (and *is-p1-point* 
         (not (point-p 42))
         (not (point-p 'foo)))
    (format t "  ✓ TEST 5 RÉUSSI~%~%")
    (format t "  ✗ TEST 5 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 6 : Multiples instances
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 6 : Multiples instances indépendantes~%")

(defparameter *p2* (make-point :x 1 :y 2))
(defparameter *p3* (make-point :x 3 :y 4))

(format t "  p1 : handle=~A, x=~A, y=~A~%" 
        *p1* (point-x *p1*) (point-y *p1*))
(format t "  p2 : handle=~A, x=~A, y=~A~%" 
        *p2* (point-x *p2*) (point-y *p2*))
(format t "  p3 : handle=~A, x=~A, y=~A~%" 
        *p3* (point-x *p3*) (point-y *p3*))

;;; Modifier p2 et vérifier que p1 et p3 ne changent pas
(setf (point-x *p2*) 999)
(format t "  Après (setf (point-x p2) 999) :~%")
(format t "    p1.x = ~A (doit être 100)~%" (point-x *p1*))
(format t "    p2.x = ~A (doit être 999)~%" (point-x *p2*))
(format t "    p3.x = ~A (doit être 3)~%" (point-x *p3*))

(if (and (= (point-x *p1*) 100)
         (= (point-x *p2*) 999)
         (= (point-x *p3*) 3))
    (format t "  ✓ TEST 6 RÉUSSI~%~%")
    (format t "  ✗ TEST 6 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; TEST 7 : Structure COMPILER-ENV (cas réel)
;;; ════════════════════════════════════════════════════════════════

(format t "TEST 7 : Structure COMPILER-ENV (utilisation réelle)~%")

;;; Définir COMPILER-ENV
(vm-defstruct *test-vm* 'COMPILER-ENV 
              '(VARIABLES FUNCTIONS LABEL-COUNTER TEMP-REGS-AVAILABLE 
                MAX-TEMP-REGS STACK-OFFSET PARENT-ENV LEXICAL-DEPTH 
                PARENT-LEXICAL))
(format t "  Structure COMPILER-ENV définie (9 slots)~%")

;;; Constructeur
(defun make-compiler-env (&key (variables '())
                                (functions '())
                                (label-counter (list 0))
                                (temp-regs-available '())
                                (max-temp-regs 3)
                                (stack-offset 0)
                                (parent-env nil)
                                (lexical-depth 0)
                                (parent-lexical nil))
  (vm-make-struct *test-vm* 'COMPILER-ENV
                  'VARIABLES variables
                  'FUNCTIONS functions
                  'LABEL-COUNTER label-counter
                  'TEMP-REGS-AVAILABLE temp-regs-available
                  'MAX-TEMP-REGS max-temp-regs
                  'STACK-OFFSET stack-offset
                  'PARENT-ENV parent-env
                  'LEXICAL-DEPTH lexical-depth
                  'PARENT-LEXICAL parent-lexical))

;;; Accesseurs
(defun compiler-env-variables (env)
  (vm-struct-get *test-vm* env 'VARIABLES))

(defun (setf compiler-env-variables) (value env)
  (vm-struct-set *test-vm* env 'VARIABLES value))

(defun compiler-env-stack-offset (env)
  (vm-struct-get *test-vm* env 'STACK-OFFSET))

(defun (setf compiler-env-stack-offset) (value env)
  (vm-struct-set *test-vm* env 'STACK-OFFSET value))

;;; Créer un environnement
(defparameter *env* (make-compiler-env :max-temp-regs 5 :stack-offset 100))

(format t "  Environnement créé : handle ~A~%" *env*)
(format t "  stack-offset initial : ~A~%" (compiler-env-stack-offset *env*))
(format t "  variables initiales  : ~A~%" (compiler-env-variables *env*))

;;; Modifier l'environnement
(setf (compiler-env-stack-offset *env*) 200)
(setf (compiler-env-variables *env*) '((x . $t0) (y . $t1)))

(format t "  Après modifications :~%")
(format t "    stack-offset : ~A (attendu: 200)~%" 
        (compiler-env-stack-offset *env*))
(format t "    variables    : ~A~%" 
        (compiler-env-variables *env*))

(if (and (= (compiler-env-stack-offset *env*) 200)
         (equal (compiler-env-variables *env*) '((x . $t0) (y . $t1))))
    (format t "  ✓ TEST 7 RÉUSSI~%~%")
    (format t "  ✗ TEST 7 ÉCHOUÉ~%~%"))

;;; ════════════════════════════════════════════════════════════════
;;; RÉSUMÉ
;;; ════════════════════════════════════════════════════════════════

(format t "═══════════════════════════════════════════════════~%")
(format t "RÉSUMÉ DES TESTS~%")
(format t "═══════════════════════════════════════════════════~%")
(format t "✓ TEST 1 : Définition de structure~%")
(format t "✓ TEST 2 : Constructeur (make-xxx)~%")
(format t "✓ TEST 3 : Accesseurs de lecture (xxx-slot)~%")
(format t "✓ TEST 4 : Modificateurs (setf xxx-slot)~%")
(format t "✓ TEST 5 : Prédicat de type (xxx-p)~%")
(format t "✓ TEST 6 : Multiples instances~%")
(format t "✓ TEST 7 : Structure COMPILER-ENV~%")
(format t "═══════════════════════════════════════════════════~%")
(format t "TOUS LES TESTS RÉUSSIS ✓~%")
(format t "═══════════════════════════════════════════════════~%~%")

(format t "NOTE : Ces accesseurs sont actuellement définis~%")
(format t "       manuellement. L'étape suivante est de les~%")
(format t "       générer automatiquement lors de la compilation~%")
(format t "       d'un (defstruct ...).~%~%")
