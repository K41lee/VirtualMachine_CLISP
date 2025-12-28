;;; ═══════════════════════════════════════════════════════════════════
;;; ACCESSEURS POUR COMPILER-ENV (Délégation à VM)
;;; ═══════════════════════════════════════════════════════════════════
;;;
;;; Ce fichier fournit les accesseurs pour la structure COMPILER-ENV
;;; en utilisant la délégation à l'environnement Lisp via les primitives VM.
;;;
;;; PRÉREQUIS :
;;;   - vm-primitives-structs.lisp doit être chargé
;;;   - Une VM doit être disponible (variable *vm*)
;;;
;;; UTILISATION :
;;;   (load "src/vm-primitives-structs.lisp")
;;;   (load "src/vm.lisp")
;;;   (defparameter *vm* (make-vm))
;;;   (load "src/compiler-env-accessors.lisp")
;;;
;;;   ;; Définir la structure
;;;   (vm-defstruct *vm* 'COMPILER-ENV 
;;;                 '(VARIABLES FUNCTIONS LABEL-COUNTER 
;;;                   TEMP-REGS-AVAILABLE MAX-TEMP-REGS STACK-OFFSET 
;;;                   PARENT-ENV LEXICAL-DEPTH PARENT-LEXICAL))
;;;
;;;   ;; Créer une instance
;;;   (defparameter *env* (make-compiler-env))
;;;
;;;   ;; Utiliser les accesseurs
;;;   (setf (compiler-env-variables *env*) '((x . $t0)))
;;;   (compiler-env-variables *env*)  ; → '((X . $T0))
;;;
;;; ═══════════════════════════════════════════════════════════════════

;;; ───────────────────────────────────────────────────────────────────
;;; CONSTRUCTEUR
;;; ───────────────────────────────────────────────────────────────────

(defun make-compiler-env (&key (variables '())
                                (functions '())
                                (label-counter (list 0))
                                (temp-regs-available '())
                                (max-temp-regs 3)
                                (stack-offset 0)
                                (parent-env nil)
                                (lexical-depth 0)
                                (parent-lexical nil))
  "Crée une nouvelle instance de COMPILER-ENV.
   
   PARAMÈTRES :
     :variables - Liste des variables locales (var . registre/offset)
     :functions - Table des fonctions définies (nom . label)
     :label-counter - Compteur pour labels uniques (liste mutable)
     :temp-regs-available - Registres temporaires disponibles
     :max-temp-regs - Nombre maximum de registres temporaires (défaut: 3)
     :stack-offset - Offset courant pour variables sur la pile
     :parent-env - Environnement parent (pour portée lexicale)
     :lexical-depth - Profondeur d'imbrication lexicale (0=global)
     :parent-lexical - Référence vers environnement parent lexical
   
   RETOURNE :
     Handle (entier) vers l'instance créée."
  (vm-make-struct *vm* 'COMPILER-ENV
                  'VARIABLES variables
                  'FUNCTIONS functions
                  'LABEL-COUNTER label-counter
                  'TEMP-REGS-AVAILABLE temp-regs-available
                  'MAX-TEMP-REGS max-temp-regs
                  'STACK-OFFSET stack-offset
                  'PARENT-ENV parent-env
                  'LEXICAL-DEPTH lexical-depth
                  'PARENT-LEXICAL parent-lexical))

;;; ───────────────────────────────────────────────────────────────────
;;; PRÉDICAT DE TYPE
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-p (obj)
  "Teste si OBJ est une instance de COMPILER-ENV.
   
   PARAMÈTRES :
     obj - Objet à tester (normalement un handle)
   
   RETOURNE :
     T si obj est un COMPILER-ENV, NIL sinon."
  (and (integerp obj)
       (= (vm-struct-p *vm* obj 'COMPILER-ENV) 1)))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : VARIABLES
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-variables (env)
  "Retourne la liste des variables locales de l'environnement.
   Format : ((var . registre/offset) ...)
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Liste des variables."
  (vm-struct-get *vm* env 'VARIABLES))

(defun (setf compiler-env-variables) (value env)
  "Modifie la liste des variables locales.
   
   PARAMÈTRES :
     value - Nouvelle liste de variables
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'VARIABLES value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : FUNCTIONS
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-functions (env)
  "Retourne la table des fonctions définies.
   Format : ((fonction . label) ...)
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Liste des fonctions."
  (vm-struct-get *vm* env 'FUNCTIONS))

(defun (setf compiler-env-functions) (value env)
  "Modifie la table des fonctions.
   
   PARAMÈTRES :
     value - Nouvelle table de fonctions
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'FUNCTIONS value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : LABEL-COUNTER
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-label-counter (env)
  "Retourne le compteur de labels (liste mutable).
   Format : (compteur)
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Liste contenant le compteur."
  (vm-struct-get *vm* env 'LABEL-COUNTER))

(defun (setf compiler-env-label-counter) (value env)
  "Modifie le compteur de labels.
   
   PARAMÈTRES :
     value - Nouveau compteur (liste)
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'LABEL-COUNTER value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : TEMP-REGS-AVAILABLE
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-temp-regs-available (env)
  "Retourne la liste des registres temporaires disponibles.
   Format : ($t0 $t1 $t2 ...)
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Liste des registres disponibles."
  (vm-struct-get *vm* env 'TEMP-REGS-AVAILABLE))

(defun (setf compiler-env-temp-regs-available) (value env)
  "Modifie la liste des registres disponibles.
   
   PARAMÈTRES :
     value - Nouvelle liste de registres
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'TEMP-REGS-AVAILABLE value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : MAX-TEMP-REGS
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-max-temp-regs (env)
  "Retourne le nombre maximum de registres temporaires.
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Entier (généralement 3)."
  (vm-struct-get *vm* env 'MAX-TEMP-REGS))

(defun (setf compiler-env-max-temp-regs) (value env)
  "Modifie le nombre maximum de registres temporaires.
   
   PARAMÈTRES :
     value - Nouveau maximum
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'MAX-TEMP-REGS value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : STACK-OFFSET
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-stack-offset (env)
  "Retourne l'offset courant pour variables sur la pile.
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Entier (offset en octets)."
  (vm-struct-get *vm* env 'STACK-OFFSET))

(defun (setf compiler-env-stack-offset) (value env)
  "Modifie l'offset de pile.
   
   PARAMÈTRES :
     value - Nouvel offset
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'STACK-OFFSET value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : PARENT-ENV
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-parent-env (env)
  "Retourne l'environnement parent (pour portée lexicale).
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Handle vers le parent, ou NIL si aucun."
  (vm-struct-get *vm* env 'PARENT-ENV))

(defun (setf compiler-env-parent-env) (value env)
  "Modifie l'environnement parent.
   
   PARAMÈTRES :
     value - Handle du parent ou NIL
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'PARENT-ENV value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : LEXICAL-DEPTH
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-lexical-depth (env)
  "Retourne la profondeur d'imbrication lexicale.
   0 = global, 1 = niveau 1, etc.
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Entier (profondeur)."
  (vm-struct-get *vm* env 'LEXICAL-DEPTH))

(defun (setf compiler-env-lexical-depth) (value env)
  "Modifie la profondeur lexicale.
   
   PARAMÈTRES :
     value - Nouvelle profondeur
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'LEXICAL-DEPTH value))

;;; ───────────────────────────────────────────────────────────────────
;;; ACCESSEURS : PARENT-LEXICAL
;;; ───────────────────────────────────────────────────────────────────

(defun compiler-env-parent-lexical (env)
  "Retourne la référence vers l'environnement parent lexical.
   Utilisé pour les closures.
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     Handle vers le parent lexical, ou NIL."
  (vm-struct-get *vm* env 'PARENT-LEXICAL))

(defun (setf compiler-env-parent-lexical) (value env)
  "Modifie le parent lexical.
   
   PARAMÈTRES :
     value - Handle du parent lexical ou NIL
     env - Handle vers un COMPILER-ENV
   
   RETOURNE :
     La nouvelle valeur."
  (vm-struct-set *vm* env 'PARENT-LEXICAL value))

;;; ═══════════════════════════════════════════════════════════════════
;;; UTILITAIRES POUR DÉBOGAGE
;;; ═══════════════════════════════════════════════════════════════════

(defun print-compiler-env (env &optional (stream t))
  "Affiche un environnement de compilation de manière lisible.
   
   PARAMÈTRES :
     env - Handle vers un COMPILER-ENV
     stream - Flux de sortie (défaut: sortie standard)
   
   RETOURNE :
     NIL"
  (format stream "~%COMPILER-ENV ~A:~%" env)
  (format stream "  Variables         : ~A~%" (compiler-env-variables env))
  (format stream "  Functions         : ~A~%" (compiler-env-functions env))
  (format stream "  Label counter     : ~A~%" (compiler-env-label-counter env))
  (format stream "  Temp regs avail   : ~A~%" (compiler-env-temp-regs-available env))
  (format stream "  Max temp regs     : ~A~%" (compiler-env-max-temp-regs env))
  (format stream "  Stack offset      : ~A~%" (compiler-env-stack-offset env))
  (format stream "  Parent env        : ~A~%" (compiler-env-parent-env env))
  (format stream "  Lexical depth     : ~A~%" (compiler-env-lexical-depth env))
  (format stream "  Parent lexical    : ~A~%" (compiler-env-parent-lexical env))
  nil)

;;; ═══════════════════════════════════════════════════════════════════
;;; FIN DES ACCESSEURS
;;; ═══════════════════════════════════════════════════════════════════

(format t "~%Accesseurs COMPILER-ENV chargés.~%")
(format t "Fonctions disponibles :~%")
(format t "  - (make-compiler-env &key ...) : Créer un environnement~%")
(format t "  - (compiler-env-p obj) : Tester le type~%")
(format t "  - (compiler-env-SLOT env) : Lire un slot~%")
(format t "  - (setf (compiler-env-SLOT env) value) : Modifier un slot~%")
(format t "  - (print-compiler-env env) : Afficher l'environnement~%")
(format t "~%")
