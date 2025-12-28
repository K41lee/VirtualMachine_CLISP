;;;; vm-primitives-structs.lisp
;;;; Primitives VM pour les structures de données (DEFSTRUCT)
;;;;
;;;; APPROCHE: Délégation à Lisp
;;;; Les structures sont stockées dans une hash-table globale *vm-structs*
;;;; Chaque structure est un vecteur [type-name slot1 slot2 ...]

;;; ============================================================================
;;; STOCKAGE GLOBAL DES STRUCTURES
;;; ============================================================================

(defparameter *vm-struct-definitions* (make-hash-table :test 'eq)
  "Table des définitions de structures : nom → (liste-slots)")

(defparameter *vm-struct-instances* (make-hash-table :test 'eql)
  "Table des instances de structures : handle → instance-vecteur")

(defparameter *vm-struct-handle-counter* 1000
  "Compteur pour générer des handles uniques pour les instances")

(defun reset-vm-struct-tables ()
  "Réinitialise les tables de structures (pour tests)"
  (clrhash *vm-struct-definitions*)
  (clrhash *vm-struct-instances*)
  (setf *vm-struct-handle-counter* 1000))

;;; ============================================================================
;;; DÉFINITION DE STRUCTURES
;;; ============================================================================

(defun vm-defstruct (vm struct-name slot-names)
  "Définit une nouvelle structure avec les slots donnés.
   Paramètres:
     - struct-name : symbole (ex: COMPILER-ENV)
     - slot-names  : liste de symboles (ex: (VARIABLES FUNCTIONS LABEL-COUNTER))
   Retourne: le nom de la structure"
  (setf (gethash struct-name *vm-struct-definitions*) slot-names)
  
  ;; Retourner le nom de la structure dans $V0
  ;; On stocke juste le symbole comme un nombre (son adresse symbolique)
  (set-register vm (get-reg :v0) (sxhash struct-name))
  struct-name)

(defun vm-struct-defined-p (struct-name)
  "Vérifie si une structure est définie"
  (not (null (gethash struct-name *vm-struct-definitions*))))

(defun vm-get-struct-slots (struct-name)
  "Retourne la liste des slots d'une structure"
  (gethash struct-name *vm-struct-definitions*))

;;; ============================================================================
;;; CRÉATION D'INSTANCES
;;; ============================================================================

(defun vm-make-struct (vm struct-name &rest initial-values)
  "Crée une instance de structure.
   Paramètres:
     - struct-name : symbole de la structure
     - initial-values : valeurs initiales pour les slots (plist ou liste)
   Retourne: un handle (entier) vers l'instance"
  (let ((slot-names (gethash struct-name *vm-struct-definitions*)))
    (unless slot-names
      (error "Structure non définie: ~A" struct-name))
    
    ;; Créer un vecteur [type-name slot1-value slot2-value ...]
    (let* ((n-slots (length slot-names))
           (instance (make-array (+ 1 n-slots) :initial-element nil)))
      
      ;; Slot 0 = nom de la structure
      (setf (aref instance 0) struct-name)
      
      ;; Initialiser les slots avec valeurs par défaut (nil)
      (loop for i from 1 to n-slots do
        (setf (aref instance i) nil))
      
      ;; Appliquer les valeurs initiales si fournies (plist)
      (when initial-values
        (loop for (slot-name value) on initial-values by #'cddr do
          (let ((slot-index (position slot-name slot-names)))
            (when slot-index
              (setf (aref instance (+ 1 slot-index)) value)))))
      
      ;; Allouer un handle et stocker l'instance
      (let ((handle *vm-struct-handle-counter*))
        (incf *vm-struct-handle-counter*)
        (setf (gethash handle *vm-struct-instances*) instance)
        
        ;; Retourner le handle dans $V0
        (set-register vm (get-reg :v0) handle)
        handle))))

;;; ============================================================================
;;; ACCESSEURS DE SLOTS
;;; ============================================================================

(defun vm-struct-get (vm handle slot-name)
  "Lit la valeur d'un slot d'une structure.
   Paramètres:
     - handle : handle de l'instance
     - slot-name : nom du slot
   Retourne: la valeur du slot"
  (let ((instance (gethash handle *vm-struct-instances*)))
    (unless instance
      (error "Instance de structure invalide: handle ~A" handle))
    
    (let* ((struct-name (aref instance 0))
           (slot-names (gethash struct-name *vm-struct-definitions*))
           (slot-index (position slot-name slot-names)))
      
      (unless slot-index
        (error "Slot ~A non trouvé dans structure ~A" slot-name struct-name))
      
      (let ((value (aref instance (+ 1 slot-index))))
        ;; Retourner la valeur dans $V0
        ;; Si la valeur est un handle/nombre, on la retourne directement
        ;; Si c'est une liste, on retourne son handle
        (set-register vm (get-reg :v0) 
                     (if (numberp value) value
                         (if (null value) 0  ; nil → 0
                             value)))
        value))))

(defun vm-struct-set (vm handle slot-name value)
  "Modifie la valeur d'un slot d'une structure.
   Paramètres:
     - handle : handle de l'instance
     - slot-name : nom du slot
     - value : nouvelle valeur
   Retourne: la nouvelle valeur"
  (let ((instance (gethash handle *vm-struct-instances*)))
    (unless instance
      (error "Instance de structure invalide: handle ~A" handle))
    
    (let* ((struct-name (aref instance 0))
           (slot-names (gethash struct-name *vm-struct-definitions*))
           (slot-index (position slot-name slot-names)))
      
      (unless slot-index
        (error "Slot ~A non trouvé dans structure ~A" slot-name struct-name))
      
      ;; Modifier le slot
      (setf (aref instance (+ 1 slot-index)) value)
      
      ;; Retourner la valeur dans $V0
      (set-register vm (get-reg :v0) 
                   (if (numberp value) value
                       (if (null value) 0 value)))
      value)))

;;; ============================================================================
;;; PRÉDICATS ET UTILITAIRES
;;; ============================================================================

(defun vm-struct-p (vm handle struct-name)
  "Vérifie si un handle est une instance d'une structure donnée.
   Retourne 1 si vrai, 0 si faux"
  (let ((instance (gethash handle *vm-struct-instances*)))
    (let ((result (if (and instance 
                          (eq (aref instance 0) struct-name))
                     1 0)))
      (set-register vm (get-reg :v0) result)
      result)))

(defun vm-struct-type (vm handle)
  "Retourne le nom de la structure d'une instance.
   Retourne 0 si l'instance est invalide"
  (let ((instance (gethash handle *vm-struct-instances*)))
    (if instance
        (let ((type-name (aref instance 0)))
          (set-register vm (get-reg :v0) (sxhash type-name))
          type-name)
        (progn
          (set-register vm (get-reg :v0) 0)
          nil))))

;;; ============================================================================
;;; EXEMPLE D'UTILISATION
;;; ============================================================================

#|

;; Définir une structure
(vm-defstruct vm 'POINT '(X Y))

;; Créer une instance
(defparameter *p1* (vm-make-struct vm 'POINT 'X 10 'Y 20))
;; → 1000 (handle)

;; Lire un slot
(vm-struct-get vm *p1* 'X)  ; → 10
(vm-struct-get vm *p1* 'Y)  ; → 20

;; Modifier un slot
(vm-struct-set vm *p1* 'X 30)
(vm-struct-get vm *p1* 'X)  ; → 30

;; Vérifier le type
(vm-struct-p vm *p1* 'POINT)  ; → 1 (vrai)
(vm-struct-p vm *p1* 'OTHER)  ; → 0 (faux)

|#

(format t "~%Primitives VM pour structures chargées.~%")
(format t "Fonctions disponibles:~%")
(format t "  - (vm-defstruct vm name slots) : Définir une structure~%")
(format t "  - (vm-make-struct vm name ...) : Créer une instance~%")
(format t "  - (vm-struct-get vm handle slot) : Lire un slot~%")
(format t "  - (vm-struct-set vm handle slot value) : Modifier un slot~%")
(format t "  - (vm-struct-p vm handle name) : Tester le type~%~%")
