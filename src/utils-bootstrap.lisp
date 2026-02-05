;;;; ============================================================================
;;;; UTILS-BOOTSTRAP.LISP
;;;; Fonctions pour construire des expressions Lisp dans la VM
;;;; ============================================================================

(defvar *vm-next-handle* 1000
  "Prochain handle disponible pour les objets créés par build-expression-in-vm
   On commence à 1000 pour éviter les conflits avec les IDs de symboles")

(defun next-handle-for-vm ()
  "Génère un nouveau handle unique pour *vm-lisp-objects*"
  (prog1 *vm-next-handle*
    (incf *vm-next-handle*)))

(defun build-atom-in-vm (atom)
  "Construit un atome en mémoire de la VM
   - NIL → retourne 0  
   - Nombre → stocke dans *vm-lisp-objects* avec tag :NUMBER
   - Symbole → retourne l'ID du symbole (après INTERN si nécessaire)"
  (cond
    ((null atom) 0)
    ((numberp atom) 
     ;; Stocker les nombres dans *vm-lisp-objects* pour éviter les conflits avec les IDs
     (let ((handle (next-handle-for-vm)))
       (setf (gethash handle *vm-lisp-objects*) (cons :NUMBER atom))
       handle))
    ((symbolp atom)
     ;; Chercher si le symbole existe déjà dans la table
     (let ((existing-id (gethash (symbol-name atom) *vm-symbol-to-id*)))
       (if existing-id
           existing-id
           ;; Sinon, l'interner (crée un nouvel ID)
           (intern-symbol (symbol-name atom)))))))

(defun build-list-in-vm (list-expr)
  "Construit une liste en mémoire de la VM
   Retourne un handle vers la structure cons
   - Liste vide → 0 (NIL)
   - Liste non-vide → handle vers (car . cdr)"
  (if (null list-expr)
      0  ; NIL = 0
      (let* ((car-handle (build-expression-in-vm (car list-expr)))
             (cdr-handle (build-list-in-vm (cdr list-expr)))
             (cons-cell (cons car-handle cdr-handle))
             (handle (next-handle-for-vm)))
        ;; Stocker la cellule cons dans *vm-lisp-objects*
        (setf (gethash handle *vm-lisp-objects*) cons-cell)
        handle)))

(defun build-expression-in-vm (expr)
  "Construit une expression Lisp arbitraire en mémoire de la VM
   - NIL → 0
   - Atome (nombre/symbole) → valeur ou ID
   - Liste → handle vers structure cons récursive
   
   Exemples:
     42                    → 42
     'FIBO                 → ID du symbole FIBO
     '(+ 1 2)              → handle vers (ID_+ . (1 . (2 . 0)))
     '(if (< n 2) n ...)   → handle vers structure complète"
  (cond
    ((null expr) 0)
    ((atom expr) (build-atom-in-vm expr))
    ((listp expr) (build-list-in-vm expr))))

(defun read-expression-from-vm (handle &optional (max-depth 50))
  "Lit une expression depuis la mémoire de la VM (pour debug/validation)
   handle: Le handle de l'objet à lire
   max-depth: Protection contre les boucles infinies
   
   Retourne l'expression reconstruite avec symboles keywords"
  (if (<= max-depth 0)
      '|...|  ; Limite de profondeur atteinte
      (if (= handle 0)
          nil
          ;; Chercher dans *vm-lisp-objects*
          (let ((obj (gethash handle *vm-lisp-objects*)))
            (if obj
                (cond
                  ;; Nombre tagué
                  ((and (consp obj) (eq (car obj) :NUMBER))
                   (cdr obj))
                  ;; Cellule cons normale
                  ((consp obj)
                   (cons (read-expression-from-vm (car obj) (1- max-depth))
                         (read-expression-from-vm (cdr obj) (1- max-depth))))
                  ;; Atome autre
                  (t obj))
                ;; Pas dans la table → c'est un ID de symbole direct
                (let ((sym-name (symbol-name-from-id handle)))
                  (if sym-name
                      (intern sym-name :keyword)
                      handle)))))))

(defun verify-expression-construction (original-expr)
  "Test de round-trip: construit une expression puis la relit
   Retourne T si l'expression reconstruite correspond à l'originale
   (modulo la conversion symboles → keywords)"
  (let* ((handle (build-expression-in-vm original-expr))
         (reconstructed (read-expression-from-vm handle)))
    (format t "  Original:      ~A~%" original-expr)
    (format t "  Handle:        ~A~%" handle)
    (format t "  Reconstructed: ~A~%" reconstructed)
    ;; Comparer (avec conversion symboles → keywords pour l'original)
    (equal reconstructed (convert-symbols-to-keywords original-expr))))

(defun convert-symbols-to-keywords (expr)
  "Convertit tous les symboles d'une expression en keywords pour comparaison"
  (cond
    ((null expr) nil)
    ((symbolp expr) (intern (symbol-name expr) :keyword))
    ((listp expr) (mapcar #'convert-symbols-to-keywords expr))
    (t expr)))

;;; ============================================================================
;;; Fonctions helper pour des constructions spécifiques
;;; ============================================================================

(defun build-defun-in-vm (name params body)
  "Construit une expression DEFUN en mémoire
   Exemple: (build-defun-in-vm 'FIBO '(N) '(IF (< N 2) N ...))
   → handle vers '(DEFUN FIBO (N) (IF (< N 2) N ...))"
  (build-expression-in-vm `(defun ,name ,params ,body)))

(defun build-if-in-vm (test then-part else-part)
  "Construit une expression IF en mémoire"
  (build-expression-in-vm `(if ,test ,then-part ,else-part)))

(defun build-lambda-in-vm (params body)
  "Construit une expression LAMBDA en mémoire"
  (build-expression-in-vm `(lambda ,params ,body)))

;;; ============================================================================
;;; Debug et inspection
;;; ============================================================================

(defun count-vm-objects ()
  "Compte le nombre d'objets dans *vm-lisp-objects*"
  (hash-table-count *vm-lisp-objects*))

(defun list-vm-objects ()
  "Liste tous les handles et objets dans *vm-lisp-objects*"
  (format t "~%Objets dans *vm-lisp-objects*:~%")
  (maphash (lambda (handle obj)
             (format t "  Handle ~A: ~A~%" handle obj))
           *vm-lisp-objects*))

(defun clear-vm-objects ()
  "Vide *vm-lisp-objects* (pour recommencer à zéro dans les tests)"
  (clrhash *vm-lisp-objects*))

;;; ============================================================================
;;; Compilation depuis un handle
;;; ============================================================================

(defun convert-keywords-to-symbols (expr)
  "Convertit tous les keywords d'une expression en symboles normaux
   Nécessaire car read-expression-from-vm retourne des keywords
   mais le compilateur attend des symboles normaux"
  (cond
    ((null expr) nil)
    ((keywordp expr) (intern (symbol-name expr)))
    ((symbolp expr) expr)
    ((listp expr) (mapcar #'convert-keywords-to-symbols expr))
    (t expr)))

(defun compile-from-handle (handle)
  "Compile une expression Lisp stockée dans la VM via son handle
   
   VERSION SIMPLIFIÉE pour être compilée dans la VM:
   - Pas de format (pas d'I/O dans la VM)
   - Appelle directement compile-lisp-to-mips-simplified
   
   Étapes:
   1. Lit l'expression depuis *vm-lisp-objects* via le handle
   2. Convertit les keywords en symboles normaux (pour le compilateur)
   3. Appelle compile-lisp-to-mips-simplified
   4. Retourne le code MIPS généré
   
   Exemple:
   (setq h (build-expression-in-vm '(defun fibo (n) ...)))
   (setq code (compile-from-handle h))
   → code MIPS complet pour FIBO"
  
  ;; Lire l'expression depuis le handle
  (let ((expr-with-keywords (read-expression-from-vm handle)))
    ;; Convertir les keywords en symboles normaux
    (let ((expr (convert-keywords-to-symbols expr-with-keywords)))
      ;; Compiler l'expression
      (compile-lisp-to-mips-simplified expr))))

(defun compile-from-handle-with-handle-return (handle)
  "Version de compile-from-handle qui retourne un HANDLE vers le résultat
   au lieu de retourner directement la liste.
   
   Utilisé pour appeler depuis la VM car celle-ci ne peut retourner
   qu'un entier (handle) dans $V0, pas une liste complète.
   
   Retourne: handle vers la liste d'instructions MIPS dans *vm-lisp-objects*"
  
  (let ((code (compile-from-handle handle)))
    ;; Stocker le résultat dans *vm-lisp-objects* et retourner le handle
    (vm-store-lisp-object code)))

;;; ============================================================================
;;; Export des symboles (si utilisé comme module)
;;; ============================================================================

;; Fonctions principales
;; - build-expression-in-vm
;; - read-expression-from-vm
;; - verify-expression-construction
;; - build-defun-in-vm
;; - build-if-in-vm
;; - build-lambda-in-vm
;; - clear-vm-objects
;; - list-vm-objects
;; - count-vm-objects
;; - compile-from-handle
;; - compile-from-handle-with-handle-return
