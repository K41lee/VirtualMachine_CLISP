;;;; heap-manager.lisp
;;;; Gestionnaire de tas (heap) pour la VM MIPS
;;;; Permet d'allouer des cons cells et de stocker des listes en mémoire

;;; ============================================================================
;;; CONFIGURATION DU HEAP
;;; ============================================================================

(defconstant +heap-start+ 10000000
  "Adresse de début du heap (10 Mo)")

(defconstant +heap-size+ 500000
  "Taille du heap en mots (500k mots = 2 Mo)")

(defconstant +cons-cell-size+ 2
  "Taille d'une cons cell en mots (CAR + CDR)")

;;; ============================================================================
;;; STRUCTURE HEAP
;;; ============================================================================

(defstruct heap
  "Structure représentant le heap de la VM"
  (memory (make-array +heap-size+ :initial-element 0))
  (next-free +heap-start+)  ; Prochaine adresse libre
  (allocated 0))            ; Nombre de mots alloués

;;; ============================================================================
;;; ALLOCATION DE MÉMOIRE
;;; ============================================================================

(defun heap-alloc (heap words)
  "Alloue 'words' mots dans le heap et retourne l'adresse.
   Utilise un bump allocator simple (pas de libération)."
  (let ((addr (heap-next-free heap)))
    (when (>= (+ addr words) (+ +heap-start+ +heap-size+))
      (error "Heap overflow: pas assez de mémoire (alloué: ~A mots)" 
             (heap-allocated heap)))
    
    ;; Mettre à jour le pointeur de mémoire libre
    (setf (heap-next-free heap) (+ addr words))
    (incf (heap-allocated heap) words)
    
    ;; Retourner l'adresse allouée
    addr))

(defun heap-store (heap addr value)
  "Stocke une valeur à l'adresse donnée dans le heap"
  (let ((offset (- addr +heap-start+)))
    (when (or (< offset 0) (>= offset +heap-size+))
      (error "Adresse hors limites du heap: ~A" addr))
    (setf (aref (heap-memory heap) offset) value)))

(defun heap-load (heap addr)
  "Charge une valeur depuis l'adresse donnée dans le heap"
  (let ((offset (- addr +heap-start+)))
    (when (or (< offset 0) (>= offset +heap-size+))
      (error "Adresse hors limites du heap: ~A" addr))
    (aref (heap-memory heap) offset)))

;;; ============================================================================
;;; ALLOCATION DE CONS CELLS
;;; ============================================================================

(defun heap-cons (heap car-val cdr-val)
  "Alloue une cons cell avec les valeurs CAR et CDR données.
   Retourne l'adresse de la cons cell."
  (let ((addr (heap-alloc heap +cons-cell-size+)))
    ;; Stocker CAR à offset 0
    (heap-store heap addr car-val)
    ;; Stocker CDR à offset 1
    (heap-store heap (+ addr 1) cdr-val)
    ;; Retourner l'adresse
    addr))

(defun heap-car (heap cons-addr)
  "Retourne le CAR de la cons cell à l'adresse donnée"
  (if (= cons-addr 0)
      0  ; NIL
      (heap-load heap cons-addr)))

(defun heap-cdr (heap cons-addr)
  "Retourne le CDR de la cons cell à l'adresse donnée"
  (if (= cons-addr 0)
      0  ; NIL
      (heap-load heap (+ cons-addr 1))))

;;; ============================================================================
;;; CONSTRUCTION DE LISTES À PARTIR DE S-EXPRESSIONS
;;; ============================================================================

(defun build-list-in-heap (heap lisp-list)
  "Construit une liste Lisp dans le heap et retourne son adresse.
   Convertit une S-expression en une chaîne de cons cells."
  (cond
    ;; Liste vide → NIL (0)
    ((null lisp-list) 0)
    
    ;; Atome (nombre ou symbole) → stocker comme valeur directe
    ((atom lisp-list)
     (cond
       ;; Nombre : retourner tel quel
       ((numberp lisp-list) lisp-list)
       
       ;; Symbole : pour l'instant, utiliser un hash code simple
       ;; (dans une vraie implémentation, il faudrait une table de symboles)
       ((symbolp lisp-list)
        (let ((name (symbol-name lisp-list)))
          ;; Hash simple : somme des codes ASCII
          (let ((hash 0))
            (loop for char across name do
              (setf hash (mod (+ (* hash 31) (char-code char)) 16777216)))
            hash)))
       
       (t lisp-list)))
    
    ;; Liste (cons) : construire récursivement
    (t
     (let* ((car-val (build-list-in-heap heap (car lisp-list)))
            (cdr-val (build-list-in-heap heap (cdr lisp-list))))
       (heap-cons heap car-val cdr-val)))))

;;; ============================================================================
;;; AFFICHAGE (DEBUG)
;;; ============================================================================

(defun print-heap-list (heap addr &optional (max-depth 20))
  "Affiche une liste stockée dans le heap (pour debug)"
  (cond
    ((= addr 0) (format t "NIL"))
    ((= max-depth 0) (format t "..."))
    (t
     (let ((car-val (heap-car heap addr))
           (cdr-val (heap-cdr heap addr)))
       (format t "(")
       (if (>= car-val +heap-start+)
           (print-heap-list heap car-val (- max-depth 1))
           (format t "~A" car-val))
       (cond
         ((= cdr-val 0) (format t ")"))
         ((>= cdr-val +heap-start+)
          (format t " ")
          (print-heap-list heap cdr-val (- max-depth 1)))
         (t
          (format t " . ~A)" cdr-val)))))))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(make-heap
          heap-alloc
          heap-store
          heap-load
          heap-cons
          heap-car
          heap-cdr
          build-list-in-heap
          print-heap-list
          +heap-start+
          +heap-size+))

(format t "~%Gestionnaire de heap chargé.~%")
(format t "Heap : ~A mots (~,1F Mo)~%" 
        +heap-size+ 
        (/ (* +heap-size+ 4) 1048576.0))
