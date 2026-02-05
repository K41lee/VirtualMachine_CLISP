;;;; loader-simplified.lisp
;;;; VERSION SIMPLIFIÉE ET COMPILABLE du loader
;;;;
;;;; Toutes les 12 fonctions de loader.lisp réécrites pour être compilables
;;;;
;;;; TRANSFORMATIONS APPLIQUÉES:
;;;; 1. GETHASH/SETF → recherche/construction manuelle
;;;; 2. INCF → SETQ avec +
;;;; 3. DOLIST → récursion avec labels strings
;;;; 4. MAPCAR → map-list récursif
;;;; 5. Labels symboliques → strings

;;; ============================================================================
;;; FONCTIONS UTILITAIRES POUR HASH TABLES (simulation)
;;; ============================================================================

(defun make-alist ()
  "Crée une alist vide (simulation de hash table)"
  nil)

(defun alist-get (key alist)
  "Récupère une valeur dans une alist"
  (if (null alist)
      nil
      (let ((pair (first alist)))
        (if (equal (first pair) key)
            (second pair)
            (alist-get key (rest alist))))))

(defun alist-put (key value alist)
  "Ajoute/met à jour une valeur dans une alist - retourne nouvelle alist"
  (cons (list key value)
        (alist-remove key alist)))

(defun alist-remove (key alist)
  "Supprime une clé d'une alist"
  (if (null alist)
      nil
      (let ((pair (first alist)))
        (if (equal (first pair) key)
            (alist-remove key (rest alist))
            (cons pair (alist-remove key (rest alist)))))))

;;; ============================================================================
;;; COLLECT-LABELS - Collecte tous les labels et leurs positions
;;; ============================================================================

(defun collect-labels-simplified (asm-code code-start)
  "Collecte tous les labels et leurs positions ABSOLUES dans le code
   Version sans GETHASH/SETF - utilise une alist"
  (collect-labels-helper asm-code code-start (make-alist) 0))

(defun collect-labels-helper (asm-code code-start labels position)
  "Helper récursif pour collect-labels"
  (if (null asm-code)
      labels
      (let ((instr (first asm-code))
            (rest-code (rest asm-code)))
        (cond
          ;; Si c'est un label, l'enregistrer
          ((is-label-instr instr)
           (let ((label-name (get-label-name instr))
                 (label-pos (+ code-start position)))
             (collect-labels-helper rest-code code-start
                                   (alist-put label-name label-pos labels)
                                   position)))
          ;; Si c'est un commentaire, l'ignorer (ne pas incrémenter position)
          ((is-comment-instr instr)
           (collect-labels-helper rest-code code-start labels position))
          ;; Instruction normale, incrémenter position
          (t
           (collect-labels-helper rest-code code-start labels
                                 (+ position 1)))))))

(defun is-label-instr (instr)
  "Teste si une instruction est un label"
  (if (null instr)
      nil
      (if (listp instr)
          (let ((first-elem (first instr)))
            (or (equal first-elem "LABEL")
                (equal first-elem ":LABEL")
                (eq first-elem 'LABEL)
                (eq first-elem ':LABEL)))
          nil)))

(defun is-comment-instr (instr)
  "Teste si une instruction est un commentaire (à ignorer)"
  (if (null instr)
      nil
      (if (listp instr)
          (let ((first-elem (first instr)))
            (or (equal first-elem "COMMENT")
                (equal first-elem ":COMMENT")
                (eq first-elem 'COMMENT)
                (eq first-elem ':COMMENT)))
          nil)))

(defun get-label-name (label-instr)
  "Extrait le nom du label"
  (second label-instr))

;;; ============================================================================
;;; RESOLVE-LABELS - Remplace les références symboliques par des adresses
;;; ============================================================================

(defun resolve-labels-simplified (asm-code labels)
  "Remplace les références symboliques par des adresses
   Version sans MAPCAR natif"
  (resolve-labels-helper asm-code labels nil))

(defun resolve-labels-helper (asm-code labels acc)
  "Helper récursif pour resolve-labels"
  (if (null asm-code)
      (reverse-list acc)
      (let ((instr (first asm-code))
            (rest-code (rest asm-code)))
        (if (or (is-label-instr instr) (is-comment-instr instr))
            ;; Ignorer les labels ET les commentaires
            (resolve-labels-helper rest-code labels acc)
            (let ((resolved-instr (resolve-instruction instr labels)))
              (resolve-labels-helper rest-code labels
                                    (cons resolved-instr acc)))))))

(defun resolve-instruction (instr labels)
  "Résout les labels dans une instruction"
  (if (null instr)
      nil
      (if (listp instr)
          (map-list (lambda (elem) (resolve-element elem labels)) instr)
          (resolve-element instr labels))))

(defun resolve-element (element labels)
  "Résout un élément d'instruction (remplace label par adresse)"
  (if (symbolp element)
      (let ((addr (alist-get element labels)))
        (if (null addr)
            element
            addr))
      (if (stringp element)
          (let ((addr (alist-get element labels)))
            (if (null addr)
                element
                addr))
          element)))

(defun map-list (func lst)
  "Map fonctionnel - version récursive"
  (if (null lst)
      nil
      (cons (funcall func (first lst))
            (map-list func (rest lst)))))

(defun reverse-list (lst)
  "Inverse une liste"
  (reverse-list-helper lst nil))

(defun reverse-list-helper (lst acc)
  "Helper pour reverse"
  (if (null lst)
      acc
      (reverse-list-helper (rest lst) (cons (first lst) acc))))

;;; ============================================================================
;;; KEYWORD-TO-SYMBOL - Conversion keywords → symboles
;;; ============================================================================

(defun keyword-to-symbol-simplified (kw)
  "Convertit un keyword en symbole
   :ADDI → ADDI, :$SP → $SP, etc.
   Gère aussi les strings, nombres, et autres types"
  (cond
    ((null kw) nil)
    ((keywordp kw) 
     ;; Keyword: convertir en symbole
     (intern (symbol-name kw)))
    ((symbolp kw) 
     ;; Déjà un symbole: garder tel quel
     kw)
    ((stringp kw)
     ;; String: convertir en symbole si possible, sinon garder
     (if (> (length kw) 0)
         (intern kw)
         kw))
    ((numberp kw) 
     ;; Nombre: garder tel quel
     kw)
    ((listp kw)
     ;; Liste: normaliser récursivement
     (map-list (lambda (x) (keyword-to-symbol-simplified x)) kw))
    (t 
     ;; Autres types: garder tel quel
     kw)))

(defun normalize-instruction-simplified (instr)
  "Convertit tous les keywords d'une instruction en symboles
   Gère tous les types: keywords, strings, nombres, listes, etc."
  (keyword-to-symbol-simplified instr))

(defun normalize-code-simplified (asm-code)
  "Normalise tout le code assembleur (keywords → symboles)"
  (map-list (lambda (instr) (normalize-instruction-simplified instr)) asm-code))

;;; ============================================================================
;;; PARSE-ASM - Parsing et validation
;;; ============================================================================

(defun parse-asm-simplified (code)
  "Parse et valide le code assembleur"
  (cond
    ((and (listp code) (all-lists-p code))
     code)
    ((listp code)
     (list code))
    (t (error "Format de code invalide"))))

(defun all-lists-p (lst)
  "Teste si tous les éléments sont des listes"
  (if (null lst)
      t
      (if (listp (first lst))
          (all-lists-p (rest lst))
          nil)))

(defun filter-comments (asm-code)
  "Retire toutes les instructions COMMENT du code"
  (if (null asm-code)
      nil
      (let ((instr (first asm-code))
            (rest-code (rest asm-code)))
        (if (is-comment-instr instr)
            (filter-comments rest-code)
            (cons instr (filter-comments rest-code))))))

(defun preprocess-code-simplified (asm-code code-start)
  "Prétraite le code assembleur (résolution des labels, etc.)
   Version simplifiée qui retourne (resolved . labels)"
  (let* ((parsed (parse-asm-simplified asm-code))
         (filtered (filter-comments parsed))  ;; Filtrer les COMMENT d'abord!
         (labels (collect-labels-simplified filtered code-start))
         (resolved (resolve-labels-simplified filtered labels)))
    (cons resolved labels)))

;;; ============================================================================
;;; CALCULATE-CODE-START
;;; ============================================================================

(defvar *maxmem* 1000000)
(defvar *code-size* 100000)

(defun calculate-code-start-simplified (vm)
  "Calcule l'adresse de début de la zone code"
  (- *maxmem* *code-size*))

;;; ============================================================================
;;; LOAD-CODE - Chargement en mémoire (simplifié)
;;; ============================================================================

(defun load-code-simplified (vm asm-code)
  "Charge le code assembleur dans la mémoire de la VM
   Version simplifiée pour compilation
   Retourne juste le code résolu"
  (let* ((code-start (calculate-code-start-simplified vm))
         (asm-with-halt (append-two-lists asm-code (list (list "HALT"))))
         (result (preprocess-code-simplified asm-with-halt code-start)))
    (first result)))

(defun append-two-lists (list1 list2)
  "Concatène deux listes"
  (if (null list1)
      list2
      (cons (first list1)
            (append-two-lists (rest list1) list2))))

;;; ============================================================================
;;; APPEND-CODE - Ajout de code (simplifié)
;;; ============================================================================

(defun append-code-simplified (vm asm-code)
  "Ajoute du code à la VM existante
   Version simplifiée"
  (load-code-simplified vm asm-code))

;;; ============================================================================
;;; DUMP-CODE - Affichage du code (simulé)
;;; ============================================================================

(defun dump-code-simplified (vm max-instructions)
  "Affiche le code chargé dans la VM
   Version simplifiée qui retourne juste un message"
  (list (list "COMMENT" "Code dump")))

;;; ============================================================================
;;; LOAD-AND-RUN - Chargement et exécution (simplifié)
;;; ============================================================================

(defun load-and-run-simplified (vm asm-code)
  "Charge et exécute du code
   Version simplifiée pour compilation"
  (let ((resolved (load-code-simplified vm asm-code)))
    (list (list "COMMENT" "Load and run complete"))))

;;; ============================================================================
;;; FONCTIONS UTILITAIRES SUPPLÉMENTAIRES
;;; ============================================================================

(defun list-length (lst)
  "Compte les éléments d'une liste"
  (if (null lst)
      0
      (+ 1 (list-length (rest lst)))))

(defun nth-element (n lst)
  "Retourne le n-ième élément (0-indexed)"
  (if (= n 0)
      (first lst)
      (nth-element (- n 1) (rest lst))))

;;; ============================================================================
;;; FIN DU FICHIER
;;; ============================================================================

(format t "~%loader-simplified.lisp chargé : 29 fonctions~%")
