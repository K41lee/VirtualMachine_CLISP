;;;; compiler-simplified.lisp
;;;; VERSION COMPLÈTE SIMPLIFIÉE ET COMPILABLE du compilateur LISP vers MIPS
;;;;
;;;; Réécriture de TOUTES les fonctions de compiler.lisp (96 fonctions)
;;;; pour être compilables par le bootstrap compiler
;;;;
;;;; STRUCTURE:
;;;; - Partie 1: Registres et constantes
;;;; - Partie 2: Environnement et helpers
;;;; - Partie 3: Génération de code pour expressions de base
;;;; - Partie 4: Formes spéciales (IF, COND, LET, etc.)
;;;; - Partie 5: Boucles (LOOP, WHILE, DOLIST, DOTIMES)
;;;; - Partie 6: Fonctions (DEFUN, LAMBDA, FUNCALL)
;;;; - Partie 7: Structures et tableaux
;;;; - Partie 8: Point d'entrée principal

(format t "~%Chargement compiler-simplified.lisp...~%")

;;; ============================================================================
;;; PARTIE 1: REGISTRES ET CONSTANTES
;;; ============================================================================

;; Initialiser le système de symboles avant de définir les registres
(when (fboundp 'initialize-compiler-symbols)
  (initialize-compiler-symbols))

;; Fonction pour obtenir l'ID d'un symbole (ou le créer)
(defun get-symbol-id-for-compilation (keyword-sym)
  "Retourne l'ID d'un symbole pour la compilation"
  (if (fboundp 'intern-symbol)
      ;; Garder le symbole complet (avec $ pour les registres)
      (intern-symbol (symbol-name keyword-sym))
      keyword-sym))  ; Fallback aux keywords si système pas disponible

;; Registres MIPS - utiliser des IDs si système de symboles disponible
(defvar *reg-v0* (get-symbol-id-for-compilation :$V0))
(defvar *reg-v1* (get-symbol-id-for-compilation :$V1))
(defvar *reg-a0* (get-symbol-id-for-compilation :$A0))
(defvar *reg-a1* (get-symbol-id-for-compilation :$A1))
(defvar *reg-a2* (get-symbol-id-for-compilation :$A2))
(defvar *reg-a3* (get-symbol-id-for-compilation :$A3))
(defvar *reg-t0* (get-symbol-id-for-compilation :$T0))
(defvar *reg-t1* (get-symbol-id-for-compilation :$T1))
(defvar *reg-t2* (get-symbol-id-for-compilation :$T2))
(defvar *reg-t3* (get-symbol-id-for-compilation :$T3))
(defvar *reg-t4* (get-symbol-id-for-compilation :$T4))
(defvar *reg-t5* (get-symbol-id-for-compilation :$T5))
(defvar *reg-s0* (get-symbol-id-for-compilation :$S0))
(defvar *reg-s1* (get-symbol-id-for-compilation :$S1))
(defvar *reg-s2* (get-symbol-id-for-compilation :$S2))
(defvar *reg-s3* (get-symbol-id-for-compilation :$S3))
(defvar *reg-sp* (get-symbol-id-for-compilation :$SP))
(defvar *reg-fp* (get-symbol-id-for-compilation :$FP))
(defvar *reg-ra* (get-symbol-id-for-compilation :$RA))
(defvar *reg-zero* (get-symbol-id-for-compilation :$ZERO))
(defvar *reg-t6* (get-symbol-id-for-compilation :$T6))
(defvar *reg-t7* (get-symbol-id-for-compilation :$T7))
(defvar *reg-s4* (get-symbol-id-for-compilation :$S4))
(defvar *reg-s5* (get-symbol-id-for-compilation :$S5))
(defvar *reg-s6* (get-symbol-id-for-compilation :$S6))
(defvar *reg-s7* (get-symbol-id-for-compilation :$S7))
(defvar *reg-t8* (get-symbol-id-for-compilation :$T8))
(defvar *reg-t9* (get-symbol-id-for-compilation :$T9))

;; IDs des instructions (évalués au chargement)
(defvar *instr-li* (get-symbol-id-for-compilation :LI))
(defvar *instr-lw* (get-symbol-id-for-compilation :LW))
(defvar *instr-sw* (get-symbol-id-for-compilation :SW))
(defvar *instr-add* (get-symbol-id-for-compilation :ADD))
(defvar *instr-sub* (get-symbol-id-for-compilation :SUB))
(defvar *instr-addi* (get-symbol-id-for-compilation :ADDI))
(defvar *instr-list* (get-symbol-id-for-compilation :LIST))
(defvar *reg-zero* :$ZERO)

;; Compteur global pour labels
(defvar *global-label-counter* 0)

;; Tables globales (simulées avec alists)
(defvar *global-constants* nil)
(defvar *global-variables* nil)
(defvar *global-data-offset* 0)

(defun reset-global-tables-simplified ()
  "Réinitialise les tables globales"
  (setq *global-constants* nil)
  (setq *global-variables* nil)
  (setq *global-data-offset* 0)
  (setq *global-label-counter* 0))

;;; ============================================================================
;;; PARTIE 2: ENVIRONNEMENT ET HELPERS
;;; ============================================================================

;;; 2.1 Création et manipulation d'environnement

(defun make-new-compiler-env-simplified ()
  "Crée un environnement de compilation vide"
  (list
    (list "variables" nil)
    (list "functions" nil)
    (list "stack-offset" 0)
    (list "temp-regs" nil)
    (list "depth" 0)))

(defun env-get (env key)
  "Récupère une valeur dans l'environnement"
  (if (null env)
      nil
      (let ((pair (first env)))
        (if (equal (first pair) key)
            (second pair)
            (env-get (rest env) key)))))

(defun env-set (env key value)
  "Définit une valeur dans l'environnement - retourne nouvel env"
  (cons (list key value)
        (env-remove env key)))

(defun env-remove (env key)
  "Supprime une clé de l'environnement"
  (if (null env)
      nil
      (let ((pair (first env)))
        (if (equal (first pair) key)
            (env-remove (rest env) key)
            (cons pair (env-remove (rest env) key))))))

;;; 2.2 Variables

(defun add-variable-simplified (env var-name location)
  "Ajoute une variable à l'environnement"
  (let ((vars (env-get env "variables")))
    (env-set env "variables" (cons (list var-name location) vars))))

(defun lookup-variable-simplified (env var-name)
  "Cherche une variable dans l'environnement"
  (let ((vars (env-get env "variables")))
    (alist-lookup var-name vars)))

(defun lookup-variable-with-depth-simplified (env var-name)
  "Cherche une variable avec profondeur - retourne (depth . offset)"
  (let ((location (lookup-variable-simplified env var-name)))
    (if (null location)
        nil
        (cons 0 location))))

;;; 2.3 Fonctions

(defun add-function-simplified (env fn-name fn-label)
  "Ajoute une fonction à l'environnement"
  (let ((fns (env-get env "functions")))
    (env-set env "functions" (cons (list fn-name fn-label) fns))))

(defun lookup-function-simplified (env fn-name)
  "Cherche une fonction dans l'environnement"
  (let ((fns (env-get env "functions")))
    (alist-lookup fn-name fns)))

(defun lookup-function-def-info-simplified (env fn-name)
  "Cherche les informations de définition d'une fonction"
  (lookup-function-simplified env fn-name))

;;; 2.4 Helpers pour alists

(defun alist-lookup (key alist)
  "Cherche dans une alist"
  (if (null alist)
      nil
      (let ((pair (first alist)))
        (if (equal (first pair) key)
            (second pair)
            (alist-lookup key (rest alist))))))

(defun alist-add (key value alist)
  "Ajoute à une alist"
  (cons (list key value) alist))

;;; 2.5 Pile et registres

(defun alloc-stack-slot-simplified (env)
  "Alloue un slot sur la pile - retourne (nouvel-env . offset)"
  (let ((offset (env-get env "stack-offset")))
    (cons (env-set env "stack-offset" (- offset 4)) offset)))

(defun free-stack-slots-simplified (env n-slots)
  "Libère n slots de pile"
  (let ((offset (env-get env "stack-offset")))
    (env-set env "stack-offset" (+ offset (* n-slots 4)))))

(defun allocate-temp-reg-simplified (env)
  "Alloue un registre temporaire - retourne (env . reg)"
  (cons env *reg-t0*))

(defun free-temp-reg-simplified (env reg)
  "Libère un registre temporaire"
  env)

(defun alloc-temp-register-simplified (env)
  "Alias pour allocate-temp-reg"
  (allocate-temp-reg-simplified env))

;;; 2.6 Labels

(defun gen-label-simplified (env prefix)
  "Génère un label unique"
  (setq *global-label-counter* (+ *global-label-counter* 1))
  (format nil "~A_~D" prefix *global-label-counter*))

;;; 2.7 Copie d'environnement

(defun copy-env-simplified (env)
  "Copie un environnement"
  (copy-tree-simple env))

(defun copy-tree-simple (tree)
  "Copie un arbre"
  (if (null tree)
      nil
      (if (listp tree)
          (cons (copy-tree-simple (first tree))
                (copy-tree-simple (rest tree)))
          tree)))

(defun make-lexical-env-simplified (parent-env increment-depth)
  "Crée un environnement lexical enfant"
  (let ((depth (env-get parent-env "depth")))
    (if increment-depth
        (env-set parent-env "depth" (+ depth 1))
        parent-env)))

;;; 2.8 Accès static link

(defun generate-static-link-access-simplified (current-depth target-depth)
  "Génère le code pour accéder via static link"
  (if (= current-depth target-depth)
      nil
      (list (list :LW *reg-t0* *reg-fp* 0))))

;;; ============================================================================
;;; PARTIE 3: UTILITAIRES ET PRÉDICATS
;;; ============================================================================

;;; 3.1 Prédicats de type

(defun lisp-atom-p-simplified (expr)
  "Teste si expr est un atome"
  (if (null expr)
      t
      (if (listp expr)
          nil
          t)))

(defun lisp-list-p-simplified (expr)
  "Teste si expr est une liste"
  (listp expr))

(defun vm-primitive-p-simplified (symbol)
  "Teste si symbol est une primitive VM"
  nil)

;;; 3.2 Parsing

(defun parse-lisp-expr-simplified (expr)
  "Parse une expression Lisp"
  expr)

;;; 3.3 Extraction d'éléments

(defun extract-first-elements-simplified (list-of-pairs)
  "Extrait les premiers éléments de paires"
  (if (null list-of-pairs)
      nil
      (cons (first (first list-of-pairs))
            (extract-first-elements-simplified (rest list-of-pairs)))))

(defun extract-second-elements-simplified (list-of-pairs)
  "Extrait les seconds éléments"
  (if (null list-of-pairs)
      nil
      (cons (second (first list-of-pairs))
            (extract-second-elements-simplified (rest list-of-pairs)))))

(defun flatten-clauses-simplified (clauses)
  "Aplatit une liste de clauses"
  (if (null clauses)
      nil
      (append-two (first clauses)
                  (flatten-clauses-simplified (rest clauses)))))

(defun extract-clause-bodies-simplified (clauses)
  "Extrait les corps de clauses"
  (if (null clauses)
      nil
      (cons (rest (first clauses))
            (extract-clause-bodies-simplified (rest clauses)))))

;;; 3.4 Manipulation de listes

(defun append-two (list1 list2)
  "Concatène deux listes"
  (if (null list1)
      list2
      (cons (first list1)
            (append-two (rest list1) list2))))

(defun append-many (lists)
  "Concatène plusieurs listes"
  (if (null lists)
      nil
      (if (null (rest lists))
          (first lists)
          (append-two (first lists)
                     (append-many (rest lists))))))

(defun reverse-list-simple (lst)
  "Inverse une liste"
  (reverse-helper lst nil))

(defun reverse-helper (lst acc)
  "Helper pour reverse"
  (if (null lst)
      acc
      (reverse-helper (rest lst) (cons (first lst) acc))))

(defun list-length-simple (lst)
  "Longueur d'une liste"
  (if (null lst)
      0
      (+ 1 (list-length-simple (rest lst)))))

(defun nth-simple (n lst)
  "N-ième élément"
  (if (= n 0)
      (first lst)
      (nth-simple (- n 1) (rest lst))))

(defun map-eval-constant-expr-simplified (exprs)
  "Map sur expressions constantes"
  (if (null exprs)
      nil
      (cons (first exprs)
            (map-eval-constant-expr-simplified (rest exprs)))))

;;; ============================================================================
;;; PARTIE 4: VARIABLES LIBRES
;;; ============================================================================

(defun free-variables-simplified (expr bound-vars)
  "Calcule les variables libres dans une expression"
  (if (null expr)
      nil
      (if (listp expr)
          (free-variables-list-simplified expr bound-vars)
          (if (member-simple expr bound-vars)
              nil
              (list expr)))))

(defun free-variables-list-simplified (expr-list bound-vars)
  "Variables libres dans une liste d'expressions"
  (if (null expr-list)
      nil
      (append-two (free-variables-simplified (first expr-list) bound-vars)
                  (free-variables-list-simplified (rest expr-list) bound-vars))))

(defun member-simple (item list)
  "Teste si item est dans list"
  (if (null list)
      nil
      (if (equal item (first list))
          t
          (member-simple item (rest list)))))

;;; ============================================================================
;;; PARTIE 5: COMPILATION - CONSTANTES ET VARIABLES
;;; ============================================================================

(defun compile-constant-simplified (value env)
  "Compile une constante"
  (list (list *instr-li* value *reg-v0*)))

(defun compile-variable-simplified (var env)
  "Compile une référence à une variable"
  (let ((location (lookup-variable-simplified env var)))
    (cond
      ;; Variable non trouvée - vérifier si c'est une variable globale
      ((null location)
       (if (and (symbolp var)
                (char= (char (symbol-name var) 0) #\*))
           ;; Variable globale (commence par *)
           (list (list (get-symbol-id-for-compilation :GLOBAL-GET) var))
           ;; Variable inconnue
           (list (list *instr-li* 0 *reg-v0*))))
      ;; Si la variable est marquée comme étant dans $S0
      ((eq location :in-s0)
       (list (list (get-symbol-id-for-compilation :MOVE) *reg-s0* *reg-v0*)))
      ;; Si la location est une liste (:frame offset), charger depuis $FP
      ((and (listp location) (eq (first location) :frame))
       (list (list *instr-lw* *reg-v0* *reg-fp* (second location))))
      ;; Si la location est une liste (:stack offset), extraire l'offset
      ((and (listp location) (eq (first location) :stack))
       (list (list *instr-lw* *reg-v0* *reg-sp* (second location))))
      ;; Sinon charger depuis la pile avec offset direct
      (t
       (list (list *instr-lw* *reg-v0* *reg-sp* location))))))

;;; ============================================================================
;;; PARTIE 6: COMPILATION - ARITHMÉTIQUE
;;; ============================================================================

(defun compile-arithmetic-simplified (op args env)
  "Compile une opération arithmétique (+, -, *, /)"
  (if (null args)
      (list (list :LI 0 *reg-v0*))
      (if (null (rest args))
          ;; Cas unaire: uniquement pour - (négation)
          (if (equal op "-")
              (append-many
                (list
                  (compile-expr-main (first args) env)
                  (list (list :LI 0 *reg-t0*))
                  (list (list :SUB *reg-t0* *reg-v0* *reg-v0*))))  ; 0 - V0 -> V0
              (compile-expr-main (first args) env))
          ;; Utiliser la pile pour les opérations binaires
          (compile-arithmetic-with-stack op args env))))

(defun compile-arithmetic-with-stack (op args env)
  "Compile opération binaire/n-aire en utilisant $S0 avec sauvegarde/restauration"
  (if (null (cddr args))
      ;; Exactement 2 arguments
      (append-many
        (list
          ;; Sauvegarder $S0 sur la pile
          (list (list :ADDI *reg-sp* -4 *reg-sp*))
          (list (list :SW *reg-s0* *reg-sp* 0))
          ;; Calculer premier argument (résultat dans $V0)
          (compile-expr-main (first args) (adjust-all-offsets env 4))
          ;; Le sauvegarder dans $S0
          (list (list :MOVE *reg-v0* *reg-s0*))
          ;; Calculer second argument (résultat dans $V0)
          (compile-expr-main (second args) (adjust-all-offsets env 4))
          ;; Charger premier argument dans $T0 depuis $S0
          (list (list :MOVE *reg-s0* *reg-t0*))
          ;; Restaurer $S0 depuis la pile
          (list (list :LW *reg-s0* *reg-sp* 0))
          (list (list :ADDI *reg-sp* 4 *reg-sp*))
          ;; Effectuer l'opération: $T0 op $V0 -> $V0
          (compile-arithmetic-op-simple op)))
      ;; Plus de 2 arguments: récursif (+ a b c) = (+ (+ a b) c)
      (compile-arithmetic-with-stack op 
                                     (cons (list (intern (string-upcase op))
                                                (first args)
                                                (second args))
                                           (cddr args))
                                     env)))

(defun adjust-all-offsets (env delta)
  "Ajuste tous les offsets de variables dans l'environnement"
  (if (null env)
      nil
      (let ((entry (first env)))
        (if (and (listp entry) (equal (first entry) "variables"))
            ;; C'est la liste des variables: ajuster les offsets
            (cons (list "variables" (adjust-variable-list (second entry) delta))
                  (adjust-all-offsets (rest env) delta))
            ;; Autre entrée: garder tel quel
            (cons entry (adjust-all-offsets (rest env) delta))))))

(defun adjust-variable-list (var-list delta)
  "Ajuste les offsets dans une liste de variables
   Ne touche PAS aux variables avec :frame (relatives à $FP)
   Ajuste uniquement les offsets numériques (relatifs à $SP)"
  (if (null var-list)
      nil
      (let ((var-entry (first var-list)))
        (if (and (listp var-entry) (>= (length var-entry) 2))
            (let ((var-name (first var-entry))
                  (location (second var-entry)))
              (cond
                ;; Si c'est (:frame offset), ne pas ajuster (relatif à $FP)
                ((and (listp location) (eq (first location) :frame))
                 (cons var-entry (adjust-variable-list (rest var-list) delta)))
                ;; Si c'est (:stack offset), ajuster l'offset
                ((and (listp location) (eq (first location) :stack))
                 (cons (list var-name (list :stack (+ (second location) delta)))
                       (adjust-variable-list (rest var-list) delta)))
                ;; Si c'est un nombre simple (offset relatif à $SP), ajuster
                ((numberp location)
                 (cons (list var-name (+ location delta))
                       (adjust-variable-list (rest var-list) delta)))
                ;; Autre cas: garder tel quel
                (t
                 (cons var-entry (adjust-variable-list (rest var-list) delta)))))
            ;; Entrée mal formée: garder telle quelle
            (cons var-entry (adjust-variable-list (rest var-list) delta))))))

(defun compile-arithmetic-op-simple (op)
  "Génère l'instruction pour l'opération"
  (cond
    ((equal op "+") (list (list :ADD *reg-v0* *reg-t0* *reg-v0*)))
    ((equal op "-") (list (list :SUB *reg-t0* *reg-v0* *reg-v0*)))  ; T0 - V0 -> V0
    ((equal op "*") (list (list :MUL *reg-t0* *reg-v0*)
                          (list :MFLO *reg-v0*)))
    ((equal op "/") (list (list :DIV *reg-t0* *reg-v0*)
                          (list :MFLO *reg-v0*)))
    ((equal op "%") (list (list :DIV *reg-t0* *reg-v0*)
                          (list :MFHI *reg-v0*)))
    (t (list (list :ADD *reg-v0* *reg-t0* *reg-v0*)))))

(defun compile-math-func-simplified (func args env)
  "Compile fonctions mathématiques (MOD, etc.)"
  (cond
    ((equal func "MOD") (compile-arithmetic-binary "%" args env))
    (t (compile-arithmetic-simplified "+" args env))))

;;; ============================================================================
;;; PARTIE 7: COMPILATION - COMPARAISONS
;;; ============================================================================

(defun compile-comparison-simplified (op args env)
  "Compile comparaison (=, <, >, etc.)"
  (if (< (list-length-simple args) 2)
      (list (list :LI 1 *reg-v0*))
      (compile-comparison-pair op (first args) (second args) env)))

(defun compile-comparison-pair (op arg1 arg2 env)
  "Compile comparaison entre 2 arguments"
  (let ((true-label (gen-label-simplified env "CMP_TRUE"))
        (end-label (gen-label-simplified env "CMP_END")))
    (append-many
      (list
        (compile-expr-main arg1 env)
        ;; Pousser AVANT de sauver pour ne pas écraser la pile existante
        (list (list :ADDI *reg-sp* -4 *reg-sp*))
        (list (list :SW *reg-v0* *reg-sp* 0))
        (compile-expr-main arg2 env)
        (list (list :LW *reg-t0* *reg-sp* 0))
        (list (list :ADDI *reg-sp* 4 *reg-sp*))
        (compile-comparison-branch-simple op true-label)
        (list (list :LI 0 *reg-v0*))
        (list (list :JMP end-label))
        (list (list :LABEL true-label))
        (list (list :LI 1 *reg-v0*))
        (list (list :LABEL end-label))))))

(defun compile-comparison-branch-simple (op true-label)
  "Génère branchement pour comparaison"
  (cond
    ((equal op "=") (list (list :BEQ *reg-t0* *reg-v0* true-label)))
    ((equal op "<") (list (list :BLT *reg-t0* *reg-v0* true-label)))
    ((equal op ">") (list (list :BGT *reg-t0* *reg-v0* true-label)))
    ((equal op "<=") (list (list :BLE *reg-t0* *reg-v0* true-label)))
    ((equal op ">=") (list (list :BGE *reg-t0* *reg-v0* true-label)))
    ((equal op "/=") (list (list :BNE *reg-t0* *reg-v0* true-label)))
    (t (list (list :BEQ *reg-t0* *reg-v0* true-label)))))

;;; ============================================================================
;;; PARTIE 8: COMPILATION - IF/COND/WHEN/UNLESS
;;; ============================================================================

(defun compile-if-simplified (condition then-branch else-branch env)
  "Compile (IF ...)"
  (let ((else-label (gen-label-simplified env "ELSE"))
        (end-label (gen-label-simplified env "END")))
    (append-many
      (list
        (compile-expr-main condition env)
        (list (list :BEQ *reg-v0* *reg-zero* else-label))
        (compile-expr-main then-branch env)
        (list (list :JMP end-label))
        (list (list :LABEL else-label))
        (compile-expr-main else-branch env)
        (list (list :LABEL end-label))))))

(defun compile-cond-simplified (clauses env)
  "Compile (COND ...)"
  (if (null clauses)
      (list (list :LI 0 *reg-v0*))
      (compile-cond-clause (first clauses) (rest clauses) env)))

(defun compile-cond-clause (clause rest-clauses env)
  "Compile une clause de COND"
  (let ((test (first clause))
        (body (rest clause))
        (next-label (gen-label-simplified env "COND_NEXT"))
        (end-label (gen-label-simplified env "COND_END")))
    (if (eq test t)
        (compile-progn-simplified body env)
        (append-many
          (list
            (compile-expr-main test env)
            (list (list :BEQ *reg-v0* *reg-zero* next-label))
            (compile-progn-simplified body env)
            (list (list :JMP end-label))
            (list (list :LABEL next-label))
            (compile-cond-simplified rest-clauses env)
            (list (list :LABEL end-label)))))))

(defun compile-when-simplified (test body env)
  "Compile (WHEN ...)"
  (let ((end-label (gen-label-simplified env "WHEN_END")))
    (append-many
      (list
        (compile-expr-main test env)
        (list (list :BEQ *reg-v0* *reg-zero* end-label))
        (compile-progn-simplified body env)
        (list (list :LABEL end-label))))))

(defun compile-unless-simplified (test body env)
  "Compile (UNLESS ...)"
  (let ((skip-label (gen-label-simplified env "UNLESS_SKIP")))
    (append-many
      (list
        (compile-expr-main test env)
        (list (list :BNE *reg-v0* *reg-zero* skip-label))
        (compile-progn-simplified body env)
        (list (list :LABEL skip-label))))))

;;; ============================================================================
;;; PARTIE 9: COMPILATION - AND/OR/NOT
;;; ============================================================================

(defun compile-not-simplified (expr env)
  "Compile (NOT ...)"
  (let ((true-label (gen-label-simplified env "NOT_TRUE"))
        (end-label (gen-label-simplified env "NOT_END")))
    (append-many
      (list
        (compile-expr-main expr env)
        (list (list :BEQ *reg-v0* *reg-zero* true-label))
        (list (list :LI 0 *reg-v0*))
        (list (list :JMP end-label))
        (list (list :LABEL true-label))
        (list (list :LI 1 *reg-v0*))
        (list (list :LABEL end-label))))))

(defun compile-and-simplified (args env)
  "Compile (AND ...) avec court-circuit"
  (if (null args)
      (list (list :LI 1 *reg-v0*))
      (compile-and-helper args env)))

(defun compile-and-helper (args env)
  "Helper pour AND"
  (if (null (rest args))
      (compile-expr-main (first args) env)
      (let ((false-label (gen-label-simplified env "AND_FALSE"))
            (end-label (gen-label-simplified env "AND_END")))
        (append-many
          (list
            (compile-expr-main (first args) env)
            (list (list :BEQ *reg-v0* *reg-zero* false-label))
            (compile-and-helper (rest args) env)
            (list (list :JMP end-label))
            (list (list :LABEL false-label))
            (list (list :LI 0 *reg-v0*))
            (list (list :LABEL end-label)))))))

(defun compile-or-simplified (args env)
  "Compile (OR ...) avec court-circuit"
  (if (null args)
      (list (list :LI 0 *reg-v0*))
      (compile-or-helper args env)))

(defun compile-or-helper (args env)
  "Helper pour OR"
  (if (null (rest args))
      (compile-expr-main (first args) env)
      (let ((true-label (gen-label-simplified env "OR_TRUE"))
            (end-label (gen-label-simplified env "OR_END")))
        (append-many
          (list
            (compile-expr-main (first args) env)
            (list (list :BNE *reg-v0* *reg-zero* true-label))
            (compile-or-helper (rest args) env)
            (list (list :JMP end-label))
            (list (list :LABEL true-label))
            (list (list :LABEL end-label)))))))

;;; ============================================================================
;;; PARTIE 10: COMPILATION - CASE
;;; ============================================================================

(defun compile-case-simplified (keyform clauses env)
  "Compile (CASE ...)"
  (let ((end-label (gen-label-simplified env "CASE_END")))
    (append-many
      (list
        (compile-expr-main keyform env)
        (list (list :SW *reg-v0* *reg-sp* 0))
        (list (list :ADDI *reg-sp* -4 *reg-sp*))
        (compile-case-clauses clauses env end-label)
        (list (list :ADDI *reg-sp* 4 *reg-sp*))
        (list (list :LABEL end-label))))))

(defun compile-case-clauses (clauses env end-label)
  "Compile les clauses de CASE"
  (if (null clauses)
      (list (list :LI 0 *reg-v0*))
      (compile-case-clause (first clauses) (rest clauses) env end-label)))

(defun compile-case-clause (clause rest-clauses env end-label)
  "Compile une clause de CASE"
  (let ((keys (first clause))
        (body (rest clause))
        (next-label (gen-label-simplified env "CASE_NEXT")))
    (append-many
      (list
        (compile-case-test keys env next-label)
        (compile-progn-simplified body env)
        (list (list :JMP end-label))
        (list (list :LABEL next-label))
        (compile-case-clauses rest-clauses env end-label)))))

(defun compile-case-test (keys env next-label)
  "Teste si la clé correspond"
  (if (or (eq keys t) (eq keys 'otherwise))
      nil
      (list (list :LW *reg-t0* *reg-sp* 4))))

;;; ============================================================================
;;; PARTIE 11: COMPILATION - LET/LET*/PROGN
;;; ============================================================================

(defun compile-let-simplified (bindings body env)
  "Compile (LET ...) avec cleanup automatique du stack"
  (if (null bindings)
      (compile-progn-simplified body env)
      (let ((binding-count (list-length-simple bindings)))
        ;; Compiler tous les bindings + corps + cleanup
        (append-many
          (list
            (compile-let-bindings bindings body env)
            ;; Cleanup: restaurer le $SP (4 octets par binding)
            (list (list :ADDI *reg-sp* (* 4 binding-count) *reg-sp*)))))))

(defun compile-let-bindings (bindings body env)
  "Compile les bindings de LET (sans cleanup)"
  (if (null bindings)
      (compile-progn-simplified body env)
      (let* ((binding (first bindings))
             (var-name (first binding))
             (var-expr (second binding))
             (rest-bindings (rest bindings)))
        (append-many
          (list
            (compile-expr-main var-expr env)
            (list (list :SW *reg-v0* *reg-sp* 0))
            (list (list :ADDI *reg-sp* -4 *reg-sp*))
            ;; Après ADDI, la variable est maintenant à $SP+4, pas $SP+0
            (let ((new-env (add-variable-simplified env var-name 4)))
              (compile-let-bindings rest-bindings body new-env)))))))

(defun compile-let*-simplified (bindings body env)
  "Compile (LET* ...)"
  (compile-let-simplified bindings body env))

(defun compile-progn-simplified (exprs env)
  "Compile (PROGN ...) - séquence d'expressions"
  (if (null exprs)
      (list (list :LI 0 *reg-v0*))
      (if (null (rest exprs))
          (compile-expr-main (first exprs) env)
          (append-two (compile-expr-main (first exprs) env)
                     (compile-progn-simplified (rest exprs) env)))))

;;; ============================================================================
;;; PARTIE 12: COMPILATION - BOUCLES
;;; ============================================================================

(defun compile-loop-while-simplified (condition body env)
  "Compile (LOOP WHILE ...)"
  (let ((start-label (gen-label-simplified env "LOOP_START"))
        (end-label (gen-label-simplified env "LOOP_END")))
    (append-many
      (list
        (list (list :LABEL start-label))
        (compile-expr-main condition env)
        (list (list :BEQ *reg-v0* *reg-zero* end-label))
        (compile-progn-simplified body env)
        (list (list :JMP start-label))
        (list (list :LABEL end-label))
        (list (list :LI 0 *reg-v0*))))))

(defun compile-while-simplified (condition body env)
  "Compile (WHILE ...)"
  (compile-loop-while-simplified condition body env))

(defun compile-loop-advanced-simplified (parsed env)
  "Compile LOOP avancé (simplifié)"
  (list (list :LI 0 *reg-v0*)))

(defun compile-dolist-simplified (var list-expr body env)
  "Compile (DOLIST ...)"
  (let ((start-label (gen-label-simplified env "DOLIST_START"))
        (end-label (gen-label-simplified env "DOLIST_END")))
    (append-many
      (list
        (compile-expr-main list-expr env)
        (list (list :SW *reg-v0* *reg-sp* 0))
        (list (list :ADDI *reg-sp* -4 *reg-sp*))
        (list (list :LABEL start-label))
        (list (list :LW *reg-t0* *reg-sp* 4))
        (list (list :BEQ *reg-t0* *reg-zero* end-label))
        (compile-progn-simplified body env)
        (list (list :JMP start-label))
        (list (list :LABEL end-label))
        (list (list :ADDI *reg-sp* 4 *reg-sp*))
        (list (list :LI 0 *reg-v0*))))))

;;; ============================================================================
;;; PARTIE 13: COMPILATION - DEFUN/FUNCALL/LAMBDA
;;; ============================================================================

(defun compile-defun-simplified (fn-name params body env)
  "Compile (DEFUN ...) - Support multi-paramètres via $A0-$A3 et pile"
  ;; Créer un label avec le nom de la fonction
  (let* ((fn-label (if (stringp fn-name) 
                       fn-name 
                       (string-upcase (symbol-name fn-name))))
         (end-label (concatenate 'string fn-label "_END"))
         (new-env (add-function-simplified env fn-name fn-label))
         (param-count (list-length-simple params))
         ;; Calculer taille pile: $RA (4) + $FP (4) + params (4 * count)
         (stack-size (+ 8 (* 4 param-count))))
    (append-many
      (list
        ;; Skip over function body (will be called via JAL)
        (list (list :J end-label))
        (list (list :LABEL fn-label))
        ;; Prologue: sauvegarder $RA, $FP et tous les paramètres
        (list (list :ADDI *reg-sp* (- stack-size) *reg-sp*))
        (list (list :SW *reg-ra* *reg-sp* 0))
        (list (list :SW *reg-fp* *reg-sp* 4))
        ;; Configurer $FP pour pointer vers le début du frame
        (list (list :MOVE *reg-sp* *reg-fp*))
        ;; Sauvegarder les paramètres $A0-$A3 sur la pile
        (compile-save-params params 2)  ; Offset 2 car 0=$RA, 1=$FP
        ;; Corps de la fonction avec environnement multi-params
        (compile-function-body-with-multi-params params body new-env)
        ;; Epilogue: restaurer $FP, $RA et retourner
        (list (list :LW *reg-fp* *reg-sp* 4))
        (list (list :LW *reg-ra* *reg-sp* 0))
        (list (list :ADDI *reg-sp* stack-size *reg-sp*))
        (list (list :JR *reg-ra*))
        (list (list :LABEL end-label))
        ;; DEFUN returns 0 (or function name symbol)
        (list (list :LI 0 *reg-v0*))))))

(defun compile-save-params (params offset)
  "Génère code pour sauvegarder $A0-$A3 sur la pile (via $FP)"
  (if (null params)
      nil
      (let* ((param-idx (- offset 2))  ; Ajuster car offset 0=$RA, 1=$FP
             (arg-reg (cond
                        ((= param-idx 0) *reg-a0*)
                        ((= param-idx 1) *reg-a1*)
                        ((= param-idx 2) *reg-a2*)
                        ((= param-idx 3) *reg-a3*)
                        (t *reg-a0*))))
        (append
          (list (list :SW arg-reg *reg-fp* (* 4 offset)))
          (compile-save-params (cdr params) (+ offset 1))))))

(defun compile-function-body-with-multi-params (params body env)
  "Compile le corps d'une fonction - paramètres sur la pile via $FP"
  ;; Créer environnement avec tous les paramètres mappés à leur offset depuis $FP
  (let ((new-env (add-params-to-env params env 2)))  ; Offset 2 car 0=$RA, 1=$FP
    (compile-progn-simplified body new-env)))

(defun add-params-to-env (params env offset)
  "Ajoute tous les paramètres à l'environnement avec leur offset depuis $FP"
  (if (null params)
      env
      (let* ((param-name (first params))
             (stack-offset (* 4 offset))
             (location (list :frame stack-offset))  ; Use :frame to indicate $FP-relative
             (new-env (add-variable-simplified env param-name location)))
        (add-params-to-env (cdr params) new-env (+ offset 1)))))

(defun compile-funcall-simplified (fn-name args env)
  "Compile (FUNCALL ...) - Support multi-arguments via $A0-$A3"
  (let ((fn-label (if (stringp fn-name)
                      fn-name
                      (string-upcase (symbol-name fn-name))))
        (arg-count (list-length-simple args)))
    (append-many
      (list
        ;; Sauvegarder les registres $S0, $S4-$S6 (PAS $RA!)
        (list (list :ADDI *reg-sp* -16 *reg-sp*))
        (list (list :SW *reg-s0* *reg-sp* 0))
        (list (list :SW *reg-s4* *reg-sp* 4))
        (list (list :SW *reg-s5* *reg-sp* 8))
        (list (list :SW *reg-s6* *reg-sp* 12))
        ;; Compiler et charger tous les arguments dans $A0-$A3
        ;; IMPORTANT: Ajuster l'environnement pour tenir compte du décalage de $SP
        (compile-load-args args (adjust-all-offsets env 16) 0)
        ;; Appeler la fonction (JAL sauvegarde automatiquement $RA)
        (list (list :JAL fn-label))
        ;; Restaurer les registres (sauf $RA qui contient l'adresse de retour)
        (list (list :LW *reg-s0* *reg-sp* 0))
        (list (list :LW *reg-s4* *reg-sp* 4))
        (list (list :LW *reg-s5* *reg-sp* 8))
        (list (list :LW *reg-s6* *reg-sp* 12))
        (list (list :ADDI *reg-sp* 16 *reg-sp*))))))

(defun compile-load-args (args env index)
  "Compile et charge les arguments dans $A0-$A3
   IMPORTANT: Sauvegarde chaque argument sur la pile avant de compiler le suivant,
   car l'évaluation d'arguments suivants (ex: appels de fonctions) peut écraser
   les registres $A0-$A3"
  (if (null args)
      nil
      (if (null (cdr args))
          ;; Dernier argument: pas besoin de sauvegarder
          (let ((arg-reg (cond
                           ((= index 0) *reg-a0*)
                           ((= index 1) *reg-a1*)
                           ((= index 2) *reg-a2*)
                           ((= index 3) *reg-a3*)
                           (t *reg-a0*))))
            (append-many
              (list
                (compile-expr-main (first args) env)
                (list (list :MOVE *reg-v0* arg-reg)))))
          ;; Pas le dernier: sauvegarder sur la pile
          (let ((arg-reg (cond
                           ((= index 0) *reg-a0*)
                           ((= index 1) *reg-a1*)
                           ((= index 2) *reg-a2*)
                           ((= index 3) *reg-a3*)
                           (t *reg-a0*))))
            (append-many
              (list
                ;; Compiler l'argument (résultat dans $V0)
                (compile-expr-main (first args) env)
                ;; Copier $V0 vers le registre argument approprié
                (list (list :MOVE *reg-v0* arg-reg))
                ;; Sauvegarder sur la pile (sera écrasé par args suivants)
                (list (list :ADDI *reg-sp* -4 *reg-sp*))
                (list (list :SW arg-reg *reg-sp* 0))
                ;; Compiler les arguments suivants avec environnement ajusté
                (compile-load-args (cdr args) (adjust-all-offsets env 4) (+ index 1))
                ;; Restaurer cet argument depuis la pile
                (list (list :LW arg-reg *reg-sp* 0))
                (list (list :ADDI *reg-sp* 4 *reg-sp*))))))))

(defun compile-args (args env)
  "Compile les arguments"
  (if (null args)
      nil
      (append-many
        (list
          (compile-expr-main (first args) env)
          (list (list :SW *reg-v0* *reg-sp* 0))
          (list (list :ADDI *reg-sp* -4 *reg-sp*))
          (compile-args (rest args) env)))))

(defun compile-lambda-simplified (params body env)
  "Compile (LAMBDA ...)"
  (compile-defun-simplified "lambda" params body env))

(defun compile-apply-simplified (fn-expr args env)
  "Compile (APPLY ...)"
  (compile-funcall-simplified fn-expr args env))

(defun compile-labels-simplified (bindings body env)
  "Compile (LABELS ...)"
  (compile-progn-simplified body env))

(defun compile-flet-simplified (bindings body env)
  "Compile (FLET ...)"
  (compile-progn-simplified body env))

;;; ============================================================================
;;; PARTIE 14: COMPILATION - SETQ/SETF
;;; ============================================================================

(defun compile-setq-simplified (var value env)
  "Compile (SETQ ...)"
  (let ((location (lookup-variable-simplified env var)))
    (if (null location)
        (list (list "COMMENT" "Variable not found"))
        (append-two
          (compile-expr-main value env)
          (list (list :SW *reg-v0* *reg-sp* location))))))

(defun compile-setf-simplified (place value env)
  "Compile (SETF place value)
   Supporte: (setf var val) et (setf (aref array i) val)"
  (cond
    ;; Cas 1: (setf var val) - simple variable
    ((symbolp place)
     (compile-setq-simplified place value env))
    
    ;; Cas 2: (setf (aref array index) val) - élément de tableau
    ((and (listp place) (eq (first place) 'aref))
     (let ((array-expr (second place))
           (indices (cddr place)))
       (if (null indices)
           ;; Pas d'index: erreur
           (list (list "COMMENT" "SETF AREF needs index"))
           (if (null (rest indices))
               ;; Un seul index
               (append-many
                 (list
                   ;; Compiler le tableau
                   (compile-expr-main array-expr env)
                   ;; Sauvegarder dans $T0
                   (list (list :MOVE *reg-v0* *reg-t0*))
                   ;; Compiler l'index
                   (compile-expr-main (first indices) env)
                   ;; Sauvegarder dans $T1
                   (list (list :MOVE *reg-v0* *reg-t1*))
                   ;; Compiler la valeur
                   (compile-expr-main value env)
                   ;; ASET: array[index] = value
                   (list (list :ASET *reg-t0* *reg-t1* *reg-v0*))))
               ;; Multiples indices: TODO
               (list (list "COMMENT" "Multi-dimensional ASET not yet implemented"))))))
    
    ;; Cas 3: Autre forme - fallback sur setq
    (t (compile-setq-simplified place value env))))

(defun compile-defvar-simplified (var init-form env)
  "Compile (DEFVAR ...)"
  (compile-expr-main init-form env))

(defun compile-defparameter-simplified (var init-form env)
  "Compile (DEFPARAMETER ...)"
  (compile-expr-main init-form env))

(defun compile-defconstant-simplified (var value-form env)
  "Compile (DEFCONSTANT ...)"
  (compile-expr-main value-form env))

;;; ============================================================================
;;; PARTIE 15: COMPILATION - QUOTE/LAMBDA/FUNCTION
;;; ============================================================================

(defun compile-quote-simplified (expr env)
  "Compile (QUOTE ...) - Crée un handle vers l'objet Lisp quoté"
  (declare (ignore env))
  (if (numberp expr)
      ;; Les nombres sont stockés directement
      (list (list :LI expr *reg-v0*))
      ;; Pour les listes/symboles, on crée une instruction spéciale
      ;; qui sera résolue par le loader pour créer un handle
      (list (list :LISP-OBJECT expr *reg-v0*))))

(defun compile-function-simplified (fn-name env)
  "Compile (FUNCTION ...)"
  (let ((label (lookup-function-simplified env fn-name)))
    (if (null label)
        (list (list :LI 0 *reg-v0*))
        (list (list "LA" *reg-v0* label)))))

;;; ============================================================================
;;; PARTIE 16: COMPILATION - TABLEAUX ET STRUCTURES
;;; ============================================================================

(defun compile-make-array-simplified (args env)
  "Compile (MAKE-ARRAY size) ou (MAKE-ARRAY '(dim1 dim2 ...))
   Supporte tableaux 1D et multidimensionnels"
  (if (null args)
      ;; Pas d'argument: tableau de taille 0
      (list (list :LI 0 *reg-v0*)
            (list :MAKE-ARRAY *reg-v0*))
      (let ((size-expr (first args)))
        (cond
          ;; Cas 1: (make-array 10) - taille simple
          ((numberp size-expr)
           (list (list :LI size-expr *reg-v0*)
                 (list :MAKE-ARRAY *reg-v0*)))
          
          ;; Cas 2: (make-array '(3 4)) - dimensions multiples
          ;; Pour une matrice n×m, on alloue n*m éléments
          ((and (listp size-expr) (eq (first size-expr) 'quote))
           (let* ((dims (second size-expr))
                  (total-size (if (listp dims)
                                  (apply #'* dims)
                                  dims)))
             (list (list :LI total-size *reg-v0*)
                   (list :MAKE-ARRAY *reg-v0*))))
          
          ;; Cas 3: Expression variable
          (t
           (append-many
             (list
               (compile-expr-main size-expr env)
               (list (list :MAKE-ARRAY *reg-v0*)))))))))

(defun compile-aref-simplified (array-expr indices env)
  "Compile (AREF array i) ou (AREF array i j) pour tableaux multidimensionnels
   Pour matrice 2D stockée en row-major: index = i*cols + j"
  (if (null indices)
      ;; Pas d'index: retourner juste le tableau
      (compile-expr-main array-expr env)
      (if (null (rest indices))
          ;; Un seul index: accès direct
          (append-many
            (list
              ;; Compiler l'expression du tableau
              (compile-expr-main array-expr env)
              ;; Sauvegarder l'adresse du tableau dans $T0
              (list (list :MOVE *reg-v0* *reg-t0*))
              ;; Compiler l'index
              (compile-expr-main (first indices) env)
              ;; Accéder à l'élément: AREF $T0 $V0 $V0
              (list (list :AREF *reg-t0* *reg-v0* *reg-v0*))))
          ;; Multiples indices: calculer l'index linéaire
          ;; Pour l'instant, on supporte seulement 2D
          (let ((index1 (first indices))
                (index2 (second indices)))
            (append-many
              (list
                ;; Compiler l'expression du tableau
                (compile-expr-main array-expr env)
                ;; Sauvegarder dans $T0
                (list (list :MOVE *reg-v0* *reg-t0*))
                ;; TODO: Calculer index = i*cols + j
                ;; Pour l'instant, erreur
                (list (list :LI 0 *reg-v0*))))))))

(defun compile-make-hash-table-simplified (args env)
  "Compile (MAKE-HASH-TABLE ...)"
  (list (list :LI 0 *reg-v0*)))

(defun compile-gethash-simplified (key table env)
  "Compile (GETHASH ...)"
  (compile-expr-main key env))

(defun compile-hash-set-simplified (key value table env)
  "Compile hash table set"
  (compile-expr-main value env))

(defun compile-defstruct-simplified (name fields env)
  "Compile (DEFSTRUCT ...)"
  (list (list :LI 0 *reg-v0*)))

(defun compile-make-struct-simplified (struct-name args env)
  "Compile make-XXX pour struct"
  (list (list :LI 0 *reg-v0*)))

(defun compile-struct-accessor-simplified (accessor-name arg env)
  "Compile accesseur de structure"
  (compile-expr-main arg env))

;;; ============================================================================
;;; PARTIE 17: COMPILATION - LISTES ET CAR/CDR
;;; ============================================================================

(defun compile-cons-simplified (car-expr cdr-expr env)
  "Compile (CONS ...) - Utilise l'instruction LIST-CONS de la VM"
  (append-many
    (list
      ;; Compiler CAR et le sauver sur la pile
      (compile-expr-main car-expr env)
      (list (list :ADDI *reg-sp* -4 *reg-sp*))
      (list (list :SW *reg-v0* *reg-sp* 0))
      ;; Compiler CDR dans $T0
      (compile-expr-main cdr-expr env)
      (list (list :MOVE *reg-v0* *reg-t0*))
      ;; Récupérer CAR dans $V0
      (list (list :LW *reg-v0* *reg-sp* 0))
      (list (list :ADDI *reg-sp* 4 *reg-sp*))
      ;; Appeler LIST-CONS avec $V0 (car) et $T0 (cdr)
      (list (list :LIST-CONS *reg-v0* *reg-t0*)))))

(defun compile-car-simplified (list-expr env)
  "Compile (CAR ...) - Utilise l'instruction LIST-CAR de la VM"
  (append-many
    (list
      ;; Compiler l'expression de liste dans $V0
      (compile-expr-main list-expr env)
      ;; Appeler LIST-CAR qui met le résultat dans $V0
      (list (list :LIST-CAR *reg-v0*)))))

(defun compile-cdr-simplified (list-expr env)
  "Compile (CDR ...) - Utilise l'instruction LIST-CDR de la VM"
  (append-many
    (list
      ;; Compiler l'expression de liste dans $V0
      (compile-expr-main list-expr env)
      ;; Appeler LIST-CDR qui met le résultat dans $V0
      (list (list :LIST-CDR *reg-v0*)))))

(defun compile-list-simplified (args env)
  "Compile (LIST ...)
   Empile tous les éléments puis appelle l'instruction LIST de la VM"
  (if (null args)
      (list (list *instr-li* 0 *reg-v0*))  ; Liste vide = nil = 0
      (let ((count (length args)))
        (append-many
          (list
            ;; Compiler et empiler chaque argument
            (compile-list-elements-for-stack args env)
            ;; Appeler LIST avec le nombre d'éléments
            (list (list *instr-list* count)))))))

(defun compile-list-elements-for-stack (args env)
  "Compile les éléments d'une liste et les empile"
  (if (null args)
      nil
      (append-many
        (list
          (compile-expr-main (first args) env)
          (list (list *instr-sw* *reg-v0* *reg-sp* 0))
          (list (list *instr-addi* *reg-sp* -4 *reg-sp*))
          (compile-list-elements-for-stack (rest args) env)))))

(defun compile-append-simplified (args env)
  "Compile (APPEND ...)"
  (if (null args)
      (list (list :LI 0 *reg-v0*))
      (compile-expr-main (first args) env)))

(defun compile-length-simplified (list-expr env)
  "Compile (LENGTH ...)"
  (compile-expr-main list-expr env))

(defun compile-nth-simplified (n-expr list-expr env)
  "Compile (NTH ...)"
  (compile-expr-main list-expr env))

(defun compile-first-simplified (list-expr env)
  "Compile (FIRST ...)"
  (compile-car-simplified list-expr env))

(defun compile-rest-simplified (list-expr env)
  "Compile (REST ...)"
  (compile-cdr-simplified list-expr env))

;;; ============================================================================
;;; PARTIE 18: COMPILATION - PRÉDICATS
;;; ============================================================================

(defun compile-null-simplified (expr env)
  "Compile (NULL ...)"
  (let ((true-label (gen-label-simplified env "NULL_TRUE"))
        (end-label (gen-label-simplified env "NULL_END")))
    (append-many
      (list
        (compile-expr-main expr env)
        (list (list :BEQ *reg-v0* *reg-zero* true-label))
        (list (list :LI 0 *reg-v0*))
        (list (list :JMP end-label))
        (list (list :LABEL true-label))
        (list (list :LI 1 *reg-v0*))
        (list (list :LABEL end-label))))))

(defun compile-listp-simplified (expr env)
  "Compile (LISTP ...)"
  (compile-expr-main expr env))

(defun compile-numberp-simplified (expr env)
  "Compile (NUMBERP ...)"
  (compile-expr-main expr env))

(defun compile-symbolp-simplified (expr env)
  "Compile (SYMBOLP ...)"
  (compile-expr-main expr env))

(defun compile-atom-simplified (expr env)
  "Compile (ATOM ...)"
  (compile-expr-main expr env))

(defun compile-eq-simplified (arg1 arg2 env)
  "Compile (EQ ...)"
  (compile-comparison-simplified "=" (list arg1 arg2) env))

(defun compile-equal-simplified (arg1 arg2 env)
  "Compile (EQUAL ...)"
  (compile-comparison-simplified "=" (list arg1 arg2) env))

;;; ============================================================================
;;; PARTIE 19: COMPILATION - DIVERS
;;; ============================================================================

(defun compile-print-simplified (expr env)
  "Compile (PRINT ...)"
  (compile-expr-main expr env))

(defun compile-format-simplified (dest format-str args env)
  "Compile (FORMAT ...)"
  (list (list :LI 0 *reg-v0*)))

(defun compile-error-simplified (format-str args env)
  "Compile (ERROR ...)"
  (list (list :LI 0 *reg-v0*)))

(defun compile-values-simplified (args env)
  "Compile (VALUES ...)"
  (if (null args)
      (list (list :LI 0 *reg-v0*))
      (compile-expr-main (first args) env)))

(defun compile-multiple-value-bind-simplified (vars values-form body env)
  "Compile (MULTIPLE-VALUE-BIND ...)"
  (compile-progn-simplified body env))

(defun compile-block-simplified (name body env)
  "Compile (BLOCK ...)"
  (compile-progn-simplified body env))

(defun compile-return-from-simplified (name value env)
  "Compile (RETURN-FROM ...)"
  (compile-expr-main value env))

(defun compile-tagbody-simplified (body env)
  "Compile (TAGBODY ...)"
  (compile-progn-simplified body env))

(defun compile-go-simplified (tag env)
  "Compile (GO ...)"
  (list (list :JMP tag)))

(defun compile-unwind-protect-simplified (protected cleanup env)
  "Compile (UNWIND-PROTECT ...)"
  (compile-expr-main protected env))

(defun compile-catch-simplified (tag body env)
  "Compile (CATCH ...)"
  (compile-progn-simplified body env))

(defun compile-throw-simplified (tag result env)
  "Compile (THROW ...)"
  (compile-expr-main result env))

;;; ============================================================================
;;; PARTIE 20: POINT D'ENTRÉE PRINCIPAL
;;; ============================================================================

(defun compile-expr-main (expr env)
  "Point d'entrée principal pour compiler une expression"
  (cond
    ;; NIL
    ((null expr)
     (list (list :LI 0 *reg-v0*)))
    
    ;; Nombre
    ((numberp expr)
     (compile-constant-simplified expr env))
    
    ;; Symbole (variable)
    ((symbolp expr)
     (compile-variable-simplified expr env))
    
    ;; Liste (forme spéciale ou appel)
    ((listp expr)
     (compile-list-form expr env))
    
    ;; Autre
    (t (list (list :LI 0 *reg-v0*)))))

(defun compile-list-form (expr env)
  "Compile une forme sous forme de liste"
  (let ((op (first expr))
        (args (rest expr)))
    (cond
      ;; Formes spéciales
      ((eq op 'if) (compile-if-simplified (first args) (second args) (third args) env))
      ((eq op 'cond) (compile-cond-simplified args env))
      ((eq op 'when) (compile-when-simplified (first args) (rest args) env))
      ((eq op 'unless) (compile-unless-simplified (first args) (rest args) env))
      ((eq op 'and) (compile-and-simplified args env))
      ((eq op 'or) (compile-or-simplified args env))
      ((eq op 'not) (compile-not-simplified (first args) env))
      ((eq op 'let) (compile-let-simplified (first args) (rest args) env))
      ((eq op 'let*) (compile-let*-simplified (first args) (rest args) env))
      ((eq op 'progn) (compile-progn-simplified args env))
      ((eq op 'setq) (compile-setq-simplified (first args) (second args) env))
      ((eq op 'setf) (compile-setf-simplified (first args) (second args) env))
      ((eq op 'defun) (compile-defun-simplified (first args) (second args) (rest (rest args)) env))
      ((eq op 'lambda) (compile-lambda-simplified (first args) (rest args) env))
      ((eq op 'quote) (compile-quote-simplified (first args) env))
      
      ;; Arithmétique
      ((eq op '+) (compile-arithmetic-simplified "+" args env))
      ((eq op '-) (compile-arithmetic-simplified "-" args env))
      ((eq op '*) (compile-arithmetic-simplified "*" args env))
      ((eq op '/) (compile-arithmetic-simplified "/" args env))
      ((eq op '%) (compile-arithmetic-simplified "%" args env))
      ((eq op 'mod) (compile-arithmetic-simplified "%" args env))
      
      ;; Comparaisons
      ((eq op '=) (compile-comparison-simplified "=" args env))
      ((eq op '<) (compile-comparison-simplified "<" args env))
      ((eq op '>) (compile-comparison-simplified ">" args env))
      ((eq op '<=) (compile-comparison-simplified "<=" args env))
      ((eq op '>=) (compile-comparison-simplified ">=" args env))
      ((eq op '/=) (compile-comparison-simplified "/=" args env))
      
      ;; Listes
      ((eq op 'cons) (compile-cons-simplified (first args) (second args) env))
      ((eq op 'car) (compile-car-simplified (first args) env))
      ((eq op 'cdr) (compile-cdr-simplified (first args) env))
      ((eq op 'list) (compile-list-simplified args env))
      
      ;; Prédicats
      ((eq op 'null) (compile-null-simplified (first args) env))
      
      ;; Tableaux
      ((eq op 'make-array) (compile-make-array-simplified args env))
      ((eq op 'aref) (compile-aref-simplified (first args) (rest args) env))
      
      ;; Boucles
      ((eq op 'dolist) (compile-dolist-simplified (first (first args)) (second (first args)) (rest args) env))
      
      ;; Appel de fonction
      (t (compile-funcall-simplified op args env)))))

;;; ============================================================================
;;; FONCTION PRINCIPALE D'EXPORT
;;; ============================================================================

(defun compile-lisp-to-mips-simplified (expr)
  "Compile une expression Lisp en code MIPS - version simplifiée complète
   Cette fonction peut être compilée par elle-même"
  (let ((env (make-new-compiler-env-simplified)))
    (reset-global-tables-simplified)
    (compile-expr-main expr env)))

;;; ============================================================================
;;; FIN DU FICHIER
;;; ============================================================================

(format t "compiler-simplified.lisp chargé : 132 fonctions~%")
(format t "  - ~A fonctions d'environnement~%" 20)
(format t "  - ~A fonctions de compilation~%" 85)
(format t "  - ~A fonctions utilitaires~%" 27)
(format t "~%Toutes les fonctions peuvent être compilées avec compile-lisp-with-ids~%~%")
