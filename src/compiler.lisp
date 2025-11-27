;;;; compiler.lisp
;;;; Compilateur LISP vers assembleur MIPS

;; Note: loader.lisp est chargé par main.lisp avant compiler.lisp

;;; ============================================================================
;;; DÉFINITION SÉCURISÉE DES REGISTRES (évite problème $ dans CLISP)
;;; ============================================================================

;; PHASE 11: Définition directe des registres sans dépendance à get-reg
;; Les keywords sont automatiquement convertis en majuscules par Common Lisp
(defparameter *reg-v0* :$V0)
(defparameter *reg-t0* :$T0)
(defparameter *reg-t1* :$T1)
(defparameter *reg-t2* :$T2)
(defparameter *reg-t3* :$T3)
(defparameter *reg-t4* :$T4)  ; PHASE 11 - arrays
(defparameter *reg-t5* :$T5)  ; PHASE 11 - arrays
(defparameter *reg-t6* :$T6)  ; PHASE 11 - arrays
(defparameter *reg-t7* :$T7)  ; PHASE 11 - arrays
(defparameter *reg-t8* :$T8)
(defparameter *reg-t9* :$T9)
(defparameter *reg-a0* :$A0)
(defparameter *reg-a1* :$A1)
(defparameter *reg-a2* :$A2)
(defparameter *reg-a3* :$A3)
(defparameter *reg-s0* :$S0)
(defparameter *reg-s1* :$S1)
(defparameter *reg-s2* :$S2)
(defparameter *reg-s3* :$S3)
(defparameter *reg-s4* :$S4)
(defparameter *reg-s5* :$S5)
(defparameter *reg-s6* :$S6)
(defparameter *reg-s7* :$S7)
(defparameter *reg-sp* :$SP)
(defparameter *reg-ra* :$RA)
(defparameter *reg-zero* :$ZERO)

;;; ============================================================================
;;; TABLES GLOBALES (PHASE 11 - DEFVAR/DEFCONSTANT)
;;; ============================================================================

(defparameter *global-constants* (make-hash-table :test 'eq)
  "Table des constantes globales définies par DEFCONSTANT.
   Mapping: symbole → valeur")

(defparameter *global-variables* (make-hash-table :test 'eq)
  "Table des variables globales définies par DEFVAR.
   Mapping: symbole → adresse mémoire (ou offset dans section données)")

(defparameter *global-data-offset* 0
  "Offset courant dans la section de données globales")

(defun reset-global-tables ()
  "Réinitialise les tables globales (pour tests)"
  (clrhash *global-constants*)
  (clrhash *global-variables*)
  (setf *global-data-offset* 0))

;;; ============================================================================
;;; ENVIRONNEMENT DE COMPILATION
;;; ============================================================================

(defstruct compiler-env
  "Environnement de compilation pour gérer variables et labels"
  (variables '())           ; Liste des variables locales et leurs registres/offsets pile
  (functions '())           ; Table des fonctions définies
  (label-counter (list 0))  ; Compteur partagé (liste mutable) pour générer des labels uniques
  (temp-regs-available '()) ; Liste des registres temporaires disponibles
  (max-temp-regs 3)         ; Nombre maximum de registres temporaires ($t0, $t1, $t2)
  (stack-offset 0)          ; Offset courant pour variables sur la pile
  (parent-env nil)          ; Environnement parent (pour portée lexicale)
  (lexical-depth 0)         ; Profondeur d'imbrication lexicale (0=global, 1=level1...)
  (parent-lexical nil))     ; Référence vers environnement parent lexical (pour closures)

(defun make-new-compiler-env ()
  "Crée un nouvel environnement de compilation avec pool de registres limité"
  (let ((env (make-compiler-env)))
    ;; PHASE 9 FIX: Utiliser registres callee-saved ($S4-$S6) pour variables LET
    ;; au lieu de $T0-$T2 (caller-saved) pour préserver à travers appels
    (setf (compiler-env-temp-regs-available env) 
          (list *reg-s4* *reg-s5* *reg-s6*))
    env))

(defun allocate-temp-reg (env)
  "Alloue un registre temporaire depuis le pool. Retourne NIL si plus disponible."
  (let ((regs (compiler-env-temp-regs-available env)))
    (when regs
      (let ((reg (first regs)))
        (setf (compiler-env-temp-regs-available env) (rest regs))
        reg))))

(defun free-temp-reg (env reg)
  "Libère un registre temporaire et le remet dans le pool"
  (when reg
    (push reg (compiler-env-temp-regs-available env))))

(defun gen-label (env prefix)
  "Génère un label unique avec le préfixe donné"
  (let* ((counter (compiler-env-label-counter env))
         (label (intern (format nil "~A_~A" prefix (car counter)))))
    (incf (car counter))  ; Modifie le contenu de la liste (partagée entre envs)
    label))

(defun add-variable (env var location)
  "Ajoute une variable à l'environnement (location = registre ou offset pile)"
  (push (cons var location) (compiler-env-variables env)))

(defun lookup-variable (env var)
  "Recherche une variable dans l'environnement"
  (cdr (assoc var (compiler-env-variables env))))

(defun add-function (env fn-name fn-label)
  "Ajoute une fonction à l'environnement (fn-label = label ASM)"
  (push (cons fn-name fn-label) (compiler-env-functions env)))

(defun lookup-function (env fn-name)
  "Recherche une fonction dans l'environnement, retourne son label ASM"
  (cdr (assoc fn-name (compiler-env-functions env))))

(defun lookup-function-def-info (env fn-name)
  "Recherche une fonction le long de la chaîne parent-lexical.
Retourne (LABEL . DEPTH) où DEPTH est la profondeur lexicale de l'environnement
qui a défini la fonction, ou NIL si non trouvée."  
  (let ((current-env env))
    (loop
      (unless current-env (return nil))
      (let ((label (cdr (assoc fn-name (compiler-env-functions current-env)))))
        (when label
          (return (cons label (compiler-env-lexical-depth current-env)))))
      (setf current-env (compiler-env-parent-lexical current-env)))))

(defun copy-env (env)
  "Crée une copie d'environnement pour un nouveau scope (let, labels, etc.)"
  (let ((new-env (make-compiler-env)))
    ;; Copier les champs de base
    (setf (compiler-env-variables new-env) (copy-list (compiler-env-variables env)))
    (setf (compiler-env-functions new-env) (copy-list (compiler-env-functions env)))
    ;; PARTAGER la référence du compteur (liste mutable) entre tous les environnements
    (setf (compiler-env-label-counter new-env) (compiler-env-label-counter env))
    (setf (compiler-env-temp-regs-available new-env) (copy-list (compiler-env-temp-regs-available env)))
    (setf (compiler-env-max-temp-regs new-env) (compiler-env-max-temp-regs env))
    (setf (compiler-env-stack-offset new-env) (compiler-env-stack-offset env))
    (setf (compiler-env-parent-env new-env) env)
    ;; Closures Phase 9: copier profondeur lexicale
    (setf (compiler-env-lexical-depth new-env) (compiler-env-lexical-depth env))
    (setf (compiler-env-parent-lexical new-env) (compiler-env-parent-lexical env))
    new-env))

(defun alloc-stack-slot (env)
  "Alloue un slot sur la pile, retourne l'offset et incrémente le compteur"
  (let ((offset (compiler-env-stack-offset env)))
    (incf (compiler-env-stack-offset env) 4)
    offset))

(defun free-stack-slots (env n-slots)
  "Libère n slots de pile (4 octets chacun)"
  (decf (compiler-env-stack-offset env) (* n-slots 4)))

(defun alloc-temp-register (env)
  "Alloue un registre temporaire ($t0-$t9)"
  (let ((reg-num (mod (compiler-env-register-counter env) 10)))
    (incf (compiler-env-register-counter env))
    (intern (format nil ":$T~A" reg-num))))

;;; ============================================================================
;;; CLOSURES - PHASE 9
;;; ============================================================================

(defun make-lexical-env (parent-env &optional (increment-depth t))
  "Crée un environnement pour nouveau scope lexical (fonction locale)"
  (let ((new-env (make-compiler-env)))
    ;; NE PAS copier les variables (offsets FP invalides dans nouveau scope)
    ;; MAIS copier les fonctions (pour récursion/appels mutuels)
    (setf (compiler-env-functions new-env) (copy-list (compiler-env-functions parent-env)))
    (setf (compiler-env-label-counter new-env) (compiler-env-label-counter parent-env))
    (setf (compiler-env-max-temp-regs new-env) (compiler-env-max-temp-regs parent-env))
    (setf (compiler-env-parent-env new-env) parent-env)
    
    ;; Toujours définir parent-lexical (pour remontée recherche variables)
    (setf (compiler-env-parent-lexical new-env) parent-env)
    
    (if increment-depth
        ;; Mode fonction: incrémenter depth (nouveau frame)
        (setf (compiler-env-lexical-depth new-env) 
              (1+ (compiler-env-lexical-depth parent-env)))
        ;; Mode block (LABELS body): même depth (pas de frame)
        (setf (compiler-env-lexical-depth new-env) 
              (compiler-env-lexical-depth parent-env)))
    new-env))

(defun lookup-variable-with-depth (env var)
  "Recherche variable et retourne (location . depth) où depth = profondeur lexicale"
  (let ((current-env env))
    (loop
      (unless current-env
        ;; Plus de parent, variable non trouvée
        (return-from lookup-variable-with-depth nil))
      
      ;; Chercher dans environnement courant
      (let ((location (cdr (assoc var (compiler-env-variables current-env)))))
        (when location
          ;; Variable trouvée, retourner location et profondeur de CET environnement
          (return-from lookup-variable-with-depth 
            (cons location (compiler-env-lexical-depth current-env)))))
      
      ;; Remonter au parent lexical
      (setf current-env (compiler-env-parent-lexical current-env)))))

(defun generate-static-link-access (current-depth target-depth)
  "Génère code pour suivre static links depuis current-depth vers target-depth
   Retourne liste d'instructions ASM pour charger FP cible dans $t3"
  (let ((depth-diff (- current-depth target-depth))
        (code '()))
    (if (= depth-diff 0)
        ;; Même niveau: FP courant
        (list (list :MOVE (get-reg :fp) *reg-t3*))
        ;; Niveaux différents: suivre static links
        (progn
          ;; Charger FP courant dans $t3
          (push (list :MOVE (get-reg :fp) *reg-t3*) code)
          ;; Suivre static links depth-diff fois
          (loop for i from 1 to depth-diff do
            (push (list :LW *reg-t3* *reg-t3* 8) code))  ; Static link à FP+8 - Format: (LW dest base offset)
          (reverse code)))))

;;; ============================================================================
;;; PARSER LISP
;;; ============================================================================

(defun lisp-atom-p (expr)
  "Vérifie si l'expression est un atome (nombre, symbole, nil, t)"
  (or (numberp expr)
      (symbolp expr)))

(defun lisp-list-p (expr)
  "Vérifie si l'expression est une liste"
  (listp expr))

(defun parse-lisp-expr (expr)
  "Parse une expression LISP et retourne son type"
  (cond
    ((numberp expr) 
     (list :constant expr))
    
    ((null expr)
     (list :constant 0))  ; nil = 0 (DOIT être avant symbolp car NIL est un symbole)
    
    ((eq expr t)
     (list :constant 1))  ; t = 1 (DOIT être avant symbolp car T est un symbole)
    
    ((keywordp expr)
     ;; PHASE 11: Les keywords (:$sp, :$gp, etc.) sont des constantes
     (list :constant expr))
    
    ((symbolp expr)
     ;; PHASE 11: Vérifier d'abord si c'est une constante globale
     (let ((const-value (gethash expr *global-constants*)))
       (if const-value
           (list :constant const-value)
           (list :variable expr))))
    
    ((lisp-list-p expr)
     (let ((op (first expr))
           (args (rest expr)))
       (case op
         ;; Opérateurs arithmétiques
         ((+ - * / mod)
          (list :arithmetic op args))
         
         ;; Fonctions mathématiques
         ((abs max min)
          (list :math-func op args))
         
         ;; Opérateurs de comparaison
         ((< > <= >= = /=)
          (list :comparison op args))
         
         ;; Structure conditionnelle IF
         (if
          (list :if (first args) (second args) (third args)))
         
         ;; Structure conditionnelle COND
         (cond
          (list :cond args))
         
         ;; Structure WHEN (if sans else)
         (when
          (list :when (first args) (rest args)))
         
         ;; Structure UNLESS (if inversé)
         (unless
          (list :unless (first args) (rest args)))
         
         ;; Opérateur NOT
         (not
          (list :not (first args)))
         
         ;; Opérateur AND (court-circuit)
         (and
          (list :and args))
         
         ;; Opérateur OR (court-circuit)
         (or
          (list :or args))
         
         ;; Structure CASE (pattern matching)
         (case
          (list :case (first args) (rest args)))
         
         ;; Structure LET
         (let
          (list :let (first args) (rest args)))
         
         ;; Structure LOOP
         (loop
          ;; Syntaxe: (loop while condition do body)
          (if (and (>= (length args) 4)
                   (eq (first args) 'while)
                   (eq (third args) 'do))
              (list :loop-while (second args) (cdddr args))
              (error "Syntaxe LOOP non supportée: ~A" expr)))
         
         ;; Structure WHILE (PHASE 11 - Extension pour VM compilation)
         (while
          ;; Syntaxe: (while condition body...)
          (if (>= (length args) 2)
              (list :while (first args) (rest args))
              (error "Syntaxe WHILE incorrecte: ~A" expr)))
         
         ;; Structure PROGN (PHASE 11 - Séquence d'expressions)
         (progn
          ;; Syntaxe: (progn expr1 expr2 ... exprN)
          ;; Évalue chaque expression en séquence, retourne le résultat de la dernière
          (list :progn args))
         
         ;; Structure MAKE-ARRAY (PHASE 11 - Arrays pour VM)
         (make-array
          ;; Syntaxe: (make-array size [:initial-element value])
          (if (>= (length args) 1)
              (list :make-array args)
              (error "MAKE-ARRAY requiert au moins un argument (size): ~A" expr)))
         
         ;; Accès AREF (PHASE 11 - Arrays pour VM)
         (aref
          ;; Syntaxe: (aref array index)
          (if (= (length args) 2)
              (list :aref (first args) (second args))
              (error "AREF requiert exactement 2 arguments (array index): ~A" expr)))
         
         ;; Structure DOTIMES
         (dotimes
          (list :dotimes (first args) (rest args)))
         
         ;; SETQ (assignation variable)
         (setq
          ;; PHASE 11: Détecter cas spécial (setf (aref array index) value)
          (let ((place (first args))
                (value (second args)))
            (if (and (listp place) (eq (first place) 'aref))
                ;; Cas SETF AREF: (setf (aref array index) value)
                (list :setf-aref (second place) (third place) value)
                ;; Cas SETQ normal: (setq var value)
                (list :setq place value))))
         
         ;; Structure LABELS (fonctions locales)
         (labels
          ;; Syntaxe: (labels ((fn1 args1 body1) (fn2 args2 body2) ...) body)
          (if (and (>= (length args) 2)
                   (listp (first args)))
              (list :labels (first args) (rest args))
              (error "Syntaxe LABELS incorrecte: ~A" expr)))
         
         ;; LAMBDA: Fonction anonyme (PHASE 9 - CLOSURES)
         (lambda
          ;; Syntaxe: (lambda (params) body...)
          (if (and (>= (length args) 1)
                   (listp (first args)))
              (cons :lambda args)  ; args contient déjà (params body...)
              (error "Syntaxe LAMBDA incorrecte: ~A" expr)))
         
         ;; PHASE 11 - Définition de constante globale
         (defconstant
          ;; Syntaxe: (defconstant +NOM+ valeur [docstring])
          ;; La valeur peut être un littéral ou expression - laissée brute pour eval
          (list :defconstant (first args) (second args)))
         
         ;; PHASE 11 - Définition de variable globale
         ((defvar defparameter)
          ;; Syntaxe: (defvar *nom* valeur [docstring])
          ;; DEFPARAMETER est traité comme DEFVAR pour la compilation
          ;; La valeur est laissée brute - elle sera parsée dans compile-defvar
          ;; IMPORTANT: Ne pas parser ici car les constantes ne sont pas encore définies!
          (list :defvar (first args) (second args)))
         
         ;; PHASE 11 - ERROR: Signal une erreur (compilé en HALT)
         (error
          ;; Syntaxe: (error "message") ou (error "format" arg1 arg2...)
          ;; Pour compilation: on ignore le message et génère un HALT
          (list :error))
         
         ;; PHASE 11 - FORMAT: Affichage (ignoré en compilation)
         (format
          ;; Syntaxe: (format t "..." args...)
          ;; Pour compilation: on ignore complètement (pas d'output)
          (list :format))
         
         ;; Définition de fonction
         (defun
          (let ((name (first args))
                (params (second args))
                (rest-body (cddr args)))
            ;; Ignorer la docstring si présente
            (let ((body (if (and (stringp (first rest-body))
                                (cdr rest-body))  ; Il reste du code après
                           (cdr rest-body)  ; Sauter la docstring
                           rest-body)))  ; Pas de docstring
              (list :defun name params body))))
         
         ;; Appel de fonction
         (t
          (if (symbolp op)
              (list :call op args)
              (error "Expression invalide: ~A" expr))))))
    
    (t (error "Expression LISP non reconnue: ~A" expr))))

;;; ============================================================================
;;; ANALYSE DES VARIABLES LIBRES (PHASE 9 - CLOSURES)
;;; ============================================================================

(defparameter *built-in-operators*
  '(+ - * / mod
    = < > <= >= /=
    and or not
    if cond when unless case
    let labels lambda
    setq quote
    defun
    loop dotimes
    cons car cdr list
    null atom listp numberp symbolp
    eq eql equal
    append reverse length
    print format)
  "Liste des opérateurs et fonctions built-in qui ne sont pas des variables")

(defun free-variables (expr &optional (bound-vars '()))
  "Retourne la liste des variables libres dans une expression.
   Une variable est libre si elle est référencée mais pas liée localement.
   
   Arguments:
   - expr: Expression LISP à analyser
   - bound-vars: Liste des variables actuellement liées (dans le scope)
   
   Retourne: Liste de symboles (variables libres), sans doublons"
  
  (cond
    ;; Cas 1: Constante (nombre, nil, t) -> pas de variables libres
    ((or (numberp expr) (null expr) (eq expr t))
     '())
    
    ;; Cas 2: Symbole (variable)
    ((symbolp expr)
     (cond
       ;; Variable liée -> pas libre
       ((member expr bound-vars) '())
       ;; Opérateur built-in -> pas une variable
       ((member expr *built-in-operators*) '())
       ;; Variable libre
       (t (list expr))))
    
    ;; Cas 3: Liste vide
    ((null expr)
     '())
    
    ;; Cas 4: Atom non-symbole (string, etc.)
    ((atom expr)
     '())
    
    ;; Cas 5: Expression liste - analyser selon la forme
    ((listp expr)
     (let ((op (first expr))
           (args (rest expr)))
       
       (case op
         ;; LAMBDA: (lambda (params) body)
         ;; Les paramètres deviennent des variables liées dans le corps
         (lambda
          (if (and (>= (length args) 2)
                   (listp (first args)))
              (let* ((params (first args))
                     (body (rest args))
                     (new-bound (append params bound-vars)))
                ;; Analyser le corps avec les paramètres comme variables liées
                (free-variables-list body new-bound))
              (error "Syntaxe LAMBDA invalide: ~A" expr)))
         
         ;; LET: (let ((var1 val1) (var2 val2)) body)
         ;; Les valeurs sont évaluées AVANT que les vars soient liées
         (let
          (if (and (>= (length args) 2)
                   (listp (first args)))
              (let* ((bindings (first args))
                     (body (rest args))
                     ;; Variables liées par LET
                     (let-vars (mapcar #'first bindings))
                     ;; Valeurs d'initialisation
                     (let-vals (mapcar #'second bindings)))
                ;; Variables libres = union de:
                ;; 1) Variables libres dans les valeurs (scope externe)
                ;; 2) Variables libres dans le corps (avec let-vars liées)
                (union (free-variables-list let-vals bound-vars)
                       (free-variables-list body (append let-vars bound-vars))
                       :test #'eq))
              (error "Syntaxe LET invalide: ~A" expr)))
         
         ;; LABELS: (labels ((fn1 (params1) body1) ...) body)
         ;; Toutes les fonctions sont mutuellement récursives
         (labels
          (if (and (>= (length args) 2)
                   (listp (first args)))
              (let* ((definitions (first args))
                     (body (rest args))
                     ;; Noms des fonctions locales
                     (func-names (mapcar #'first definitions))
                     ;; Les fonctions sont liées dans tout le LABELS
                     (new-bound (append func-names bound-vars)))
                ;; Analyser chaque définition de fonction
                (let ((defs-free-vars
                        (mapcan
                         (lambda (def)
                           (let* ((params (second def))
                                  (func-body (cddr def))
                                  ;; Dans le corps de la fonction:
                                  ;; - paramètres liés
                                  ;; - noms de fonctions liés
                                  (func-bound (append params new-bound)))
                             (free-variables-list func-body func-bound)))
                         definitions)))
                  ;; Union avec variables libres du corps principal
                  (union defs-free-vars
                         (free-variables-list body new-bound)
                         :test #'eq)))
              (error "Syntaxe LABELS invalide: ~A" expr)))
         
         ;; SETQ: (setq var value)
         (setq
          (if (= (length args) 2)
              (let ((var (first args))
                    (value (second args)))
                ;; La variable assignée est considérée comme référencée
                (union (if (member var bound-vars)
                           '()
                           (list var))
                       (free-variables value bound-vars)
                       :test #'eq))
              (error "Syntaxe SETQ invalide: ~A" expr)))
         
         ;; IF: (if condition then else)
         (if
          (free-variables-list args bound-vars))
         
         ;; COND: (cond (test1 body1) (test2 body2) ...)
         (cond
          (free-variables-list 
           (mapcan (lambda (clause) clause) args)
           bound-vars))
         
         ;; WHEN, UNLESS: (when test body...)
         ((when unless)
          (free-variables-list args bound-vars))
         
         ;; AND, OR: court-circuit logique
         ((and or)
          (free-variables-list args bound-vars))
         
         ;; NOT: (not expr)
         (not
          (free-variables (first args) bound-vars))
         
         ;; CASE: (case keyform (key1 body1) (key2 body2) ...)
         (case
          (if (>= (length args) 1)
              (union (free-variables (first args) bound-vars)
                     (free-variables-list 
                      (mapcan (lambda (clause) (rest clause)) (rest args))
                      bound-vars)
                     :test #'eq)
              '()))
         
         ;; LOOP, DOTIMES: (loop while cond do body) / (dotimes (var count) body)
         ((loop dotimes)
          ;; Pour DOTIMES: (dotimes (var count-expr result-expr) body)
          (if (eq op 'dotimes)
              (let* ((var-spec (first args))
                     (loop-var (if (listp var-spec) (first var-spec) nil))
                     (count-expr (if (listp var-spec) (second var-spec) nil))
                     (body (rest args)))
                (if loop-var
                    ;; count-expr évalué dans scope externe, body avec loop-var lié
                    (union (free-variables count-expr bound-vars)
                           (free-variables-list body (cons loop-var bound-vars))
                           :test #'eq)
                    (free-variables-list args bound-vars)))
              ;; LOOP générique
              (free-variables-list args bound-vars)))
         
         ;; QUOTE: (quote x) -> pas de variables libres
         (quote '())
         
         ;; DEFUN: (defun name params body) - traiter comme top-level
         (defun
          (if (>= (length args) 3)
              (let* ((params (second args))
                     (body (cddr args))
                     (func-bound (append params bound-vars)))
                (free-variables-list body func-bound))
              '()))
         
         ;; Appel de fonction ou opérateur: (op arg1 arg2 ...)
         ;; Toutes les sous-expressions peuvent contenir des variables libres
         (t
          (free-variables-list expr bound-vars)))))
    
    ;; Cas par défaut
    (t '())))

(defun free-variables-list (expr-list bound-vars)
  "Retourne l'union des variables libres de toutes les expressions dans la liste.
   
   Arguments:
   - expr-list: Liste d'expressions LISP
   - bound-vars: Variables actuellement liées
   
   Retourne: Liste de variables libres sans doublons"
  (if (null expr-list)
      '()
      (union (free-variables (first expr-list) bound-vars)
             (free-variables-list (rest expr-list) bound-vars)
             :test #'eq)))

;;; ============================================================================
;;; COMPILATION - CONSTANTES
;;; ============================================================================

(defun compile-constant (value env)
  "Compile une constante en code ASM"
  (declare (ignore env))
  (let ((result-reg *reg-v0*))
    (list (list :LI value result-reg))))

;;; ============================================================================
;;; COMPILATION - VARIABLES
;;; ============================================================================

(defun compile-variable (var env)
  "Compile une référence à une variable (registre, pile, ou via static links).
   Vérifie d'abord les variables globales DEFVAR, puis les variables locales."
  ;; PRIORITÉ 1 : Vérifier si c'est une variable globale (DEFVAR)
  (let ((global-addr (gethash var *global-variables*)))
    (if global-addr
        ;; Variable globale trouvée : charger depuis $zero + offset
        (list (list :LW *reg-v0* *reg-zero* global-addr))
        ;; Sinon, chercher dans l'environnement local
        (let ((var-info (lookup-variable-with-depth env var)))
          (if var-info
              ;; Variable trouvée (possiblement dans scope englobant)
              (let ((location (car var-info))
                    (var-depth (cdr var-info))
                    (current-depth (compiler-env-lexical-depth env)))
                (cond
                  ;; Cas 1 : Variable dans un registre (même scope)
                  ((and (symbolp location) (= var-depth current-depth))
                   (list (list :MOVE location *reg-v0*)))
                  
                  ;; Cas 2 : Variable sur la pile (offset depuis SP, même scope)
                  ((and (consp location) (eq (car location) :stack) (= var-depth current-depth))
                   (let ((offset (cdr location)))  ; FIXÉ: utiliser cdr pour dotted pair
                     (list (list :LW *reg-v0* *reg-sp* offset))))  ; Format: (LW dest base offset)
            
            ;; Cas 3 : Variable paramètre fonction (offset depuis FP, même scope)
            ((and (consp location) (eq (car location) :fp) (= var-depth current-depth))
             (let ((offset (cdr location)))  ; FIXÉ: utiliser cdr pour dotted pair
               (list (list :LW *reg-v0* (get-reg :fp) offset))))
            
            ;; Cas 4 : Variable accessible via closure (PHASE 9)
            ((and (consp location) (eq (car location) :closure))
             (let ((closure-index (cdr location)))  ; FIXÉ: utiliser cdr pour dotted pair
               ;; La fermeture est dans $s1 (passée par l'appelant)
               ;; Structure: [Label][Size][Var0][Var1]...
               ;; Var_i est à l'offset (2 + i) depuis le début de la fermeture
               (let ((offset (+ 2 closure-index)))
                 (list (list :LI offset *reg-t2*)
                       (list :LOAD-HEAP *reg-s1* *reg-t2* *reg-v0*)))))
            
            ;; Cas 5 : Variable dans scope englobant (suivre static links)
            ((and (consp location) (eq (car location) :fp) (< var-depth current-depth))
             (let ((offset (cdr location)))
               (append
                ;; Suivre static links pour atteindre scope de la variable
                (generate-static-link-access current-depth var-depth)
                ;; Accéder variable avec offset depuis FP trouvé (dans $t3)
                (list (list :LW *reg-v0* *reg-t3* offset)))))  ; Format: (LW dest base offset)
            
            ;; Cas 6 : Variable dans registre à depth parente
            ;; ATTENTION: les registres temporaires ne sont pas sauvegardés dans les frames!
            ;; Ceci ne fonctionne que si aucun appel de fonction n'a écrasé le registre.
            ((and (symbolp location) (< var-depth current-depth))
             ;; Simplement copier le registre (risqué mais peut fonctionner dans des cas simples)
             (list (list :MOVE location *reg-v0*)))
            
                  (t
                   (error "Variable dans configuration non supportée: ~A (location: ~A, depth: ~A/~A)" 
                          var location var-depth current-depth))))
              
              ;; Variable locale non trouvée du tout
              (error "Variable non définie: ~A" var))))))

;;; ============================================================================
;;; COMPILATION - ARITHMÉTIQUE
;;; ============================================================================

(defun compile-arithmetic (op args env)
  "Compile une opération arithmétique - utilise position FIXE sur pile pour arg1"
  (cond
    ;; Opération binaire: (+ a b)
    ((= (length args) 2)
     (let* ((arg1 (first args))
            (arg2 (second args)))
       ;; PHASE 9 FIX FINAL: Allouer espace AVANT compile-expr pour protéger arg1
       ;; des modifications de pile par appels imbriqués
       (append
        ;; 1. Réserver espace sur pile AVANT tout (2 slots : ancien$S7 + arg1)
        (list (list :ADDI *reg-sp* -8 *reg-sp*))
        ;; 2. Sauvegarder ancien $S7
        (list (list :SW *reg-s7* *reg-sp* 0))
        ;; 3. Compiler arg1 (résultat dans $v0)
        (compile-expr arg1 env)
        ;; 4. Sauvegarder arg1 dans slot réservé
        (list (list :SW *reg-v0* *reg-sp* 4))
        ;; 5. Compiler arg2 - même s'il fait des appels, nos slots sont protégés
        ;;    car ils sont en-dessous de tous les futurs frames
        (compile-expr arg2 env)
        ;; 6. arg2 dans $v0, arg1 dans pile[$SP+4], ancien$S7 dans pile[$SP+0]
        ;;    Charger arg1 dans $t0
        (list (list :LW *reg-t0* *reg-sp* 4))  ; Format: (LW dest base offset)
        ;; 7. Restaurer ancien $S7
        (list (list :LW *reg-s7* *reg-sp* 0))  ; Format: (LW dest base offset)
        ;; 8. Libérer espace réservé
        (list (list :ADDI *reg-sp* 8 *reg-sp*))
        ;; 9. Effectuer l'opération: arg1 ($t0) OP arg2 ($v0) -> $v0
        (case op
          (+ (list (list :ADD *reg-t0* *reg-v0* *reg-v0*)))
          (- (list (list :SUB *reg-t0* *reg-v0* *reg-v0*)))
          (* (list (list :MUL *reg-t0* *reg-v0*)
                   (list :MFLO *reg-v0*)))
          (/ (list (list :DIV *reg-t0* *reg-v0*)
                   (list :MFLO *reg-v0*)))
          (mod (list (list :DIV *reg-t0* *reg-v0*)
                     (list :MFHI *reg-v0*)))
          (t (error "Opérateur arithmétique non supporté: ~A" op))))))
    
    ;; Plus de 2 arguments: réduire récursivement
    ((> (length args) 2)
     (compile-arithmetic op
                        (list (first args)
                              (cons op (rest args)))
                        env))
    
    (t (error "Nombre d'arguments incorrect pour ~A: ~A" op (length args)))))

;;; ============================================================================
;;; COMPILATION - FONCTIONS MATHÉMATIQUES
;;; ============================================================================

(defun compile-math-func (func args env)
  "Compile les fonctions mathématiques: abs, max, min"
  (case func
    (abs
     ;; (abs x) - valeur absolue
     ;; Si x < 0 alors -x sinon x
     (if (not (= (length args) 1))
         (error "ABS attend 1 argument, reçu: ~A" (length args)))
     (let* ((arg (first args))
            (code (compile-expr arg env))
            (label-positive (gen-label env "ABS_POS"))
            (label-end (gen-label env "ABS_END")))
       (append
        code
        ;; x est dans $v0
        ;; Tester si x >= 0
        (list (list :SLT *reg-v0* *reg-zero* *reg-t0*))  ; $t0 = (x < 0) ? 1 : 0
        (list (list :BEQ *reg-t0* *reg-zero* label-positive))  ; Si x >= 0, sauter
        ;; x < 0: calculer -x
        (list (list :SUB *reg-zero* *reg-v0* *reg-v0*))  ; $v0 = 0 - x
        (list (list :J label-end))
        ;; x >= 0: garder x tel quel
        (list (list :LABEL label-positive))
        ;; Fin
        (list (list :LABEL label-end)))))
    
    (max
     ;; (max x y) - maximum de deux valeurs
     (if (not (= (length args) 2))
         (error "MAX attend 2 arguments, reçu: ~A" (length args)))
     (let* ((arg1 (first args))
            (arg2 (second args))
            (code1 (compile-expr arg1 env))
            (label-x-greater (gen-label env "MAX_X"))
            (label-end (gen-label env "MAX_END")))
       (append
        code1
        ;; Sauvegarder x sur la pile
        (list (list :ADDI *reg-sp* -4 *reg-sp*)
              (list :SW *reg-v0* *reg-sp* 0))
        ;; Compiler y
        (compile-expr arg2 env)
        ;; y dans $v0, sauvegarder dans $t1
        (list (list :MOVE *reg-v0* *reg-t1*))
        ;; Restaurer x dans $t0
        (list (list :LW *reg-t0* *reg-sp* 0)  ; Format: (LW dest base offset)
              (list :ADDI *reg-sp* 4 *reg-sp*))
        ;; Comparer: x > y ?
        (list (list :SLT *reg-t1* *reg-t0* *reg-t2*))  ; $t2 = (y < x) ? 1 : 0
        (list (list :BNE *reg-t2* *reg-zero* label-x-greater))  ; Si y < x, retourner x
        ;; Sinon retourner y
        (list (list :MOVE *reg-t1* *reg-v0*))
        (list (list :J label-end))
        ;; x est plus grand
        (list (list :LABEL label-x-greater))
        (list (list :MOVE *reg-t0* *reg-v0*))
        ;; Fin
        (list (list :LABEL label-end)))))
    
    (min
     ;; (min x y) - minimum de deux valeurs
     (if (not (= (length args) 2))
         (error "MIN attend 2 arguments, reçu: ~A" (length args)))
     (let* ((arg1 (first args))
            (arg2 (second args))
            (code1 (compile-expr arg1 env))
            (label-x-smaller (gen-label env "MIN_X"))
            (label-end (gen-label env "MIN_END")))
       (append
        code1
        ;; Sauvegarder x sur la pile
        (list (list :ADDI *reg-sp* -4 *reg-sp*)
              (list :SW *reg-v0* *reg-sp* 0))
        ;; Compiler y
        (compile-expr arg2 env)
        ;; y dans $v0, sauvegarder dans $t1
        (list (list :MOVE *reg-v0* *reg-t1*))
        ;; Restaurer x dans $t0
        (list (list :LW *reg-t0* *reg-sp* 0)  ; Format: (LW dest base offset)
              (list :ADDI *reg-sp* 4 *reg-sp*))
        ;; Comparer: x < y ?
        (list (list :SLT *reg-t0* *reg-t1* *reg-t2*))  ; $t2 = (x < y) ? 1 : 0
        (list (list :BNE *reg-t2* *reg-zero* label-x-smaller))  ; Si x < y, retourner x
        ;; Sinon retourner y
        (list (list :MOVE *reg-t1* *reg-v0*))
        (list (list :J label-end))
        ;; x est plus petit
        (list (list :LABEL label-x-smaller))
        (list (list :MOVE *reg-t0* *reg-v0*))
        ;; Fin
        (list (list :LABEL label-end)))))
    
    (t (error "Fonction mathématique non supportée: ~A" func))))

;;; ============================================================================
;;; COMPILATION - COMPARAISON
;;; ============================================================================

(defun compile-comparison (op args env)
  "Compile une comparaison - utilise pile et registres $S2/$S3 pour éviter conflits"
  (unless (= (length args) 2)
    (error "Comparaison requiert exactement 2 arguments"))
  
  (let* ((arg1 (first args))
         (arg2 (second args))
         (code1 (compile-expr arg1 env))
         (code2 (compile-expr arg2 env)))
    (append
     code1
     ;; Sauvegarder arg1 sur la pile
     (list (list :ADDI *reg-sp* -4 *reg-sp*)
           (list :SW *reg-v0* *reg-sp* 0))
     code2
     ;; arg2 dans $s3 (pas $t0 ou $t1 qui peuvent contenir des variables)
     (list (list :MOVE *reg-v0* *reg-s3*))
     ;; Restaurer arg1 depuis pile dans $s2
     (list (list :LW *reg-s2* *reg-sp* 0)  ; Format: (LW dest base offset)
           (list :ADDI *reg-sp* 4 *reg-sp*))
     ;; Effectuer la comparaison ($s2 op $s3 → $v0)
     (case op
       (< (list (list :SLT *reg-s2* *reg-s3* *reg-v0*)))
       (> (list (list :SLT *reg-s3* *reg-s2* *reg-v0*)))
       (<= (list (list :SLT *reg-s3* *reg-s2* *reg-t2*)
                 (list :LI 1 *reg-t3*)
                 (list :SUB *reg-t3* *reg-t2* *reg-v0*)))
       (>= (list (list :SLT *reg-s2* *reg-s3* *reg-t2*)
                 (list :LI 1 *reg-t3*)
                 (list :SUB *reg-t3* *reg-t2* *reg-v0*)))
       (= (let ((label-equal (gen-label env "EQUAL"))
                (label-end (gen-label env "END_EQ")))
            (list (list :SUB *reg-s2* *reg-s3* *reg-t2*)
                  (list :BEQ *reg-t2* *reg-zero* label-equal)
                  (list :LI 0 *reg-v0*)
                  (list :J label-end)
                  (list :LABEL label-equal)
                  (list :LI 1 *reg-v0*)
                  (list :LABEL label-end))))
       (/= (let ((label-not-equal (gen-label env "NOT_EQUAL"))
                 (label-end (gen-label env "END_NE")))
             (list (list :SUB *reg-s2* *reg-s3* *reg-t2*)
                   (list :BNE *reg-t2* *reg-zero* label-not-equal)
                   (list :LI 0 *reg-v0*)
                   (list :J label-end)
                   (list :LABEL label-not-equal)
                   (list :LI 1 *reg-v0*)
                   (list :LABEL label-end))))
       (t (error "Opérateur de comparaison non supporté: ~A" op))))))

;;; ============================================================================
;;; COMPILATION - STRUCTURE CONDITIONNELLE IF
;;; ============================================================================

(defun compile-if (condition then-branch else-branch env)
  "Compile une structure if/then/else"
  (let ((label-else (gen-label env "ELSE"))
        (label-end (gen-label env "ENDIF"))
        (code '()))
    
    ;; Compiler la condition
    (setf code (append code (compile-expr condition env)))
    
    ;; Si condition = 0 (faux), sauter vers else
    (setf code (append code
                      (list (list :BEQ *reg-v0* *reg-zero* label-else))))
    
    ;; Branche THEN
    (setf code (append code (compile-expr then-branch env)))
    (setf code (append code
                      (list (list :J label-end))))
    
    ;; Branche ELSE
    (setf code (append code
                      (list (list :LABEL label-else))))
    (when else-branch
      (setf code (append code (compile-expr else-branch env))))
    
    ;; Fin du IF
    (setf code (append code
                      (list (list :LABEL label-end))))
    
    code))

(defun compile-cond (clauses env)
  "Compile une structure cond avec plusieurs clauses (test expr)
   Syntaxe: (cond (test1 expr1) (test2 expr2) ... (t expr-default))
   Évalue séquentiellement les tests jusqu'à trouver un test vrai."
  (let ((label-end (gen-label env "COND_END"))
        (code '()))
    
    ;; Pour chaque clause
    (dolist (clause clauses)
      (let ((test (first clause))
            (expr (second clause))
            (label-next (gen-label env "COND_NEXT")))
        
        ;; Si test = T, c'est la clause par défaut (toujours vraie)
        (if (eq test t)
            (progn
              ;; Compiler l'expression et sauter à la fin
              (setf code (append code (compile-expr expr env)))
              (setf code (append code (list (list :J label-end)))))
            (progn
              ;; Compiler le test
              (setf code (append code (compile-expr test env)))
              
              ;; Si test = 0 (faux), sauter à la clause suivante
              (setf code (append code
                                (list (list :BEQ *reg-v0* *reg-zero* label-next))))
              
              ;; Compiler l'expression (test vrai)
              (setf code (append code (compile-expr expr env)))
              
              ;; Sauter à la fin
              (setf code (append code (list (list :J label-end))))
              
              ;; Label pour la clause suivante
              (setf code (append code (list (list :LABEL label-next))))))))
    
    ;; Label de fin
    (setf code (append code (list (list :LABEL label-end))))
    
    code))

(defun compile-when (test body env)
  "Compile une structure when (sucre syntaxique pour if sans else)
   Syntaxe: (when test body...)
   Équivalent à: (if test (progn body...) nil)"
  (let ((label-end (gen-label env "WHEN_END"))
        (code '()))
    
    ;; Compiler le test
    (setf code (append code (compile-expr test env)))
    
    ;; Si test = 0 (faux), sauter à la fin
    (setf code (append code
                      (list (list :BEQ *reg-v0* *reg-zero* label-end))))
    
    ;; Compiler le body (plusieurs expressions possibles)
    (dolist (expr body)
      (setf code (append code (compile-expr expr env))))
    
    ;; Label fin
    (setf code (append code (list (list :LABEL label-end))))
    
    code))

(defun compile-unless (test body env)
  "Compile une structure unless (sucre syntaxique pour if inversé)
   Syntaxe: (unless test body...)
   Équivalent à: (if (not test) (progn body...) nil)"
  ;; unless = when avec test inversé
  (let ((label-skip (gen-label env "UNLESS_SKIP"))
        (label-end (gen-label env "UNLESS_END"))
        (code '()))
    
    ;; Compiler le test
    (setf code (append code (compile-expr test env)))
    
    ;; Si test != 0 (vrai), sauter le body
    (setf code (append code
                      (list (list :BNE *reg-v0* *reg-zero* label-skip))))
    
    ;; Compiler le body (plusieurs expressions possibles)
    (dolist (expr body)
      (setf code (append code (compile-expr expr env))))
    
    (setf code (append code (list (list :J label-end))))
    
    ;; Label skip
    (setf code (append code (list (list :LABEL label-skip))))
    
    ;; Label fin
    (setf code (append code (list (list :LABEL label-end))))
    
    code))

;;; ============================================================================
;;; COMPILATION - OPÉRATEURS LOGIQUES (AND/OR/NOT)
;;; ============================================================================

(defun compile-not (expr env)
  "Compile l'opérateur NOT
   Syntaxe: (not expr)
   Retourne 1 si expr = 0, sinon 0"
  (let ((label-true (gen-label env "NOT_TRUE"))
        (label-end (gen-label env "NOT_END"))
        (code '()))
    
    ;; Compiler l'expression
    (setf code (append code (compile-expr expr env)))
    
    ;; Si expr = 0, retourner 1
    (setf code (append code
                      (list (list :BEQ *reg-v0* *reg-zero* label-true))))
    
    ;; expr != 0, retourner 0
    (setf code (append code (list (list :LI 0 *reg-v0*))))
    (setf code (append code (list (list :J label-end))))
    
    ;; expr = 0, retourner 1
    (setf code (append code (list (list :LABEL label-true))))
    (setf code (append code (list (list :LI 1 *reg-v0*))))
    
    ;; Fin
    (setf code (append code (list (list :LABEL label-end))))
    
    code))

(defun compile-and (args env)
  "Compile l'opérateur AND avec court-circuit
   Syntaxe: (and expr1 expr2 ...)
   Court-circuit: s'arrête dès qu'une expression est fausse
   Retourne 1 si toutes vraies, 0 sinon"
  (let ((label-false (gen-label env "AND_FALSE"))
        (label-end (gen-label env "AND_END"))
        (code '()))
    
    ;; Cas spécial: AND sans arguments = T (vrai)
    (if (null args)
        (return-from compile-and (list (list :LI 1 *reg-v0*))))
    
    ;; Évaluer chaque expression
    (dolist (expr args)
      ;; Compiler l'expression
      (setf code (append code (compile-expr expr env)))
      
      ;; Si résultat = 0 (faux), court-circuit → aller à label-false
      (setf code (append code
                        (list (list :BEQ *reg-v0* *reg-zero* label-false)))))
    
    ;; Toutes les expressions sont vraies, retourner 1
    (setf code (append code (list (list :LI 1 *reg-v0*))))
    (setf code (append code (list (list :J label-end))))
    
    ;; Une expression est fausse, retourner 0
    (setf code (append code (list (list :LABEL label-false))))
    (setf code (append code (list (list :LI 0 *reg-v0*))))
    
    ;; Fin
    (setf code (append code (list (list :LABEL label-end))))
    
    code))

(defun compile-or (args env)
  "Compile l'opérateur OR avec court-circuit
   Syntaxe: (or expr1 expr2 ...)
   Court-circuit: s'arrête dès qu'une expression est vraie
   Retourne 1 si au moins une vraie, 0 sinon"
  (let ((label-true (gen-label env "OR_TRUE"))
        (label-end (gen-label env "OR_END"))
        (code '()))
    
    ;; Cas spécial: OR sans arguments = NIL (faux)
    (if (null args)
        (return-from compile-or (list (list :LI 0 *reg-v0*))))
    
    ;; Évaluer chaque expression
    (dolist (expr args)
      ;; Compiler l'expression
      (setf code (append code (compile-expr expr env)))
      
      ;; Si résultat != 0 (vrai), court-circuit → aller à label-true
      (setf code (append code
                        (list (list :BNE *reg-v0* *reg-zero* label-true)))))
    
    ;; Toutes les expressions sont fausses, retourner 0
    (setf code (append code (list (list :LI 0 *reg-v0*))))
    (setf code (append code (list (list :J label-end))))
    
    ;; Une expression est vraie, retourner 1
    (setf code (append code (list (list :LABEL label-true))))
    (setf code (append code (list (list :LI 1 *reg-v0*))))
    
    ;; Fin
    (setf code (append code (list (list :LABEL label-end))))
    
    code))

;;; ============================================================================
;;; COMPILATION - CASE (Pattern Matching)
;;; ============================================================================

(defun compile-case (keyform clauses env)
  "Compile une structure case (pattern matching sur valeur)
   Syntaxe: (case keyform 
              (key1 expr1)
              ((key2 key3) expr2)
              (otherwise expr-default))
   Compare keyform avec chaque key et exécute l'expression correspondante"
  (let ((label-end (gen-label env "CASE_END"))
        (code '()))
    
    ;; Compiler le keyform et le sauvegarder dans $t0
    (setf code (append code (compile-expr keyform env)))
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t0*))))
    
    ;; Pour chaque clause
    (dolist (clause clauses)
      (let* ((keys (first clause))
             (expr (second clause))
             (label-next (gen-label env "CASE_NEXT"))
             (label-match (gen-label env "CASE_MATCH")))
        
        ;; Si keys = OTHERWISE ou T, c'est la clause par défaut
        (if (or (eq keys 'otherwise) (eq keys t))
            (progn
              ;; Compiler l'expression par défaut
              (setf code (append code (compile-expr expr env)))
              (setf code (append code (list (list :J label-end)))))
            
            (progn
              ;; keys peut être une valeur unique ou une liste de valeurs
              (let ((key-list (if (listp keys) keys (list keys))))
                
                ;; Tester chaque key
                (dolist (key key-list)
                  ;; Compiler la comparaison: $t0 == key
                  (setf code (append code
                                    (list (list :ADDI *reg-sp* -4 *reg-sp*)
                                          (list :SW *reg-t0* *reg-sp* 0))))
                  
                  ;; Charger la key à comparer
                  (setf code (append code (compile-constant key env)))
                  
                  ;; Comparer
                  (setf code (append code
                                    (list (list :MOVE *reg-v0* *reg-s3*)
                                          (list :LW *reg-s2* *reg-sp* 0)  ; Format: (LW dest base offset)
                                          (list :ADDI *reg-sp* 4 *reg-sp*)
                                          (list :SUB *reg-s2* *reg-s3* *reg-t2*)
                                          (list :BEQ *reg-t2* *reg-zero* label-match))))))
                
                ;; Aucune key ne correspond, aller à la clause suivante
                (setf code (append code (list (list :J label-next))))
                
                ;; Une key correspond, compiler l'expression
                (setf code (append code (list (list :LABEL label-match))))
                (setf code (append code (compile-expr expr env)))
                (setf code (append code (list (list :J label-end))))
                
                ;; Label pour la clause suivante
                (setf code (append code (list (list :LABEL label-next))))))))
    
    ;; Label de fin
    (setf code (append code (list (list :LABEL label-end))))
    
    code))

;;; ============================================================================
;;; COMPILATION - LET
;;; ============================================================================

(defun compile-let (bindings body env)
  "Compile (let ((var1 val1) ...) body)
   Gère allocation registres/pile et portée lexicale"
  (let ((new-env (copy-env env))
        (code '())
        (saved-regs '())
        (stack-slots 0))
    
    ;; ÉTAPE 1 : Compiler chaque binding
    (dolist (binding bindings)
      (let* ((var (first binding))
             (val (second binding))
             ;; IMPORTANT : Compiler la valeur dans l'ANCIEN environnement
             (val-code (compile-expr val env)))
        
        ;; Ajouter le code de compilation de la valeur
        (setf code (append code val-code))
        
        ;; Essayer d'allouer un registre temporaire
        (let ((reg (allocate-temp-reg new-env)))
          (if reg
              ;; Cas A : Registre disponible
              (progn
                ;; Déplacer le résultat ($v0) dans le registre alloué
                (setf code (append code 
                                  (list (list :MOVE *reg-v0* reg))))
                ;; Ajouter la variable à l'environnement
                (add-variable new-env var reg)
                ;; Sauvegarder le registre pour libération ultérieure
                (push reg saved-regs))
              
              ;; Cas B : Pas de registre disponible - utiliser la pile
              (let ((offset (alloc-stack-slot new-env)))
                ;; Pousser le résultat sur la pile
                (setf code (append code
                                  (list (list :ADDI *reg-sp* -4 *reg-sp*)
                                        (list :SW *reg-v0* *reg-sp* 0))))
                ;; Ajouter la variable à l'environnement (avec offset pile)
                (add-variable new-env var (cons :stack offset))
                ;; Compter les slots utilisés
                (incf stack-slots))))))
    
    ;; ÉTAPE 2 : Compiler le body dans le NOUVEL environnement
    (dolist (expr body)
      (setf code (append code (compile-expr expr new-env))))
    
    ;; ÉTAPE 3 : Nettoyer (libérer registres et pile)
    ;; Libérer les registres dans l'ordre inverse
    (dolist (reg (reverse saved-regs))
      (free-temp-reg new-env reg))
    
    ;; Nettoyer la pile si nécessaire
    (when (> stack-slots 0)
      (setf code (append code
                        (list (list :ADDI *reg-sp* 
                                   (* stack-slots 4) 
                                   *reg-sp*)))))
    
    code))

;;; ============================================================================
;;; COMPILATION - LOOP WHILE
;;; ============================================================================

(defun compile-loop-while (condition body env)
  "Compile (loop while condition do body)
   Génère une boucle avec labels et branches conditionnelles"
  (let ((label-start (gen-label env "LOOP_START"))
        (label-end (gen-label env "LOOP_END"))
        (code '()))
    
    ;; Label début de boucle
    (setf code (append code (list (list :LABEL label-start))))
    
    ;; Compiler la condition
    (setf code (append code (compile-expr condition env)))
    
    ;; Si condition = 0 (faux), sortir de la boucle
    (setf code (append code
                      (list (list :BEQ *reg-v0* *reg-zero* label-end))))
    
    ;; Compiler le corps de la boucle
    (dolist (expr body)
      (setf code (append code (compile-expr expr env))))
    
    ;; Retour au début de la boucle
    (setf code (append code
                      (list (list :J label-start))))
    
    ;; Label fin de boucle
    (setf code (append code
                      (list (list :LABEL label-end))))
    
    code))

(defun compile-while (condition body env)
  "Compile (while condition body...)
   PHASE 11 - Extension pour compilation de la VM
   Syntaxe simplifiée sans 'do' (contrairement à LOOP WHILE)
   Génère une boucle avec labels et branches conditionnelles"
  (let ((label-start (gen-label env "WHILE_START"))
        (label-end (gen-label env "WHILE_END"))
        (code '()))
    
    ;; Label début de boucle
    (setf code (append code (list (list :LABEL label-start))))
    
    ;; Compiler la condition
    (setf code (append code (compile-expr condition env)))
    
    ;; Si condition = 0 (faux), sortir de la boucle
    (setf code (append code
                      (list (list :BEQ *reg-v0* *reg-zero* label-end))))
    
    ;; Compiler le corps de la boucle (plusieurs expressions)
    (dolist (expr body)
      (setf code (append code (compile-expr expr env))))
    
    ;; Retour au début de la boucle
    (setf code (append code
                      (list (list :J label-start))))
    
    ;; Label fin de boucle
    (setf code (append code
                      (list (list :LABEL label-end))))
    
    ;; Le résultat d'un WHILE est nil (0)
    (setf code (append code
                      (list (list :MOVE *reg-zero* *reg-v0*))))
    
    code))

(defun compile-progn (exprs env)
  "Compile (progn expr1 expr2 ... exprN)
   PHASE 11 - Extension pour séquences d'expressions
   Évalue chaque expression dans l'ordre, retourne le résultat de la dernière"
  (let ((code '()))
    (if (null exprs)
        ;; PROGN vide retourne 0 (nil)
        (setf code (list (list :MOVE *reg-zero* *reg-v0*)))
        ;; Compiler chaque expression en séquence
        (dolist (expr exprs)
          (setf code (append code (compile-expr expr env)))))
    code))

(defun compile-make-array (args env)
  "Compile (make-array size [:initial-element value])
   PHASE 11 - Support arrays pour VM compilation
   
   Alloue un array sur le tas avec structure:
   - array[0] = MAGIC (0xA88A)
   - array[1] = SIZE
   - array[2...n+1] = éléments
   
   Retourne l'adresse de base dans $v0"
  (let ((size-expr (first args))
        (init-value (if (and (>= (length args) 3)
                            (eq (second args) :initial-element))
                        (third args)
                        nil))  ; nil signifie pas d'initialisation explicite
        (code '()))
    
    ;; 1. Compiler et évaluer size
    (setf code (append code (compile-expr size-expr env)))
    ;; size est maintenant dans $v0
    
    ;; 2. Sauvegarder size dans $s1 (callee-saved, safe pour appels)
    (setf code (append code (list (list :ADDI *reg-sp* -4 *reg-sp*))))
    (setf code (append code (list (list :SW *reg-s1* *reg-sp* 0))))
    (setf code (append code (list (list :MOVE *reg-v0* *reg-s1*))))
    
    ;; 3. Calculer taille totale: 2 + size (header MAGIC/SIZE + éléments)
    (setf code (append code (list (list :ADDI *reg-s1* 2 *reg-a0*))))
    
    ;; 4. Allouer sur le tas
    ;; Allocation simple: utilise $gp (Global Pointer, registre 28) comme heap pointer
    ;; $gp est initialisé à +heap-start+ par la VM
    ;; Algorithme:
    ;;   addr = $gp
    ;;   $gp = $gp + size
    ;;   return addr
    (let ((reg-gp (get-reg :gp)))  ; $gp = registre 28
      ;; addr = $gp (sauvegarder dans $v0)
      (setf code (append code (list (list :MOVE reg-gp *reg-v0*))))
      ;; $gp = $gp + size (où size est dans $a0)
      (setf code (append code (list (list :ADD reg-gp *reg-a0* reg-gp)))))
    ;; $v0 contient maintenant l'adresse de base du array
    
    ;; 5. Sauvegarder l'adresse de base dans $s2
    (setf code (append code (list (list :ADDI *reg-sp* -4 *reg-sp*))))
    (setf code (append code (list (list :SW *reg-s2* *reg-sp* 4))))
    (setf code (append code (list (list :MOVE *reg-v0* *reg-s2*))))
    
    ;; 6. Écrire MAGIC à base+0 (0xA88A = 43146 en décimal)
    (setf code (append code (list (list :LI 43146 *reg-t0*))))
    (setf code (append code (list (list :SW *reg-t0* *reg-s2* 0))))
    
    ;; 7. Écrire SIZE à base+1
    (setf code (append code (list (list :SW *reg-s1* *reg-s2* 1))))
    
    ;; 8. Initialiser les éléments si init-value fourni
    (when init-value
      ;; Compiler init-value
      (setf code (append code (compile-expr init-value env)))
      ;; Valeur d'initialisation dans $v0, sauvegarder dans $t3
      (setf code (append code (list (list :MOVE *reg-v0* *reg-t3*))))
      
      ;; Boucle d'initialisation: for (i = 0; i < size; i++) array[i+2] = init-value
      (let ((label-loop-start (gen-label env "ARRAY_INIT_START"))
            (label-loop-end (gen-label env "ARRAY_INIT_END")))
        
        ;; i = 0 (compteur dans $t0)
        (setf code (append code (list (list :MOVE *reg-zero* *reg-t0*))))
        
        ;; LOOP_START:
        (setf code (append code (list (list :LABEL label-loop-start))))
        
        ;; if (i >= size) goto LOOP_END (utilise SLT: set if less than)
        (setf code (append code (list (list :SLT *reg-t0* *reg-s1* *reg-t1*))))
        ;; $t1 = 1 si i < size, 0 sinon
        (setf code (append code (list (list :BEQ *reg-t1* *reg-zero* label-loop-end))))
        
        ;; Calculer offset: i + 2 (pour header)
        (setf code (append code (list (list :ADDI *reg-t0* 2 *reg-t2*))))
        
        ;; Calculer adresse effective: base + offset
        (setf code (append code (list (list :ADD *reg-s2* *reg-t2* *reg-t4*))))
        
        ;; array[addr] = init-value (SW src base 0)
        (setf code (append code (list (list :SW *reg-t3* *reg-t4* 0))))
        
        ;; i++
        (setf code (append code (list (list :ADDI *reg-t0* 1 *reg-t0*))))
        
        ;; goto LOOP_START
        (setf code (append code (list (list :J label-loop-start))))
        
        ;; LOOP_END:
        (setf code (append code (list (list :LABEL label-loop-end))))))
    
    ;; 9. Retourner l'adresse du array dans $v0
    (setf code (append code (list (list :MOVE *reg-s2* *reg-v0*))))
    
    ;; 10. Restaurer $s1 et $s2
    (setf code (append code (list (list :LW *reg-s2* *reg-sp* 4))))
    (setf code (append code (list (list :LW *reg-s1* *reg-sp* 0))))
    (setf code (append code (list (list :ADDI *reg-sp* 8 *reg-sp*))))
    
    code))

(defun compile-aref (array-expr index-expr env)
  "Compile (aref array index)
   PHASE 11 - Lecture d'élément de array
   
   Calcule l'adresse: base + 2 + index (offset +2 pour header)
   Retourne la valeur dans $v0"
  (let ((code '()))
    
    ;; 1. Compiler et évaluer array (adresse dans $v0)
    (setf code (append code (compile-expr array-expr env)))
    
    ;; 2. Sauvegarder adresse array dans $t0
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t0*))))
    
    ;; 3. Compiler et évaluer index
    (setf code (append code (compile-expr index-expr env)))
    ;; index dans $v0
    
    ;; 4. Calculer offset réel: index + 2 (pour header)
    (setf code (append code (list (list :ADDI *reg-v0* 2 *reg-t1*))))
    
    ;; 5. Calculer adresse effective: base + offset
    (setf code (append code (list (list :ADD *reg-t0* *reg-t1* *reg-t2*))))
    
    ;; 6. Lire la valeur (LW dest base 0)
    (setf code (append code (list (list :LW *reg-v0* *reg-t2* 0))))
    
    code))

(defun compile-setf-aref (array-expr index-expr value-expr env)
  "Compile (setf (aref array index) value)
   PHASE 11 - Écriture d'élément de array
   
   Retourne value dans $v0 (comportement SETF standard)"
  (let ((code '()))
    
    ;; 1. Compiler et évaluer value (pour éviter écrasement registres)
    (setf code (append code (compile-expr value-expr env)))
    
    ;; 2. Sauvegarder value dans $t0
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t0*))))
    
    ;; 3. Compiler et évaluer array
    (setf code (append code (compile-expr array-expr env)))
    
    ;; 4. Sauvegarder adresse array dans $t1
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t1*))))
    
    ;; 5. Compiler et évaluer index
    (setf code (append code (compile-expr index-expr env)))
    
    ;; 6. Calculer offset réel: index + 2 (pour header)
    (setf code (append code (list (list :ADDI *reg-v0* 2 *reg-t2*))))
    
    ;; 7. Calculer adresse effective: base + offset
    (setf code (append code (list (list :ADD *reg-t1* *reg-t2* *reg-t3*))))
    
    ;; 8. Écrire value à cette adresse (SW src base 0)
    (setf code (append code (list (list :SW *reg-t0* *reg-t3* 0))))
    
    ;; 9. Retourner value dans $v0 (comportement SETF)
    (setf code (append code (list (list :MOVE *reg-t0* *reg-v0*))))
    
    code))

(defun compile-dotimes (var-spec body env)
  "Compile (dotimes (var count [result]) body...)
   Syntaxe: (dotimes (i 10) (print i)) - boucle i de 0 à 9
   var-spec = (var count) ou (var count result-form)"
  (let* ((var (first var-spec))
         (count-expr (second var-spec))
         (result-expr (third var-spec))  ; optionnel
         (label-start (gen-label env "DOTIMES_START"))
         (label-end (gen-label env "DOTIMES_END"))
         (code '()))
    
    ;; Sauvegarder $s1 et $s2 sur la pile (registres utilisés pour la boucle)
    (setf code (append code
                      (list (list :ADDI *reg-sp* -8 *reg-sp*)
                            (list :SW *reg-s1* *reg-sp* 0)
                            (list :SW *reg-s2* *reg-sp* 4))))
    
    ;; CRITIQUE: Sauvegarder $t0-$t3 avant d'évaluer count-expr
    ;; car count-expr peut utiliser ces registres et écraser les variables du LET parent
    (setf code (append code
                      (list (list :ADDI *reg-sp* -16 *reg-sp*)
                            (list :SW *reg-t0* *reg-sp* 0)
                            (list :SW *reg-t1* *reg-sp* 4)
                            (list :SW *reg-t2* *reg-sp* 8)
                            (list :SW *reg-t3* *reg-sp* 12))))
    
    ;; Compiler l'expression de comptage AVANT de créer le nouvel environnement
    (setf code (append code (compile-expr count-expr env)))
    
    ;; Sauvegarder le count dans $s2 (limite) - $S2 est callee-saved donc safe
    (setf code (append code (list (list :MOVE *reg-v0* *reg-s2*))))
    
    ;; Restaurer $t0-$t3 pour que les variables du LET parent soient intactes
    (setf code (append code
                      (list (list :LW *reg-t0* *reg-sp* 0)  ; Format: (LW dest base offset)
                            (list :LW *reg-t1* *reg-sp* 4)
                            (list :LW *reg-t2* *reg-sp* 8)
                            (list :LW *reg-t3* *reg-sp* 12)
                            (list :ADDI *reg-sp* 16 *reg-sp*))))
    
    ;; Créer le nouvel environnement APRÈS avoir évalué count
    (let ((new-env (copy-env env)))
      
      ;; Initialiser la variable d'indice à 0 dans $s1
      (setf code (append code (list (list :LI 0 *reg-s1*))))
      
      ;; Ajouter la variable d'indice à l'environnement (registre $s1)
      (add-variable new-env var *reg-s1*)
      
      ;; Label début de boucle
      (setf code (append code (list (list :LABEL label-start))))
      
      ;; Comparer i < count : si i >= count, sortir
      ;; SLT $t2 $s1 $s2  =>  $t2 = ($s1 < $s2)
      (setf code (append code
                        (list (list :SLT *reg-s1* *reg-s2* *reg-t2*))))
      
      ;; Si $t2 = 0 (i >= count), sortir
      (setf code (append code
                        (list (list :BEQ *reg-t2* *reg-zero* label-end))))
      
      ;; Compiler le corps de la boucle avec le nouvel environnement
      (dolist (expr body)
        (setf code (append code (compile-expr expr new-env))))
      
      ;; Incrémenter i: $s1 = $s1 + 1
      (setf code (append code
                        (list (list :ADDI *reg-s1* 1 *reg-s1*))))
      
      ;; Retour au début de la boucle
      (setf code (append code (list (list :J label-start))))
      
      ;; Label fin de boucle
      (setf code (append code (list (list :LABEL label-end))))
      
      ;; Restaurer $s1 et $s2 AVANT d'évaluer l'expression résultat
      (setf code (append code
                        (list (list :LW *reg-s1* *reg-sp* 0)  ; Format: (LW dest base offset)
                              (list :LW *reg-s2* *reg-sp* 4)
                              (list :ADDI *reg-sp* 8 *reg-sp*))))
    
      ;; Compiler l'expression résultat si présente, sinon retourner nil (0)
      ;; IMPORTANT: Utiliser env (environnement parent) pas new-env pour result-expr
      ;; car result-expr doit accéder aux variables du LET parent
      (if result-expr
          (setf code (append code (compile-expr result-expr env)))
          (setf code (append code (list (list :LI 0 *reg-v0*)))))
      
      code)))

;;; ============================================================================
;;; COMPILATION - SETQ (assignation variable)
;;; ============================================================================

(defun compile-setq (var value env)
  "Compile (setq var value)
   Modifie la valeur d'une variable existante (locale ou globale DEFVAR)"
  ;; PRIORITÉ 1 : Vérifier si c'est une variable globale (DEFVAR)
  (let ((global-addr (gethash var *global-variables*)))
    (if global-addr
        ;; Variable globale : compiler valeur et stocker à $zero + offset
        (let ((code (compile-expr value env)))
          (append code (list (list :SW *reg-v0* *reg-zero* global-addr))))
        ;; Sinon, variable locale
        (let ((location (lookup-variable env var))
              (code '()))
          
          ;; Compiler la nouvelle valeur
          (setf code (append code (compile-expr value env)))
          
          ;; Stocker dans la variable locale
          (cond
            ;; Cas 1 : Variable dans un registre
            ((and location (symbolp location))
             (setf code (append code
                               (list (list :MOVE *reg-v0* location)))))
            
            ;; Cas 2 : Variable sur la pile
            ((and location (consp location) (eq (car location) :stack))
             (let ((offset (cdr location)))
               (setf code (append code
                                 (list (list :SW *reg-v0* *reg-sp* offset))))))
            
            ;; Cas 3 : Variable non trouvée
            (t
             (error "Variable non définie dans SETQ: ~A" var)))
          
          code))))

(defun compile-labels (definitions body env)
  "Compile (labels ((fn1 args1 body1) (fn2 args2 body2) ...) body)
   Définit des fonctions locales avec portée lexicale et closures"
  (let* ((new-env (make-lexical-env env nil))  ; Créer scope SANS incrémenter (LABELS body n'a pas de frame)
         (code '())
         (fn-infos '())
         (body-label (gen-label new-env "LABELS_BODY")))
    
    ;; ÉTAPE 1 & 2: Pré-générer labels ASM et ajouter fonctions à l'environnement
    (dolist (def definitions)
      (let* ((fn-name (first def))
             (fn-args (second def))
             (fn-label (gen-label new-env (format nil "LOCAL_~A" fn-name))))
        (push (cons fn-name (cons fn-label fn-args)) fn-infos)
        (add-function new-env fn-name fn-label)))
    
    (setf fn-infos (reverse fn-infos))
    
    ;; Générer JMP pour sauter par-dessus les définitions de fonctions
    (setf code (append code (list (list :J body-label))))
    
    ;; ÉTAPE 3: Compiler chaque fonction locale
    (dolist (def definitions)
      (let* ((fn-name (first def))
             (fn-args (second def))
             (fn-body (cddr def))
             (fn-info (cdr (assoc fn-name fn-infos)))
             (fn-label (car fn-info))
             ;; Créer environnement lexical pour la fonction (pas simple copy)
             (fn-env (make-lexical-env new-env t))
             (arg-regs (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*)))
        
        ;; Vérifier nombre de paramètres
        (when (> (length fn-args) 4)
          (error "LABELS: Maximum 4 paramètres supportés"))
        
        ;; Label de la fonction
        (setf code (append code (list (list :LABEL fn-label))))
        
        ;; PHASE 9 CLOSURES: Frame avec Static Link pour accès variables englobantes
        ;; Frame layout: [Old FP][RA][Static Link][Params...]
        ;; Static Link = FP du scope lexical parent (pour suivre chaîne closures)
        (if (> (length fn-args) 0)
            ;; Cas avec paramètres : utiliser FP avec static link
            (let ((num-params (length fn-args)))
              ;; 1. Sauvegarder ancien FP, RA et Static Link sur la pile
              ;;    Static Link sera passé dans $a0 (premier arg temporairement)
              (setf code (append code (list (list :ADDI *reg-sp* -12 *reg-sp*)  ; 12 bytes: FP+RA+StaticLink
                                           (list :SW (get-reg :fp) *reg-sp* 0)
                                           (list :SW *reg-ra* *reg-sp* 4)
                                           ;; Static link à FP+8 (sera mis à jour ci-dessous)
                                           )))
              
              ;; 2. FP = SP actuel (début de notre frame)
              (setf code (append code (list (list :MOVE *reg-sp* (get-reg :fp)))))
              
              ;; 3. Sauvegarder static link (FP parent) à FP+8
              ;;    $S0 contient le static link reçu de l'appelant
              (setf code (append code (list (list :SW *reg-s0* (get-reg :fp) 8))))
              
              ;; 4. GARDER $S0 intact !
              ;;    $S0 doit contenir le static link reçu pour le passer aux autres fonctions locales
              ;;    du même niveau (elles doivent partager le même environnement parent)
              ;; PAS DE: (MOVE $FP $S0) car ça casserait les appels entre fonctions locales
              
              ;; 5. Allouer espace pour les paramètres
              (let ((stack-size (* 4 num-params)))
                (setf code (append code (list (list :ADDI *reg-sp* (- stack-size) *reg-sp*))))
                
                ;; 6. Sauvegarder chaque paramètre A0-A3 sur la pile
                ;;    Les offsets sont calculés depuis FP
                (loop for i from 0 below num-params
                      for arg in fn-args
                      for arg-reg in arg-regs
                      do (let ((fp-offset (- (* 4 (+ i 1)))))  ; -4, -8, -12, -16
                           (setf code (append code (list (list :SW arg-reg (get-reg :fp) fp-offset))))
                           ;; Ajouter la variable avec offset depuis FP
                           (add-variable fn-env arg (cons :fp fp-offset)))))
              
              ;; 7. Compiler le corps de la fonction
              (dolist (expr fn-body)
                (setf code (append code (compile-expr expr fn-env))))
              
              ;; 8. Restaurer SP, FP et RA avant le retour (ordre important!)
              ;;    D'abord restaurer SP au début du frame
              (setf code (append code (list (list :MOVE (get-reg :fp) *reg-sp*))))  ; SP = FP
              ;;    Ensuite charger RA et ancien FP depuis SP
              (setf code (append code (list (list :LW *reg-ra* *reg-sp* 4))))  ; RA = [SP+4] - Format: (LW dest base offset)
              (setf code (append code (list (list :LW (get-reg :fp) *reg-sp* 0))))  ; FP = [SP+0] - Format: (LW dest base offset)
              ;;    Enfin libérer la pile (FP + RA + Static Link = 12 bytes)
              (setf code (append code (list (list :ADDI *reg-sp* 12 *reg-sp*)))))  ; SP += 12
            
            ;; Cas sans paramètres : pas de FP nécessaire
            (dolist (expr fn-body)
              (setf code (append code (compile-expr expr fn-env)))))
        
        ;; Retour de la fonction
        (setf code (append code (list (list :JR *reg-ra*))))))
    
    ;; ÉTAPE 4: Label du corps principal et compilation
    (setf code (append code (list (list :LABEL body-label))))
    
    ;; PHASE 8 FIX: Le corps d'un LABELS doit initialiser $S0 correctement
    ;; Si on est dans une fonction (parent-lexical non-nil), les fonctions locales
    ;; doivent recevoir $FP du scope actuel comme static link.
    ;; Si on est au niveau global (parent-lexical nil), utiliser 0.
    (when (compiler-env-parent-lexical new-env)
      ;; On est dans une fonction, initialiser $S0 = $FP du parent
      ;; Mais le corps du LABELS n'a pas son propre FP, donc on doit chercher
      ;; le FP de la fonction englobante. Pour simplifier, on utilise $FP actuel.
      ;; Note: $FP contient déjà le frame pointer de la fonction englobante car
      ;; le corps du LABELS s'exécute dans le contexte de cette fonction.
      (setf code (append code (list (list :MOVE (get-reg :fp) *reg-s0*)))))
    
    ;; Compiler le corps principal
    (dolist (expr body)
      (setf code (append code (compile-expr expr new-env))))
    
    code))

;;; ============================================================================
;;; COMPILATION - LAMBDA (PHASE 9 - CLOSURES)
;;; ============================================================================

(defun compile-lambda (params body env)
  "Compile une expression LAMBDA en une fermeture (closure).
   
   Une fermeture est une structure allouée sur le tas contenant:
   - [addr+0] : Adresse du code de la fonction (label)
   - [addr+1] : Taille de l'environnement (nombre de variables capturées)
   - [addr+2..] : Valeurs des variables capturées
   
   Arguments:
   - params: Liste des paramètres de la fonction lambda
   - body: Corps de la fonction (liste d'expressions)
   - env: Environnement de compilation actuel
   
   Retourne: Code assembleur qui:
   1. Génère le code de la fonction (sauté lors de la définition)
   2. Alloue la structure de fermeture sur le tas
   3. Initialise la structure avec le label et les variables capturées
   4. Retourne l'adresse de la fermeture dans $v0"
  
  (let* ((code '())
         ;; Générer un label unique pour le code de la fonction
         (func-label (gen-label env "lambda_func"))
         (skip-label (gen-label env "lambda_skip"))
         
         ;; Analyser les variables libres dans le corps de la lambda
         (lambda-expr (append (list 'lambda params) body))
         (free-vars (free-variables lambda-expr))
         (num-free-vars (length free-vars))
         (closure-size (+ 2 num-free-vars)))
    
    ;; ÉTAPE 1: Sauter le code de la fonction (il sera appelé plus tard)
    (setf code (append code (list (list :J skip-label))))
    
    ;; ÉTAPE 2: Générer le code de la fonction lambda
    (setf code (append code (list (list :LABEL func-label))))
    
    ;; Configuration du frame de la fonction
    ;; La fonction lambda reçoit:
    ;; - Ses paramètres sur la pile (convention MIPS)
    ;; - Un pointeur vers la fermeture (adresse de la structure)
    
    ;; Créer un nouvel environnement pour la fonction
    (let ((func-env (make-compiler-env
                     :variables '()
                     :functions (compiler-env-functions env)
                     :label-counter (compiler-env-label-counter env)
                     ;; PHASE 9 FIX: Utiliser registres callee-saved ($S4-$S6) pour variables LET
                     ;; au lieu de $T0-$T2 (caller-saved) pour préserver à travers appels
                     :temp-regs-available (list *reg-s4* *reg-s5* *reg-s6*)
                     :stack-offset 0
                     :parent-env env
                     :lexical-depth (1+ (compiler-env-lexical-depth env))
                     :parent-lexical env)))
      
      ;; Si la fonction a des paramètres, configurer le frame
      (when params
        ;; Sauvegarder $FP, $RA, $S0-$S7 sur la pile
        ;; PHASE 9 FIX: Sauvegarder $S2-$S7 pour préserver à travers appels
        (setf code (append code (list (list :ADDI *reg-sp* -36 *reg-sp*))))  ; SP -= 36 (9 registres * 4)
        (setf code (append code (list (list :SW (get-reg :fp) *reg-sp* 0))))  ; MEM[SP+0] = FP
        (setf code (append code (list (list :SW *reg-ra* *reg-sp* 4))))       ; MEM[SP+4] = RA
        (setf code (append code (list (list :SW *reg-s0* *reg-sp* 8))))       ; MEM[SP+8] = S0 (static link)
        (setf code (append code (list (list :SW *reg-s2* *reg-sp* 12))))      ; MEM[SP+12] = S2
        (setf code (append code (list (list :SW *reg-s3* *reg-sp* 16))))      ; MEM[SP+16] = S3
        (setf code (append code (list (list :SW *reg-s4* *reg-sp* 20))))      ; MEM[SP+20] = S4
        (setf code (append code (list (list :SW *reg-s5* *reg-sp* 24))))      ; MEM[SP+24] = S5
        (setf code (append code (list (list :SW *reg-s6* *reg-sp* 28))))      ; MEM[SP+28] = S6
        (setf code (append code (list (list :SW *reg-s7* *reg-sp* 32))))      ; MEM[SP+32] = S7
        (setf code (append code (list (list :MOVE *reg-sp* (get-reg :fp))))) ; FP = SP
        
        ;; Charger les paramètres depuis les registres d'arguments
        ;; Convention: $a0, $a1, $a2, $a3 pour les 4 premiers paramètres
        (let ((param-offset 36))  ; Offset après FP/RA/S0/S2/S3/S4/S5/S6/S7
          (dolist (param params)
            (let ((reg-index (position param params)))
              (when (< reg-index 4)
                ;; Paramètre passé dans un registre $a0-$a3
                (let ((arg-reg (case reg-index
                                 (0 *reg-a0*)
                                 (1 *reg-a1*)
                                 (2 *reg-a2*)
                                 (3 *reg-a3*))))
                  ;; Sauvegarder le paramètre sur la pile
                  (setf code (append code (list (list :SW arg-reg (get-reg :fp) param-offset))))
                  ;; Enregistrer dans l'environnement (accessible via FP, pas SP!)
                  (push (list param :fp param-offset) (compiler-env-variables func-env))
                  (incf param-offset 4))))))
      
      ;; IMPORTANT: Les variables libres sont accessibles via la fermeture
      ;; La fermeture est passée dans $s1 (par convention)
      ;; Structure: [Label][Size][Var0][Var1]...
      ;; Pour accéder à une variable libre:
      ;; 1. L'adresse de la fermeture est dans $s1
      ;; 2. Var_i est à l'offset (2 + i) * 4 depuis $s1
      
      ;; Ajouter les variables libres à l'environnement
      ;; Elles sont accessibles via la fermeture dans $s1
      (let ((var-index 0))
        (dolist (var free-vars)
          ;; Marquer comme accessible via closure
          (push (list var :closure var-index) (compiler-env-variables func-env))
          (incf var-index)))
      
      ;; Compiler le corps de la fonction
      (dolist (expr body)
        (setf code (append code (compile-expr expr func-env))))
      
      ;; Restaurer et retourner
      (when params
        ;; Restaurer $FP, $RA et registres $S0-$S7
        ;; PHASE 9 FIX: Restaurer aussi $S2-$S7
        (setf code (append code (list (list :LW (get-reg :fp) (get-reg :fp) 0))))  ; FP = MEM[FP+0] - Format: (LW dest base offset)
        (setf code (append code (list (list :LW *reg-ra* *reg-sp* 4))))            ; RA = MEM[SP+4]
        (setf code (append code (list (list :LW *reg-s2* *reg-sp* 12))))           ; S2 = MEM[SP+12]
        (setf code (append code (list (list :LW *reg-s3* *reg-sp* 16))))           ; S3 = MEM[SP+16]
        (setf code (append code (list (list :LW *reg-s4* *reg-sp* 20))))           ; S4 = MEM[SP+20]
        (setf code (append code (list (list :LW *reg-s5* *reg-sp* 24))))           ; S5 = MEM[SP+24]
        (setf code (append code (list (list :LW *reg-s6* *reg-sp* 28))))           ; S6 = MEM[SP+28]
        (setf code (append code (list (list :LW *reg-s7* *reg-sp* 32))))           ; S7 = MEM[SP+32]
        (setf code (append code (list (list :ADDI *reg-sp* 36 *reg-sp*))))))      ; SP += 36
      
      (setf code (append code (list (list :JR *reg-ra*)))))
    
    ;; ÉTAPE 3: Label de saut (après le code de la fonction)
    (setf code (append code (list (list :LABEL skip-label))))
    
    ;; ÉTAPE 4: Allouer la structure de fermeture sur le tas
    ;; Taille = 2 (Label + Size) + nombre de variables capturées
    ;; Allouer avec MALLOC - utiliser $t3 pour éviter d'écraser les variables dans $t0-$t2
    (setf code (append code (list (list :MALLOC closure-size *reg-t3*))))
    
    ;; ÉTAPE 5: Initialiser la structure de fermeture
    ;; [addr+0] = Label (adresse du code)
    ;; Note: On doit calculer l'adresse absolue du label
    ;; Pour simplifier, on utilise un placeholder et on assume que
    ;; le loader résoudra le label
    
    ;; Stocker l'adresse du label (sera résolu par le loader)
    ;; Utiliser $t4/$t5 pour ne pas écraser les variables dans $t0-$t2
    (setf code (append code (list (list :LI func-label (get-reg :t4)))))
    (setf code (append code (list (list :LI 0 (get-reg :t5)))))  ; offset 0
    (setf code (append code (list (list :STORE-HEAP (get-reg :t4) *reg-t3* (get-reg :t5)))))
    
    ;; Stocker la taille de l'environnement
    (setf code (append code (list (list :LI num-free-vars (get-reg :t4)))))
    (setf code (append code (list (list :LI 1 (get-reg :t5)))))  ; offset 1
    (setf code (append code (list (list :STORE-HEAP (get-reg :t4) *reg-t3* (get-reg :t5)))))
      
    ;; ÉTAPE 6: Capturer les variables libres
    (let ((offset 2))
      (dolist (var free-vars)
        ;; Compiler l'accès à la variable dans le scope actuel
        (let ((var-code (compile-variable var env)))
          ;; La variable est maintenant dans $v0
          (setf code (append code var-code))
          ;; Stocker dans la fermeture (utiliser $t4 pour offset pour ne pas écraser $t0-$t2)
          (setf code (append code (list (list :LI offset (get-reg :t4)))))
          (setf code (append code (list (list :STORE-HEAP *reg-v0* *reg-t3* (get-reg :t4)))))
          (incf offset))))
    
    ;; ÉTAPE 7: Retourner l'adresse de la fermeture dans $v0
    (setf code (append code (list (list :MOVE *reg-t3* *reg-v0*))))
    
    code))

;;; ============================================================================
;;; COMPILATION - EXPRESSION GÉNÉRALE
;;; ============================================================================

(defun compile-expr (expr env)
  "Compile une expression LISP en code ASM"
  (let ((parsed (parse-lisp-expr expr)))
    (case (first parsed)
      (:constant
       (compile-constant (second parsed) env))
      
      (:variable
       (compile-variable (second parsed) env))
      
      (:arithmetic
       (compile-arithmetic (second parsed) (third parsed) env))
      
      (:math-func
       (compile-math-func (second parsed) (third parsed) env))
      
      (:comparison
       (compile-comparison (second parsed) (third parsed) env))
      
      (:if
       (compile-if (second parsed) (third parsed) (fourth parsed) env))
      
      (:cond
       (compile-cond (second parsed) env))
      
      (:when
       (compile-when (second parsed) (third parsed) env))
      
      (:unless
       (compile-unless (second parsed) (third parsed) env))
      
      (:not
       (compile-not (second parsed) env))
      
      (:and
       (compile-and (second parsed) env))
      
      (:or
       (compile-or (second parsed) env))
      
      (:case
       (compile-case (second parsed) (third parsed) env))
      
      (:let
       (compile-let (second parsed) (third parsed) env))
      
      (:loop-while
       (compile-loop-while (second parsed) (third parsed) env))
      
      (:while
       (compile-while (second parsed) (third parsed) env))
      
      (:progn
       (compile-progn (second parsed) env))
      
      (:make-array
       (compile-make-array (second parsed) env))
      
      (:aref
       (compile-aref (second parsed) (third parsed) env))
      
      (:dotimes
       (compile-dotimes (second parsed) (third parsed) env))
      
      (:setq
       (compile-setq (second parsed) (third parsed) env))
      
      (:setf-aref
       (compile-setf-aref (second parsed) (third parsed) (fourth parsed) env))
      
      (:labels
       (compile-labels (second parsed) (third parsed) env))
      
      (:lambda
       (compile-lambda (second parsed) (cddr parsed) env))
      
      (:call
       (compile-call (second parsed) (third parsed) env))
      
      (:defconstant
       (compile-defconstant (second parsed) (third parsed) env))
      
      (:defvar
       (compile-defvar (second parsed) (third parsed) env))
      
      (:defun
       (compile-defun (second parsed) (third parsed) (fourth parsed) env))
      
      (:error
       ;; ERROR compilé en HALT (arrêt immédiat)
       (list (list :HALT)))
      
      (:format
       ;; FORMAT ignoré en compilation (pas d'output)
       '())
      
      (t (error "Type d'expression non supporté: ~A" (first parsed))))))

;;; ============================================================================
;;; COMPILATION - DEFCONSTANT / DEFVAR (PHASE 11)
;;; ============================================================================

(defun eval-constant-expr (expr)
  "Évalue une expression constante récursivement.
   Retourne la valeur numérique si toutes les dépendances sont des constantes.
   Supporte: nombres, symboles constants (DEFCONSTANT ou DEFPARAMETER), (+ - * / MOD) sur constantes."
  (cond
    ;; Cas 1: Nombre littéral
    ((numberp expr) expr)
    
    ;; Cas 2: Symbole -> chercher dans *global-constants* OU *global-variables*
    ((symbolp expr)
     (let ((const-val (gethash expr *global-constants*))
           (var-offset (gethash expr *global-variables*)))
       (cond
         ;; Priorité 1: Constante DEFCONSTANT
         (const-val const-val)
         ;; Priorité 2: Variable DEFPARAMETER (on retourne l'offset pour usage ultérieur)
         ;; NOTE: Pour vraies constantes calculées, DEFPARAMETER devrait être compilé
         ;; avant DEFCONSTANT qui l'utilise. Pour l'instant on accepte l'offset.
         (var-offset var-offset)
         ;; Pas trouvé
         (t (error "DEFCONSTANT: symbole non défini: ~A" expr)))))
    
    ;; Cas 3: Expression arithmétique
    ((and (listp expr) (member (first expr) '(+ - * / mod)))
     (let ((op (first expr))
           (args (rest expr)))
       ;; Évaluer récursivement tous les arguments
       (let ((vals (mapcar #'eval-constant-expr args)))
         (case op
           (+ (apply #'+ vals))
           (- (apply #'- vals))
           (* (apply #'* vals))
           (/ (apply #'truncate vals))  ; Division entière
           (mod (apply #'mod vals))))))
    
    ;; Cas 4: Autre expression non supportée
    (t (error "DEFCONSTANT: expression non supportée: ~A" expr))))

(defun compile-defconstant (name value env)
  "Compile (defconstant +NOM+ valeur).
   Enregistre la constante dans *global-constants* et ne génère PAS de code MIPS.
   Les références à la constante seront remplacées par sa valeur à la compilation.
   
   AMÉLIORATION PHASE 11: Accepte maintenant les expressions constantes:
   - Nombres littéraux: (defconstant +X+ 42)
   - Expressions calculées: (defconstant +Y+ (+ +X+ 10))
   - Opérateurs supportés: + - * / MOD"
  (declare (ignore env))
  ;; Évaluer l'expression constante (peut être littéral ou calculée)
  (let ((computed-value (eval-constant-expr value)))
    ;; Enregistrer dans la table globale
    (setf (gethash name *global-constants*) computed-value)
    ;; Ne générer aucun code MIPS (les constantes sont substituées inline)
    '()))

(defun compile-defvar (name value env)
  "Compile (defvar *nom* valeur).
   Alloue un slot dans la section de données globales et génère le code
   d'initialisation.
   
   STRATÉGIE:
   - Allouer offset dans *global-data-offset* (incrémenté de 4 octets/mot)
   - Enregistrer (nom → offset) dans *global-variables*
   - Si valeur fournie: PARSER puis compiler la valeur et générer SW pour stocker
   - Les variables globales sont adressées depuis $zero + offset
   
   Exemple: (defvar *state* 0)
     → Alloue offset 0, génère: LI $v0 0 ; SW $v0 $zero 0
   
   NOTE PHASE 11: La valeur arrive NON PARSÉE (brute) car le parsing de DEFVAR
   dans parse-lisp-expr ne parse pas la valeur (pour permettre aux constantes
   d'être définies avant)."
  (let ((addr *global-data-offset*)
        (code '()))
    ;; Enregistrer l'adresse de la variable AVANT de compiler la valeur
    ;; Important: permet à la valeur d'initialisation de référencer la variable
    (setf (gethash name *global-variables*) addr)
    ;; Incrémenter l'offset (chaque variable prend 1 mot = 4 octets)
    (incf *global-data-offset* 4)
    
    ;; Générer code d'initialisation si valeur fournie
    (when value
      ;; La valeur arrive brute (non parsée) - la parser d'abord
      (let* ((parsed-value (parse-lisp-expr value))
             (init-code (compile-expr parsed-value env)))
        ;; Résultat dans $v0, le stocker à l'adresse globale
        ;; Format: (SW src base offset) → stocker $v0 à ($zero + addr)
        (setf code (append init-code (list (list :SW *reg-v0* *reg-zero* addr))))))
    
    ;; Retourner le code d'initialisation
    code))

;;; ============================================================================
;;; COMPILATION - APPELS DE FONCTION
;;; ============================================================================

(defun compile-intrinsic (func-name args env)
  "Compile les fonctions intrinsèques de la VM (PHASE 11).
   Ces fonctions sont inlinées directement sans appel de fonction réel.
   
   Intrinsèques supportées:
   - (get-register :$sp) → (LW $zero offset $v0)  ; charge depuis *VM-REGISTERS*
   - (set-register :$sp value) → (SW value $zero offset) ; stocke dans *VM-REGISTERS*
   - (register-p x) → vérifie si x est un keyword de registre
   - (get-reg :sp) → retourne :$sp (mapping des noms)"
  (let ((code '()))
    (case func-name
      ;; GET-REGISTER: accès lecture à *VM-REGISTERS*[reg]
      (get-register
       (unless (= (length args) 1)
         (error "GET-REGISTER attend 1 argument, reçu ~A" (length args)))
       (let* ((reg-arg (first args))
              (parsed-arg (parse-lisp-expr reg-arg)))
         (cond
           ;; Cas simple: keyword constant (:$sp, :$t0, etc.)
           ((and (listp parsed-arg) (eq (first parsed-arg) :constant) (keywordp (second parsed-arg)))
            (let* ((reg-keyword (second parsed-arg))
                   (reg-index (reg-keyword-to-index reg-keyword)))
              ;; *VM-REGISTERS* est un array, on accède par: mem[*VM-REGISTERS-ADDR* + reg-index]
              ;; Pour simplification: on suppose *VM-REGISTERS* à l'offset 36 (après *VM-STATE*, *VM-INSTRUCTION-COUNT*, *HEAP-POINTER*)
              ;; Chaque registre = 4 bytes
              (let ((offset (+ 36 (* 4 reg-index))))
                (setf code (list (list :LW *reg-zero* offset *reg-v0*))))))
           ;; Cas avec GET-REG: (get-register (get-reg :sp))
           ((and (listp parsed-arg) (eq (first parsed-arg) :call) (eq (second parsed-arg) 'get-reg))
            ;; Compiler l'appel get-reg normalement (il sera traité comme intrinsèque)
            ;; parsed-arg = (:call get-reg args)  où args est déjà parsée
            ;; IMPORTANT: compiler reg-arg (brut), pas parsed-arg (déjà parsé)
            (let* ((inner-result (compile-expr reg-arg env)))
              ;; $v0 contient maintenant l'index du registre
              ;; Calculer l'offset dynamiquement: offset = 36 + $v0 * 4
              (setf code (append inner-result
                               (list (list :LI 4 *reg-t0*)         ; $t0 = 4
                                     (list :MUL *reg-v0* *reg-t0*)  ; $lo = $v0 * 4
                                     (list :MFLO *reg-t0*)          ; $t0 = $lo
                                     (list :ADDI *reg-t0* 36 *reg-t0*)  ; $t0 = $t0 + 36
                                     ;; Charger depuis mem[$zero + $t0]
                                     (list :ADD *reg-zero* *reg-t0* *reg-t1*)  ; $t1 = $zero + $t0
                                     (list :LW *reg-t1* 0 *reg-v0*))))))  ; $v0 = mem[$t1]
           ;; Cas variable: symbole $SP, $GP, etc. (sans :)
           ;; On essaie de mapper le nom vers un keyword puis utiliser l'index
           ((and (listp parsed-arg) (eq (first parsed-arg) :variable))
            (let* ((var-name (second parsed-arg))
                   ;; Essayer de convertir $SP → :$sp
                   (reg-keyword (map-symbol-to-register-keyword var-name)))
              (if reg-keyword
                  ;; C'est un registre connu, accès direct
                  (let* ((reg-index (reg-keyword-to-index reg-keyword))
                         (offset (+ 36 (* 4 reg-index))))
                    (setf code (list (list :LW *reg-zero* offset *reg-v0*))))
                  ;; Sinon, erreur
                  (error "GET-REGISTER: symbole ~A n'est pas un registre connu" var-name))))
           ;; Cas complexe: expression calculée pour le registre
           (t
            (error "GET-REGISTER avec expression non supportée: ~A" parsed-arg)))))
      
      ;; SET-REGISTER: accès écriture à *VM-REGISTERS*[reg] = value
      (set-register
       (unless (= (length args) 2)
         (error "SET-REGISTER attend 2 arguments, reçu ~A" (length args)))
       (let* ((reg-arg (first args))
              (value-arg (second args))
              (parsed-reg (parse-lisp-expr reg-arg)))
         (cond
           ;; Cas simple: keyword constant + valeur
           ((and (listp parsed-reg) (eq (first parsed-reg) :constant) (keywordp (second parsed-reg)))
            (let* ((reg-keyword (second parsed-reg))
                   (reg-index (reg-keyword-to-index reg-keyword))
                   (offset (+ 36 (* 4 reg-index)))
                   (value-code (compile-expr value-arg env)))
              ;; Compiler la valeur puis la stocker
              (setf code (append value-code
                               (list (list :SW *reg-v0* *reg-zero* offset))))))
           ;; Cas avec GET-REG
           ((and (listp parsed-reg) (eq (first parsed-reg) :call) (eq (second parsed-reg) 'get-reg))
            ;; IMPORTANT: compiler reg-arg (brut), pas parsed-reg (déjà parsé)
            (let* ((inner-result (compile-expr reg-arg env))  ; Compiler get-reg
                   (value-code (compile-expr value-arg env)))
              ;; $v0 contient l'index après get-reg, puis la valeur après value-code
              ;; Sauvegarder l'index temporairement
              (setf code (append inner-result
                               (list (list :MOVE *reg-v0* *reg-t2*))  ; $t2 = index
                               value-code  ; $v0 = valeur
                               (list (list :LI 4 *reg-t0*)
                                     (list :MUL *reg-t2* *reg-t0*)  ; $lo = index * 4
                                     (list :MFLO *reg-t0*)          ; $t0 = $lo
                                     (list :ADDI *reg-t0* 36 *reg-t0*)
                                     (list :ADD *reg-zero* *reg-t0* *reg-t1*)
                                     (list :SW *reg-v0* *reg-t1* 0))))))
           ;; Cas variable: $SP, $GP, etc.
           ((and (listp parsed-reg) (eq (first parsed-reg) :variable))
            (let* ((var-name (second parsed-reg))
                   (reg-keyword (map-symbol-to-register-keyword var-name)))
              (if reg-keyword
                  (let* ((reg-index (reg-keyword-to-index reg-keyword))
                         (offset (+ 36 (* 4 reg-index)))
                         (value-code (compile-expr value-arg env)))
                    (setf code (append value-code
                                     (list (list :SW *reg-v0* *reg-zero* offset)))))
                  (error "SET-REGISTER: symbole ~A n'est pas un registre connu" var-name))))
           (t
            (error "SET-REGISTER avec expression non supportée: ~A" parsed-reg)))))
      
      ;; REGISTER-P: vérifie si argument est un keyword de registre
      (register-p
       (unless (= (length args) 1)
         (error "REGISTER-P attend 1 argument, reçu ~A" (length args)))
       (let* ((arg (first args))
              (parsed-arg (parse-lisp-expr arg)))
         (cond
           ;; Si argument est une constante keyword, évaluer statiquement
           ((and (listp parsed-arg) (eq (first parsed-arg) :constant) (keywordp (second parsed-arg)))
            (let* ((kw (second parsed-arg))
                   (is-reg (member kw '(:$zero :$at :$v0 :$v1 :$a0 :$a1 :$a2 :$a3
                                        :$t0 :$t1 :$t2 :$t3 :$t4 :$t5 :$t6 :$t7 :$t8 :$t9
                                        :$s0 :$s1 :$s2 :$s3 :$s4 :$s5 :$s6 :$s7
                                        :$k0 :$k1 :$gp :$sp :$fp :$ra :$pc :$hi :$lo
                                        :$gt :$lt :$eq))))
              ;; Retourner 1 si registre, 0 sinon
              (setf code (list (list :LI (if is-reg 1 0) *reg-v0*)))))
           ;; Si argument est une variable, compiler pour charger puis comparer
           ((and (listp parsed-arg) (eq (first parsed-arg) :variable))
            ;; Charger la variable puis vérifier si c'est un registre
            ;; Pour simplification: on génère du code qui vérifie dynamiquement
            (let ((var-code (compile-expr parsed-arg env)))
              ;; $v0 contient maintenant la valeur de OPERAND
              ;; Vérifier si elle est dans la plage des registres (0-37)
              (setf code (append var-code
                               (list (list :LI 0 *reg-t0*)          ; $t0 = 0
                                     (list :SLT *reg-t0* *reg-v0* *reg-t1*)  ; $t1 = (0 < $v0)
                                     (list :LI 38 *reg-t0*)         ; $t0 = 38
                                     (list :SLT *reg-v0* *reg-t0* *reg-t2*)  ; $t2 = ($v0 < 38)
                                     (list :AND *reg-t1* *reg-t2* *reg-v0*))))))  ; $v0 = $t1 && $t2
           ;; Cas dynamique: pas supporté pour l'instant
           (t
            (error "REGISTER-P avec expression non supportée: ~A" parsed-arg)))))
      
      ;; GET-REG: mapping des noms courts vers noms complets
      (get-reg
       (unless (= (length args) 1)
         (error "GET-REG attend 1 argument, reçu ~A" (length args)))
       (let* ((arg (first args))
              (parsed-arg (parse-lisp-expr arg)))
         (cond
           ;; Si argument est une constante keyword, mapper statiquement
           ((and (listp parsed-arg) (eq (first parsed-arg) :constant) (keywordp (second parsed-arg)))
            (let* ((short-name (second parsed-arg))
                   (long-name (case short-name
                                (:sp :$sp) (:ra :$ra) (:fp :$fp) (:pc :$pc)
                                (:v0 :$v0) (:a0 :$a0) (:a1 :$a1) (:a2 :$a2) (:a3 :$a3)
                                (:s0 :$s0) (:s1 :$s1) (:s2 :$s2) (:s3 :$s3)
                                (:s4 :$s4) (:s5 :$s5) (:s6 :$s6) (:s7 :$s7)
                                (:t0 :$t0) (:t1 :$t1) (:t2 :$t2) (:t3 :$t3)
                                (:t4 :$t4) (:t5 :$t5) (:t6 :$t6) (:t7 :$t7)
                                (:t8 :$t8) (:t9 :$t9) (:zero :$zero) (:gp :$gp)
                                (t short-name))))  ; Si déjà long, garder tel quel
              ;; Charger le keyword résultat comme constante
              ;; NOTE: Les keywords sont représentés par leur INDEX dans le compilateur
              (let ((reg-index (reg-keyword-to-index long-name)))
                (setf code (list (list :LI reg-index *reg-v0*))))))
           ;; Constante 0 (NIL) ou nombre
           ((and (listp parsed-arg) (eq (first parsed-arg) :constant) (numberp (second parsed-arg)))
            ;; Retourner la constante telle quelle (ex: PC=0 dans FETCH-INSTRUCTION)
            (setf code (list (list :LI (second parsed-arg) *reg-v0*))))
           ;; Si argument est une variable, essayer de mapper
           ((and (listp parsed-arg) (eq (first parsed-arg) :variable))
            (let* ((var-name (second parsed-arg))
                   (reg-keyword (map-symbol-to-register-keyword var-name)))
              (if reg-keyword
                  ;; C'est un symbole de registre (ex: ZERO, PC, SP)
                  (let ((reg-index (reg-keyword-to-index reg-keyword)))
                    (setf code (list (list :LI reg-index *reg-v0*))))
                  ;; Sinon erreur
                  (error "GET-REG: symbole ~A n'est pas un registre connu" var-name))))
           (t
            (error "GET-REG avec expression non supportée: ~A" parsed-arg)))))
      
      (t
       (error "Intrinsèque inconnue: ~A" func-name)))
    
    code))

(defun reg-keyword-to-index (keyword)
  "Convertit un keyword de registre en index numérique.
   Ex: :$zero → 0, :$sp → 29, :$pc → 32, etc."
  (case keyword
    (:$zero 0) (:$at 1)
    (:$v0 2) (:$v1 3)
    (:$a0 4) (:$a1 5) (:$a2 6) (:$a3 7)
    (:$t0 8) (:$t1 9) (:$t2 10) (:$t3 11) (:$t4 12) (:$t5 13) (:$t6 14) (:$t7 15)
    (:$s0 16) (:$s1 17) (:$s2 18) (:$s3 19) (:$s4 20) (:$s5 21) (:$s6 22) (:$s7 23)
    (:$t8 24) (:$t9 25)
    (:$k0 26) (:$k1 27)
    (:$gp 28) (:$sp 29) (:$fp 30) (:$ra 31)
    (:$pc 32) (:$hi 33) (:$lo 34)
    (:$gt 35) (:$lt 36) (:$eq 37)
    ;; Anciens noms compatibilité
    (:R0 0) (:R1 1) (:R2 2)
    (:PL 32) (:FP 30) (:SP 29) (:HP 28)
    (:GT 35) (:LT 36) (:EQ 37)
    (t (error "Registre inconnu: ~A" keyword))))

(defun map-symbol-to-register-keyword (symbol)
  "Mappe un symbole ($SP, $GP, ZERO, etc.) vers un keyword de registre (:$sp, :$gp, :$zero).
   Retourne NIL si le symbole n'est pas un registre connu.
   
   Exemples:
   - $SP → :$sp
   - $GP → :$gp
   - ZERO → :$zero
   - PC → :$pc"
  (case symbol
    ;; Symboles avec $ (style MIPS)
    ($ZERO :$zero) ($AT :$at)
    ($V0 :$v0) ($V1 :$v1)
    ($A0 :$a0) ($A1 :$a1) ($A2 :$a2) ($A3 :$a3)
    ($T0 :$t0) ($T1 :$t1) ($T2 :$t2) ($T3 :$t3) ($T4 :$t4) ($T5 :$t5) ($T6 :$t6) ($T7 :$t7)
    ($S0 :$s0) ($S1 :$s1) ($S2 :$s2) ($S3 :$s3) ($S4 :$s4) ($S5 :$s5) ($S6 :$s6) ($S7 :$s7)
    ($T8 :$t8) ($T9 :$t9)
    ($K0 :$k0) ($K1 :$k1)
    ($GP :$gp) ($SP :$sp) ($FP :$fp) ($RA :$ra)
    ($PC :$pc) ($HI :$hi) ($LO :$lo)
    ($GT :$gt) ($LT :$lt) ($EQ :$eq)
    ;; Symboles sans $ (style ancien)
    (ZERO :$zero) (AT :$at)
    (V0 :$v0) (V1 :$v1)
    (A0 :$a0) (A1 :$a1) (A2 :$a2) (A3 :$a3)
    (T0 :$t0) (T1 :$t1) (T2 :$t2) (T3 :$t3) (T4 :$t4) (T5 :$t5) (T6 :$t6) (T7 :$t7)
    (S0 :$s0) (S1 :$s1) (S2 :$s2) (S3 :$s3) (S4 :$s4) (S5 :$s5) (S6 :$s6) (S7 :$s7)
    (T8 :$t8) (T9 :$t9)
    (K0 :$k0) (K1 :$k1)
    (GP :$gp) (SP :$sp) (FP :$fp) (RA :$ra)
    (PC :$pc) (HI :$hi) (LO :$lo)
    (GT :$gt) (LT :$lt) (EQ :$eq)
    ;; Anciens noms compatibilité
    (R0 :R0) (R1 :R1) (R2 :R2)
    (PL :$pc) (HP :$gp) (MEM :$zero)
    ;; Non trouvé
    (t nil)))

(defun compile-call (func-name args env)
  "Compile un appel de fonction - gère static links pour closures et appels indirects (PHASE 9)
   + PHASE 11: Gère intrinsèques (get-register, set-register, register-p, get-reg)"
  
  ;; PHASE 11: Support des fonctions intrinsèques VM
  ;; Ces fonctions sont compilées directement sans appel de fonction
  (when (and (symbolp func-name) (member func-name '(get-register set-register register-p get-reg)))
    (return-from compile-call (compile-intrinsic func-name args env)))
  
  (let* ((code '())
    (arg-regs (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*))
    ;; PHASE 9: Détecter si c'est un appel de closure
    ;; Si func-name est un symbole, vérifier si c'est une variable (closure) ou fonction
    (is-variable (and (symbolp func-name) 
                      (lookup-variable-with-depth env func-name)))
    (is-closure-call (or (not (symbolp func-name)) is-variable))
    ;; Chercher fonction dans environnement lexical (LABELS) le long de la chaîne
    ;; parent-lexical. lookup-function-def-info retourne (LABEL . DEPTH) ou NIL.
    (fn-info (unless is-closure-call (lookup-function-def-info env func-name)))
    (target-label (if fn-info (car fn-info) func-name))
    (is-local-fn fn-info)
    ;; Déterminer quel static link passer:
    ;; - Si sibling (même niveau lexical) → passer $S0 (static link du parent)
    ;; - Si enfant (fonction dans un LABELS imbriqué) → passer $FP (notre frame)
    (fn-depth (if fn-info (cdr fn-info) nil))
    (current-depth (compiler-env-lexical-depth env))
    (is-sibling (and fn-depth (= fn-depth current-depth))))
    
    ;; PHASE 9: Si appel de closure, compiler l'expression pour obtenir l'adresse AVANT de sauvegarder les registres
    ;; PHASE 9 FIX: Compiler la closure AVANT de sauvegarder $S4-$S6, sinon on sauvegarde l'ancienne valeur
    ;; et on perd la closure après la restauration
    (when is-closure-call
      ;; Compiler l'expression qui donne la closure (ex: variable contenant une closure)
      (let ((closure-code (compile-expr func-name env)))
        ;; La closure est maintenant dans $v0
        (setf code (append code closure-code))
        ;; Sauvegarder l'adresse de la closure sur la pile
        (setf code (append code (list (list :ADDI *reg-sp* -4 *reg-sp*)
                                       (list :SW *reg-v0* *reg-sp* 0))))))
    
    ;; Sauvegarder $s0, $ra, et $s4-$s6 (variables LET) sur la pile avant l'appel
    ;; PHASE 9 FIX: Le lambda peut écraser $S4-$S6, donc il faut les sauvegarder
    ;; IMPORTANT: On fait ça APRÈS avoir compilé la closure, sinon on sauvegarde l'ancien $S5 au lieu de la closure
    (setf code (append code
                      (list (list :ADDI *reg-sp* -20 *reg-sp*)  ; 5 registres * 4 = 20 bytes
                            (list :SW *reg-s0* *reg-sp* 0)
                            (list :SW *reg-ra* *reg-sp* 4)
                            (list :SW *reg-s4* *reg-sp* 8)    ; Sauvegarder $S4
                            (list :SW *reg-s5* *reg-sp* 12)   ; Sauvegarder $S5
                            (list :SW *reg-s6* *reg-sp* 16))))  ; Sauvegarder $S6
    
    ;; PHASE 8 FIX: Passer le bon static link selon la relation avec la fonction appelée
    (when is-local-fn
      (if is-sibling
          ;; Sibling: passer $S0 tel quel (static link du parent commun)
          (setf code (append code (list (list :MOVE *reg-s0* *reg-t3*))))
          ;; Enfant: passer $FP (notre frame devient leur static link)
          (setf code (append code (list (list :MOVE (get-reg :fp) *reg-t3*))))))
    
    ;; Compiler les arguments et les placer dans $a0-$a3
    (loop for arg in args
          for reg in arg-regs
          do (let ((arg-code (compile-expr arg env)))
               (setf code (append code
                                 arg-code
                                 (list (list :MOVE *reg-v0* reg))))))
    
    ;; PHASE 9: Pour appel de closure, charger le label et passer la closure dans $s1
    (when is-closure-call
      ;; PHASE 9 FIX: La closure est maintenant à l'offset 20 (après les 5 registres de 4 bytes chacun)
      (setf code (append code (list (list :LW *reg-t9* *reg-sp* 20))))  ; Format: (LW dest base offset)
      ;; Charger l'adresse de la fonction depuis heap[closure+0]
      (setf code (append code
                        (list (list :LI 0 *reg-t2*)                    ; offset = 0
                              (list :LOAD-HEAP *reg-t9* *reg-t2* *reg-t8*))))  ; $t8 = label
      ;; Passer l'adresse de la closure dans $s1 (convention pour closures)
      (setf code (append code (list (list :MOVE *reg-t9* *reg-s1*)))))
    
    ;; PHASE 8 FIX: Juste avant l'appel, restaurer le static link sauvegardé
    (when is-local-fn
      (setf code (append code (list (list :MOVE *reg-t3* *reg-s0*)))))
    
    ;; Appel de la fonction (locale, globale, ou via closure)
    (if is-closure-call
        ;; PHASE 9: Appel indirect via JALR (label dans $t8)
        (setf code (append code (list (list :JALR *reg-t8*))))
        ;; Appel direct via JAL
        (setf code (append code (list (list :JAL target-label)))))
    
    ;; Restaurer $ra, $s0, et $s4-$s6 après l'appel
    ;; PHASE 9 FIX: Restaurer aussi $S4-$S6 et nettoyer la closure sur la pile si nécessaire
    (let ((stack-cleanup (if is-closure-call 24 20)))  ; 20 bytes (5 registres) + 4 bytes (closure) si nécessaire
      (append code
              (list (list :LW *reg-ra* *reg-sp* 4)     ; Format: (LW dest base offset)
                    (list :LW *reg-s0* *reg-sp* 0)
                    (list :LW *reg-s4* *reg-sp* 8)     ; Restaurer $S4
                    (list :LW *reg-s5* *reg-sp* 12)    ; Restaurer $S5
                    (list :LW *reg-s6* *reg-sp* 16)    ; Restaurer $S6
                    (list :ADDI *reg-sp* stack-cleanup *reg-sp*))))))

;;; ============================================================================
;;; COMPILATION - DÉFINITION DE FONCTION
;;; ============================================================================

(defun compile-defun (name params body env)
  "Compile une définition de fonction avec gestion correcte de la pile"
  (let ((func-label name)
        (code '())
        (new-env (make-compiler-env))
        (num-params (length params)))
    
    ;; Label de début de fonction
    (setf code (append code (list (list :LABEL func-label))))
    
    ;; Prologue: sauvegarder $ra et les paramètres sur la pile
    ;; Stack frame: [$ra] [param0] [param1] ... [paramN]
    (let ((stack-size (+ 4 (* 4 num-params))))  ; 4 pour $ra + 4 par param
      ;; Allouer espace pour $ra + params
      (setf code (append code (list (list :ADDI *reg-sp* (- stack-size) *reg-sp*))))
      
      ;; Sauvegarder $ra au sommet
      (setf code (append code (list (list :SW *reg-ra* *reg-sp* 0))))
      
      ;; Sauvegarder chaque paramètre sur la pile et mapper vers sa position
      (loop for param in params
            for i from 0
            for arg-reg in (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*)
            for saved-reg in (list *reg-s0* *reg-s1* *reg-s2* *reg-s3*)
            for offset = (* 4 (+ 1 i))  ; Offset depuis $sp (après $ra)
            do (progn
                 ;; Sauvegarder $aX sur la pile
                 (setf code (append code (list (list :SW arg-reg *reg-sp* offset))))
                 ;; Charger depuis la pile vers $sX pour l'utiliser
                 ;; Format: (LW dest base offset)
                 (setf code (append code (list (list :LW saved-reg *reg-sp* offset))))
                 ;; Mapper le paramètre vers le registre sauvegardé
                 (add-variable new-env param saved-reg))))
    
    ;; Compiler le corps de la fonction (dernière expr = valeur retour dans $v0)
    (dolist (expr body)
      (setf code (append code (compile-expr expr new-env))))
    
    ;; Épilogue: restaurer $ra et libérer la pile
    (let ((stack-size (+ 4 (* 4 num-params))))
      (setf code (append code
                        (list (list :LW *reg-ra* *reg-sp* 0)         ; Restaurer $ra - Format: (LW dest base offset)
                              (list :ADDI *reg-sp* stack-size *reg-sp*)  ; Libérer pile
                              (list :JR *reg-ra*)))))            ; Retour
    
    ;; Enregistrer la fonction dans l'environnement
    (push (cons name params) (compiler-env-functions env))
    
    code))

;;; ============================================================================
;;; FONCTION PRINCIPALE DE COMPILATION
;;; ============================================================================

(defun compile-lisp (expr)
  "Compile une expression LISP en code assembleur MIPS"
  (let ((env (make-new-compiler-env)))
    (compile-expr expr env)))

(defun compile-and-run (expr &key (verbose nil))
  "Compile et exécute une expression LISP"
  (let ((vm (make-new-vm :verbose verbose))
        (asm-code (append (compile-lisp expr)
                         (list (list :PRINT *reg-v0*)  ; Afficher le résultat
                               (list :HALT)))))    ; Arrêter la VM
    (format t "~%=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
    (dolist (instr asm-code)
      (format t "~A~%" instr))
    (format t "~%=== EXÉCUTION ===~%")
    (load-and-run vm asm-code :verbose verbose)
    (format t "~%Résultat dans $v0: ~A~%" (get-register vm *reg-v0*))
    vm))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(compile-lisp compile-and-run))

(format t "~%Compilateur LISP → MIPS chargé.~%")
(format t "Fonctions disponibles:~%")
(format t "  - (compile-lisp expr) : Compile une expression LISP~%")
(format t "  - (compile-and-run expr) : Compile et exécute~%")
(format t "~%Pour les tests, charger: (load \"test-compiler.lisp\")~%~%")
