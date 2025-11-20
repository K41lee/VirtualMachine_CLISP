;;;; compiler.lisp
;;;; Compilateur LISP vers assembleur MIPS

(load "loader.lisp")

;;; ============================================================================
;;; DÉFINITION SÉCURISÉE DES REGISTRES (évite problème $ dans CLISP)
;;; ============================================================================

;; Créer les symboles de registres depuis la liste *register-names*
(defparameter *reg-v0* (get-reg :v0))
(defparameter *reg-t0* (get-reg :t0))
(defparameter *reg-t1* (get-reg :t1))
(defparameter *reg-t2* (get-reg :t2))
(defparameter *reg-t3* (get-reg :t3))
(defparameter *reg-a0* (get-reg :a0))
(defparameter *reg-a1* (get-reg :a1))
(defparameter *reg-a2* (get-reg :a2))
(defparameter *reg-a3* (get-reg :a3))
(defparameter *reg-s0* (get-reg :s0))
(defparameter *reg-s1* (get-reg :s1))
(defparameter *reg-s2* (get-reg :s2))
(defparameter *reg-s3* (get-reg :s3))
(defparameter *reg-sp* (get-reg :sp))
(defparameter *reg-ra* (get-reg :ra))
(defparameter *reg-zero* (get-reg :zero))

;;; ============================================================================
;;; ENVIRONNEMENT DE COMPILATION
;;; ============================================================================

(defstruct compiler-env
  "Environnement de compilation pour gérer variables et labels"
  (variables '())           ; Liste des variables locales et leurs registres/offsets pile
  (functions '())           ; Table des fonctions définies
  (label-counter 0)         ; Compteur pour générer des labels uniques
  (temp-regs-available '()) ; Liste des registres temporaires disponibles
  (max-temp-regs 3)         ; Nombre maximum de registres temporaires ($t0, $t1, $t2)
  (stack-offset 0)          ; Offset courant pour variables sur la pile
  (parent-env nil))         ; Environnement parent (pour portée lexicale)

(defun make-new-compiler-env ()
  "Crée un nouvel environnement de compilation avec pool de registres limité"
  (let ((env (make-compiler-env)))
    ;; Initialiser le pool de registres temporaires (seulement $t0, $t1, $t2)
    (setf (compiler-env-temp-regs-available env) 
          (list *reg-t0* *reg-t1* *reg-t2*))
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
  (let ((label (intern (format nil "~A_~A" prefix (compiler-env-label-counter env)))))
    (incf (compiler-env-label-counter env))
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

(defun copy-env (env)
  "Crée une copie d'environnement pour un nouveau scope (let, labels, etc.)"
  (let ((new-env (make-compiler-env)))
    ;; Copier les champs de base
    (setf (compiler-env-variables new-env) (copy-list (compiler-env-variables env)))
    (setf (compiler-env-functions new-env) (copy-list (compiler-env-functions env)))
    (setf (compiler-env-label-counter new-env) (compiler-env-label-counter env))
    (setf (compiler-env-temp-regs-available new-env) (copy-list (compiler-env-temp-regs-available env)))
    (setf (compiler-env-max-temp-regs new-env) (compiler-env-max-temp-regs env))
    (setf (compiler-env-stack-offset new-env) (compiler-env-stack-offset env))
    (setf (compiler-env-parent-env new-env) env)
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
    
    ((symbolp expr)
     (list :variable expr))
    
    ((null expr)
     (list :constant 0))  ; nil = 0
    
    ((eq expr t)
     (list :constant 1))  ; t = 1
    
    ((lisp-list-p expr)
     (let ((op (first expr))
           (args (rest expr)))
       (case op
         ;; Opérateurs arithmétiques
         ((+ - * / mod)
          (list :arithmetic op args))
         
         ;; Opérateurs de comparaison
         ((< > <= >= = /=)
          (list :comparison op args))
         
         ;; Structure conditionnelle IF
         (if
          (list :if (first args) (second args) (third args)))
         
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
         
         ;; SETQ (assignation variable)
         (setq
          (list :setq (first args) (second args)))
         
         ;; Structure LABELS (fonctions locales)
         (labels
          ;; Syntaxe: (labels ((fn1 args1 body1) (fn2 args2 body2) ...) body)
          (if (and (>= (length args) 2)
                   (listp (first args)))
              (list :labels (first args) (rest args))
              (error "Syntaxe LABELS incorrecte: ~A" expr)))
         
         ;; Définition de fonction
         (defun
          (list :defun (first args) (second args) (cddr args)))
         
         ;; Appel de fonction
         (t
          (if (symbolp op)
              (list :call op args)
              (error "Expression invalide: ~A" expr))))))
    
    (t (error "Expression LISP non reconnue: ~A" expr))))

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
  "Compile une référence à une variable (registre ou pile)"
  (let ((location (lookup-variable env var)))
    (cond
      ;; Cas 1 : Variable dans un registre
      ((and location (symbolp location))
       (list (list :MOVE location *reg-v0*)))
      
      ;; Cas 2 : Variable sur la pile (offset depuis SP)
      ((and location (consp location) (eq (car location) :stack))
       (let ((offset (cdr location)))
         (list (list :LW *reg-sp* offset *reg-v0*))))
      
      ;; Cas 3 : Variable paramètre de fonction (offset depuis FP)
      ((and location (consp location) (eq (car location) :fp))
       (let ((offset (cdr location)))
         (list (list :LW (get-reg :fp) offset *reg-v0*))))
      
      ;; Cas 4 : Variable non trouvée
      (t
       (error "Variable non définie: ~A" var)))))

;;; ============================================================================
;;; COMPILATION - ARITHMÉTIQUE
;;; ============================================================================

(defun compile-arithmetic (op args env)
  "Compile une opération arithmétique - utilise la pile pour éviter conflits avec registres caller-saved"
  (cond
    ;; Opération binaire: (+ a b)
    ((= (length args) 2)
     (let* ((arg1 (first args))
            (arg2 (second args))
            (code1 (compile-expr arg1 env)))
       ;; TOUJOURS utiliser la pile pour arg1 (sûr pour les appels récursifs)
       ;; Les registres T sont caller-saved et peuvent être écrasés par les appels
       (append
        code1
        ;; Sauvegarder arg1 sur la pile
        (list (list :ADDI *reg-sp* -4 *reg-sp*)
              (list :SW *reg-v0* *reg-sp* 0))
        ;; Compiler arg2
        (compile-expr arg2 env)
        ;; arg2 dans $v0, sauvegarder dans un registre temporaire
        (list (list :MOVE *reg-v0* *reg-t1*))
        ;; Restaurer arg1 depuis pile dans un autre registre
        (list (list :LW *reg-sp* 0 *reg-t0*)
              (list :ADDI *reg-sp* 4 *reg-sp*))
        ;; Effectuer l'opération
        (case op
          (+ (list (list :ADD *reg-t0* *reg-t1* *reg-v0*)))
          (- (list (list :SUB *reg-t0* *reg-t1* *reg-v0*)))
          (* (list (list :MUL *reg-t0* *reg-t1*)
                   (list :MFLO *reg-v0*)))
          (/ (list (list :DIV *reg-t0* *reg-t1*)
                   (list :MFLO *reg-v0*)))
          (mod (list (list :DIV *reg-t0* *reg-t1*)
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
     (list (list :LW *reg-sp* 0 *reg-s2*)
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

;;; ============================================================================
;;; COMPILATION - SETQ (assignation variable)
;;; ============================================================================

(defun compile-setq (var value env)
  "Compile (setq var value)
   Modifie la valeur d'une variable existante"
  (let ((location (lookup-variable env var))
        (code '()))
    
    ;; Compiler la nouvelle valeur
    (setf code (append code (compile-expr value env)))
    
    ;; Stocker dans la variable
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
    
    code))

(defun compile-labels (definitions body env)
  "Compile (labels ((fn1 args1 body1) (fn2 args2 body2) ...) body)
   Définit des fonctions locales avec portée lexicale"
  (let* ((new-env (copy-env env))
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
             (fn-env (copy-env new-env))
             (arg-regs (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*)))
        
        ;; Vérifier nombre de paramètres
        (when (> (length fn-args) 4)
          (error "LABELS: Maximum 4 paramètres supportés"))
        
        ;; Label de la fonction
        (setf code (append code (list (list :LABEL fn-label))))
        
        ;; SOLUTION ROBUSTE : Frame Pointer pour accès stable aux paramètres
        ;; Utilisons $FP (registre 30) comme base fixe pour les paramètres
        ;; Le registre FP est préservé et ne change pas pendant l'exécution
        (if (> (length fn-args) 0)
            ;; Cas avec paramètres : utiliser FP
            (let ((num-params (length fn-args)))
              ;; 1. Sauvegarder ancien FP et RA sur la pile
              (setf code (append code (list (list :ADDI *reg-sp* -8 *reg-sp*)
                                           (list :SW (get-reg :fp) *reg-sp* 0)
                                           (list :SW *reg-ra* *reg-sp* 4))))
              
              ;; 2. FP = SP actuel (début de notre frame)
              (setf code (append code (list (list :MOVE *reg-sp* (get-reg :fp)))))
              
              ;; 3. Allouer espace pour les paramètres
              (let ((stack-size (* 4 num-params)))
                (setf code (append code (list (list :ADDI *reg-sp* (- stack-size) *reg-sp*))))
                
                ;; 4. Sauvegarder chaque paramètre A0-A3 sur la pile
                ;;    Les offsets sont calculés depuis FP
                (loop for i from 0 below num-params
                      for arg in fn-args
                      for arg-reg in arg-regs
                      do (let ((fp-offset (- (* 4 (+ i 1)))))  ; -4, -8, -12, -16
                           (setf code (append code (list (list :SW arg-reg (get-reg :fp) fp-offset))))
                           ;; Ajouter la variable avec offset depuis FP
                           (add-variable fn-env arg (cons :fp fp-offset)))))
              
              ;; 5. Compiler le corps de la fonction
              (dolist (expr fn-body)
                (setf code (append code (compile-expr expr fn-env))))
              
              ;; 6. Restaurer SP, FP et RA avant le retour (ordre important!)
              ;;    D'abord restaurer SP au début du frame
              (setf code (append code (list (list :MOVE (get-reg :fp) *reg-sp*))))  ; SP = FP
              ;;    Ensuite charger RA et ancien FP depuis SP
              (setf code (append code (list (list :LW *reg-sp* 4 *reg-ra*))))  ; RA = [SP+4]
              (setf code (append code (list (list :LW *reg-sp* 0 (get-reg :fp)))))  ; FP = [SP+0]
              ;;    Enfin libérer la pile (FP + RA = 8 bytes)
              (setf code (append code (list (list :ADDI *reg-sp* 8 *reg-sp*)))))  ; SP += 8
            
            ;; Cas sans paramètres : pas de FP nécessaire
            (dolist (expr fn-body)
              (setf code (append code (compile-expr expr fn-env)))))
        
        ;; Retour de la fonction
        (setf code (append code (list (list :JR *reg-ra*))))))
    
    ;; ÉTAPE 4: Label du corps principal et compilation
    (setf code (append code (list (list :LABEL body-label))))
    
    ;; Compiler le corps principal
    (dolist (expr body)
      (setf code (append code (compile-expr expr new-env))))
    
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
      
      (:comparison
       (compile-comparison (second parsed) (third parsed) env))
      
      (:if
       (compile-if (second parsed) (third parsed) (fourth parsed) env))
      
      (:let
       (compile-let (second parsed) (third parsed) env))
      
      (:loop-while
       (compile-loop-while (second parsed) (third parsed) env))
      
      (:setq
       (compile-setq (second parsed) (third parsed) env))
      
      (:labels
       (compile-labels (second parsed) (third parsed) env))
      
      (:call
       (compile-call (second parsed) (third parsed) env))
      
      (:defun
       (compile-defun (second parsed) (third parsed) (fourth parsed) env))
      
      (t (error "Type d'expression non supporté: ~A" (first parsed))))))

;;; ============================================================================
;;; COMPILATION - APPELS DE FONCTION
;;; ============================================================================

(defun compile-call (func-name args env)
  "Compile un appel de fonction - sauvegarde $s0 car il contient les paramètres"
  (let ((code '())
        (arg-regs (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*))
        ;; Chercher fonction dans environnement local (LABELS) sinon utiliser nom global (DEFUN)
        (target-label (or (lookup-function env func-name) func-name)))
    
    ;; Sauvegarder $s0 et $ra sur la pile avant l'appel
    (setf code (append code
                      (list (list :ADDI *reg-sp* -8 *reg-sp*)
                            (list :SW *reg-s0* *reg-sp* 0)
                            (list :SW *reg-ra* *reg-sp* 4))))
    
    ;; Compiler les arguments et les placer dans $a0-$a3
    (loop for arg in args
          for reg in arg-regs
          do (let ((arg-code (compile-expr arg env)))
               (setf code (append code
                                 arg-code
                                 (list (list :MOVE *reg-v0* reg))))))
    
    ;; Appel de la fonction (locale ou globale)
    (setf code (append code (list (list :JAL target-label))))
    
    ;; Restaurer $ra et $s0 après l'appel
    (append code
            (list (list :LW *reg-sp* 4 *reg-ra*)
                  (list :LW *reg-sp* 0 *reg-s0*)
                  (list :ADDI *reg-sp* 8 *reg-sp*)))))

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
                 (setf code (append code (list (list :LW *reg-sp* offset saved-reg))))
                 ;; Mapper le paramètre vers le registre sauvegardé
                 (add-variable new-env param saved-reg))))
    
    ;; Compiler le corps de la fonction (dernière expr = valeur retour dans $v0)
    (dolist (expr body)
      (setf code (append code (compile-expr expr new-env))))
    
    ;; Épilogue: restaurer $ra et libérer la pile
    (let ((stack-size (+ 4 (* 4 num-params))))
      (setf code (append code
                        (list (list :LW *reg-sp* 0 *reg-ra*)         ; Restaurer $ra
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
