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
  (max-temp-regs 3))        ; Nombre maximum de registres temporaires ($t0, $t1, $t2)

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
  "Compile l'accès à une variable"
  (let ((reg (lookup-variable env var)))
    (if reg
        ;; Variable trouvée, la copier dans $v0
        (list (list :MOVE reg *reg-v0*))
        ;; Variable non trouvée, erreur
        (error "Variable non définie: ~A" var))))

;;; ============================================================================
;;; COMPILATION - ARITHMÉTIQUE
;;; ============================================================================

(defun compile-arithmetic (op args env)
  "Compile une opération arithmétique - utilise registres si disponibles, sinon pile"
  (cond
    ;; Opération binaire: (+ a b)
    ((= (length args) 2)
     (let* ((arg1 (first args))
            (arg2 (second args))
            (code1 (compile-expr arg1 env))
            ;; Essayer d'allouer deux registres temporaires
            (reg1 (allocate-temp-reg env))
            (reg2 (allocate-temp-reg env)))
       
       (cond
         ;; Cas 1: Deux registres disponibles - optimal
         ((and reg1 reg2)
          (prog1
              (append
               code1
               (list (list :MOVE *reg-v0* reg1))  ; arg1 → reg1
               (compile-expr arg2 env)
               (list (list :MOVE *reg-v0* reg2))  ; arg2 → reg2
               (case op
                 (+ (list (list :ADD reg1 reg2 *reg-v0*)))
                 (- (list (list :SUB reg1 reg2 *reg-v0*)))
                 (* (list (list :MUL reg1 reg2)
                          (list :MFLO *reg-v0*)))
                 (/ (list (list :DIV reg1 reg2)
                          (list :MFLO *reg-v0*)))
                 (mod (list (list :DIV reg1 reg2)
                            (list :MFHI *reg-v0*)))
                 (t (error "Opérateur arithmétique non supporté: ~A" op))))
            ;; Libérer les registres
            (free-temp-reg env reg2)
            (free-temp-reg env reg1)))
         
         ;; Cas 2: Un seul registre disponible - spill arg2 sur pile
         (reg1
          (prog1
              (append
               code1
               (list (list :MOVE *reg-v0* reg1))  ; arg1 → reg1
               (compile-expr arg2 env)
               ;; arg2 reste dans $v0, opération reg1 op $v0
               (case op
                 (+ (list (list :ADD reg1 *reg-v0* *reg-v0*)))
                 (- (list (list :SUB reg1 *reg-v0* *reg-v0*)))
                 (* (list (list :MUL reg1 *reg-v0*)
                          (list :MFLO *reg-v0*)))
                 (/ (list (list :DIV reg1 *reg-v0*)
                          (list :MFLO *reg-v0*)))
                 (mod (list (list :DIV reg1 *reg-v0*)
                            (list :MFHI *reg-v0*)))
                 (t (error "Opérateur arithmétique non supporté: ~A" op))))
            (free-temp-reg env reg1)))
         
         ;; Cas 3: Aucun registre disponible - utiliser pile pour arg1
         (t
          (append
           code1
           ;; Sauvegarder arg1 sur la pile
           (list (list :ADDI *reg-sp* -4 *reg-sp*)
                 (list :SW *reg-v0* *reg-sp* 0))
           (compile-expr arg2 env)
           (list (list :MOVE *reg-v0* *reg-t1*))  ; arg2 → $t1
           ;; Restaurer arg1 depuis pile
           (list (list :LW *reg-sp* 0 *reg-t0*)   ; arg1 → $t0
                 (list :ADDI *reg-sp* 4 *reg-sp*))
           (case op
             (+ (list (list :ADD *reg-t0* *reg-t1* *reg-v0*)))
             (- (list (list :SUB *reg-t0* *reg-t1* *reg-v0*)))
             (* (list (list :MUL *reg-t0* *reg-t1*)
                      (list :MFLO *reg-v0*)))
             (/ (list (list :DIV *reg-t0* *reg-t1*)
                      (list :MFLO *reg-v0*)))
             (mod (list (list :DIV *reg-t0* *reg-t1*)
                        (list :MFHI *reg-v0*)))
             (t (error "Opérateur arithmétique non supporté: ~A" op))))))))
    
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
  "Compile une comparaison"
  (unless (= (length args) 2)
    (error "Comparaison requiert exactement 2 arguments"))
  
  (let* ((arg1 (first args))
         (arg2 (second args))
         (code1 (compile-expr arg1 env))
         (reg1 *reg-t0*)
         (code2 (compile-expr arg2 env))
         (reg2 *reg-t1*))
    (append
     code1
     (list (list :MOVE *reg-v0* reg1))  ; Résultat 1 dans $t0
     code2
     (list (list :MOVE *reg-v0* reg2))  ; Résultat 2 dans $t1
     (case op
       (< (list (list :SLT reg1 reg2 *reg-v0*)))
       (> (list (list :SLT reg2 reg1 *reg-v0*)))
       (<= (list (list :SLT reg2 reg1 *reg-t2*)
                 (list :LI 1 *reg-t3*)
                 (list :SUB *reg-t3* *reg-t2* *reg-v0*)))
       (>= (list (list :SLT reg1 reg2 *reg-t2*)
                 (list :LI 1 *reg-t3*)
                 (list :SUB *reg-t3* *reg-t2* *reg-v0*)))
       (= (list (list :SUB reg1 reg2 *reg-t2*)
                (list :BEQ *reg-t2* *reg-zero* (gen-label env "EQUAL"))
                (list :LI 0 *reg-v0*)
                (list :J (gen-label env "END_EQ"))
                (list :LABEL (gen-label env "EQUAL"))
                (list :LI 1 *reg-v0*)
                (list :LABEL (gen-label env "END_EQ"))))
       (/= (list (list :SUB reg1 reg2 *reg-t2*)
                 (list :BNE *reg-t2* *reg-zero* (gen-label env "NOT_EQUAL"))
                 (list :LI 0 *reg-v0*)
                 (list :J (gen-label env "END_NE"))
                 (list :LABEL (gen-label env "NOT_EQUAL"))
                 (list :LI 1 *reg-v0*)
                 (list :LABEL (gen-label env "END_NE"))))
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
        (arg-regs (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*)))
    ;; Sauvegarder $s0 sur la pile avant l'appel (contient le paramètre de la fonction courante)
    (setf code (append code
                      (list (list :ADDI *reg-sp* -4 *reg-sp*)
                            (list :SW *reg-s0* *reg-sp* 0))))
    ;; Compiler les arguments et les placer dans $a0-$a3
    (loop for arg in args
          for reg in arg-regs
          do (let ((arg-code (compile-expr arg env)))
               (setf code (append code
                                 arg-code
                                 (list (list :MOVE *reg-v0* reg))))))
    ;; Appel de la fonction
    (setf code (append code (list (list :JAL func-name))))
    ;; Restaurer $s0 après l'appel
    (append code
            (list (list :LW *reg-sp* 0 *reg-s0*)
                  (list :ADDI *reg-sp* 4 *reg-sp*)))))

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
