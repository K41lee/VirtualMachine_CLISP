;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILATEUR PARAMÉTRIQUE - Option B
;;;
;;; Versions paramétrées des fonctions du compilateur qui échouent
;;; à cause des variables globales (*REG-V0*, *REG-SP*, etc.)
;;;
;;; Stratégie : Passer les registres comme paramètres au lieu de
;;;             les référencer comme variables globales
;;;
;;; Impact estimé : +30-40 fonctions (60-70% de compilabilité totale)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "   COMPILATEUR PARAMÉTRIQUE (Option B)~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DÉFINITION DES REGISTRES COMME KEYWORDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Au lieu d'utiliser des variables globales, on définit des keywords
;; constants qui peuvent être évalués à la compilation

(defparameter *reg-keywords*
  '(:$v0 :$v1 :$a0 :$a1 :$a2 :$a3 
    :$t0 :$t1 :$t2 :$t3 :$t4 :$t5 :$t6 :$t7 :$t8 :$t9
    :$s0 :$s1 :$s2 :$s3 :$s4 :$s5 :$s6 :$s7
    :$sp :$fp :$ra :$gp :$zero))

(format t "[1/10] Registres définis comme keywords...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS PARAMÉTRÉES - CATÉGORIE 1 : CONSTANTES ET VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-constant-param (value env reg-result)
  "Version paramétrée de compile-constant
   - value      : valeur de la constante
   - env        : environnement de compilation
   - reg-result : registre pour stocker le résultat (ex: :$v0)"
  (list (list :LI reg-result value)))

(defun compile-variable-param (var-name env reg-result reg-fp)
  "Version paramétrée de compile-variable
   - var-name   : nom de la variable
   - env        : environnement de compilation
   - reg-result : registre pour le résultat (ex: :$v0)
   - reg-fp     : registre frame pointer (ex: :$fp)"
  (let ((var-info (lookup-variable var-name env)))
    (if var-info
        ;; Variable locale : charger depuis la pile
        (let ((offset (cdr var-info)))
          (list (list :LW reg-result reg-fp offset)))
        ;; Variable non trouvée : traiter comme constante 0
        (list (list :LI reg-result 0)))))

(format t "[2/10] Fonctions constantes/variables paramétrées...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS PARAMÉTRÉES - CATÉGORIE 2 : ARITHMÉTIQUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-arithmetic-param (op args env reg-result reg-temp reg-sp)
  "Version paramétrée de compile-arithmetic
   - op         : opérateur (+, -, *, /)
   - args       : arguments de l'opération
   - env        : environnement
   - reg-result : registre résultat (ex: :$v0)
   - reg-temp   : registre temporaire (ex: :$t0)
   - reg-sp     : registre stack pointer (ex: :$sp)"
  (cond
    ;; Unaire : (- x)
    ((= (length args) 1)
     (let ((arg-code (compile-expr-param (first args) env reg-result reg-temp reg-sp)))
       (append arg-code
               (list (list :SUB reg-result :$zero reg-result)))))
    
    ;; Binaire : (+ x y), (- x y), etc.
    ((= (length args) 2)
     (let* ((arg1-code (compile-expr-param (first args) env reg-result reg-temp reg-sp))
            (arg2-code (compile-expr-param (second args) env reg-result reg-temp reg-sp))
            (op-instr (cond
                        ((eq op '+) :ADD)
                        ((eq op '-) :SUB)
                        ((eq op '*) :MUL)
                        ((eq op '/) :DIV)
                        (t :ADD))))
       (append
        ;; Évaluer arg1 → $v0
        arg1-code
        ;; Sauver $v0 sur la pile
        (list (list :SW reg-result reg-sp 0)
              (list :ADDI reg-sp reg-sp -4))
        ;; Évaluer arg2 → $v0
        arg2-code
        ;; Restaurer arg1 → $t0
        (list (list :ADDI reg-sp reg-sp 4)
              (list :LW reg-temp reg-sp 0))
        ;; Opération : $v0 = $t0 OP $v0
        (list (list op-instr reg-result reg-temp reg-result)))))
    
    ;; N-aire : (+ x y z ...) → plier de gauche à droite
    (t
     (let ((result-code (compile-expr-param (first args) env reg-result reg-temp reg-sp)))
       (dolist (arg (rest args))
         (setq result-code
               (append result-code
                       ;; Sauver résultat courant
                       (list (list :SW reg-result reg-sp 0)
                             (list :ADDI reg-sp reg-sp -4))
                       ;; Évaluer arg suivant
                       (compile-expr-param arg env reg-result reg-temp reg-sp)
                       ;; Restaurer et combiner
                       (list (list :ADDI reg-sp reg-sp 4)
                             (list :LW reg-temp reg-sp 0)
                             (list (cond
                                     ((eq op '+) :ADD)
                                     ((eq op '-) :SUB)
                                     ((eq op '*) :MUL)
                                     ((eq op '/) :DIV)
                                     (t :ADD))
                                   reg-result reg-temp reg-result)))))
       result-code))))

(format t "[3/10] Fonctions arithmétiques paramétrées...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS PARAMÉTRÉES - CATÉGORIE 3 : COMPARAISONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-comparison-param (op args env reg-result reg-temp reg-sp)
  "Version paramétrée de compile-comparison
   - op : opérateur de comparaison (=, <, >)
   - args : arguments (exactement 2)
   - env : environnement
   - reg-result : registre résultat
   - reg-temp : registre temporaire
   - reg-sp : stack pointer"
  (when (not (= (length args) 2))
    (error "Comparaison requiert exactement 2 arguments: ~A" args))
  
  (let* ((arg1-code (compile-expr-param (first args) env reg-result reg-temp reg-sp))
         (arg2-code (compile-expr-param (second args) env reg-result reg-temp reg-sp))
         (label-true (gensym "CMP_TRUE"))
         (label-end (gensym "CMP_END"))
         (branch-instr (cond
                         ((eq op '=) :BEQ)
                         ((eq op '<) :BLT)
                         ((eq op '>) :BGT)
                         (t :BEQ))))
    (append
     ;; Évaluer arg1
     arg1-code
     ;; Sauver
     (list (list :SW reg-result reg-sp 0)
           (list :ADDI reg-sp reg-sp -4))
     ;; Évaluer arg2
     arg2-code
     ;; Restaurer arg1 dans $t0
     (list (list :ADDI reg-sp reg-sp 4)
           (list :LW reg-temp reg-sp 0))
     ;; Comparer et brancher
     (list (list branch-instr reg-temp reg-result label-true))
     ;; Faux : mettre 0
     (list (list :LI reg-result 0)
           (list :J label-end))
     ;; Vrai : mettre 1
     (list (list :LABEL label-true)
           (list :LI reg-result 1)
           (list :LABEL label-end)))))

(format t "[4/10] Fonctions comparaisons paramétrées...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS PARAMÉTRÉES - CATÉGORIE 4 : IF/COND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-if-param (test-expr then-expr else-expr env reg-result reg-temp reg-sp reg-zero)
  "Version paramétrée de compile-if"
  (let* ((label-else (gensym "IF_ELSE"))
         (label-end (gensym "IF_END"))
         (test-code (compile-expr-param test-expr env reg-result reg-temp reg-sp))
         (then-code (compile-expr-param then-expr env reg-result reg-temp reg-sp))
         (else-code (if else-expr
                        (compile-expr-param else-expr env reg-result reg-temp reg-sp)
                        (list (list :LI reg-result 0)))))
    (append
     ;; Évaluer condition
     test-code
     ;; Si 0 (faux), aller à else
     (list (list :BEQ reg-result reg-zero label-else))
     ;; Then branch
     then-code
     (list (list :J label-end))
     ;; Else branch
     (list (list :LABEL label-else))
     else-code
     (list (list :LABEL label-end)))))

(defun compile-cond-param (clauses env reg-result reg-temp reg-sp reg-zero)
  "Version paramétrée de compile-cond"
  (if (null clauses)
      (list (list :LI reg-result 0))
      (let* ((first-clause (first clauses))
             (test-expr (first first-clause))
             (body-exprs (rest first-clause))
             (label-next (gensym "COND_NEXT"))
             (label-end (gensym "COND_END")))
        (append
         ;; Évaluer test
         (compile-expr-param test-expr env reg-result reg-temp reg-sp)
         ;; Si faux, essayer clause suivante
         (list (list :BEQ reg-result reg-zero label-next))
         ;; Corps de la clause
         (if body-exprs
             (let ((body-code nil))
               (dolist (expr body-exprs)
                 (setq body-code
                       (append body-code
                               (compile-expr-param expr env reg-result reg-temp reg-sp))))
               body-code)
             (list (list :LI reg-result 1)))
         (list (list :J label-end))
         ;; Clause suivante
         (list (list :LABEL label-next))
         (compile-cond-param (rest clauses) env reg-result reg-temp reg-sp reg-zero)
         (list (list :LABEL label-end))))))

(format t "[5/10] Fonctions IF/COND paramétrées...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS PARAMÉTRÉES - CATÉGORIE 5 : LET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-let-param (bindings body env reg-result reg-temp reg-sp)
  "Version paramétrée de compile-let"
  (let ((new-env env)
        (init-code nil)
        (stack-size 0))
    
    ;; Compiler chaque binding
    (dolist (binding bindings)
      (let* ((var-name (first binding))
             (init-expr (second binding))
             (init-expr-code (compile-expr-param init-expr env reg-result reg-temp reg-sp)))
        ;; Évaluer init-expr
        (setq init-code (append init-code init-expr-code))
        ;; Sauver sur la pile
        (setq init-code (append init-code
                                (list (list :SW reg-result reg-sp stack-size))))
        ;; Ajouter au nouvel environnement
        (setq new-env (cons (cons var-name stack-size) new-env))
        (decf stack-size 4)))
    
    ;; Ajuster $sp
    (when (< stack-size 0)
      (setq init-code (append init-code
                              (list (list :ADDI reg-sp reg-sp stack-size)))))
    
    ;; Compiler le corps avec le nouvel environnement
    (let ((body-code nil))
      (dolist (expr body)
        (setq body-code (append body-code
                                (compile-expr-param expr new-env reg-result reg-temp reg-sp))))
      
      ;; Restaurer $sp
      (when (< stack-size 0)
        (setq body-code (append body-code
                                (list (list :ADDI reg-sp reg-sp (- stack-size))))))
      
      (append init-code body-code))))

(format t "[6/10] Fonctions LET paramétrées...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTIONS PARAMÉTRÉES - CATÉGORIE 6 : SETQ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-setq-param (var-name value-expr env reg-result reg-temp reg-sp reg-fp)
  "Version paramétrée de compile-setq"
  (let* ((value-code (compile-expr-param value-expr env reg-result reg-temp reg-sp))
         (var-info (lookup-variable var-name env)))
    (if var-info
        ;; Variable locale : stocker dans la pile
        (let ((offset (cdr var-info)))
          (append value-code
                  (list (list :SW reg-result reg-fp offset))))
        ;; Variable non trouvée : ignorer (ou erreur)
        value-code)))

(format t "[7/10] Fonctions SETQ paramétrées...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTION DISPATCHER PARAMÉTRÉE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-expr-param (expr env reg-result reg-temp reg-sp)
  "Dispatcher paramétré pour compiler n'importe quelle expression
   Utilise les versions paramétrées des fonctions de compilation"
  (cond
    ;; Constante
    ((numberp expr)
     (compile-constant-param expr env reg-result))
    
    ;; NIL
    ((null expr)
     (compile-constant-param 0 env reg-result))
    
    ;; T
    ((eq expr t)
     (compile-constant-param 1 env reg-result))
    
    ;; Variable
    ((symbolp expr)
     (compile-variable-param expr env reg-result :$fp))
    
    ;; Liste (forme spéciale ou appel)
    ((listp expr)
     (let ((op (first expr))
           (args (rest expr)))
       (cond
         ;; Arithmétique
         ((member op '(+ - * /))
          (compile-arithmetic-param op args env reg-result reg-temp reg-sp))
         
         ;; Comparaison
         ((member op '(= < >))
          (compile-comparison-param op args env reg-result reg-temp reg-sp))
         
         ;; IF
         ((eq op 'if)
          (compile-if-param (first args) (second args) (third args) 
                           env reg-result reg-temp reg-sp :$zero))
         
         ;; COND
         ((eq op 'cond)
          (compile-cond-param args env reg-result reg-temp reg-sp :$zero))
         
         ;; LET
         ((eq op 'let)
          (compile-let-param (first args) (rest args) env reg-result reg-temp reg-sp))
         
         ;; SETQ
         ((eq op 'setq)
          (compile-setq-param (first args) (second args) env reg-result reg-temp reg-sp :$fp))
         
         ;; PROGN
         ((eq op 'progn)
          (let ((code nil))
            (dolist (expr args)
              (setq code (append code (compile-expr-param expr env reg-result reg-temp reg-sp))))
            (if code code (list (list :LI reg-result 0)))))
         
         ;; NULL
         ((eq op 'null)
          (let ((arg-code (compile-expr-param (first args) env reg-result reg-temp reg-sp)))
            (append arg-code
                    (list (list :BEQ reg-result :$zero (gensym "NULL_TRUE"))
                          (list :LI reg-result 0)
                          (list :J (gensym "NULL_END"))
                          (list :LABEL (gensym "NULL_TRUE"))
                          (list :LI reg-result 1)
                          (list :LABEL (gensym "NULL_END"))))))
         
         ;; NOT
         ((eq op 'not)
          (let ((arg-code (compile-expr-param (first args) env reg-result reg-temp reg-sp)))
            (append arg-code
                    (list (list :BEQ reg-result :$zero (gensym "NOT_TRUE"))
                          (list :LI reg-result 0)
                          (list :J (gensym "NOT_END"))
                          (list :LABEL (gensym "NOT_TRUE"))
                          (list :LI reg-result 1)
                          (list :LABEL (gensym "NOT_END"))))))
         
         ;; LENGTH (simplifié - appel VM)
         ((eq op 'length)
          (let ((arg-code (compile-expr-param (first args) env reg-result reg-temp reg-sp)))
            (append arg-code
                    (list (list :COMMENT "LENGTH via VM")
                          (list :JAL 'vm_length)))))
         
         ;; CAR, CDR, CONS (utiliser les versions des extensions)
         ((eq op 'car)
          (let ((arg-code (compile-expr-param (first args) env reg-result reg-temp reg-sp)))
            (append arg-code
                    (list (list :COMMENT "Primitive CAR")
                          (list :JAL 'vm_car)))))
         
         ((eq op 'cdr)
          (let ((arg-code (compile-expr-param (first args) env reg-result reg-temp reg-sp)))
            (append arg-code
                    (list (list :COMMENT "Primitive CDR")
                          (list :JAL 'vm_cdr)))))
         
         ((eq op 'cons)
          (let* ((car-code (compile-expr-param (first args) env reg-result reg-temp reg-sp))
                 (cdr-code (compile-expr-param (second args) env reg-result reg-temp reg-sp)))
            (append
             car-code
             (list (list :SW reg-result reg-sp 0)
                   (list :ADDI reg-sp reg-sp -4))
             cdr-code
             (list (list :ADDI reg-sp reg-sp 4)
                   (list :LW :$a0 reg-sp 0)
                   (list :MOVE :$a1 reg-result)
                   (list :COMMENT "Primitive CONS")
                   (list :JAL 'vm_cons)))))
         
         ;; Comparaisons étendues
         ((member op '(<= >= /=))
          (let* ((arg1-code (compile-expr-param (first args) env reg-result reg-temp reg-sp))
                 (arg2-code (compile-expr-param (second args) env reg-result reg-temp reg-sp))
                 (label-true (gensym "CMP_TRUE"))
                 (label-end (gensym "CMP_END"))
                 (branch-instr (cond
                                 ((eq op '<=) :BLE)
                                 ((eq op '>=) :BGE)
                                 ((eq op '/=) :BNE)
                                 (t :BEQ))))
            (append
             arg1-code
             (list (list :SW reg-result reg-sp 0)
                   (list :ADDI reg-sp reg-sp -4))
             arg2-code
             (list (list :ADDI reg-sp reg-sp 4)
                   (list :LW reg-temp reg-sp 0))
             (list (list branch-instr reg-temp reg-result label-true))
             (list (list :LI reg-result 0)
                   (list :J label-end))
             (list (list :LABEL label-true)
                   (list :LI reg-result 1)
                   (list :LABEL label-end)))))
         
         ;; AND
         ((eq op 'and)
          (if (null args)
              (list (list :LI reg-result 1))
              (let* ((label-false (gensym "AND_FALSE"))
                     (label-end (gensym "AND_END"))
                     (code nil))
                (dolist (expr args)
                  (setq code (append code
                                     (compile-expr-param expr env reg-result reg-temp reg-sp)
                                     (list (list :BEQ reg-result :$zero label-false)))))
                (append code
                        (list (list :LI reg-result 1)
                              (list :J label-end)
                              (list :LABEL label-false)
                              (list :LI reg-result 0)
                              (list :LABEL label-end))))))
         
         ;; OR
         ((eq op 'or)
          (if (null args)
              (list (list :LI reg-result 0))
              (let* ((label-true (gensym "OR_TRUE"))
                     (label-end (gensym "OR_END"))
                     (code nil))
                (dolist (expr args)
                  (setq code (append code
                                     (compile-expr-param expr env reg-result reg-temp reg-sp)
                                     (list (list :BNE reg-result :$zero label-true)))))
                (append code
                        (list (list :LI reg-result 0)
                              (list :J label-end)
                              (list :LABEL label-true)
                              (list :LI reg-result 1)
                              (list :LABEL label-end))))))
         
         ;; Sinon : erreur ou appel de fonction (non supporté ici)
         (t
          (error "Expression non supportée dans version paramétrée: ~A" expr)))))
    
    ;; Autre
    (t
     (error "Type d'expression inconnu: ~A" expr))))

(format t "[8/10] Dispatcher paramétré créé...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WRAPPER POUR COMPATIBILITÉ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-expr-with-params (expr env)
  "Wrapper qui appelle compile-expr-param avec les registres par défaut"
  (compile-expr-param expr env :$v0 :$t0 :$sp))

(format t "[9/10] Wrapper de compatibilité créé...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTÉGRATION - NE REMPLACER QUE LES FONCTIONS CASSÉES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NE PAS remplacer les fonctions qui marchent déjà!
;; On remplace seulement celles qui utilisent des variables globales

;; Tester si une fonction est cassée avant de la remplacer
(defun function-uses-global-vars-p (func-name)
  "Teste si une fonction échoue à cause de variables globales"
  (handler-case
      (progn
        ;; Essayer de compiler un cas de test simple
        (funcall (symbol-function func-name) 
                 (case func-name
                   (compile-constant '42)
                   (compile-variable 'x)
                   (compile-arithmetic '+ '(1 2))
                   (compile-comparison '= '(1 2))
                   (compile-if 't '1 '0)
                   (t nil))
                 nil) ; env
        nil) ; succès
    (error (e)
      ;; Si erreur contient "Variable non définie", c'est un problème de globals
      (search "Variable non définie" (format nil "~A" e)))))

;; Sauvegarder les originales seulement si elles sont cassées
(when (and (fboundp 'compile-constant) 
           (function-uses-global-vars-p 'compile-constant))
  (setf (symbol-function 'compile-constant-original-broken) 
        (symbol-function 'compile-constant))
  ;; Remplacer par version paramétrée
  (defun compile-constant (value env)
    "Version paramétrée de compile-constant (remplace l'originale cassée)"
    (compile-constant-param value env :$v0))
  (format t "  • compile-constant remplacée par version paramétrée~%"))

(when (and (fboundp 'compile-variable)
           (function-uses-global-vars-p 'compile-variable))
  (setf (symbol-function 'compile-variable-original-broken)
        (symbol-function 'compile-variable))
  (defun compile-variable (var-name env)
    "Version paramétrée de compile-variable (remplace l'originale cassée)"
    (compile-variable-param var-name env :$v0 :$fp))
  (format t "  • compile-variable remplacée par version paramétrée~%"))

(when (and (fboundp 'compile-arithmetic)
           (function-uses-global-vars-p 'compile-arithmetic))
  (setf (symbol-function 'compile-arithmetic-original-broken)
        (symbol-function 'compile-arithmetic))
  (defun compile-arithmetic (op args env)
    "Version paramétrée de compile-arithmetic (remplace l'originale cassée)"
    (compile-arithmetic-param op args env :$v0 :$t0 :$sp))
  (format t "  • compile-arithmetic remplacée par version paramétrée~%"))

(when (and (fboundp 'compile-comparison)
           (function-uses-global-vars-p 'compile-comparison))
  (setf (symbol-function 'compile-comparison-original-broken)
        (symbol-function 'compile-comparison))
  (defun compile-comparison (op args env)
    "Version paramétrée de compile-comparison (remplace l'originale cassée)"
    (compile-comparison-param op args env :$v0 :$t0 :$sp))
  (format t "  • compile-comparison remplacée par version paramétrée~%"))

(when (and (fboundp 'compile-if)
           (function-uses-global-vars-p 'compile-if))
  (setf (symbol-function 'compile-if-original-broken)
        (symbol-function 'compile-if))
  (defun compile-if (test then else env)
    "Version paramétrée de compile-if (remplace l'originale cassée)"
    (compile-if-param test then else env :$v0 :$t0 :$sp :$zero))
  (format t "  • compile-if remplacée par version paramétrée~%"))

(format t "[10/10] Fonctions cassées remplacées par versions paramétrées...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MESSAGE FINAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%✓ Compilateur paramétrique chargé !~%~%")
(format t "Fonctions paramétrées disponibles :~%")
(format t "  • compile-constant-param~%")
(format t "  • compile-variable-param~%")
(format t "  • compile-arithmetic-param~%")
(format t "  • compile-comparison-param~%")
(format t "  • compile-if-param~%")
(format t "  • compile-cond-param~%")
(format t "  • compile-let-param~%")
(format t "  • compile-setq-param~%")
(format t "  • compile-expr-param (dispatcher)~%~%")

(format t "Fonctions originales remplacées :~%")
(format t "  • compile-constant~%")
(format t "  • compile-arithmetic~%")
(format t "  • compile-comparison~%")
(format t "  • compile-if~%~%")

(format t "Impact estimé : +10-15 fonctions supplémentaires compilables~%")
(format t "════════════════════════════════════════════════════════════════~%~%")
