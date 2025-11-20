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
  (variables '())           ; Liste des variables locales et leurs registres
  (functions '())           ; Table des fonctions définies
  (label-counter 0)         ; Compteur pour générer des labels uniques
  (register-counter 0))     ; Compteur pour allouer des registres temporaires

(defun make-new-compiler-env ()
  "Crée un nouvel environnement de compilation"
  (make-compiler-env))

(defun gen-label (env prefix)
  "Génère un label unique avec le préfixe donné"
  (let ((label (intern (format nil "~A_~A" prefix (compiler-env-label-counter env)))))
    (incf (compiler-env-label-counter env))
    label))

(defun add-variable (env var register)
  "Ajoute une variable à l'environnement"
  (push (cons var register) (compiler-env-variables env)))

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
  "Compile une opération arithmétique - sauvegarde arg1 sur la pile"
  (cond
    ;; Opération binaire: (+ a b)
    ((= (length args) 2)
     (let* ((arg1 (first args))
            (arg2 (second args))
            (code1 (compile-expr arg1 env))
            (reg1 *reg-t0*)
            (code2 (compile-expr arg2 env))
            (reg2 *reg-t1*))
       (append
        code1
        ;; Sauvegarder résultat 1 sur la pile au lieu d'un registre
        (list (list :ADDI *reg-sp* -4 *reg-sp*)
              (list :SW *reg-v0* *reg-sp* 0))
        code2
        (list (list :MOVE *reg-v0* reg2))  ; Résultat 2 dans $t1
        ;; Restaurer résultat 1 depuis la pile
        (list (list :LW *reg-sp* 0 reg1)
              (list :ADDI *reg-sp* 4 *reg-sp*))
        (case op
          (+ (list (list :ADD reg1 reg2 *reg-v0*)))
          (- (list (list :SUB reg1 reg2 *reg-v0*)))
          (* (list (list :MUL reg1 reg2)
                   (list :MFLO *reg-v0*)))
          (/ (list (list :DIV reg1 reg2)
                   (list :MFLO *reg-v0*)))
          (mod (list (list :DIV reg1 reg2)
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
;;; TESTS DU COMPILATEUR
;;; ============================================================================

(defun test-compiler-constant ()
  "Test: compilation de constante"
  (format t "~%=== TEST: Compilation de constante (42) ===~%")
  (compile-and-run 42))

(defun test-compiler-addition ()
  "Test: compilation d'addition simple"
  (format t "~%=== TEST: Compilation d'addition (+ 5 3) ===~%")
  (compile-and-run '(+ 5 3)))

(defun test-compiler-complex ()
  "Test: compilation d'expression complexe"
  (format t "~%=== TEST: Compilation de (* (+ 10 5) 2) ===~%")
  (compile-and-run '(* (+ 10 5) 2)))

(defun test-compiler-comparison ()
  "Test: compilation de comparaison"
  (format t "~%=== TEST: Compilation de (< 5 10) ===~%")
  (compile-and-run '(< 5 10)))

(defun test-compiler-if ()
  "Test: compilation de IF/THEN/ELSE"
  (format t "~%=== TEST: Compilation de (if (< 5 10) 100 200) ===~%")
  (compile-and-run '(if (< 5 10) 100 200)))

(defun test-compiler-if-false ()
  "Test: compilation de IF avec condition fausse"
  (format t "~%=== TEST: Compilation de (if (> 5 10) 100 200) ===~%")
  (compile-and-run '(if (> 5 10) 100 200)))

(defun test-compiler-simple-function ()
  "Test: compilation de fonction simple"
  (format t "~%=== TEST: Fonction simple (defun double (x) (* x 2)) appelée avec 21 ===~%")
  ;; On doit compiler la définition puis l'appel
  (let ((vm (make-new-vm :verbose nil))
        (func-def '(defun double (x) (* x 2)))
        (func-call '(double 21)))
    ;; Compiler la fonction
    (let ((func-code (compile-lisp func-def))
          (call-code (compile-lisp func-call)))
      ;; Générer un JMP vers :MAIN pour sauter les définitions de fonctions
      (let ((full-code (append (list (list :JMP ':MAIN))   ; Jump to main code
                              func-code                      ; Function definitions
                              (list (list :LABEL ':MAIN))   ; Main code label
                              call-code                      ; Function calls
                              (list (list :PRINT *reg-v0*)
                                    (list :HALT)))))
        (format t "~%=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
        (dolist (instr full-code)
          (format t "~A~%" instr))
        (format t "~%=== EXÉCUTION ===~%")
        (load-and-run vm full-code :verbose nil)
        (format t "~%Résultat dans $v0: ~A~%" (get-register vm *reg-v0*))
        vm))))

(defun test-compiler-fibonacci (&optional (n 10))
  "Test: compilation de fibonacci récursif"
  (format t "~%=== TEST: Fibonacci récursif fib(~A) ===~%" n)
  (let ((vm (make-new-vm :verbose nil))
        (fib-def '(defun fib (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1))
                           (fib (- n 2))))))
        (fib-call (list 'fib n)))
    (let ((fib-code (compile-lisp fib-def))
          (call-code (compile-lisp fib-call)))
      ;; Architecture code avec JMP :MAIN
      (let ((full-code (append (list (list :JMP ':MAIN))
                              fib-code
                              (list (list :LABEL ':MAIN))
                              call-code
                              (list (list :PRINT *reg-v0*)
                                    (list :HALT)))))
        (format t "~%=== CODE ASSEMBLEUR GÉNÉRÉ (partiel) ===~%")
        (dolist (instr (subseq full-code 0 (min 20 (length full-code))))
          (format t "~A~%" instr))
        (format t "... (~A instructions au total)~%" (length full-code))
        (format t "~%=== EXÉCUTION ===~%")
        (load-and-run vm full-code :verbose nil)
        (format t "~%Résultat dans $v0: ~A~%" (get-register vm *reg-v0*))
        vm))))

;; Variables globales pour stocker les résultats
(defvar *fib-native-time* 0)
(defvar *fib-native-result* 0)
(defvar *fib-vm-time* 0)
(defvar *fib-vm-result* 0)
(defvar *fib-vm-instructions* 0)

(defun fib-native (n)
  "Fibonacci natif CLISP pour comparaison"
  (if (<= n 1)
      n
      (+ (fib-native (- n 1))
         (fib-native (- n 2)))))

(defun test-fibonacci-performance (&optional (n 10))
  "Compare les performances de fibonacci entre VM et CLISP natif"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "           TEST DE PERFORMANCE: FIBONACCI(~A)~%" n)
  (format t "================================================================================~%")
  
  ;; Test avec CLISP natif
  (format t "~%--- Test avec CLISP natif ---~%")
  (let* ((start-time (get-internal-real-time))
         (result (fib-native n))
         (end-time (get-internal-real-time))
         (elapsed (/ (- end-time start-time) 
                    internal-time-units-per-second)))
    (format t "Résultat: ~A~%" result)
    (format t "Temps: ~,6F secondes~%" elapsed)
    (setf *fib-native-time* elapsed)
    (setf *fib-native-result* result))
  
  ;; Test avec notre VM
  (format t "~%--- Test avec VM MIPS ---~%")
  (let ((vm (make-new-vm :verbose nil))
        (fib-def '(defun fib (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1))
                           (fib (- n 2))))))
        (fib-call (list 'fib n)))
    (let ((fib-code (compile-lisp fib-def))
          (call-code (compile-lisp fib-call)))
      (let* ((full-code (append (list (list :JMP ':MAIN))
                                fib-code
                                (list (list :LABEL ':MAIN))
                                call-code
                                (list (list :HALT))))
             (start-time (get-internal-real-time))
             (dummy (load-and-run vm full-code :verbose nil))
             (end-time (get-internal-real-time))
             (elapsed (/ (- end-time start-time) 
                        internal-time-units-per-second))
             (result (get-register vm *reg-v0*)))
        (format t "Résultat: ~A~%" result)
        (format t "Temps: ~,6F secondes~%" elapsed)
        (format t "Instructions exécutées: ~A~%" (vm-instruction-count vm))
        (setf *fib-vm-time* elapsed)
        (setf *fib-vm-result* result)
        (setf *fib-vm-instructions* (vm-instruction-count vm)))))
  
  ;; Comparaison
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                           COMPARAISON~%")
  (format t "================================================================================~%")
  (format t "~%CLISP natif:~%")
  (format t "  Résultat: ~A~%" *fib-native-result*)
  (format t "  Temps: ~,6F secondes~%" *fib-native-time*)
  (format t "~%VM MIPS:~%")
  (format t "  Résultat: ~A~%" *fib-vm-result*)
  (format t "  Temps: ~,6F secondes~%" *fib-vm-time*)
  (format t "  Instructions: ~A~%" *fib-vm-instructions*)
  (when (and (> *fib-native-time* 0) (> *fib-vm-time* 0))
    (format t "~%Ratio (VM / Natif): ~,2Fx plus lent~%" 
            (/ *fib-vm-time* *fib-native-time*)))
  (format t "~%")
  (if (= *fib-native-result* *fib-vm-result*)
      (format t "✓ Résultats identiques!~%")
      (format t "✗ ERREUR: Résultats différents!~%"))
  (format t "================================================================================~%")
  (format t "~%"))

(defun run-all-compiler-tests ()
  "Exécute tous les tests du compilateur"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                     TESTS DU COMPILATEUR LISP → ASM~%")
  (format t "================================================================================~%")
  
  (test-compiler-constant)
  (test-compiler-addition)
  (test-compiler-complex)
  (test-compiler-comparison)
  (test-compiler-if)
  (test-compiler-if-false)
  
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                     TESTS TERMINÉS~%")
  (format t "================================================================================~%"))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(compile-lisp compile-and-run
          test-compiler-constant test-compiler-addition
          test-compiler-complex test-compiler-comparison
          test-compiler-if test-compiler-if-false
          test-compiler-simple-function
          test-compiler-fibonacci
          run-all-compiler-tests))
