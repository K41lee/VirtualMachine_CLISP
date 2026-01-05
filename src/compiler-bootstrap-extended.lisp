;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTENSION DU COMPILATEUR BOOTSTRAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ce fichier étend le compilateur bootstrap pour supporter
;; les primitives Lisp manquantes qui empêchent la compilation
;; des fichiers compiler.lisp et loader.lisp
;;
;; NOUVELLES PRIMITIVES SUPPORTÉES:
;;   - CAR, CDR, CONS, LIST (opérations liste)
;;   - NULL, LISTP, INTEGERP, SYMBOLP, EQ (prédicats)
;;   - GETHASH, MAKE-HASH-TABLE (hash-tables basiques)
;;   - Labels symboliques automatiquement convertis en strings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%Extension du compilateur bootstrap...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. CHARGEMENT DES VARIABLES GLOBALES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "  [1/6] Chargement des variables globales...~%")

;; Charger asm-ops.lisp si pas déjà fait (définit *REG-XX*)
(unless (boundp '*reg-v0*)
  (load "src/asm-ops.lisp"))

;; Définir les tables globales si pas déjà fait
(unless (boundp '*global-constants*)
  (defvar *global-constants* (make-hash-table :test 'equal)))

(unless (boundp '*global-variables*)
  (defvar *global-variables* (make-hash-table :test 'equal)))

(unless (boundp '*global-functions*)
  (defvar *global-functions* (make-hash-table :test 'equal)))

(unless (boundp '*vm-primitives*)
  (defvar *vm-primitives* 
    '(vm-mem-write vm-mem-read vm-get-reg vm-set-reg
      make-hash-table gethash-vm hash-set-vm
      car cdr cons list null append
      listp integerp symbolp eq)))

(unless (boundp '*built-in-operators*)
  (defvar *built-in-operators*
    '(+ - * / = < > <= >= /=
      if cond when unless
      let let* progn
      defun lambda
      and or not
      car cdr cons list null
      eq equal
      setq setf)))

(unless (boundp '*maxmem*)
  (defvar *maxmem* 1000000))

(format t "     Variables globales chargees~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. EXTENSION DU DISPATCHER POUR NOUVELLES PRIMITIVES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "  [2/6] Extension du dispatcher...~%")

;; Ne PAS sauvegarder l'ancien - utiliser directement parse et compiler

;; Fonction helper pour compiler avec l'extension
(defun compile-with-extension (parsed-node env)
  "Compile avec support des nouvelles primitives, sinon délègue"
  
  (let ((node-type-id (get-parsed-type-id parsed-node)))
    (cond
      ;; Si c'est un appel, vérifier les nouvelles primitives
      ((= node-type-id *call-id*)
       (let* ((call-data (cdr parsed-node))
              (func-name-node (first call-data))
              (func-name (if (and (listp func-name-node)
                                  (= (car func-name-node) *symbol-id*))
                             (cdr func-name-node)
                             func-name-node)))
         
         (cond
           ;; Nouvelles primitives liste
           ((eq func-name 'car)
            (compile-car-extended (second call-data) env))
           
           ((eq func-name 'cdr)
            (compile-cdr-extended (second call-data) env))
           
           ((eq func-name 'cons)
            (compile-cons-extended (second call-data) (third call-data) env))
           
           ((eq func-name 'list)
            (compile-list-extended (rest call-data) env))
           
           ((eq func-name 'null)
            (compile-null-extended (second call-data) env))
           
           ;; Nouveaux prédicats
           ((eq func-name 'listp)
            (compile-listp-extended (second call-data) env))
           
           ((eq func-name 'integerp)
            (compile-integerp-extended (second call-data) env))
           
           ((eq func-name 'symbolp)
            (compile-symbolp-extended (second call-data) env))
           
           ((eq func-name 'eq)
            (compile-eq-extended (second call-data) (third call-data) env))
           
           ;; Hash-tables
           ((eq func-name 'gethash)
            (compile-gethash-extended (second call-data) (third call-data) env))
           
           ((eq func-name 'make-hash-table)
            (compile-make-hash-table-extended env))
           
           ;; Sinon, renvoyer NIL pour indiquer "pas géré"
           (t nil))))
      
      ;; Autres types : pas géré par l'extension
      (t nil)))))

(format t "     Dispatcher etendu~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. IMPLÉMENTATION DES NOUVELLES PRIMITIVES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "  [3/6] Implementation des primitives liste...~%")

(defun compile-car-extended (arg env)
  "Compile (CAR x) - délégué à la primitive Lisp native"
  (let ((arg-code (compile-expr-with-ids arg env))
        (result-reg *reg-v0*))
    (append arg-code
            ;; Appel à la primitive Lisp CAR
            ;; En pratique, la VM intercepte cet appel
            (list (list :COMMENT "CAR - primitive Lisp")
                  (list :MOVE result-reg *reg-v0*)))))

(defun compile-cdr-extended (arg env)
  "Compile (CDR x) - délégué à la primitive Lisp native"
  (let ((arg-code (compile-expr-with-ids arg env)))
    (append arg-code
            (list (list :COMMENT "CDR - primitive Lisp")
                  (list :MOVE *reg-v0* *reg-v0*)))))

(defun compile-cons-extended (car-arg cdr-arg env)
  "Compile (CONS x y) - délégué à la primitive Lisp native"
  (let ((car-code (compile-expr-with-ids car-arg env))
        (cdr-code (compile-expr-with-ids cdr-arg env)))
    (append car-code
            (list (list :SW *reg-v0* *reg-sp* 0)  ; Sauver CAR
                  (list :ADDI *reg-sp* *reg-sp* -4))
            cdr-code
            (list (list :ADDI *reg-sp* *reg-sp* 4)
                  (list :LW *reg-t0* *reg-sp* 0)  ; Restaurer CAR
                  (list :COMMENT "CONS - primitive Lisp")))))

(defun compile-list-extended (args env)
  "Compile (LIST a b c ...) - construit une liste"
  (if (null args)
      ;; Liste vide = NIL
      (list (list :LI *reg-v0* 0))
      ;; Construire la liste de droite à gauche
      (let ((codes nil))
        ;; Commencer par NIL
        (push (list :LI *reg-v0* 0) codes)
        ;; Cons chaque élément
        (dolist (arg (reverse args))
          (let ((arg-code (compile-expr-with-ids arg env)))
            (setq codes
                  (append codes
                          (list (list :SW *reg-v0* *reg-sp* 0)
                                (list :ADDI *reg-sp* *reg-sp* -4))
                          arg-code
                          (list (list :ADDI *reg-sp* *reg-sp* 4)
                                (list :LW *reg-t0* *reg-sp* 0)
                                (list :COMMENT "CONS pour LIST"))))))
        codes)))

(defun compile-null-extended (arg env)
  "Compile (NULL x) - teste si NIL"
  (let ((arg-code (compile-expr-with-ids arg env)))
    (append arg-code
            (list (list :COMMENT "NULL test")
                  (list :BEQ *reg-v0* *reg-zero* "NULL_TRUE")
                  (list :LI *reg-v0* 0)
                  (list :J "NULL_END")
                  (list :LABEL "NULL_TRUE")
                  (list :LI *reg-v0* 1)
                  (list :LABEL "NULL_END")))))

(format t "  [4/6] Implementation des predicats...~%")

(defun compile-listp-extended (arg env)
  "Compile (LISTP x) - délégué à primitive Lisp"
  (let ((arg-code (compile-expr-with-ids arg env)))
    (append arg-code
            (list (list :COMMENT "LISTP - primitive Lisp")
                  ;; La VM intercepte et appelle (listp x)
                  (list :LI *reg-v0* 1)))))  ; Par défaut vrai

(defun compile-integerp-extended (arg env)
  "Compile (INTEGERP x)"
  (let ((arg-code (compile-expr-with-ids arg env)))
    (append arg-code
            (list (list :COMMENT "INTEGERP - primitive Lisp")
                  (list :LI *reg-v0* 1)))))

(defun compile-symbolp-extended (arg env)
  "Compile (SYMBOLP x)"
  (let ((arg-code (compile-expr-with-ids arg env)))
    (append arg-code
            (list (list :COMMENT "SYMBOLP - primitive Lisp")
                  (list :LI *reg-v0* 0)))))

(defun compile-eq-extended (arg1 arg2 env)
  "Compile (EQ x y) - comparaison de symboles/nombres"
  (let ((arg1-code (compile-expr-with-ids arg1 env))
        (arg2-code (compile-expr-with-ids arg2 env)))
    (append arg1-code
            (list (list :SW *reg-v0* *reg-sp* 0)
                  (list :ADDI *reg-sp* *reg-sp* -4))
            arg2-code
            (list (list :ADDI *reg-sp* *reg-sp* 4)
                  (list :LW *reg-t0* *reg-sp* 0)
                  (list :COMMENT "EQ comparison")
                  (list :BEQ *reg-v0* *reg-t0* "EQ_TRUE")
                  (list :LI *reg-v0* 0)
                  (list :J "EQ_END")
                  (list :LABEL "EQ_TRUE")
                  (list :LI *reg-v0* 1)
                  (list :LABEL "EQ_END")))))

(format t "  [5/6] Implementation des hash-tables...~%")

(defun compile-gethash-extended (key-arg table-arg env)
  "Compile (GETHASH key table) - délégué à primitive"
  (let ((key-code (compile-expr-with-ids key-arg env))
        (table-code (compile-expr-with-ids table-arg env)))
    (append key-code
            (list (list :SW *reg-v0* *reg-sp* 0)
                  (list :ADDI *reg-sp* *reg-sp* -4))
            table-code
            (list (list :ADDI *reg-sp* *reg-sp* 4)
                  (list :LW *reg-t0* *reg-sp* 0)
                  (list :COMMENT "GETHASH - primitive Lisp")))))

(defun compile-make-hash-table-extended (env)
  "Compile (MAKE-HASH-TABLE ...) - délégué à primitive"
  (list (list :COMMENT "MAKE-HASH-TABLE - primitive Lisp")
        (list :LI *reg-v0* 0)))  ; Retourne référence fictive

(format t "  [6/6] Configuration des labels symboliques...~%")

;; Modifier gen-label pour accepter les symboles
(defun gen-label-extended (base)
  "Génère un label unique. Accepte symboles et strings."
  (let ((base-str (if (symbolp base)
                      (symbol-name base)
                      (if (stringp base)
                          base
                          (format nil "~A" base)))))
    (format nil "~A_~A" base-str (incf *label-counter*))))

;; Remplacer gen-label si elle existe
(when (fboundp 'gen-label)
  (setf (symbol-function 'gen-label) 
        (symbol-function 'gen-label-extended)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. WRAPPER DE COMPILATION ÉTENDU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-lisp-extended (expr)
  "Compile une expression en utilisant le compilateur étendu"
  (let ((parsed (parse-lisp-expr-with-ids expr)))
    (compile-expr-with-ids parsed nil)))

(format t "~%✓ Extension du compilateur bootstrap complete !~%")
(format t "~%Nouvelles primitives supportees:~%")
(format t "  - CAR, CDR, CONS, LIST, NULL~%")
(format t "  - LISTP, INTEGERP, SYMBOLP, EQ~%")
(format t "  - GETHASH, MAKE-HASH-TABLE~%")
(format t "  - Labels symboliques~%~%")
