;;;; list-runtime.lisp
;;;; Runtime support pour les listes dans le compilateur
;;;; Génère du code MIPS pour construire des listes en mémoire

;;; ============================================================================
;;; CHARGEMENT DES DÉPENDANCES
;;; ============================================================================

;; Charger la table de symboles si pas déjà chargée
(unless (fboundp 'intern-symbol)
  (load "src/symbol-table.lisp"))

;;; ============================================================================
;;; DÉFINITION DES REGISTRES (pour compatibilité standalone)
;;; ============================================================================

(defparameter *reg-v0* :$V0)
(defparameter *reg-a0* :$A0)
(defparameter *reg-a1* :$A1)
(defparameter *reg-sp* :$SP)
(defparameter *reg-ra* :$RA)
(defparameter *reg-gp* :$GP)
(defparameter *reg-zero* :$ZERO)

;;; ============================================================================
;;; COMPILATION DES LISTES QUOTÉES
;;; ============================================================================

(defun compile-quoted-list (list env)
  "Compile une liste quotée en générant du code pour la construire dans le heap.
   Retourne du code MIPS qui construit la liste et met son adresse dans $V0."
  (cond
    ;; NIL → retourner 0
    ((null list)
     (list (list :MOVE *reg-zero* *reg-v0*)))
    
    ;; Atome (nombre ou symbole)
    ((atom list)
     (cond
       ;; Nombre : charger directement
       ((numberp list)
        (list (list :LI list *reg-v0*)))
       
       ;; Symbole : utiliser l'interning
       ((symbolp list)
        (let ((symbol-id (intern-symbol list)))
          (list (list :LI symbol-id *reg-v0*))))
       
       (t
        (list (list :LI 0 *reg-v0*)))))
    
    ;; Liste (cons) : compiler récursivement
    (t
     (compile-cons-construction (car list) (cdr list) env))))

(defun compile-cons-construction (car-expr cdr-expr env)
  "Génère du code pour construire une cons cell (car-expr . cdr-expr).
   Utilise la fonction runtime CONS qui alloue dans le heap."
  (let ((code '()))
    
    ;; 1. Compiler et évaluer le CAR
    (setf code (append code (compile-quoted-list car-expr env)))
    ;; Résultat CAR dans $V0
    
    ;; 2. Sauvegarder CAR sur la pile
    (setf code (append code (list
                             (list :ADDI *reg-sp* -4 *reg-sp*)  ; Allouer espace pile
                             (list :SW *reg-v0* *reg-sp* 0))))   ; Sauver CAR
    
    ;; 3. Compiler et évaluer le CDR
    (setf code (append code (compile-quoted-list cdr-expr env)))
    ;; Résultat CDR dans $V0
    
    ;; 4. Restaurer CAR dans $A0, CDR reste dans $V0
    (setf code (append code (list
                             (list :LW *reg-a0* *reg-sp* 0)      ; CAR dans $A0
                             (list :ADDI *reg-sp* 4 *reg-sp*)))) ; Libérer pile
    
    ;; 5. CDR est déjà dans $V0, on le copie dans $A1
    (setf code (append code (list
                             (list :MOVE *reg-v0* *reg-a1*))))    ; CDR dans $A1
    
    ;; 6. Appeler la fonction CONS runtime
    ;;    $A0 = CAR, $A1 = CDR
    ;;    Résultat : adresse de la cons cell dans $V0
    (setf code (append code (compile-cons-call env)))
    
    code))

(defun compile-cons-call (env)
  "Génère du code pour appeler la fonction CONS runtime.
   Entrée : $A0 = CAR, $A1 = CDR
   Sortie : $V0 = adresse de la cons cell"
  (let ((cons-label (intern "RUNTIME_CONS"))
        (code '()))
    
    ;; Sauvegarder $RA
    (setf code (append code (list
                             (list :ADDI *reg-sp* -4 *reg-sp*)
                             (list :SW *reg-ra* *reg-sp* 0))))
    
    ;; Appeler CONS
    (setf code (append code (list
                             (list :JAL cons-label))))
    
    ;; Restaurer $RA
    (setf code (append code (list
                             (list :LW *reg-ra* *reg-sp* 0)
                             (list :ADDI *reg-sp* 4 *reg-sp*))))
    
    code))

;;; ============================================================================
;;; RUNTIME CONS (à inclure dans le code généré)
;;; ============================================================================

(defun generate-cons-runtime ()
  "Génère le code runtime pour la fonction CONS.
   CONS(CAR, CDR) alloue 2 mots dans le heap et retourne l'adresse."
  (list
   ;; Label de la fonction CONS
   (list :LABEL 'RUNTIME_CONS)
   
   ;; Allouer 2 mots dans le heap
   ;; Le heap pointer est dans $GP
   (list :MOVE *reg-gp* *reg-v0*)           ; Adresse de la cons = heap pointer actuel
   
   ;; Stocker CAR (dans $A0) à offset 0
   (list :SW *reg-a0* *reg-gp* 0)
   
   ;; Stocker CDR (dans $A1) à offset 1
   (list :SW *reg-a1* *reg-gp* 1)
   
   ;; Avancer le heap pointer de 2 mots
   (list :ADDI *reg-gp* 2 *reg-gp*)
   
   ;; Retourner (adresse déjà dans $V0)
   (list :JR *reg-ra*)))

;;; ============================================================================
;;; RUNTIME CAR/CDR/NULL
;;; ============================================================================

(defun generate-car-runtime ()
  "Génère le code runtime pour CAR.
   CAR(addr) retourne le premier élément de la cons cell."
  (list
   (list :LABEL 'RUNTIME_CAR)
   
   ;; Tester si NIL (0)
   (list :BEQ *reg-a0* *reg-zero* 'CAR_NIL)
   
   ;; Charger le mot à offset 0
   (list :LW *reg-v0* *reg-a0* 0)
   (list :JR *reg-ra*)
   
   ;; Cas NIL : retourner 0
   (list :LABEL 'CAR_NIL)
   (list :MOVE *reg-zero* *reg-v0*)
   (list :JR *reg-ra*)))

(defun generate-cdr-runtime ()
  "Génère le code runtime pour CDR.
   CDR(addr) retourne le reste de la liste."
  (list
   (list :LABEL 'RUNTIME_CDR)
   
   ;; Tester si NIL (0)
   (list :BEQ *reg-a0* *reg-zero* 'CDR_NIL)
   
   ;; Charger le mot à offset 1
   (list :LW *reg-v0* *reg-a0* 1)
   (list :JR *reg-ra*)
   
   ;; Cas NIL : retourner 0
   (list :LABEL 'CDR_NIL)
   (list :MOVE *reg-zero* *reg-v0*)
   (list :JR *reg-ra*)))

(defun generate-null-runtime ()
  "Génère le code runtime pour NULL.
   NULL(addr) retourne 1 si addr == 0, sinon 0."
  (list
   (list :LABEL 'RUNTIME_NULL)
   
   ;; Tester si 0
   (list :BEQ *reg-a0* *reg-zero* 'NULL_TRUE)
   
   ;; Cas faux : retourner 0
   (list :MOVE *reg-zero* *reg-v0*)
   (list :JR *reg-ra*)
   
   ;; Cas vrai : retourner 1
   (list :LABEL 'NULL_TRUE)
   (list :LI 1 *reg-v0*)
   (list :JR *reg-ra*)))

;;; ============================================================================
;;; GÉNÉRATION DU RUNTIME COMPLET
;;; ============================================================================

(defun generate-list-runtime ()
  "Génère tout le code runtime pour les listes (CONS, CAR, CDR, NULL)."
  (append
   (generate-cons-runtime)
   (generate-car-runtime)
   (generate-cdr-runtime)
   (generate-null-runtime)))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(compile-quoted-list
          generate-list-runtime
          symbol-hash))

(format t "~%Runtime support pour listes chargé.~%")
