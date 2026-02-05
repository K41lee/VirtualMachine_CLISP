;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PHASE 2 : FONCTIONS AVEC LABELS SYMBOLIQUES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ces fonctions utilisent des labels symboliques qui doivent
;;; être convertis en strings pour être compilables.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%Chargement Phase 2: Labels symboliques...~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-CASE - Version simplifiée
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-case-clause-simplified (key-expr clause end-label env)
  "Compile une clause de CASE - version simplifiée"
  (let* ((keys (first clause))
         (body (rest clause))
         (next-label (gen-label-simplified "CASE_NEXT")))
    (append
     ;; Tester si la clé correspond
     (if (listp keys)
         ;; Multiple keys: (1 2 3) → tester chacune
         (compile-case-test-keys-simplified key-expr keys next-label env)
         ;; Single key ou T/OTHERWISE
         (if (or (eq keys 't) (eq keys 'otherwise))
             nil  ; Pas de test, toujours vrai
             (compile-case-test-key-simplified key-expr keys next-label env)))
     ;; Compiler le corps
     (compile-progn-simplified body env)
     (list (list :J end-label))
     ;; Label pour clause suivante
     (list (list :LABEL next-label)))))

(defun compile-case-test-key-simplified (key-expr key next-label env)
  "Teste une clé de CASE"
  (append
   ;; key-expr déjà compilé et dans *reg-v0*
   (list (list :LI *reg-t0* key))
   (list (list :BNE *reg-v0* *reg-t0* next-label))))

(defun compile-case-test-keys-simplified (key-expr keys next-label env)
  "Teste plusieurs clés de CASE"
  (if (null keys)
      (list (list :J next-label))  ; Aucune correspondance
      (append
       (compile-case-test-key-simplified key-expr (first keys) "NEXT_KEY" env)
       (list (list :LABEL "NEXT_KEY"))
       (compile-case-test-keys-simplified key-expr (rest keys) next-label env))))

(defun compile-case-simplified (key-expr clauses env)
  "Compile (CASE key ...) - version avec strings"
  (let ((end-label (gen-label-simplified "CASE_END")))
    (append
     ;; Compiler la clé
     (compile-expr-with-ids key-expr env)
     ;; Sauver la clé
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     ;; Compiler les clauses
     (compile-case-clauses-simplified clauses end-label env)
     ;; Nettoyer la pile
     (list (list :ADDI *reg-sp* *reg-sp* 4))
     ;; Label de fin
     (list (list :LABEL end-label)))))

(defun compile-case-clauses-simplified (clauses end-label env)
  "Compile toutes les clauses de CASE"
  (if (null clauses)
      (list (list :LI *reg-v0* 0))  ; Aucune correspondance → NIL
      ;; Charger la clé sauvegardée
      (append
       (list (list :LW *reg-v0* *reg-sp* 4))
       (compile-case-clause-simplified '*reg-v0* (first clauses) end-label env)
       (compile-case-clauses-simplified (rest clauses) end-label env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-WHILE - Version simplifiée avec récursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-while-simplified (condition body env)
  "Compile (WHILE cond body...) - version avec strings"
  (let ((start-label (gen-label-simplified "WHILE_START"))
        (end-label (gen-label-simplified "WHILE_END")))
    (append
     ;; Label de début
     (list (list :LABEL start-label))
     ;; Tester la condition
     (compile-expr-with-ids condition env)
     (list (list :BEQ *reg-v0* *reg-zero* end-label))
     ;; Compiler le corps
     (compile-progn-simplified body env)
     ;; Retourner au début
     (list (list :J start-label))
     ;; Label de fin
     (list (list :LABEL end-label))
     ;; Retourner NIL
     (list (list :LI *reg-v0* 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-LOOP-WHILE - Version simplifiée
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-loop-while-simplified (condition body env)
  "Compile (LOOP WHILE cond DO ...) - version avec strings"
  (compile-while-simplified condition body env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-DOLIST - Version simplifiée
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-dolist-simplified (var list-expr body env)
  "Compile (DOLIST (var list) body...) - version simplifiée"
  (let ((start-label (gen-label-simplified "DOLIST_START"))
        (end-label (gen-label-simplified "DOLIST_END"))
        (list-offset -4)
        (var-offset -8))
    (append
     ;; Compiler la liste
     (compile-expr-with-ids list-expr env)
     ;; Sauver la liste sur la pile
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     ;; Label de début
     (list (list :LABEL start-label))
     ;; Charger la liste courante
     (list (list :LW *reg-t0* *reg-sp* 4))
     ;; Tester si liste vide (NULL)
     (list (list :BEQ *reg-t0* *reg-zero* end-label))
     ;; CAR: obtenir l'élément courant (primitive VM)
     (list (list :COMMENT "CAR - get current element"))
     (list (list :MOVE *reg-v0* *reg-t0*))  ; Simplifié
     ;; Sauver l'élément comme variable var
     (list (list :SW *reg-v0* *reg-sp* var-offset))
     ;; Compiler le corps avec var liée
     (compile-progn-simplified body env)
     ;; CDR: avancer dans la liste (primitive VM)
     (list (list :LW *reg-t0* *reg-sp* 4))
     (list (list :COMMENT "CDR - get rest of list"))
     ;; Sauver la nouvelle liste
     (list (list :SW *reg-t0* *reg-sp* 4))
     ;; Retourner au début
     (list (list :J start-label))
     ;; Label de fin
     (list (list :LABEL end-label))
     ;; Nettoyer la pile
     (list (list :ADDI *reg-sp* *reg-sp* 4))
     ;; Retourner NIL
     (list (list :LI *reg-v0* 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-DOTIMES - Version simplifiée
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-dotimes-simplified (var count-expr body env)
  "Compile (DOTIMES (var count) body...) - version avec strings"
  (let ((start-label (gen-label-simplified "DOTIMES_START"))
        (end-label (gen-label-simplified "DOTIMES_END"))
        (counter-offset -4)
        (limit-offset -8))
    (append
     ;; Compiler le compteur
     (compile-expr-with-ids count-expr env)
     ;; Sauver la limite
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     ;; Initialiser le compteur à 0
     (list (list :LI *reg-v0* 0)
           (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     ;; Label de début
     (list (list :LABEL start-label))
     ;; Charger compteur et limite
     (list (list :LW *reg-t0* *reg-sp* 4))  ; Compteur
     (list (list :LW *reg-t1* *reg-sp* 8))  ; Limite
     ;; Tester si compteur >= limite
     (list (list :BGE *reg-t0* *reg-t1* end-label))
     ;; Compiler le corps
     (compile-progn-simplified body env)
     ;; Incrémenter le compteur
     (list (list :LW *reg-t0* *reg-sp* 4))
     (list (list :ADDI *reg-t0* *reg-t0* 1))
     (list (list :SW *reg-t0* *reg-sp* 4))
     ;; Retourner au début
     (list (list :J start-label))
     ;; Label de fin
     (list (list :LABEL end-label))
     ;; Nettoyer la pile
     (list (list :ADDI *reg-sp* *reg-sp* 8))
     ;; Retourner NIL
     (list (list :LI *reg-v0* 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-CAR/CDR/NULL - Versions simplifiées
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-car-simplified (list-expr env)
  "Compile (CAR list) - délégué à primitive VM"
  (let ((null-label (gen-label-simplified "CAR_NULL"))
        (end-label (gen-label-simplified "CAR_END")))
    (append
     (compile-expr-with-ids list-expr env)
     ;; Vérifier si NIL
     (list (list :BEQ *reg-v0* *reg-zero* null-label))
     ;; Appel primitive CAR (simplifié)
     (list (list :COMMENT "CAR primitive"))
     (list (list :J end-label))
     (list (list :LABEL null-label))
     (list (list :LI *reg-v0* 0))
     (list (list :LABEL end-label)))))

(defun compile-cdr-simplified (list-expr env)
  "Compile (CDR list) - délégué à primitive VM"
  (let ((null-label (gen-label-simplified "CDR_NULL"))
        (end-label (gen-label-simplified "CDR_END")))
    (append
     (compile-expr-with-ids list-expr env)
     (list (list :BEQ *reg-v0* *reg-zero* null-label))
     (list (list :COMMENT "CDR primitive"))
     (list (list :J end-label))
     (list (list :LABEL null-label))
     (list (list :LI *reg-v0* 0))
     (list (list :LABEL end-label)))))

(defun compile-null-simplified (expr env)
  "Compile (NULL expr) - teste si NIL"
  (let ((true-label (gen-label-simplified "NULL_TRUE"))
        (end-label (gen-label-simplified "NULL_END")))
    (append
     (compile-expr-with-ids expr env)
     (list (list :BEQ *reg-v0* *reg-zero* true-label))
     (list (list :LI *reg-v0* 0))
     (list (list :J end-label))
     (list (list :LABEL true-label))
     (list (list :LI *reg-v0* 1))
     (list (list :LABEL end-label)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-ASSOC/MEMBER/APPEND - Versions simplifiées
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-assoc-simplified (key alist env)
  "Compile (ASSOC key alist) - version simplifiée"
  (let ((loop-label (gen-label-simplified "ASSOC_LOOP"))
        (found-label (gen-label-simplified "ASSOC_FOUND"))
        (end-label (gen-label-simplified "ASSOC_END")))
    (append
     ;; Compiler la clé
     (compile-expr-with-ids key env)
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     ;; Compiler l'alist
     (compile-expr-with-ids alist env)
     ;; Boucle de recherche
     (list (list :LABEL loop-label))
     (list (list :BEQ *reg-v0* *reg-zero* end-label))
     ;; CAR pour obtenir la paire courante
     (list (list :COMMENT "CAR - get current pair"))
     ;; Comparer les clés (simplifié)
     (list (list :LW *reg-t0* *reg-sp* 4))  ; Clé recherchée
     (list (list :BEQ *reg-v0* *reg-t0* found-label))
     ;; CDR pour passer à la suivante
     (list (list :COMMENT "CDR - get rest"))
     (list (list :J loop-label))
     ;; Trouvé
     (list (list :LABEL found-label))
     (list (list :J end-label))
     ;; Fin
     (list (list :LABEL end-label))
     (list (list :ADDI *reg-sp* *reg-sp* 4)))))

(defun compile-member-simplified (item list env)
  "Compile (MEMBER item list) - version simplifiée"
  (let ((loop-label (gen-label-simplified "MEMBER_LOOP"))
        (found-label (gen-label-simplified "MEMBER_FOUND"))
        (end-label (gen-label-simplified "MEMBER_END")))
    (append
     (compile-expr-with-ids item env)
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     (compile-expr-with-ids list env)
     (list (list :LABEL loop-label))
     (list (list :BEQ *reg-v0* *reg-zero* end-label))
     (list (list :COMMENT "CAR and compare"))
     (list (list :LW *reg-t0* *reg-sp* 4))
     (list (list :BEQ *reg-v0* *reg-t0* found-label))
     (list (list :COMMENT "CDR"))
     (list (list :J loop-label))
     (list (list :LABEL found-label))
     (list (list :LI *reg-v0* 1))
     (list (list :J end-label))
     (list (list :LABEL end-label))
     (list (list :ADDI *reg-sp* *reg-sp* 4)))))

(defun compile-append-simplified (list1 list2 env)
  "Compile (APPEND list1 list2) - version simplifiée 2 arguments"
  (let ((loop-label (gen-label-simplified "APPEND_LOOP"))
        (end-label (gen-label-simplified "APPEND_END")))
    (append
     (compile-expr-with-ids list1 env)
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     (compile-expr-with-ids list2 env)
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     ;; Implémentation simplifiée
     (list (list :COMMENT "APPEND primitive"))
     (list (list :LW *reg-v0* *reg-sp* 4))
     (list (list :ADDI *reg-sp* *reg-sp* 8))
     (list (list :LABEL end-label)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-LENGTH/NTH - Versions simplifiées
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-length-simplified (list-expr env)
  "Compile (LENGTH list) - version avec strings"
  (let ((loop-label (gen-label-simplified "LENGTH_LOOP"))
        (end-label (gen-label-simplified "LENGTH_END")))
    (append
     (compile-expr-with-ids list-expr env)
     (list (list :LI *reg-t0* 0))  ; Compteur
     (list (list :LABEL loop-label))
     (list (list :BEQ *reg-v0* *reg-zero* end-label))
     (list (list :ADDI *reg-t0* *reg-t0* 1))
     (list (list :COMMENT "CDR"))
     (list (list :J loop-label))
     (list (list :LABEL end-label))
     (list (list :MOVE *reg-v0* *reg-t0*)))))

(defun compile-nth-simplified (n-expr list-expr env)
  "Compile (NTH n list) - version avec strings"
  (let ((loop-label (gen-label-simplified "NTH_LOOP"))
        (found-label (gen-label-simplified "NTH_FOUND"))
        (end-label (gen-label-simplified "NTH_END")))
    (append
     (compile-expr-with-ids n-expr env)
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     (compile-expr-with-ids list-expr env)
     (list (list :LABEL loop-label))
     (list (list :LW *reg-t0* *reg-sp* 4))
     (list (list :BEQ *reg-t0* *reg-zero* found-label))
     (list (list :BEQ *reg-v0* *reg-zero* end-label))
     (list (list :ADDI *reg-t0* *reg-t0* -1))
     (list (list :SW *reg-t0* *reg-sp* 4))
     (list (list :COMMENT "CDR"))
     (list (list :J loop-label))
     (list (list :LABEL found-label))
     (list (list :COMMENT "CAR"))
     (list (list :LABEL end-label))
     (list (list :ADDI *reg-sp* *reg-sp* 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-LABELS/LAMBDA - Versions simplifiées
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-labels-simplified (bindings body env)
  "Compile (LABELS ...) - version simplifiée"
  ;; Version très simplifiée qui compile juste le corps
  (let ((body-label (gen-label-simplified "LABELS_BODY")))
    (append
     (list (list :LABEL body-label))
     (compile-progn-simplified body env))))

(defun compile-lambda-simplified (params body env)
  "Compile (LAMBDA ...) - version simplifiée"
  (let ((func-label (gen-label-simplified "lambda_func")))
    (append
     (list (list :LABEL func-label))
     (compile-progn-simplified body env)
     (list (list :JR *reg-ra*)))))

(format t "  Phase 2 chargée: 15 fonctions avec labels symboliques~%")
(format t "    - compile-case-simplified~%")
(format t "    - compile-while/loop-while-simplified~%")
(format t "    - compile-dolist/dotimes-simplified~%")
(format t "    - compile-car/cdr/null-simplified~%")
(format t "    - compile-assoc/member/append-simplified~%")
(format t "    - compile-length/nth-simplified~%")
(format t "    - compile-labels/lambda-simplified~%~%")
