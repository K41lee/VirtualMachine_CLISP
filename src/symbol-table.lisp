;;;; symbol-table.lisp
;;;; Table de symboles avec interning pour préserver la correspondance
;;;; symbole (string) ↔ ID numérique

;;; ============================================================================
;;; CONSTANTES ET STRUCTURES
;;; ============================================================================

(defparameter *symbol-id-counter* 10000
  "Compteur pour générer des IDs uniques pour les symboles.
   Commence à 10000 pour éviter les conflits avec d'autres valeurs.")

(defparameter *symbol-name-to-id* (make-hash-table :test 'equal)
  "Table: nom de symbole (string) → ID numérique.
   Permet de retrouver ou créer l'ID d'un symbole.")

(defparameter *symbol-id-to-name* (make-hash-table :test 'eql)
  "Table: ID numérique → nom de symbole (string).
   Permet de retrouver le nom depuis l'ID.")

(defparameter *symbol-id-to-heap-addr* (make-hash-table :test 'eql)
  "Table: ID numérique → adresse heap du symbole.
   Utilisé si on stocke les symboles en mémoire VM.")

;;; ============================================================================
;;; FONCTIONS D'INTERNING
;;; ============================================================================

(defun reset-symbol-table ()
  "Réinitialise la table de symboles (utile pour les tests)."
  (clrhash *symbol-name-to-id*)
  (clrhash *symbol-id-to-name*)
  (clrhash *symbol-id-to-heap-addr*)
  (setf *symbol-id-counter* 10000))

(defun intern-symbol (name)
  "Interne un symbole: retourne son ID numérique.
   Si le symbole existe déjà, retourne l'ID existant.
   Sinon, crée un nouveau symbole avec un nouvel ID.
   
   Args:
     name : string ou symbol (sera converti en string)
   
   Returns:
     ID numérique du symbole (entier >= 10000)"
  (let ((name-str (if (symbolp name)
                      (symbol-name name)
                      name)))
    ;; Vérifier si le symbole existe déjà
    (let ((existing-id (gethash name-str *symbol-name-to-id*)))
      (if existing-id
          ;; Symbole existant : retourner l'ID
          existing-id
          ;; Nouveau symbole : créer un ID
          (let ((new-id (incf *symbol-id-counter*)))
            (setf (gethash name-str *symbol-name-to-id*) new-id)
            (setf (gethash new-id *symbol-id-to-name*) name-str)
            new-id)))))

(defun get-symbol-name (id)
  "Retourne le nom (string) d'un symbole depuis son ID.
   
   Args:
     id : ID numérique du symbole
   
   Returns:
     string : nom du symbole, ou NIL si l'ID n'existe pas"
  (gethash id *symbol-id-to-name*))

(defun get-symbol-id (name)
  "Retourne l'ID d'un symbole depuis son nom, ou NIL si inexistant.
   (N'interné pas le symbole s'il n'existe pas)
   
   Args:
     name : string ou symbol
   
   Returns:
     ID numérique ou NIL"
  (let ((name-str (if (symbolp name)
                      (symbol-name name)
                      name)))
    (gethash name-str *symbol-name-to-id*)))

(defun symbol-interned-p (name)
  "Vérifie si un symbole est déjà interné.
   
   Args:
     name : string ou symbol
   
   Returns:
     T si interné, NIL sinon"
  (not (null (get-symbol-id name))))

(defun get-all-symbols ()
  "Retourne une liste de tous les symboles internés.
   
   Returns:
     Liste de paires (name . id)"
  (let ((symbols '()))
    (maphash #'(lambda (name id)
                 (push (cons name id) symbols))
             *symbol-name-to-id*)
    (sort symbols #'< :key #'cdr)))

(defun symbol-count ()
  "Retourne le nombre de symboles internés."
  (hash-table-count *symbol-name-to-id*))

;;; ============================================================================
;;; FONCTIONS DE COMPILATION
;;; ============================================================================

(defun compile-intern-symbol (name-expr env)
  "Génère du code MIPS pour interner un symbole au runtime.
   Cette fonction est appelée quand on a besoin d'interner un symbole
   de manière dynamique dans le code compilé.
   
   Args:
     name-expr : expression qui évalue vers un string/symbole
     env : environnement de compilation
   
   Returns:
     Code ASM qui met l'ID du symbole dans $V0"
  ;; Pour l'instant, on utilise l'interning statique
  ;; TODO: Implémenter interning dynamique si nécessaire
  (error "Dynamic symbol interning not yet implemented"))

(defun compile-symbol-name (id-expr env)
  "Génère du code MIPS pour récupérer le nom d'un symbole depuis son ID.
   
   Args:
     id-expr : expression qui évalue vers un ID de symbole
     env : environnement de compilation
   
   Returns:
     Code ASM qui met l'adresse du string dans $V0"
  ;; Pour l'instant, non implémenté
  ;; TODO: Implémenter storage des strings en mémoire VM
  (error "Symbol-name runtime not yet implemented"))

(defun compile-symbol-eq (id1-expr id2-expr env)
  "Génère du code MIPS pour comparer deux symboles.
   La comparaison est simple: comparer les IDs.
   
   Args:
     id1-expr : expression pour le premier ID
     id2-expr : expression pour le second ID
     env : environnement de compilation
   
   Returns:
     Code ASM qui met 1 dans $V0 si égaux, 0 sinon"
  (load "src/compiler.lisp")  ; Pour avoir accès aux registres
  (let ((code '()))
    ;; Compiler première expression → $V0
    (setf code (append code (funcall #'compile-expr id1-expr env)))
    ;; Sauvegarder sur pile
    (setf code (append code (list
                             (list :ADDI :$sp -4 :$sp)
                             (list :SW :$v0 :$sp 0))))
    ;; Compiler seconde expression → $V0
    (setf code (append code (funcall #'compile-expr id2-expr env)))
    ;; Charger première valeur dans $T0
    (setf code (append code (list (list :LW :$t0 :$sp 0))))
    (setf code (append code (list (list :ADDI :$sp 4 :$sp))))
    ;; Comparer $T0 et $V0
    (setf code (append code (list
                             (list :SEQ :$t0 :$v0)
                             (list :MOVE :$eq :$v0))))
    code))

;;; ============================================================================
;;; AFFICHAGE ET DEBUG
;;; ============================================================================

(defun print-symbol-table ()
  "Affiche le contenu de la table de symboles."
  (format t "~%=== TABLE DE SYMBOLES ===~%")
  (format t "Nombre de symboles: ~A~%" (symbol-count))
  (format t "~%Symboles internés:~%")
  (let ((symbols (get-all-symbols)))
    (dolist (sym symbols)
      (format t "  ~5A → ~A~%" (cdr sym) (car sym))))
  (format t "~%=========================~%"))

(defun dump-symbol-table-stats ()
  "Affiche des statistiques sur la table de symboles."
  (format t "~%=== STATISTIQUES TABLE DE SYMBOLES ===~%")
  (format t "Nombre de symboles: ~A~%" (symbol-count))
  (format t "Prochain ID: ~A~%" (1+ *symbol-id-counter*))
  (format t "Range d'IDs: [10001, ~A]~%" *symbol-id-counter*)
  (format t "=====================================~%"))

;;; ============================================================================
;;; EXPORT DES SYMBOLES (pour debugging/inspection)
;;; ============================================================================

(defun export-symbol-table-to-list ()
  "Export la table de symboles sous forme de liste associative.
   Utile pour debugging ou pour sauvegarder l'état.
   
   Returns:
     Liste de la forme ((name . id) ...)"
  (get-all-symbols))

(defun import-symbol-table-from-list (symbol-list)
  "Importe une table de symboles depuis une liste associative.
   Utile pour restaurer un état sauvegardé.
   
   Args:
     symbol-list : Liste de la forme ((name . id) ...)"
  (reset-symbol-table)
  (dolist (sym symbol-list)
    (let ((name (car sym))
          (id (cdr sym)))
      (setf (gethash name *symbol-name-to-id*) id)
      (setf (gethash id *symbol-id-to-name*) name)
      (when (> id *symbol-id-counter*)
        (setf *symbol-id-counter* id)))))

;;; ============================================================================
;;; SYMBOLES PRÉDÉFINIS - PRÉ-INTERNING
;;; ============================================================================

(defun intern-predefined-symbols ()
  "Interne les symboles clés du langage LISP.
   Retourne une liste associative (nom . id) pour référence.
   
   Cette fonction DOIT être appelée au chargement pour garantir
   que le parser et le dispatcher puissent reconnaître les mots-clés."
  
  (let ((symbols '()))
    ;; Formes spéciales et définitions
    (push (cons "DEFUN" (intern-symbol "DEFUN")) symbols)
    (push (cons "DEFVAR" (intern-symbol "DEFVAR")) symbols)
    (push (cons "DEFCONSTANT" (intern-symbol "DEFCONSTANT")) symbols)
    (push (cons "LAMBDA" (intern-symbol "LAMBDA")) symbols)
    (push (cons "QUOTE" (intern-symbol "QUOTE")) symbols)
    (push (cons "FUNCTION" (intern-symbol "FUNCTION")) symbols)
    
    ;; Contrôle de flux
    (push (cons "IF" (intern-symbol "IF")) symbols)
    (push (cons "COND" (intern-symbol "COND")) symbols)
    (push (cons "CASE" (intern-symbol "CASE")) symbols)
    (push (cons "PROGN" (intern-symbol "PROGN")) symbols)
    (push (cons "PROG1" (intern-symbol "PROG1")) symbols)
    (push (cons "WHEN" (intern-symbol "WHEN")) symbols)
    (push (cons "UNLESS" (intern-symbol "UNLESS")) symbols)
    
    ;; Variables et affectations
    (push (cons "LET" (intern-symbol "LET")) symbols)
    (push (cons "LET*" (intern-symbol "LET*")) symbols)
    (push (cons "SETQ" (intern-symbol "SETQ")) symbols)
    (push (cons "SETF" (intern-symbol "SETF")) symbols)
    
    ;; Boucles
    (push (cons "LOOP" (intern-symbol "LOOP")) symbols)
    (push (cons "DOLIST" (intern-symbol "DOLIST")) symbols)
    (push (cons "DOTIMES" (intern-symbol "DOTIMES")) symbols)
    (push (cons "DO" (intern-symbol "DO")) symbols)
    
    ;; Opérateurs arithmétiques
    (push (cons "+" (intern-symbol "+")) symbols)
    (push (cons "-" (intern-symbol "-")) symbols)
    (push (cons "*" (intern-symbol "*")) symbols)
    (push (cons "/" (intern-symbol "/")) symbols)
    (push (cons "MOD" (intern-symbol "MOD")) symbols)
    (push (cons "1+" (intern-symbol "1+")) symbols)
    (push (cons "1-" (intern-symbol "1-")) symbols)
    
    ;; Comparaisons
    (push (cons "=" (intern-symbol "=")) symbols)
    (push (cons "/=" (intern-symbol "/=")) symbols)
    (push (cons "<" (intern-symbol "<")) symbols)
    (push (cons ">" (intern-symbol ">")) symbols)
    (push (cons "<=" (intern-symbol "<=")) symbols)
    (push (cons ">=" (intern-symbol ">=")) symbols)
    (push (cons "EQ" (intern-symbol "EQ")) symbols)
    (push (cons "EQL" (intern-symbol "EQL")) symbols)
    (push (cons "EQUAL" (intern-symbol "EQUAL")) symbols)
    
    ;; Opérateurs logiques
    (push (cons "AND" (intern-symbol "AND")) symbols)
    (push (cons "OR" (intern-symbol "OR")) symbols)
    (push (cons "NOT" (intern-symbol "NOT")) symbols)
    
    ;; Manipulation de listes
    (push (cons "CAR" (intern-symbol "CAR")) symbols)
    (push (cons "CDR" (intern-symbol "CDR")) symbols)
    (push (cons "CONS" (intern-symbol "CONS")) symbols)
    (push (cons "LIST" (intern-symbol "LIST")) symbols)
    (push (cons "APPEND" (intern-symbol "APPEND")) symbols)
    (push (cons "REVERSE" (intern-symbol "REVERSE")) symbols)
    (push (cons "LENGTH" (intern-symbol "LENGTH")) symbols)
    (push (cons "NTH" (intern-symbol "NTH")) symbols)
    (push (cons "FIRST" (intern-symbol "FIRST")) symbols)
    (push (cons "SECOND" (intern-symbol "SECOND")) symbols)
    (push (cons "REST" (intern-symbol "REST")) symbols)
    
    ;; Prédicats de type
    (push (cons "NULL" (intern-symbol "NULL")) symbols)
    (push (cons "ATOM" (intern-symbol "ATOM")) symbols)
    (push (cons "NUMBERP" (intern-symbol "NUMBERP")) symbols)
    (push (cons "SYMBOLP" (intern-symbol "SYMBOLP")) symbols)
    (push (cons "LISTP" (intern-symbol "LISTP")) symbols)
    (push (cons "CONSP" (intern-symbol "CONSP")) symbols)
    
    ;; Constantes
    (push (cons "T" (intern-symbol "T")) symbols)
    (push (cons "NIL" (intern-symbol "NIL")) symbols)
    
    ;; ==================================================================
    ;; TYPES DE NŒUDS AST - Pour le dispatcher (compile-expr)
    ;; ==================================================================
    ;; Ces symboles représentent les types de nœuds retournés par
    ;; parse-lisp-expr. Ils permettent au dispatcher de comparer
    ;; des IDs numériques au lieu de keywords.
    
    (push (cons "CONSTANT" (intern-symbol "CONSTANT")) symbols)
    (push (cons "VARIABLE" (intern-symbol "VARIABLE")) symbols)
    (push (cons "ARITHMETIC" (intern-symbol "ARITHMETIC")) symbols)
    (push (cons "MATH-FUNC" (intern-symbol "MATH-FUNC")) symbols)
    (push (cons "COMPARISON" (intern-symbol "COMPARISON")) symbols)
    (push (cons "FUNCALL" (intern-symbol "FUNCALL")) symbols)
    (push (cons "CALL" (intern-symbol "CALL")) symbols)
    (push (cons "LAMBDA-NODE" (intern-symbol "LAMBDA-NODE")) symbols)
    (push (cons "LABELS" (intern-symbol "LABELS")) symbols)
    (push (cons "MAKE-ARRAY" (intern-symbol "MAKE-ARRAY")) symbols)
    (push (cons "AREF" (intern-symbol "AREF")) symbols)
    (push (cons "SETF-AREF" (intern-symbol "SETF-AREF")) symbols)
    (push (cons "INCF" (intern-symbol "INCF")) symbols)
    (push (cons "DECF" (intern-symbol "DECF")) symbols)
    (push (cons "LOOP-WHILE" (intern-symbol "LOOP-WHILE")) symbols)
    (push (cons "LOOP-ADVANCED" (intern-symbol "LOOP-ADVANCED")) symbols)
    (push (cons "WHILE" (intern-symbol "WHILE")) symbols)
    (push (cons "ASSOC" (intern-symbol "ASSOC")) symbols)
    (push (cons "MEMBER" (intern-symbol "MEMBER")) symbols)
    (push (cons "DEFSTRUCT" (intern-symbol "DEFSTRUCT")) symbols)
    (push (cons "VM-PRIMITIVE" (intern-symbol "VM-PRIMITIVE")) symbols)
    (push (cons "ERROR" (intern-symbol "ERROR")) symbols)
    (push (cons "FORMAT" (intern-symbol "FORMAT")) symbols)
    
    ;; Retourner la liste triée
    (sort symbols #'< :key #'cdr)))

(defun get-predefined-symbol-id (name)
  "Retourne l'ID d'un symbole prédéfini.
   Retourne NIL si le symbole n'est pas prédéfini ou pas encore interné.
   
   Args:
     name : string ou symbol
   
   Returns:
     ID numérique ou NIL"
  (get-symbol-id name))

;;; ============================================================================
;;; CONSTANTES GLOBALES POUR LES SYMBOLES CLÉS
;;; ============================================================================

(defparameter *predefined-symbols* nil
  "Liste des symboles prédéfinis avec leurs IDs.
   Remplie par intern-predefined-symbols.")

(defun init-symbol-constants ()
  "Initialise les constantes globales pour les symboles clés.
   Doit être appelée après intern-predefined-symbols."
  
  ;; Interne d'abord tous les symboles
  (setf *predefined-symbols* (intern-predefined-symbols))
  
  ;; Créer des constantes pour accès rapide
  (defparameter *defun-id* (get-symbol-id "DEFUN"))
  (defparameter *defvar-id* (get-symbol-id "DEFVAR"))
  (defparameter *defconstant-id* (get-symbol-id "DEFCONSTANT"))
  (defparameter *lambda-id* (get-symbol-id "LAMBDA"))
  (defparameter *quote-id* (get-symbol-id "QUOTE"))
  (defparameter *if-id* (get-symbol-id "IF"))
  (defparameter *cond-id* (get-symbol-id "COND"))
  (defparameter *let-id* (get-symbol-id "LET"))
  (defparameter *let*-id* (get-symbol-id "LET*"))
  (defparameter *setq-id* (get-symbol-id "SETQ"))
  (defparameter *progn-id* (get-symbol-id "PROGN"))
  (defparameter *loop-id* (get-symbol-id "LOOP"))
  (defparameter *dolist-id* (get-symbol-id "DOLIST"))
  
  ;; Opérateurs
  (defparameter *+-id* (get-symbol-id "+"))
  (defparameter *--id* (get-symbol-id "-"))
  (defparameter *multiply-id* (get-symbol-id "*"))
  (defparameter *divide-id* (get-symbol-id "/"))
  (defparameter *=-id* (get-symbol-id "="))
  (defparameter *<-id* (get-symbol-id "<"))
  (defparameter *>-id* (get-symbol-id ">"))
  
  ;; Listes
  (defparameter *car-id* (get-symbol-id "CAR"))
  (defparameter *cdr-id* (get-symbol-id "CDR"))
  (defparameter *cons-id* (get-symbol-id "CONS"))
  (defparameter *list-id* (get-symbol-id "LIST"))
  
  ;; Prédicats
  (defparameter *null-id* (get-symbol-id "NULL"))
  (defparameter *atom-id* (get-symbol-id "ATOM"))
  (defparameter *consp-id* (get-symbol-id "CONSP"))
  
  ;; Constantes
  (defparameter *t-id* (get-symbol-id "T"))
  (defparameter *nil-id* (get-symbol-id "NIL"))
  
  ;; =================================================================
  ;; IDs des types de nœuds AST (pour dispatcher compile-expr-with-ids)
  ;; =================================================================
  (defparameter *constant-type-id* (get-symbol-id "CONSTANT"))
  (defparameter *variable-type-id* (get-symbol-id "VARIABLE"))
  (defparameter *arithmetic-type-id* (get-symbol-id "ARITHMETIC"))
  (defparameter *math-func-type-id* (get-symbol-id "MATH-FUNC"))
  (defparameter *comparison-type-id* (get-symbol-id "COMPARISON"))
  (defparameter *if-type-id* (get-symbol-id "IF"))
  (defparameter *cond-type-id* (get-symbol-id "COND"))
  (defparameter *when-type-id* (get-symbol-id "WHEN"))
  (defparameter *unless-type-id* (get-symbol-id "UNLESS"))
  (defparameter *not-type-id* (get-symbol-id "NOT"))
  (defparameter *and-type-id* (get-symbol-id "AND"))
  (defparameter *or-type-id* (get-symbol-id "OR"))
  (defparameter *case-type-id* (get-symbol-id "CASE"))
  (defparameter *let-type-id* (get-symbol-id "LET"))
  (defparameter *let*-type-id* (get-symbol-id "LET*"))
  (defparameter *loop-while-type-id* (get-symbol-id "LOOP-WHILE"))
  (defparameter *loop-advanced-type-id* (get-symbol-id "LOOP-ADVANCED"))
  (defparameter *while-type-id* (get-symbol-id "WHILE"))
  (defparameter *dolist-type-id* (get-symbol-id "DOLIST"))
  (defparameter *progn-type-id* (get-symbol-id "PROGN"))
  (defparameter *make-array-type-id* (get-symbol-id "MAKE-ARRAY"))
  (defparameter *aref-type-id* (get-symbol-id "AREF"))
  (defparameter *dotimes-type-id* (get-symbol-id "DOTIMES"))
  (defparameter *incf-type-id* (get-symbol-id "INCF"))
  (defparameter *decf-type-id* (get-symbol-id "DECF"))
  (defparameter *cons-type-id* (get-symbol-id "CONS"))
  (defparameter *car-type-id* (get-symbol-id "CAR"))
  (defparameter *cdr-type-id* (get-symbol-id "CDR"))
  (defparameter *null-type-id* (get-symbol-id "NULL"))
  (defparameter *length-type-id* (get-symbol-id "LENGTH"))
  (defparameter *nth-type-id* (get-symbol-id "NTH"))
  (defparameter *assoc-type-id* (get-symbol-id "ASSOC"))
  (defparameter *member-type-id* (get-symbol-id "MEMBER"))
  (defparameter *append-type-id* (get-symbol-id "APPEND"))
  (defparameter *vm-primitive-type-id* (get-symbol-id "VM-PRIMITIVE"))
  (defparameter *setq-type-id* (get-symbol-id "SETQ"))
  (defparameter *setf-aref-type-id* (get-symbol-id "SETF-AREF"))
  (defparameter *labels-type-id* (get-symbol-id "LABELS"))
  (defparameter *lambda-type-id* (get-symbol-id "LAMBDA-NODE"))
  (defparameter *call-type-id* (get-symbol-id "CALL"))
  (defparameter *defconstant-type-id* (get-symbol-id "DEFCONSTANT"))
  (defparameter *defstruct-type-id* (get-symbol-id "DEFSTRUCT"))
  (defparameter *defvar-type-id* (get-symbol-id "DEFVAR"))
  (defparameter *defun-type-id* (get-symbol-id "DEFUN"))
  (defparameter *error-type-id* (get-symbol-id "ERROR"))
  (defparameter *format-type-id* (get-symbol-id "FORMAT"))
  (defparameter *funcall-type-id* (get-symbol-id "FUNCALL"))
  
  (format t "  - ~A symboles prédéfinis internés~%" (length *predefined-symbols*)))

;;; ============================================================================
;;; INITIALISATION AUTOMATIQUE
;;; ============================================================================

;; Initialiser les symboles au chargement du module
(init-symbol-constants)

(format t "~%✓ Module symbol-table.lisp chargé~%")
(format t "  - intern-symbol : Interne un symbole et retourne son ID~%")
(format t "  - get-symbol-name : Récupère le nom depuis l'ID~%")
(format t "  - get-symbol-id : Récupère l'ID depuis le nom~%")
(format t "  - print-symbol-table : Affiche la table complète~%")
(format t "  - Symboles clés disponibles: *defun-id*, *if-id*, *quote-id*, etc.~%")
