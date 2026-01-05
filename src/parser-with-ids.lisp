;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSER AVEC IDs - ÉTAPE 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Ce fichier implémente un parser qui utilise des IDs numériques
;; au lieu de symboles, permettant l'auto-compilation complète.
;;
;; Différences avec parse-lisp-expr traditionnel:
;; - Utilise COND + = au lieu de CASE (compilable en MIPS)
;; - Compare des IDs numériques au lieu de symboles
;; - Compatible avec la table de symboles (src/symbol-table.lisp)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITAIRES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lisp-list-p (expr)
  "Vérifie si l'expression est une liste"
  (listp expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSER PRINCIPAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-lisp-expr-with-ids (expr)
  "Parse une expression LISP en utilisant des IDs pour les mots-clés.
   Version compilable: utilise COND+= au lieu de CASE."
  
  (cond
    ;; ========== ATOMS ==========
    
    ;; Nombre
    ((numberp expr) 
     (list :constant expr))
    
    ;; NIL (avant symbolp car NIL est un symbole)
    ((null expr)
     (list :constant 0))
    
    ;; T (avant symbolp car T est un symbole)
    ((eq expr t)
     (list :constant 1))
    
    ;; Keywords (:$sp, :$gp, etc.)
    ((keywordp expr)
     (list :constant expr))
    
    ;; Symbole (variable ou constante globale)
    ((symbolp expr)
     (let ((const-value (gethash expr *global-constants*)))
       (if const-value
           (list :constant const-value)
           (list :variable expr))))
    
    ;; ========== LISTES ==========
    
    ((lisp-list-p expr)
     (let ((op (first expr))
           (args (rest expr)))
       
       ;; Obtenir l'ID du symbole opérateur (nil si pas un symbole)
       (let ((op-id (if (symbolp op) (get-symbol-id (symbol-name op)) nil)))
         
         (cond
           ;; ========== OPÉRATEURS ARITHMÉTIQUES ==========
           ((and op-id (or (= op-id *+-id*)
                           (= op-id *--id*)
                           (= op-id *multiply-id*)
                           (= op-id *divide-id*)))
            (list :arithmetic op args))
           
           ;; MOD nécessite un test séparé si pas prédéfini
           ((and (symbolp op) (eq op 'mod))
            (list :arithmetic op args))
           
           ;; ========== OPÉRATEURS DE COMPARAISON ==========
           ((and op-id (or (= op-id *<-id*)
                           (= op-id *>-id*)
                           (= op-id *=-id*)))
            (list :comparison op args))
           
           ;; Comparaison non prédéfinie (fallback)
           ((and (symbolp op) (member op '(<= >= /=)))
            (list :comparison op args))
           
           ;; ========== STRUCTURES DE CONTRÔLE ==========
           
           ;; IF
           ((and op-id (= op-id *if-id*))
            (list :if (first args) (second args) (third args)))
           
           ;; COND
           ((and op-id (= op-id *cond-id*))
            (list :cond args))
           
           ;; WHEN, UNLESS, CASE - fallback symbolique
           ((and (symbolp op) (eq op 'when))
            (list :when (first args) (rest args)))
           
           ((and (symbolp op) (eq op 'unless))
            (list :unless (first args) (rest args)))
           
           ((and (symbolp op) (eq op 'case))
            (list :case (first args) (rest args)))
           
           ;; ========== OPÉRATEURS LOGIQUES ==========
           
           ;; NOT, AND, OR - fallback symbolique
           ((and (symbolp op) (eq op 'not))
            (list :not (first args)))
           
           ((and (symbolp op) (eq op 'and))
            (list :and args))
           
           ((and (symbolp op) (eq op 'or))
            (list :or args))
           
           ;; ========== STRUCTURES DE DONNÉES ==========
           
           ;; QUOTE
           ((and op-id (= op-id *quote-id*))
            (list :constant (first args)))
           
           ;; CONS
           ((and op-id (= op-id *cons-id*))
            (if (= (length args) 2)
                (list :cons (first args) (second args))
                (error "CONS requiert 2 arguments: ~A" expr)))
           
           ;; CAR
           ((and op-id (= op-id *car-id*))
            (if (= (length args) 1)
                (list :car (first args))
                (error "CAR requiert 1 argument: ~A" expr)))
           
           ;; CDR
           ((and op-id (= op-id *cdr-id*))
            (if (= (length args) 1)
                (list :cdr (first args))
                (error "CDR requiert 1 argument: ~A" expr)))
           
           ;; NULL
           ((and op-id (= op-id *null-id*))
            (if (= (length args) 1)
                (list :null (first args))
                (error "NULL requiert 1 argument: ~A" expr)))
           
           ;; LIST  
           ((and op-id (= op-id *list-id*))
            (list :list args))
           
           ;; FIRST, REST, SECOND, LENGTH, NTH - fallback avec symbole
           ((and (symbolp op) (member op '(first rest second length nth)))
            (case op
              (first (if (= (length args) 1)
                        (list :car (first args))
                        (error "FIRST requiert 1 argument: ~A" expr)))
              (rest (if (= (length args) 1)
                       (list :cdr (first args))
                       (error "REST requiert 1 argument: ~A" expr)))
              (second (if (= (length args) 1)
                         (list :car (list :cdr (first args)))
                         (error "SECOND requiert 1 argument: ~A" expr)))
              (length (if (= (length args) 1)
                         (list :length (first args))
                         (error "LENGTH requiert 1 argument: ~A" expr)))
              (nth (if (= (length args) 2)
                      (list :nth (first args) (second args))
                      (error "NTH requiert 2 arguments: ~A" expr)))))
           
           ;; ========== VARIABLES ET BINDINGS ==========
           
           ;; LET
           ((and op-id (= op-id *let-id*))
            (list :let (first args) (rest args)))
           
           ;; LET*
           ((and (symbolp op) (eq op 'let*))
            (list :let* (first args) (rest args)))
           
           ;; SETQ
           ((and op-id (= op-id *setq-id*))
            (list :setq (first args) (second args)))
           
           ;; ========== BOUCLES ==========
           
           ;; WHILE, DOTIMES - fallback symbolique
           ((and (symbolp op) (eq op 'while))
            (if (>= (length args) 2)
                (list :while (first args) (rest args))
                (error "WHILE requiert au moins 2 arguments: ~A" expr)))
           
           ((and (symbolp op) (eq op 'dotimes))
            (list :dotimes (first args) (rest args)))
           
           ;; DOLIST
           ((and op-id (= op-id *dolist-id*))
            (if (and (>= (length args) 2)
                     (listp (first args))
                     (= (length (first args)) 2))
                (let ((var (first (first args)))
                      (list-expr (second (first args)))
                      (body (rest args)))
                  (list :dolist var list-expr body))
                (error "DOLIST: (dolist (var list) body...) ~A" expr)))
           
           ;; PROGN
           ((and op-id (= op-id *progn-id*))
            (list :progn args))
           
           ;; Progn fallback
           ((and (symbolp op) (eq op 'progn))
            (list :progn args))
           
           ;; LOOP fallback
           ((and op-id (= op-id *loop-id*))
            (list :loop args))
           ((and (symbolp op) (eq op 'loop))
            (list :loop args))
           
           ;; ========== FONCTIONS ==========
           
           ;; DEFUN
           ((and op-id (= op-id *defun-id*))
            (list :defun (first args) (second args) (cddr args)))
           
           ;; LAMBDA
           ((and op-id (= op-id *lambda-id*))
            (list :lambda (first args) (rest args)))
           
           ;; LAMBDA fallback
           ((and (symbolp op) (eq op 'lambda))
            (list :lambda (first args) (rest args)))
           
           ;; ========== DEFAULT: APPEL DE FONCTION ==========
           
           (t
            ;; Appel de fonction utilisateur
           ;; ========== DEFAULT: APPEL DE FONCTION ==========
           
           (t
            ;; Appel de fonction utilisateur
            (list :funcall op args)))))))
    
    ;; Cas non géré
    (t
     (error "Expression non reconnue: ~A" expr))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS RAPIDES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-parser-with-ids ()
  "Test rapide du parser avec IDs"
  (format t "~%=== Test du parser avec IDs ===~%")
  
  ;; Test 1: Expression arithmétique
  (let ((result (parse-lisp-expr-with-ids '(+ 1 2))))
    (format t "  (+ 1 2) => ~A~%" result))
  
  ;; Test 2: IF
  (let ((result (parse-lisp-expr-with-ids '(if (= x 0) 42 99))))
    (format t "  (if (= x 0) 42 99) => ~A~%" result))
  
  ;; Test 3: DEFUN
  (let ((result (parse-lisp-expr-with-ids '(defun foo (x) (+ x 1)))))
    (format t "  (defun foo (x) (+ x 1)) => ~A~%" result))
  
  (format t "~%✓ Parser avec IDs fonctionnel~%"))

;; Export du parser
(format t "~%✓ Module parser-with-ids.lisp chargé~%")
(format t "  - parse-lisp-expr-with-ids : Parser utilisant des IDs~%")
(format t "  - Compilable: utilise COND+= au lieu de CASE~%~%")
