;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPATCHER AVEC IDs - ÉTAPE 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Ce fichier implémente un dispatcher qui utilise des comparaisons
;; d'IDs numériques au lieu de CASE avec keywords, permettant ainsi
;; l'auto-compilation complète.
;;
;; Différences avec compile-expr traditionnel:
;; - Utilise COND + = au lieu de CASE (compilable en MIPS)
;; - Compare des IDs numériques au lieu de keywords
;; - Compatible avec parse-lisp-expr-with-ids
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITAIRES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keyword-to-symbol-name (keyword)
  "Convertit un keyword (:constant) en nom de symbole (\"CONSTANT\").
   
   Args:
     keyword : keyword Lisp (ex: :constant, :if, :arithmetic)
   
   Returns:
     string : nom en majuscules (ex: \"CONSTANT\", \"IF\", \"ARITHMETIC\")"
  (if (keywordp keyword)
      (symbol-name keyword)
      (if (symbolp keyword)
          (symbol-name keyword)
          (error "keyword-to-symbol-name: argument invalide ~A" keyword))))

(defun get-parsed-type-id (parsed-expr)
  "Extrait l'ID du type de nœud d'une expression parsée.
   
   Args:
     parsed-expr : expression parsée (ex: (:constant 42))
   
   Returns:
     ID numérique du type ou NIL si introuvable
   
   Exemples:
     (:constant 42) → *constant-type-id* (ex: 10089)
     (:if ...) → *if-type-id* (ex: 10097)
     (:arithmetic + (1 2)) → *arithmetic-type-id* (ex: 10091)"
  
  (let ((type-keyword (first parsed-expr)))
    (if (keywordp type-keyword)
        (get-symbol-id (keyword-to-symbol-name type-keyword))
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISPATCHER PRINCIPAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-expr-with-ids (expr env)
  "Compile une expression LISP en code ASM en utilisant des IDs.
   Version compilable: utilise COND+= au lieu de CASE.
   
   Args:
     expr : expression LISP à compiler
     env : environnement de compilation
   
   Returns:
     Liste d'instructions ASM
   
   Différences avec compile-expr:
     - COND au lieu de CASE (compilable MIPS)
     - Comparaison d'IDs au lieu de keywords
     - Nécessite symbol-table.lisp chargé"
  
  (let* ((parsed (parse-lisp-expr expr))
         (type-id (get-parsed-type-id parsed)))
    
    (cond
      ;; ========== VALEURS ==========
      
      ;; :constant
      ((and type-id (= type-id *constant-type-id*))
       (compile-constant (second parsed) env))
      
      ;; :variable
      ((and type-id (= type-id *variable-type-id*))
       (compile-variable (second parsed) env))
      
      ;; ========== OPÉRATIONS ARITHMÉTIQUES ==========
      
      ;; :arithmetic (+ - * / mod)
      ((and type-id (= type-id *arithmetic-type-id*))
       (compile-arithmetic (second parsed) (third parsed) env))
      
      ;; :math-func (abs max min)
      ((and type-id (= type-id *math-func-type-id*))
       (compile-math-func (second parsed) (third parsed) env))
      
      ;; ========== COMPARAISONS ==========
      
      ;; :comparison (< > <= >= = /=)
      ((and type-id (= type-id *comparison-type-id*))
       (compile-comparison (second parsed) (third parsed) env))
      
      ;; ========== STRUCTURES DE CONTRÔLE ==========
      
      ;; :if
      ((and type-id (= type-id *if-type-id*))
       (compile-if (second parsed) (third parsed) (fourth parsed) env))
      
      ;; :cond
      ((and type-id (= type-id *cond-type-id*))
       (compile-cond (second parsed) env))
      
      ;; :when
      ((and type-id (= type-id *when-type-id*))
       (compile-when (second parsed) (third parsed) env))
      
      ;; :unless
      ((and type-id (= type-id *unless-type-id*))
       (compile-unless (second parsed) (third parsed) env))
      
      ;; :case
      ((and type-id (= type-id *case-type-id*))
       (compile-case (second parsed) (third parsed) env))
      
      ;; ========== OPÉRATEURS LOGIQUES ==========
      
      ;; :not
      ((and type-id (= type-id *not-type-id*))
       (compile-not (second parsed) env))
      
      ;; :and
      ((and type-id (= type-id *and-type-id*))
       (compile-and (second parsed) env))
      
      ;; :or
      ((and type-id (= type-id *or-type-id*))
       (compile-or (second parsed) env))
      
      ;; ========== VARIABLES ET BINDINGS ==========
      
      ;; :let
      ((and type-id (= type-id *let-type-id*))
       (compile-let (second parsed) (third parsed) env))
      
      ;; :let*
      ((and type-id (= type-id *let*-type-id*))
       (compile-let* (second parsed) (third parsed) env))
      
      ;; :setq
      ((and type-id (= type-id *setq-type-id*))
       (compile-setq (second parsed) (third parsed) env))
      
      ;; ========== BOUCLES ==========
      
      ;; :loop-while
      ((and type-id (= type-id *loop-while-type-id*))
       (compile-loop-while (second parsed) (third parsed) env))
      
      ;; :loop-advanced
      ((and type-id (= type-id *loop-advanced-type-id*))
       (compile-loop-advanced parsed env))
      
      ;; :while
      ((and type-id (= type-id *while-type-id*))
       (compile-while (second parsed) (third parsed) env))
      
      ;; :dotimes
      ((and type-id (= type-id *dotimes-type-id*))
       (compile-dotimes (second parsed) (third parsed) env))
      
      ;; :dolist
      ((and type-id (= type-id *dolist-type-id*))
       (compile-dolist (second parsed) (third parsed) (fourth parsed) env))
      
      ;; :progn
      ((and type-id (= type-id *progn-type-id*))
       (compile-progn (second parsed) env))
      
      ;; ========== TABLEAUX ==========
      
      ;; :make-array
      ((and type-id (= type-id *make-array-type-id*))
       (compile-make-array (second parsed) env))
      
      ;; :aref
      ((and type-id (= type-id *aref-type-id*))
       (compile-aref (second parsed) (third parsed) env))
      
      ;; :setf-aref
      ((and type-id (= type-id *setf-aref-type-id*))
       (compile-setf-aref (second parsed) (third parsed) (fourth parsed) env))
      
      ;; ========== INCRÉMENTATION / DÉCRÉMENTATION ==========
      
      ;; :incf
      ((and type-id (= type-id *incf-type-id*))
       (compile-incf (second parsed) (third parsed) env))
      
      ;; :decf
      ((and type-id (= type-id *decf-type-id*))
       (compile-decf (second parsed) (third parsed) env))
      
      ;; ========== LISTES ==========
      
      ;; :cons
      ((and type-id (= type-id *cons-type-id*))
       (compile-cons (second parsed) (third parsed) env))
      
      ;; :car
      ((and type-id (= type-id *car-type-id*))
       (compile-car (second parsed) env))
      
      ;; :cdr
      ((and type-id (= type-id *cdr-type-id*))
       (compile-cdr (second parsed) env))
      
      ;; :null
      ((and type-id (= type-id *null-type-id*))
       (compile-null (second parsed) env))
      
      ;; :length
      ((and type-id (= type-id *length-type-id*))
       (compile-length (second parsed) env))
      
      ;; :nth
      ((and type-id (= type-id *nth-type-id*))
       (compile-nth (second parsed) (third parsed) env))
      
      ;; :assoc
      ((and type-id (= type-id *assoc-type-id*))
       (compile-assoc (second parsed) (third parsed) env))
      
      ;; :member
      ((and type-id (= type-id *member-type-id*))
       (compile-member (second parsed) (third parsed) env))
      
      ;; :append
      ((and type-id (= type-id *append-type-id*))
       (compile-append (second parsed) (third parsed) env))
      
      ;; ========== PRIMITIVES VM ==========
      
      ;; :vm-primitive
      ((and type-id (= type-id *vm-primitive-type-id*))
       (compile-vm-primitive (second parsed) (third parsed) env))
      
      ;; ========== FONCTIONS ==========
      
      ;; :labels
      ((and type-id (= type-id *labels-type-id*))
       (compile-labels (second parsed) (third parsed) env))
      
      ;; :lambda
      ((and type-id (= type-id *lambda-type-id*))
       (compile-lambda (second parsed) (cddr parsed) env))
      
      ;; :call
      ((and type-id (= type-id *call-type-id*))
       (compile-call (second parsed) (third parsed) env))
      
      ;; :funcall
      ((and type-id (= type-id *funcall-type-id*))
       (compile-call (second parsed) (third parsed) env))
      
      ;; ========== DÉFINITIONS ==========
      
      ;; :defconstant
      ((and type-id (= type-id *defconstant-type-id*))
       (compile-defconstant (second parsed) (third parsed) env))
      
      ;; :defstruct
      ((and type-id (= type-id *defstruct-type-id*))
       (compile-defstruct (second parsed) (third parsed) env))
      
      ;; :defvar
      ((and type-id (= type-id *defvar-type-id*))
       (compile-defvar (second parsed) (third parsed) env))
      
      ;; :defun
      ((and type-id (= type-id *defun-type-id*))
       (compile-defun (second parsed)   ; name
                     (third parsed)    ; params (original)
                     (fourth parsed)   ; body
                     (fifth parsed)    ; parsed-params (plist)
                     env))
      
      ;; ========== GESTION D'ERREURS ==========
      
      ;; :error
      ((and type-id (= type-id *error-type-id*))
       (list (list :HALT)))
      
      ;; :format
      ((and type-id (= type-id *format-type-id*))
       '())  ; FORMAT ignoré en compilation
      
      ;; ========== FALLBACK ==========
      
      (t 
       (error "Type d'expression non supporté: ~A (ID: ~A)" 
              (first parsed) type-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS RAPIDES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-dispatcher-with-ids ()
  "Test rapide du dispatcher avec IDs"
  (format t "~%=== Test du dispatcher avec IDs ===~%")
  
  ;; Test 1: Constante
  (format t "  Test :constant~%")
  (let* ((parsed '(:constant 42))
         (type-id (get-parsed-type-id parsed)))
    (format t "    Parsed: ~A~%" parsed)
    (format t "    Type ID: ~A~%" type-id)
    (format t "    *constant-type-id*: ~A~%" *constant-type-id*)
    (format t "    Match: ~A~%" (= type-id *constant-type-id*)))
  
  ;; Test 2: Arithmétique
  (format t "~%  Test :arithmetic~%")
  (let* ((parsed '(:arithmetic + (1 2)))
         (type-id (get-parsed-type-id parsed)))
    (format t "    Parsed: ~A~%" parsed)
    (format t "    Type ID: ~A~%" type-id)
    (format t "    *arithmetic-type-id*: ~A~%" *arithmetic-type-id*)
    (format t "    Match: ~A~%" (= type-id *arithmetic-type-id*)))
  
  ;; Test 3: IF
  (format t "~%  Test :if~%")
  (let* ((parsed '(:if test then else))
         (type-id (get-parsed-type-id parsed)))
    (format t "    Parsed: ~A~%" parsed)
    (format t "    Type ID: ~A~%" type-id)
    (format t "    *if-type-id*: ~A~%" *if-type-id*)
    (format t "    Match: ~A~%" (= type-id *if-type-id*)))
  
  (format t "~%✓ Dispatcher avec IDs fonctionnel~%"))

;; Export du dispatcher
(format t "~%✓ Module dispatcher-with-ids.lisp chargé~%")
(format t "  - compile-expr-with-ids : Dispatcher utilisant des IDs~%")
(format t "  - get-parsed-type-id : Extrait l'ID du type de nœud~%")
(format t "  - Compilable: utilise COND+= au lieu de CASE~%~%")
