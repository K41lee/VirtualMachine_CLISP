;;;; lambda-list-expander.lisp
;;;; Expansion des lambda-lists avec paramètres avancés
;;;; 
;;;; OBJECTIF BOOTSTRAPPING :
;;;;   Transformer les paramètres avancés (&optional, &rest, &key)
;;;;   en code Lisp standard que le compilateur peut compiler

(load "src/lambda-list-parser.lisp")

;;; ═══════════════════════════════════════════════════════════════════
;;; STRATÉGIE D'EXPANSION
;;; ═══════════════════════════════════════════════════════════════════
;;;
;;; PRINCIPE :
;;;   Transformer une fonction avec paramètres avancés en une fonction
;;;   qui accepte &rest et fait la distribution manuelle des arguments
;;;
;;; EXEMPLE SIMPLE :
;;;   (defun f (a &optional (b 10)) body)
;;;   →
;;;   (defun f (a &rest __args__)
;;;     (let ((b (if (>= (length __args__) 1)
;;;                  (nth 0 __args__)
;;;                  10)))
;;;       body))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPANSION DES PARAMÈTRES &OPTIONAL
;;; ═══════════════════════════════════════════════════════════════════

(defun expand-optional-params (optional-params rest-var)
  "Génère les bindings LET pour les paramètres &optional.
   
   PARAMÈTRES :
     optional-params - Liste de paramètres optionnels ((param default) ...)
     rest-var - Nom de la variable &rest contenant les args (SYMBOLE, pas gensym)
   
   RETOURNE :
     Liste de bindings pour LET
   
   EXEMPLE :
     (expand-optional-params '((b 10) (c 20)) '__args__)
     →
     ((b (if (>= (length __args__) 1) (nth 0 __args__) 10))
      (c (if (>= (length __args__) 2) (nth 1 __args__) 20)))"
  
  (let ((bindings '())
        (index 0))
    (dolist (param-spec optional-params)
      (let* ((param-name (first param-spec))
             (default-value (second param-spec))
             (binding `(,param-name
                       (if (>= (length ,rest-var) ,(+ index 1))
                           (nth ,index ,rest-var)
                           ,default-value))))
        (push binding bindings)
        (incf index)))
    (nreverse bindings)))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPANSION DES PARAMÈTRES &KEY
;;; ═══════════════════════════════════════════════════════════════════

(defun expand-key-params (key-params args-var)
  "Génère les bindings LET pour les paramètres &key.
   
   PARAMÈTRES :
     key-params - Liste de paramètres keyword ((param default) ...)
     args-var - Nom de la variable contenant les args keyword
   
   RETOURNE :
     Liste de bindings pour LET
   
   EXEMPLE :
     (expand-key-params '((x 1) (y 2)) '__args__)
     →
     ((x (getf __args__ :x 1))
      (y (getf __args__ :y 2)))"
  
  (let ((bindings '()))
    (dolist (param-spec key-params)
      (let* ((param-name (first param-spec))
             (default-value (second param-spec))
             (keyword (intern (symbol-name param-name) :keyword))
             (binding `(,param-name (getf ,args-var ,keyword ,default-value))))
        (push binding bindings)))
    (nreverse bindings)))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPANSION COMPLÈTE D'UNE LAMBDA-LIST
;;; ═══════════════════════════════════════════════════════════════════

(defun expand-lambda-list-body (parsed-lambda-list body rest-var)
  "Expanse une lambda-list avec paramètres avancés.
   
   PARAMÈTRES :
     parsed-lambda-list - Lambda-list parsée (plist)
     body - Corps de la fonction
     rest-var - Symbole à utiliser pour &rest (déjà créé)
   
   RETOURNE :
     Nouveau corps avec bindings pour paramètres avancés
   
   STRATÉGIE :
     1. Si paramètres avancés → utiliser rest-var fourni
     2. Générer bindings pour &optional
     3. Gérer &rest (juste un rename)
     4. Générer bindings pour &key
     5. Wrapper le body dans un LET* avec tous les bindings"
  
  (let* ((required (get-required-params parsed-lambda-list))
         (optional (get-optional-params parsed-lambda-list))
         (rest-param (get-rest-param parsed-lambda-list))
         (key-params (get-key-params parsed-lambda-list))
         (has-advanced (has-advanced-params-p parsed-lambda-list)))
    
    (if (not has-advanced)
        ;; Pas de paramètres avancés → retourner le body tel quel
        body
        
        ;; Avec paramètres avancés → générer l'expansion
        (let ((bindings '()))
          
          ;; CAS 1 : &optional seulement (pas de &key)
          (when (and optional (not key-params))
            (setf bindings (expand-optional-params optional rest-var))
            ;; Si on a aussi &rest, l'ajouter après les optionnels
            (when rest-param
              (let* ((num-optional (length optional))
                     (rest-binding `(,rest-param (nthcdr ,num-optional ,rest-var))))
                (setf bindings (append bindings (list rest-binding))))))
          
          ;; CAS 2 : &key (avec ou sans &optional)
          (when key-params
            (let ((key-args-var rest-var))
              ;; Si on a &optional, il faut calculer où commencent les keywords
              (when optional
                ;; Trouver le premier keyword dans les args
                ;; Les optionnels viennent avant les keywords
                (let ((key-start-var (gensym "__KEY-START__")))
                  ;; Compter combien d'optionnels sont fournis
                  ;; (en s'arrêtant au premier keyword)
                  (push `(,key-start-var
                         ,(generate-find-key-start rest-var (length optional)))
                        bindings)
                  ;; Générer bindings pour optionnels (jusqu'au keyword)
                  (let ((opt-bindings (expand-optional-params-before-keys 
                                       optional rest-var key-start-var)))
                    (setf bindings (append bindings opt-bindings)))
                  ;; Les args key commencent après les optionnels
                  (setf key-args-var `(nthcdr ,key-start-var ,rest-var))))
              
              ;; Bindings pour les keywords
              (let ((key-bindings (expand-key-params key-params key-args-var)))
                (setf bindings (append bindings key-bindings)))
              
              ;; Si on a &rest avec &key, il faut combiner optionnels + keys
              (when rest-param
                (let ((rest-binding `(,rest-param ,rest-var)))
                  (setf bindings (append bindings (list rest-binding)))))))
          
          ;; CAS 3 : &rest seulement (pas de &optional ni &key)
          (when (and rest-param (not optional) (not key-params))
            (push `(,rest-param ,rest-var) bindings))
          
          ;; Retourner le body wrappé dans LET*
          `((let* ,bindings
              ,@body))))))

;;; Fonction auxiliaire pour CAS 2 avec &optional avant &key
(defun generate-find-key-start (rest-var num-optional)
  "Génère du code pour trouver où commencent les keywords.
   Compte combien d'arguments non-keyword sont fournis (max = num-optional)."
  `(let ((count 0))
     (dolist (arg ,rest-var count)
       (if (and (< count ,num-optional)
                (not (keywordp arg)))
           (incf count)
           (return count)))))

(defun expand-optional-params-before-keys (optional-params rest-var key-start-var)
  "Génère bindings pour optionnels qui peuvent être suivis de keywords."
  (let ((bindings '())
        (index 0))
    (dolist (param-spec optional-params)
      (let* ((param-name (first param-spec))
             (default-value (second param-spec))
             (binding `(,param-name
                       (if (> ,key-start-var ,index)
                           (nth ,index ,rest-var)
                           ,default-value))))
        (push binding bindings)
        (incf index)))
    (nreverse bindings)))

;;; ═══════════════════════════════════════════════════════════════════
;;; INTERFACE PRINCIPALE
;;; ═══════════════════════════════════════════════════════════════════

(defun expand-function-with-advanced-params (name lambda-list body)
  "Expanse une fonction avec paramètres avancés.
   
   PARAMÈTRES :
     name - Nom de la fonction
     lambda-list - Liste de paramètres (peut contenir &optional, &rest, &key)
     body - Corps de la fonction
   
   RETOURNE :
     Forme DEFUN expansée
   
   EXEMPLE :
     (expand-function-with-advanced-params 
       'foo 
       '(a &optional (b 10))
       '((+ a b)))
     →
     (defun foo (a &rest __rest-args__)
       (let* ((b (if (>= (length __rest-args__) 1) 
                     (nth 0 __rest-args__) 
                     10)))
         (+ a b)))"
  
  ;; Parser la lambda-list
  (let* ((parsed (parse-lambda-list lambda-list))
         (required (get-required-params parsed))
         (has-advanced (has-advanced-params-p parsed)))
    
    (if (not has-advanced)
        ;; Pas de paramètres avancés → retourner DEFUN normale
        `(defun ,name ,lambda-list ,@body)
        
        ;; Avec paramètres avancés → générer l'expansion
        (let* ((rest-var (gensym "__REST-ARGS__"))
               (new-lambda-list `(,@required &rest ,rest-var))
               (new-body (expand-lambda-list-body parsed body rest-var)))
          `(defun ,name ,new-lambda-list ,@new-body)))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DE L'EXPANDER
;;; ═══════════════════════════════════════════════════════════════════

(defun test-lambda-list-expander ()
  "Teste l'expander de lambda-list."
  (format t "~%═══════════════════════════════════════════════════~%")
  (format t "TEST EXPANDER LAMBDA-LIST~%")
  (format t "═══════════════════════════════════════════════════~%~%")
  
  ;; Test 1 : &optional simple
  (format t "TEST 1 : Expansion &optional~%")
  (format t "  Entrée : (defun f (a &optional (b 10)) (+ a b))~%")
  (let ((expanded (expand-function-with-advanced-params 
                   'f 
                   '(a &optional (b 10))
                   '((+ a b)))))
    (format t "  Expansé : ~S~%" expanded)
    (format t "  ✓ TEST 1 TERMINÉ~%~%"))
  
  ;; Test 2 : &key simple
  (format t "TEST 2 : Expansion &key~%")
  (format t "  Entrée : (defun f (a &key (x 1)) (+ a x))~%")
  (let ((expanded (expand-function-with-advanced-params 
                   'f 
                   '(a &key (x 1))
                   '((+ a x)))))
    (format t "  Expansé : ~S~%" expanded)
    (format t "  ✓ TEST 2 TERMINÉ~%~%"))
  
  ;; Test 3 : &rest simple
  (format t "TEST 3 : Expansion &rest~%")
  (format t "  Entrée : (defun f (a &rest r) (cons a r))~%")
  (let ((expanded (expand-function-with-advanced-params 
                   'f 
                   '(a &rest r)
                   '((cons a r)))))
    (format t "  Expansé : ~S~%" expanded)
    (format t "  ✓ TEST 3 TERMINÉ~%~%"))
  
  (format t "═══════════════════════════════════════════════════~%")
  (format t "TESTS TERMINÉS~%")
  (format t "═══════════════════════════════════════════════════~%~%"))

;;; ═══════════════════════════════════════════════════════════════════
;;; FIN DU FICHIER
;;; ═══════════════════════════════════════════════════════════════════

(format t "~%Expander lambda-list chargé.~%")
(format t "Fonctions disponibles :~%")
(format t "  - (expand-function-with-advanced-params name params body)~%")
(format t "  - (test-lambda-list-expander) : Tester l'expander~%")
(format t "~%")
