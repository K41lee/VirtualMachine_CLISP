;;;; lambda-list-parser.lisp
;;;; Parser pour les listes de paramètres Lisp (lambda lists)
;;;; 
;;;; OBJECTIF BOOTSTRAPPING :
;;;;   Permettre au compilateur de gérer les fonctions avec paramètres
;;;;   avancés (&optional, &rest, &key) utilisés dans compiler.lisp

;;; ═══════════════════════════════════════════════════════════════════
;;; STRUCTURE DES LAMBDA LISTS
;;; ═══════════════════════════════════════════════════════════════════
;;;
;;; FORMAT STANDARD :
;;;   (required... [&optional optional...] [&rest rest] [&key key...])
;;;
;;; EXEMPLE :
;;;   (a b &optional (c 10) &rest r &key (x 1) (y 2))
;;;
;;; SECTIONS (ORDRE STRICT) :
;;;   1. Required   : Paramètres obligatoires
;;;   2. &optional  : Paramètres optionnels avec valeurs par défaut
;;;   3. &rest      : Paramètre collectant les arguments restants
;;;   4. &key       : Paramètres nommés (keywords)
;;;
;;; FORMAT DE SORTIE DU PARSER :
;;;   Plist avec les clés :required, :optional, :rest, :key
;;;   (:required (a b) 
;;;    :optional ((c 10)) 
;;;    :rest r 
;;;    :key ((x 1) (y 2)))

;;; ═══════════════════════════════════════════════════════════════════
;;; FONCTION PRINCIPALE : PARSE-LAMBDA-LIST
;;; ═══════════════════════════════════════════════════════════════════

(defun parse-lambda-list (lambda-list)
  "Parse une lambda-list et retourne une structure analysée.
   
   PARAMÈTRES :
     lambda-list - Liste de paramètres (a b &optional c &rest r &key x)
   
   RETOURNE :
     Plist avec :required, :optional, :rest, :key
   
   EXEMPLES :
     (parse-lambda-list '(a b))
     → (:required (a b))
     
     (parse-lambda-list '(a &optional (b 10)))
     → (:required (a) :optional ((b 10)))
     
     (parse-lambda-list '(a &rest r))
     → (:required (a) :rest r)
     
     (parse-lambda-list '(a &key (x 1)))
     → (:required (a) :key ((x 1)))"
  
  (let ((required '())
        (optional '())
        (rest-param nil)
        (key '())
        (state :required))  ; États : :required, :optional, :rest, :key
    
    ;; Parcourir la lambda-list
    (dolist (item lambda-list)
      (cond
        ;; Changement d'état : &optional
        ((eq item '&optional)
         (when (not (eq state :required))
           (error "Lambda-list invalide : &optional mal placé"))
         (setf state :optional))
        
        ;; Changement d'état : &rest
        ((eq item '&rest)
         (when (not (or (eq state :required) (eq state :optional)))
           (error "Lambda-list invalide : &rest mal placé"))
         (setf state :rest))
        
        ;; Changement d'état : &key
        ((eq item '&key)
         (when (not (or (eq state :required) (eq state :optional) 
                       (eq state :rest) (eq state :after-rest)))
           (error "Lambda-list invalide : &key mal placé"))
         (setf state :key))
        
        ;; Traiter le paramètre selon l'état actuel
        (t
         (case state
           (:required
            (push item required))
           
           (:optional
            ;; Format : param ou (param default-value)
            (if (consp item)
                (push item optional)
                (push (list item nil) optional)))
           
           (:rest
            ;; &rest doit être suivi d'exactement un paramètre
            (when rest-param
              (error "Lambda-list invalide : plusieurs paramètres après &rest"))
            (setf rest-param item)
            ;; Après &rest param, on peut avoir &key
            (setf state :after-rest))
           
           (:after-rest
            ;; On ne devrait pas arriver ici sauf si &key suit
            (error "Lambda-list invalide : paramètre après &rest sans &key"))
           
           (:key
            ;; Format : param ou (param default-value)
            (if (consp item)
                (push item key)
                (push (list item nil) key)))))))
    
    ;; Inverser les listes (elles ont été construites à l'envers)
    (setf required (nreverse required))
    (setf optional (nreverse optional))
    (setf key (nreverse key))
    
    ;; Construire la plist de sortie
    (let ((result '()))
      (when required
        (setf result (append result (list :required required))))
      (when optional
        (setf result (append result (list :optional optional))))
      (when rest-param
        (setf result (append result (list :rest rest-param))))
      (when key
        (setf result (append result (list :key key))))
      result)))

;;; ═══════════════════════════════════════════════════════════════════
;;; FONCTIONS UTILITAIRES D'EXTRACTION
;;; ═══════════════════════════════════════════════════════════════════

(defun get-required-params (parsed-lambda-list)
  "Extrait les paramètres requis d'une lambda-list parsée."
  (getf parsed-lambda-list :required))

(defun get-optional-params (parsed-lambda-list)
  "Extrait les paramètres optionnels d'une lambda-list parsée."
  (getf parsed-lambda-list :optional))

(defun get-rest-param (parsed-lambda-list)
  "Extrait le paramètre &rest d'une lambda-list parsée."
  (getf parsed-lambda-list :rest))

(defun get-key-params (parsed-lambda-list)
  "Extrait les paramètres &key d'une lambda-list parsée."
  (getf parsed-lambda-list :key))

(defun has-optional-params-p (parsed-lambda-list)
  "Teste si la lambda-list a des paramètres &optional."
  (not (null (getf parsed-lambda-list :optional))))

(defun has-rest-param-p (parsed-lambda-list)
  "Teste si la lambda-list a un paramètre &rest."
  (not (null (getf parsed-lambda-list :rest))))

(defun has-key-params-p (parsed-lambda-list)
  "Teste si la lambda-list a des paramètres &key."
  (not (null (getf parsed-lambda-list :key))))

(defun has-advanced-params-p (parsed-lambda-list)
  "Teste si la lambda-list a des paramètres avancés (non-requis)."
  (or (has-optional-params-p parsed-lambda-list)
      (has-rest-param-p parsed-lambda-list)
      (has-key-params-p parsed-lambda-list)))

;;; ═══════════════════════════════════════════════════════════════════
;;; VALIDATION
;;; ═══════════════════════════════════════════════════════════════════

(defun validate-lambda-list (lambda-list)
  "Valide la syntaxe d'une lambda-list.
   Retourne T si valide, sinon signale une erreur."
  
  ;; Essayer de parser - cela fera la validation
  (parse-lambda-list lambda-list)
  t)

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DU PARSER
;;; ═══════════════════════════════════════════════════════════════════

(defun test-lambda-list-parser ()
  "Teste le parser de lambda-list."
  (format t "~%═══════════════════════════════════════════════════~%")
  (format t "TEST PARSER LAMBDA-LIST~%")
  (format t "═══════════════════════════════════════════════════~%~%")
  
  ;; Test 1 : Paramètres requis seulement
  (format t "TEST 1 : (a b c)~%")
  (let ((parsed (parse-lambda-list '(a b c))))
    (format t "  Parsed : ~S~%" parsed)
    (format t "  Required : ~S~%" (get-required-params parsed))
    (if (equal (get-required-params parsed) '(a b c))
        (format t "  ✓ TEST 1 RÉUSSI~%~%")
        (format t "  ✗ TEST 1 ÉCHOUÉ~%~%")))
  
  ;; Test 2 : &optional simple
  (format t "TEST 2 : (a &optional b)~%")
  (let ((parsed (parse-lambda-list '(a &optional b))))
    (format t "  Parsed : ~S~%" parsed)
    (format t "  Required : ~S~%" (get-required-params parsed))
    (format t "  Optional : ~S~%" (get-optional-params parsed))
    (if (and (equal (get-required-params parsed) '(a))
             (equal (get-optional-params parsed) '((b nil))))
        (format t "  ✓ TEST 2 RÉUSSI~%~%")
        (format t "  ✗ TEST 2 ÉCHOUÉ~%~%")))
  
  ;; Test 3 : &optional avec valeur par défaut
  (format t "TEST 3 : (a &optional (b 10) (c 20))~%")
  (let ((parsed (parse-lambda-list '(a &optional (b 10) (c 20)))))
    (format t "  Parsed : ~S~%" parsed)
    (format t "  Optional : ~S~%" (get-optional-params parsed))
    (if (equal (get-optional-params parsed) '((b 10) (c 20)))
        (format t "  ✓ TEST 3 RÉUSSI~%~%")
        (format t "  ✗ TEST 3 ÉCHOUÉ~%~%")))
  
  ;; Test 4 : &rest
  (format t "TEST 4 : (a b &rest r)~%")
  (let ((parsed (parse-lambda-list '(a b &rest r))))
    (format t "  Parsed : ~S~%" parsed)
    (format t "  Rest : ~S~%" (get-rest-param parsed))
    (if (equal (get-rest-param parsed) 'r)
        (format t "  ✓ TEST 4 RÉUSSI~%~%")
        (format t "  ✗ TEST 4 ÉCHOUÉ~%~%")))
  
  ;; Test 5 : &key
  (format t "TEST 5 : (a &key (x 1) (y 2))~%")
  (let ((parsed (parse-lambda-list '(a &key (x 1) (y 2)))))
    (format t "  Parsed : ~S~%" parsed)
    (format t "  Key : ~S~%" (get-key-params parsed))
    (if (equal (get-key-params parsed) '((x 1) (y 2)))
        (format t "  ✓ TEST 5 RÉUSSI~%~%")
        (format t "  ✗ TEST 5 ÉCHOUÉ~%~%")))
  
  ;; Test 6 : Combinaison complète
  (format t "TEST 6 : (a b &optional c &rest r &key x)~%")
  (let ((parsed (parse-lambda-list '(a b &optional c &rest r &key x))))
    (format t "  Parsed : ~S~%" parsed)
    (if (and (equal (get-required-params parsed) '(a b))
             (equal (get-optional-params parsed) '((c nil)))
             (equal (get-rest-param parsed) 'r)
             (equal (get-key-params parsed) '((x nil))))
        (format t "  ✓ TEST 6 RÉUSSI~%~%")
        (format t "  ✗ TEST 6 ÉCHOUÉ~%~%")))
  
  (format t "═══════════════════════════════════════════════════~%")
  (format t "TESTS TERMINÉS~%")
  (format t "═══════════════════════════════════════════════════~%~%"))

;;; ═══════════════════════════════════════════════════════════════════
;;; FIN DU FICHIER
;;; ═══════════════════════════════════════════════════════════════════

(format t "~%Parser lambda-list chargé.~%")
(format t "Fonctions disponibles :~%")
(format t "  - (parse-lambda-list lambda-list) : Parser une lambda-list~%")
(format t "  - (get-required-params parsed) : Extraire les requis~%")
(format t "  - (get-optional-params parsed) : Extraire les optionnels~%")
(format t "  - (get-rest-param parsed) : Extraire le &rest~%")
(format t "  - (get-key-params parsed) : Extraire les &key~%")
(format t "  - (test-lambda-list-parser) : Tester le parser~%")
(format t "~%")
