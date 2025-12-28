;;; ═══════════════════════════════════════════════════════════════════
;;; PARSER LOOP AVANCÉ
;;; ═══════════════════════════════════════════════════════════════════
;;;
;;; Ce fichier implémente le parsing des constructions LOOP avancées.
;;; Il analyse la syntaxe LOOP et retourne une structure de données
;;; que le compilateur peut ensuite expanser et compiler.
;;;
;;; PATTERNS SUPPORTÉS :
;;;   (loop for var from start to end do body)
;;;   (loop for var from start below end do body)
;;;   (loop for var in list do body)
;;;   (loop for var in list collect expr)
;;;   (loop for var from start to end collect expr)
;;;   (loop for var1 in list1 for var2 in list2 do body)
;;;   (loop while condition do body) -- déjà supporté
;;;
;;; ═══════════════════════════════════════════════════════════════════

(defun parse-loop-clause (tokens)
  "Parse une clause FOR d'un LOOP.
   
   PARAMÈTRES :
     tokens - Liste de tokens commençant par FOR
   
   RETOURNE :
     (clause remaining-tokens)
     où clause est (:for var :from/:in/:below/:to start/list)
     et remaining-tokens sont les tokens restants."
  (unless (and tokens (eq (first tokens) 'for))
    (error "Clause LOOP doit commencer par FOR: ~A" tokens))
  
  (let ((var (second tokens))
        (keyword (third tokens))
        (rest-tokens (cdddr tokens)))
    
    (case keyword
      ;; FOR var FROM start TO end
      ((from)
       (let ((start (first rest-tokens))
             (to-or-below (second rest-tokens)))
         (cond
           ;; FOR var FROM start TO end
           ((eq to-or-below 'to)
            (let ((end (third rest-tokens)))
              (values (list :for var :from start :to end)
                      (cdddr rest-tokens))))
           
           ;; FOR var FROM start BELOW end
           ((eq to-or-below 'below)
            (let ((end (third rest-tokens)))
              (values (list :for var :from start :below end)
                      (cdddr rest-tokens))))
           
           (t
            (error "FOR FROM doit être suivi de TO ou BELOW: ~A" tokens)))))
      
      ;; FOR var IN list
      ((in)
       (let ((list-expr (first rest-tokens)))
         (values (list :for var :in list-expr)
                 (cdr rest-tokens))))
      
      ;; FOR var = expr (assignation)
      ((=)
       (let ((expr (first rest-tokens)))
         (values (list :for var := expr)
                 (cdr rest-tokens))))
      
      (t
       (error "Mot-clé LOOP non supporté: ~A" keyword)))))

(defun parse-loop-clauses (tokens)
  "Parse toutes les clauses FOR consécutives.
   
   PARAMÈTRES :
     tokens - Liste de tokens
   
   RETOURNE :
     (clauses remaining-tokens)
     où clauses est une liste de clauses FOR."
  (let ((clauses nil)
        (current-tokens tokens))
    
    ;; Collecter toutes les clauses FOR consécutives
    (loop while (and current-tokens (eq (first current-tokens) 'for))
          do (multiple-value-bind (clause rest)
                 (parse-loop-clause current-tokens)
               (push clause clauses)
               (setf current-tokens rest)))
    
    (values (reverse clauses) current-tokens)))

(defun parse-loop-action (tokens)
  "Parse l'action d'un LOOP (DO, COLLECT, APPEND, etc).
   
   PARAMÈTRES :
     tokens - Liste de tokens après les clauses FOR
   
   RETOURNE :
     (action body)
     où action est :do, :collect, :append, etc
     et body est la liste d'expressions."
  (unless tokens
    (error "LOOP nécessite une action (DO, COLLECT, etc)"))
  
  (let ((action-keyword (first tokens)))
    (case action-keyword
      ((do)
       (values :do (rest tokens)))
      
      ((collect)
       (values :collect (list (second tokens))))
      
      ((append)
       (values :append (list (second tokens))))
      
      ((when)
       ;; (loop ... when condition do/collect ...)
       (let ((condition (second tokens))
             (sub-action (third tokens))
             (sub-body (cdddr tokens)))
         (values :when (list condition sub-action sub-body))))
      
      (t
       (error "Action LOOP non supportée: ~A" action-keyword)))))

(defun parse-loop-advanced (args)
  "Parse une construction LOOP avancée.
   
   PARAMÈTRES :
     args - Liste de tokens du LOOP (sans le mot LOOP lui-même)
   
   RETOURNE :
     Structure (:loop-advanced :clauses ... :action ... :body ...)
     ou :loop-while pour le cas simple."
  
  ;; Cas spécial : (loop while condition do body) -- déjà supporté
  (when (and (>= (length args) 4)
             (eq (first args) 'while)
             (eq (third args) 'do))
    (return-from parse-loop-advanced
      (list :loop-while (second args) (cdddr args))))
  
  ;; Cas général : FOR clauses
  (unless (eq (first args) 'for)
    (error "LOOP doit commencer par FOR ou WHILE: ~A" args))
  
  ;; Parser les clauses FOR
  (multiple-value-bind (clauses remaining-tokens)
      (parse-loop-clauses args)
    
    ;; Parser l'action
    (multiple-value-bind (action body)
        (parse-loop-action remaining-tokens)
      
      ;; Retourner la structure parsée
      ;; Format: (:loop-advanced . (:clauses ... :action ... :body ...))
      (cons :loop-advanced
            (list :clauses clauses
                  :action action
                  :body body)))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DU PARSER
;;; ═══════════════════════════════════════════════════════════════════

(defun test-loop-parser ()
  "Teste le parser LOOP."
  (format t "~%═══════════════════════════════════════════════════~%")
  (format t "TEST PARSER LOOP~%")
  (format t "═══════════════════════════════════════════════════~%~%")
  
  ;; Test 1 : FOR FROM TO DO
  (format t "TEST 1 : (loop for i from 1 to 5 do (print i))~%")
  (let ((parsed (parse-loop-advanced '(for i from 1 to 5 do (print i)))))
    (format t "  Résultat : ~A~%" parsed)
    (format t "~%"))
  
  ;; Test 2 : FOR FROM BELOW DO
  (format t "TEST 2 : (loop for i from 0 below 5 do (print i))~%")
  (let ((parsed (parse-loop-advanced '(for i from 0 below 5 do (print i)))))
    (format t "  Résultat : ~A~%" parsed)
    (format t "~%"))
  
  ;; Test 3 : FOR IN DO
  (format t "TEST 3 : (loop for x in list do (print x))~%")
  (let ((parsed (parse-loop-advanced '(for x in list do (print x)))))
    (format t "  Résultat : ~A~%" parsed)
    (format t "~%"))
  
  ;; Test 4 : FOR IN COLLECT
  (format t "TEST 4 : (loop for x in list collect (* x 2))~%")
  (let ((parsed (parse-loop-advanced '(for x in list collect (* x 2)))))
    (format t "  Résultat : ~A~%" parsed)
    (format t "~%"))
  
  ;; Test 5 : Deux FOR en parallèle
  (format t "TEST 5 : (loop for x in list1 for y in list2 do (print (+ x y)))~%")
  (let ((parsed (parse-loop-advanced '(for x in list1 for y in list2 do (print (+ x y))))))
    (format t "  Résultat : ~A~%" parsed)
    (format t "~%"))
  
  ;; Test 6 : WHILE (cas déjà supporté)
  (format t "TEST 6 : (loop while condition do body)~%")
  (let ((parsed (parse-loop-advanced '(while condition do body))))
    (format t "  Résultat : ~A~%" parsed)
    (format t "~%"))
  
  (format t "═══════════════════════════════════════════════════~%")
  (format t "TOUS LES TESTS AFFICHÉS~%")
  (format t "═══════════════════════════════════════════════════~%~%"))

;;; ═══════════════════════════════════════════════════════════════════
;;; FIN DU FICHIER
;;; ═══════════════════════════════════════════════════════════════════

(format t "~%Parser LOOP avancé chargé.~%")
(format t "Fonctions disponibles :~%")
(format t "  - (parse-loop-advanced args) : Parser un LOOP~%")
(format t "  - (test-loop-parser) : Tester le parser~%")
(format t "~%")
