;;; ═══════════════════════════════════════════════════════════════════
;;; INTÉGRATION LOOP : Parser + Expander
;;; ═══════════════════════════════════════════════════════════════════
;;;
;;; Ce fichier connecte le parser LOOP avec les expansions.
;;; Il prend une structure LOOP parsée et génère le code expansé.
;;;
;;; ═══════════════════════════════════════════════════════════════════

(load "src/loop-parser.lisp")
(load "src/loop-expander.lisp")

(defun expand-loop-from-parsed (parsed)
  "Génère le code expansé à partir d'une structure LOOP parsée.
   
   PARAMÈTRES :
     parsed - Structure retournée par parse-loop-advanced
   
   RETOURNE :
     Code Lisp expansé (LET, WHILE, SETQ, etc)."
  
  ;; Cas 1 : LOOP WHILE (déjà supporté par le compilateur)
  (when (eq (first parsed) :loop-while)
    ;; Retourner tel quel, le compilateur sait déjà le gérer
    (return-from expand-loop-from-parsed parsed))
  
  ;; Cas 2 : LOOP ADVANCED (FOR clauses)
  (unless (eq (first parsed) :loop-advanced)
    (error "Type de LOOP inconnu: ~A" (first parsed)))
  
  (let* ((clauses (getf (rest parsed) :clauses))
         (action (getf (rest parsed) :action))
         (body (getf (rest parsed) :body))
         (num-clauses (length clauses)))
    
    ;; Cas avec une seule clause FOR
    (when (= num-clauses 1)
      (let* ((clause (first clauses))
             ;; clause format: (:FOR var :FROM start :TO end)
             (var (second (member :for clause)))
             (from (second (member :from clause)))
             (to (second (member :to clause)))
             (below (second (member :below clause)))
             (in (second (member :in clause))))
        
        ;; FOR var FROM start TO end
        (when (and from to)
          (case action
            ((:do)
             (return-from expand-loop-from-parsed
               (expand-loop-for-from-to var from to body)))
            ((:collect)
             (return-from expand-loop-from-parsed
               (expand-loop-for-from-to-collect var from to (first body))))
            (t
             (error "Action non supportée avec FROM/TO: ~A" action))))
        
        ;; FOR var FROM start BELOW end
        (when (and from below)
          (case action
            ((:do)
             (return-from expand-loop-from-parsed
               (expand-loop-for-from-below var from below body)))
            (t
             (error "Action non supportée avec FROM/BELOW: ~A" action))))
        
        ;; FOR var IN list
        (when in
          (case action
            ((:do)
             (return-from expand-loop-from-parsed
               (expand-loop-for-in var in body)))
            ((:collect)
             (return-from expand-loop-from-parsed
               (expand-loop-for-in-collect var in (first body))))
            (t
             (error "Action non supportée avec IN: ~A" action))))))
    
    ;; Cas avec deux clauses FOR (parallèle)
    (when (= num-clauses 2)
      (let* ((clause1 (first clauses))
             (clause2 (second clauses))
             (var1 (second (member :for clause1)))
             (var2 (second (member :for clause2)))
             (in1 (second (member :in clause1)))
             (in2 (second (member :in clause2))))
        
        ;; FOR var1 IN list1 FOR var2 IN list2
        (when (and in1 in2)
          (case action
            ((:do)
             (return-from expand-loop-from-parsed
               (expand-loop-for-in-parallel var1 in1 var2 in2 body)))
            (t
             (error "Action non supportée avec deux FOR IN: ~A" action))))))
    
    ;; Cas plus complexe : 3+ clauses ou combinaisons FROM+IN
    ;; Pour l'instant, générer une erreur
    (error "LOOP avec ~A clauses non supporté pour l'instant: ~A" 
           num-clauses parsed)))

;;; ═══════════════════════════════════════════════════════════════════
;;; FONCTION PRINCIPALE : Parse + Expand
;;; ═══════════════════════════════════════════════════════════════════

(defun parse-and-expand-loop (args)
  "Parse et expanse un LOOP en une seule étape.
   
   PARAMÈTRES :
     args - Liste de tokens du LOOP (sans le mot LOOP)
   
   RETOURNE :
     Code Lisp expansé."
  (let ((parsed (parse-loop-advanced args)))
    (expand-loop-from-parsed parsed)))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS
;;; ═══════════════════════════════════════════════════════════════════

(defun test-loop-integration ()
  "Teste l'intégration parser + expander."
  (format t "~%═══════════════════════════════════════════════════~%")
  (format t "TEST INTÉGRATION LOOP (Parser + Expander)~%")
  (format t "═══════════════════════════════════════════════════~%~%")
  
  ;; Test 1
  (format t "TEST 1 : (loop for i from 1 to 5 do (print i))~%")
  (let ((expanded (parse-and-expand-loop '(for i from 1 to 5 do (print i)))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 2
  (format t "TEST 2 : (loop for x in '(1 2 3) collect (* x 2))~%")
  (let ((expanded (parse-and-expand-loop '(for x in (quote (1 2 3)) collect (* x 2)))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 3
  (format t "TEST 3 : (loop for x in list1 for y in list2 do (print (+ x y)))~%")
  (let ((expanded (parse-and-expand-loop '(for x in list1 for y in list2 do (print (+ x y))))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 4 : Exécution
  (format t "TEST 4 : Exécution de (loop for i from 1 to 3 do ...)~%")
  (defmacro while (condition &body body)
    `(loop while ,condition do ,@body))
  (defparameter *result* nil)
  (eval (parse-and-expand-loop '(for i from 1 to 3 do (setq *result* (cons i *result*)))))
  (setq *result* (reverse *result*))
  (format t "  Résultat : ~A~%" *result*)
  (format t "  Attendu  : (1 2 3)~%")
  (if (equal *result* '(1 2 3))
      (format t "  ✓ TEST 4 RÉUSSI~%~%")
      (format t "  ✗ TEST 4 ÉCHOUÉ~%~%"))
  
  (format t "═══════════════════════════════════════════════════~%")
  (format t "INTÉGRATION FONCTIONNELLE~%")
  (format t "═══════════════════════════════════════════════════~%~%"))

;;; ═══════════════════════════════════════════════════════════════════
;;; FIN DU FICHIER
;;; ═══════════════════════════════════════════════════════════════════

(format t "~%Intégration LOOP chargée.~%")
(format t "Fonction principale :~%")
(format t "  - (parse-and-expand-loop args) : Parse et expanse en une étape~%")
(format t "  - (test-loop-integration) : Tester l'intégration~%")
(format t "~%")
