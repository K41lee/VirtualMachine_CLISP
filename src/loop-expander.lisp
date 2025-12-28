;;; ═══════════════════════════════════════════════════════════════════
;;; EXPANSION LOOP POUR BOOTSTRAPPING
;;; ═══════════════════════════════════════════════════════════════════
;;;
;;; Ce fichier implémente l'expansion des constructions LOOP avancées
;;; en code Lisp simple (LET, WHILE, SETQ, PROGN, etc).
;;;
;;; APPROCHE :
;;;   Au lieu de compiler LOOP directement en MIPS, on le transforme
;;;   en constructions déjà supportées. Le code expansé est ensuite
;;;   compilé normalement.
;;;
;;; PATTERNS SUPPORTÉS :
;;;   1. (loop for var from start to end do body)
;;;   2. (loop for var from start below end do body)
;;;   3. (loop for var in list do body)
;;;   4. (loop for var in list collect expr)
;;;   5. Clauses FOR multiples en parallèle
;;;
;;; ═══════════════════════════════════════════════════════════════════

;;; ───────────────────────────────────────────────────────────────────
;;; GENSYM POUR VARIABLES TEMPORAIRES
;;; ───────────────────────────────────────────────────────────────────

(defparameter *compiler-gensym-counter* 0
  "Compteur pour générer des symboles uniques.")

(defun compiler-gensym (&optional (prefix "G"))
  "Génère un symbole unique pour éviter les conflits de noms.
   
   PARAMÈTRES :
     prefix - Préfixe du symbole (défaut: 'G')
   
   RETOURNE :
     Un symbole unique (ex: G1, LIST2, RESULT3)
   
   EXEMPLES :
     (compiler-gensym)          → G1
     (compiler-gensym \"LIST\")   → LIST2
     (compiler-gensym \"RESULT\") → RESULT3"
  (incf *compiler-gensym-counter*)
  (intern (format nil "~A~A" prefix *compiler-gensym-counter*)))

;;; ───────────────────────────────────────────────────────────────────
;;; EXPANSION : (loop for var from start to end do body)
;;; ───────────────────────────────────────────────────────────────────

(defun expand-loop-for-from-to (var start end body)
  "Expanse (loop for var from start to end do body) en code simple.
   
   TRANSFORMATION :
     (loop for i from 1 to 10 do body)
     →
     (let ((i start))
       (while (<= i end)
         body
         (setq i (+ i 1))))
   
   PARAMÈTRES :
     var   - Variable de boucle
     start - Valeur initiale
     end   - Valeur finale (inclusive)
     body  - Corps de la boucle (liste d'expressions)
   
   RETOURNE :
     Code expansé sous forme de S-expression."
  `(let ((,var ,start))
     (while (<= ,var ,end)
       ,@body
       (setq ,var (+ ,var 1)))))

;;; ───────────────────────────────────────────────────────────────────
;;; EXPANSION : (loop for var from start below end do body)
;;; ───────────────────────────────────────────────────────────────────

(defun expand-loop-for-from-below (var start end body)
  "Expanse (loop for var from start below end do body) en code simple.
   
   TRANSFORMATION :
     (loop for i from 0 below 10 do body)
     →
     (let ((i start))
       (while (< i end)
         body
         (setq i (+ i 1))))
   
   PARAMÈTRES :
     var   - Variable de boucle
     start - Valeur initiale
     end   - Valeur finale (exclusive)
     body  - Corps de la boucle
   
   RETOURNE :
     Code expansé."
  `(let ((,var ,start))
     (while (< ,var ,end)
       ,@body
       (setq ,var (+ ,var 1)))))

;;; ───────────────────────────────────────────────────────────────────
;;; EXPANSION : (loop for var in list do body)
;;; ───────────────────────────────────────────────────────────────────

(defun expand-loop-for-in (var list-expr body)
  "Expanse (loop for var in list do body) en code simple.
   
   TRANSFORMATION :
     (loop for x in list do body)
     →
     (let ((#:list-temp list))
       (while #:list-temp
         (let ((x (car #:list-temp)))
           body
           (setq #:list-temp (cdr #:list-temp)))))
   
   PARAMÈTRES :
     var       - Variable de boucle
     list-expr - Expression qui évalue à une liste
     body      - Corps de la boucle
   
   RETOURNE :
     Code expansé."
  (let ((list-temp (compiler-gensym "LIST")))
    `(let ((,list-temp ,list-expr))
       (while ,list-temp
         (let ((,var (car ,list-temp)))
           ,@body
           (setq ,list-temp (cdr ,list-temp)))))))

;;; ───────────────────────────────────────────────────────────────────
;;; EXPANSION : (loop for var in list collect expr)
;;; ───────────────────────────────────────────────────────────────────

(defun expand-loop-for-in-collect (var list-expr expr)
  "Expanse (loop for var in list collect expr) en code simple.
   
   TRANSFORMATION :
     (loop for x in list collect (* x 2))
     →
     (let ((#:result nil)
           (#:list-temp list))
       (while #:list-temp
         (let ((x (car #:list-temp)))
           (setq #:result (cons (* x 2) #:result))
           (setq #:list-temp (cdr #:list-temp))))
       (reverse #:result))
   
   PARAMÈTRES :
     var       - Variable de boucle
     list-expr - Expression qui évalue à une liste
     expr      - Expression à collecter
   
   RETOURNE :
     Code expansé."
  (let ((result (compiler-gensym "RESULT"))
        (list-temp (compiler-gensym "LIST")))
    `(let ((,result nil)
           (,list-temp ,list-expr))
       (while ,list-temp
         (let ((,var (car ,list-temp)))
           (setq ,result (cons ,expr ,result))
           (setq ,list-temp (cdr ,list-temp))))
       (reverse ,result))))

;;; ───────────────────────────────────────────────────────────────────
;;; EXPANSION : (loop for var from start to end collect expr)
;;; ───────────────────────────────────────────────────────────────────

(defun expand-loop-for-from-to-collect (var start end expr)
  "Expanse (loop for var from start to end collect expr).
   
   TRANSFORMATION :
     (loop for i from 1 to 5 collect (* i 2))
     →
     (let ((#:result nil)
           (i start))
       (while (<= i end)
         (setq #:result (cons (* i 2) #:result))
         (setq i (+ i 1)))
       (reverse #:result))
   
   PARAMÈTRES :
     var   - Variable de boucle
     start - Valeur initiale
     end   - Valeur finale (inclusive)
     expr  - Expression à collecter
   
   RETOURNE :
     Code expansé."
  (let ((result (compiler-gensym "RESULT")))
    `(let ((,result nil)
           (,var ,start))
       (while (<= ,var ,end)
         (setq ,result (cons ,expr ,result))
         (setq ,var (+ ,var 1)))
       (reverse ,result))))

;;; ───────────────────────────────────────────────────────────────────
;;; EXPANSION : Deux FOR en parallèle (for x in list1 for y in list2)
;;; ───────────────────────────────────────────────────────────────────

(defun expand-loop-for-in-parallel (var1 list1 var2 list2 body)
  "Expanse deux clauses FOR IN en parallèle.
   
   TRANSFORMATION :
     (loop for x in list1 for y in list2 do body)
     →
     (let ((#:list-temp1 list1)
           (#:list-temp2 list2))
       (while (and #:list-temp1 #:list-temp2)
         (let ((x (car #:list-temp1))
               (y (car #:list-temp2)))
           body
           (setq #:list-temp1 (cdr #:list-temp1))
           (setq #:list-temp2 (cdr #:list-temp2)))))
   
   PARAMÈTRES :
     var1, var2 - Variables de boucle
     list1, list2 - Listes à parcourir
     body - Corps de la boucle
   
   RETOURNE :
     Code expansé."
  (let ((list-temp1 (compiler-gensym "LIST"))
        (list-temp2 (compiler-gensym "LIST")))
    `(let ((,list-temp1 ,list1)
           (,list-temp2 ,list2))
       (while (and ,list-temp1 ,list-temp2)
         (let ((,var1 (car ,list-temp1))
               (,var2 (car ,list-temp2)))
           ,@body
           (setq ,list-temp1 (cdr ,list-temp1))
           (setq ,list-temp2 (cdr ,list-temp2)))))))

;;; ───────────────────────────────────────────────────────────────────
;;; EXPANSION : FOR avec calcul (for var = expr)
;;; ───────────────────────────────────────────────────────────────────

(defun expand-loop-for-equals (bindings body)
  "Expanse (loop for var = expr do body).
   
   TRANSFORMATION :
     (loop for x = 10 for y = (* x 2) do body)
     →
     (let ((x 10)
           (y (* x 2)))
       body)
   
   Note : Cette forme ne boucle pas, c'est juste un LET.
         Si on veut une vraie boucle, il faut combiner avec d'autres clauses.
   
   PARAMÈTRES :
     bindings - Liste de paires (var expr)
     body     - Corps de la boucle
   
   RETOURNE :
     Code expansé."
  `(let ,bindings
     ,@body))

;;; ═══════════════════════════════════════════════════════════════════
;;; UTILITAIRES DE TEST
;;; ═══════════════════════════════════════════════════════════════════

(defun test-loop-expansion ()
  "Teste les expansions de LOOP."
  (format t "~%═══════════════════════════════════════════════════~%")
  (format t "TEST EXPANSIONS LOOP~%")
  (format t "═══════════════════════════════════════════════════~%~%")
  
  ;; Test 1 : for from to
  (format t "TEST 1 : (loop for i from 1 to 5 do (print i))~%")
  (let ((expanded (expand-loop-for-from-to 'i 1 5 '((print i)))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 2 : for from below
  (format t "TEST 2 : (loop for i from 0 below 5 do (print i))~%")
  (let ((expanded (expand-loop-for-from-below 'i 0 5 '((print i)))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 3 : for in
  (format t "TEST 3 : (loop for x in '(1 2 3) do (print x))~%")
  (let ((expanded (expand-loop-for-in 'x '(quote (1 2 3)) '((print x)))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 4 : for in collect
  (format t "TEST 4 : (loop for x in '(1 2 3) collect (* x 2))~%")
  (let ((expanded (expand-loop-for-in-collect 'x '(quote (1 2 3)) '(* x 2))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 5 : for from to collect
  (format t "TEST 5 : (loop for i from 1 to 5 collect (* i 2))~%")
  (let ((expanded (expand-loop-for-from-to-collect 'i 1 5 '(* i 2))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  ;; Test 6 : deux for in parallèle
  (format t "TEST 6 : (loop for x in list1 for y in list2 do (print (+ x y)))~%")
  (let ((expanded (expand-loop-for-in-parallel 'x 'list1 'y 'list2 
                                                '((print (+ x y))))))
    (format t "  Expansion :~%")
    (pprint expanded)
    (format t "~%~%"))
  
  (format t "═══════════════════════════════════════════════════~%")
  (format t "TOUS LES TESTS AFFICHÉS~%")
  (format t "═══════════════════════════════════════════════════~%~%"))

;;; ═══════════════════════════════════════════════════════════════════
;;; FIN DU FICHIER
;;; ═══════════════════════════════════════════════════════════════════

(format t "~%Expansions LOOP chargées.~%")
(format t "Fonctions disponibles :~%")
(format t "  - (expand-loop-for-from-to var start end body)~%")
(format t "  - (expand-loop-for-from-below var start end body)~%")
(format t "  - (expand-loop-for-in var list body)~%")
(format t "  - (expand-loop-for-in-collect var list expr)~%")
(format t "  - (expand-loop-for-from-to-collect var start end expr)~%")
(format t "  - (expand-loop-for-in-parallel var1 list1 var2 list2 body)~%")
(format t "  - (test-loop-expansion) : Tester toutes les expansions~%")
(format t "~%")
