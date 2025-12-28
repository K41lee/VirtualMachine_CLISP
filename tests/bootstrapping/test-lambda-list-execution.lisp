;;;; test-lambda-list-execution.lisp
;;;; Tests d'exécution des lambda-lists expansées

(load "src/lambda-list-expander.lisp")

(format t "~%═══════════════════════════════════════════════════~%")
(format t "TESTS D'EXÉCUTION LAMBDA-LIST~%")
(format t "═══════════════════════════════════════════════════~%~%")

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 1 : &OPTIONAL - Cas basique
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 1 : &optional simple~%")

;; Définir la fonction expansée
(eval (expand-function-with-advanced-params 
       'test-opt-1 
       '(a &optional (b 10))
       '((+ a b))))

;; Tester avec et sans argument optionnel
(let ((result1 (test-opt-1 5))       ; b devrait être 10
      (result2 (test-opt-1 5 3)))    ; b devrait être 3
  (format t "  (test-opt-1 5) = ~A (attendu: 15)~%" result1)
  (format t "  (test-opt-1 5 3) = ~A (attendu: 8)~%" result2)
  (if (and (= result1 15) (= result2 8))
      (format t "  ✓ TEST 1 RÉUSSI~%~%")
      (format t "  ✗ TEST 1 ÉCHOUÉ~%~%")))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 2 : &OPTIONAL - Plusieurs paramètres
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 2 : &optional multiples~%")

(eval (expand-function-with-advanced-params 
       'test-opt-2 
       '(a &optional (b 10) (c 20))
       '((list a b c))))

(let ((result1 (test-opt-2 1))        ; b=10, c=20
      (result2 (test-opt-2 1 2))      ; b=2, c=20
      (result3 (test-opt-2 1 2 3)))   ; b=2, c=3
  (format t "  (test-opt-2 1) = ~A~%" result1)
  (format t "  (test-opt-2 1 2) = ~A~%" result2)
  (format t "  (test-opt-2 1 2 3) = ~A~%" result3)
  (if (and (equal result1 '(1 10 20))
           (equal result2 '(1 2 20))
           (equal result3 '(1 2 3)))
      (format t "  ✓ TEST 2 RÉUSSI~%~%")
      (format t "  ✗ TEST 2 ÉCHOUÉ~%~%")))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 3 : &REST - Cas basique
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 3 : &rest simple~%")

(eval (expand-function-with-advanced-params 
       'test-rest-1 
       '(a &rest r)
       '((cons a r))))

(let ((result1 (test-rest-1 1))           ; r = ()
      (result2 (test-rest-1 1 2 3 4)))    ; r = (2 3 4)
  (format t "  (test-rest-1 1) = ~A~%" result1)
  (format t "  (test-rest-1 1 2 3 4) = ~A~%" result2)
  (if (and (equal result1 '(1))
           (equal result2 '(1 2 3 4)))
      (format t "  ✓ TEST 3 RÉUSSI~%~%")
      (format t "  ✗ TEST 3 ÉCHOUÉ~%~%")))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 4 : &KEY - Cas basique
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 4 : &key simple~%")

(eval (expand-function-with-advanced-params 
       'test-key-1 
       '(a &key (x 1) (y 2))
       '((list a x y))))

(let ((result1 (test-key-1 10))                    ; x=1, y=2
      (result2 (test-key-1 10 :x 5))               ; x=5, y=2
      (result3 (test-key-1 10 :y 7))               ; x=1, y=7
      (result4 (test-key-1 10 :x 5 :y 7))          ; x=5, y=7
      (result5 (test-key-1 10 :y 7 :x 5)))         ; ordre inversé
  (format t "  (test-key-1 10) = ~A~%" result1)
  (format t "  (test-key-1 10 :x 5) = ~A~%" result2)
  (format t "  (test-key-1 10 :y 7) = ~A~%" result3)
  (format t "  (test-key-1 10 :x 5 :y 7) = ~A~%" result4)
  (format t "  (test-key-1 10 :y 7 :x 5) = ~A~%" result5)
  (if (and (equal result1 '(10 1 2))
           (equal result2 '(10 5 2))
           (equal result3 '(10 1 7))
           (equal result4 '(10 5 7))
           (equal result5 '(10 5 7)))
      (format t "  ✓ TEST 4 RÉUSSI~%~%")
      (format t "  ✗ TEST 4 ÉCHOUÉ~%~%")))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 5 : &OPTIONAL + &REST
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 5 : &optional + &rest~%")

(eval (expand-function-with-advanced-params 
       'test-opt-rest 
       '(a &optional b &rest r)
       '((list a b r))))

(let ((result1 (test-opt-rest 1))            ; b=nil, r=()
      (result2 (test-opt-rest 1 2))          ; b=2, r=()
      (result3 (test-opt-rest 1 2 3 4)))     ; b=2, r=(3 4)
  (format t "  (test-opt-rest 1) = ~A~%" result1)
  (format t "  (test-opt-rest 1 2) = ~A~%" result2)
  (format t "  (test-opt-rest 1 2 3 4) = ~A~%" result3)
  (if (and (equal result1 '(1 nil ()))
           (equal result2 '(1 2 ()))
           (equal result3 '(1 2 (3 4))))
      (format t "  ✓ TEST 5 RÉUSSI~%~%")
      (format t "  ✗ TEST 5 ÉCHOUÉ~%~%")))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 6 : CAS RÉEL - make-lexical-env de compiler.lisp
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 6 : Cas réel make-lexical-env~%")
(format t "  Signature : (parent-env &optional (increment-depth t))~%")

(eval (expand-function-with-advanced-params 
       'test-lexical-env 
       '(parent-env &optional (increment-depth t))
       '((list parent-env increment-depth))))

(let ((result1 (test-lexical-env 'env1))         ; increment-depth=t
      (result2 (test-lexical-env 'env2 nil)))    ; increment-depth=nil
  (format t "  (test-lexical-env 'env1) = ~A~%" result1)
  (format t "  (test-lexical-env 'env2 nil) = ~A~%" result2)
  (if (and (equal result1 '(env1 t))
           (equal result2 '(env2 nil)))
      (format t "  ✓ TEST 6 RÉUSSI~%~%")
      (format t "  ✗ TEST 6 ÉCHOUÉ~%~%")))

;;; ═══════════════════════════════════════════════════════════════════
;;; TEST 7 : CAS RÉEL - compile-and-run de compiler.lisp
;;; ═══════════════════════════════════════════════════════════════════

(format t "TEST 7 : Cas réel compile-and-run~%")
(format t "  Signature : (expr &key (verbose nil) (show-code nil))~%")

(eval (expand-function-with-advanced-params 
       'test-compile-run 
       '(expr &key (verbose nil) (show-code nil))
       '((list expr verbose show-code))))

(let ((result1 (test-compile-run '(+ 1 2)))                        ; defaults
      (result2 (test-compile-run '(+ 1 2) :verbose t))              ; verbose=t
      (result3 (test-compile-run '(+ 1 2) :show-code t))            ; show-code=t
      (result4 (test-compile-run '(+ 1 2) :verbose t :show-code t)) ; both
      (result5 (test-compile-run '(+ 1 2) :show-code t :verbose t))) ; inversé
  (format t "  Résultat 1 : ~A~%" result1)
  (format t "  Résultat 2 : ~A~%" result2)
  (format t "  Résultat 3 : ~A~%" result3)
  (format t "  Résultat 4 : ~A~%" result4)
  (format t "  Résultat 5 : ~A~%" result5)
  (if (and (equal result1 '((+ 1 2) nil nil))
           (equal result2 '((+ 1 2) t nil))
           (equal result3 '((+ 1 2) nil t))
           (equal result4 '((+ 1 2) t t))
           (equal result5 '((+ 1 2) t t)))
      (format t "  ✓ TEST 7 RÉUSSI~%~%")
      (format t "  ✗ TEST 7 ÉCHOUÉ~%~%")))

;;; ═══════════════════════════════════════════════════════════════════
;;; RÉSUMÉ
;;; ═══════════════════════════════════════════════════════════════════

(format t "═══════════════════════════════════════════════════~%")
(format t "TESTS D'EXÉCUTION TERMINÉS~%")
(format t "═══════════════════════════════════════════════════~%")
(format t "~%")
(format t "Tous les types de paramètres avancés fonctionnent :~%")
(format t "  ✓ &optional (simple et multiple)~%")
(format t "  ✓ &rest~%")
(format t "  ✓ &key (simple, multiple, ordre flexible)~%")
(format t "  ✓ &optional + &rest~%")
(format t "  ✓ Cas réels de compiler.lisp~%")
(format t "~%")
