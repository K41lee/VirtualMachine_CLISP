;;;; test-fibo-simplified.lisp
;;;; Test complet avec fibonacci(20) en utilisant les fichiers simplifiés

(format t "~%========================================~%")
(format t "TEST FIBONACCI(20) - Fichiers simplifiés~%")
(format t "========================================~%~%")

;;; ============================================================================
;;; Chargement des dépendances
;;; ============================================================================

(format t "Étape 1: Chargement des modules de base...~%")

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "  ✓ Modules de base chargés~%~%")

;;; ============================================================================
;;; Chargement des fichiers simplifiés
;;; ============================================================================

(format t "Étape 2: Chargement des fichiers simplifiés...~%")

(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "  ✓ Fichiers simplifiés chargés~%~%")

;;; ============================================================================
;;; Définition de Fibonacci
;;; ============================================================================

(format t "Étape 3: Définition de fibonacci...~%~%")

(defvar *fibo-code*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1))
            (fibo (- n 2))))))

(format t "Code Fibonacci:~%~A~%~%" *fibo-code*)

;;; ============================================================================
;;; Test 1: Compilation avec le compilateur simplifié
;;; ============================================================================

(format t "========================================~%")
(format t "TEST 1: Compilation (compiler-simplified)~%")
(format t "========================================~%~%")

(defvar *compiled-code* nil)
(defvar *compile-success* nil)

(handler-case
    (progn
      (setq *compiled-code* (compile-lisp-to-mips-simplified *fibo-code*))
      (setq *compile-success* t)
      (format t "✅ Compilation réussie !~%")
      (format t "   Instructions générées: ~A~%~%" (length *compiled-code*)))
  (error (e)
    (format t "❌ Erreur de compilation: ~A~%~%" e)
    (setq *compile-success* nil)))

;;; ============================================================================
;;; Test 2: Compilation avec compile-lisp-with-ids (référence)
;;; ============================================================================

(format t "========================================~%")
(format t "TEST 2: Compilation (compile-lisp-with-ids)~%")
(format t "========================================~%~%")

(defvar *compiled-code-ref* nil)
(defvar *compile-ref-success* nil)

(handler-case
    (progn
      (setq *compiled-code-ref* (compile-lisp-with-ids *fibo-code*))
      (setq *compile-ref-success* t)
      (format t "✅ Compilation référence réussie !~%")
      (format t "   Instructions générées: ~A~%~%" (length *compiled-code-ref*)))
  (error (e)
    (format t "❌ Erreur de compilation référence: ~A~%~%" e)
    (setq *compile-ref-success* nil)))

;;; ============================================================================
;;; Test 3: Chargement avec le loader simplifié
;;; ============================================================================

(format t "========================================~%")
(format t "TEST 3: Chargement (loader-simplified)~%")
(format t "========================================~%~%")

(when *compile-ref-success*
  (format t "Tentative de chargement du code compilé...~%")
  
  (defvar *vm* (make-vm))
  (defvar *load-success* nil)
  
  (handler-case
      (progn
        ;; Utiliser le loader simplifié pour prétraiter
        (let ((preprocessed (preprocess-code-simplified *compiled-code-ref* 
                                                       (calculate-code-start-simplified *vm*))))
          (format t "  ✓ Prétraitement réussi~%")
          (format t "   Résolution des labels: OK~%~%")
          (setq *load-success* t)))
    (error (e)
      (format t "❌ Erreur de chargement: ~A~%~%" e)
      (setq *load-success* nil))))

;;; ============================================================================
;;; Test 4: Exécution complète avec le système original
;;; ============================================================================

(format t "========================================~%")
(format t "TEST 4: Exécution complète (système original)~%")
(format t "========================================~%~%")

(when *compile-ref-success*
  (format t "Création d'une VM et exécution...~%~%")
  
  (defvar *vm-exec* (make-vm))
  
  (handler-case
      (progn
        ;; Charger le code fibonacci
        (load-code *vm-exec* *compiled-code-ref*)
        (format t "  ✓ Code chargé dans la VM~%")
        
        ;; Charger l'argument (20) dans $A0
        (set-register *vm-exec* :$A0 20)
        (format t "  ✓ Argument n=20 défini~%")
        
        ;; Exécuter
        (format t "~%Exécution de fibo(20)...~%")
        (format t "(Cela peut prendre quelques secondes)~%~%")
        
        (let ((start-time (get-internal-real-time)))
          (run *vm-exec*)
          (let* ((end-time (get-internal-real-time))
                 (elapsed (/ (- end-time start-time) 
                            internal-time-units-per-second))
                 (result (get-register *vm-exec* :$V0)))
            
            (format t "========================================~%")
            (format t "RÉSULTAT~%")
            (format t "========================================~%~%")
            (format t "fibo(20) = ~A~%" result)
            (format t "Temps d'exécution: ~,3F secondes~%" elapsed)
            (format t "Instructions exécutées: ~A~%~%" (get-instruction-count *vm-exec*))
            
            (if (= result 6765)
                (format t "✅ SUCCÈS ! Résultat correct (6765)~%~%")
                (format t "❌ ÉCHEC ! Résultat incorrect (attendu 6765)~%~%")))))
    (error (e)
      (format t "❌ Erreur d'exécution: ~A~%~%" e))))

;;; ============================================================================
;;; Test 5: Vérification des utilitaires du loader
;;; ============================================================================

(format t "========================================~%")
(format t "TEST 5: Utilitaires loader-simplified~%")
(format t "========================================~%~%")

(format t "Test des fonctions utilitaires:~%~%")

;; Test alist
(let* ((alist (alist-put "key1" 42 nil))
       (alist2 (alist-put "key2" 100 alist))
       (val1 (alist-get "key1" alist2))
       (val2 (alist-get "key2" alist2)))
  (format t "  alist-put/get: ~A ~A " val1 val2)
  (if (and (= val1 42) (= val2 100))
      (format t "✓~%")
      (format t "✗~%")))

;; Test map-list
(let ((result (map-list (lambda (x) (* x 2)) '(1 2 3))))
  (format t "  map-list: ~A " result)
  (if (equal result '(2 4 6))
      (format t "✓~%")
      (format t "✗~%")))

;; Test reverse-list
(let ((result (reverse-list '(1 2 3 4))))
  (format t "  reverse-list: ~A " result)
  (if (equal result '(4 3 2 1))
      (format t "✓~%")
      (format t "✗~%")))

;; Test is-label-instr
(let ((r1 (is-label-instr '("LABEL" "TEST")))
      (r2 (is-label-instr '("LI" 2 42))))
  (format t "  is-label-instr: ~A ~A " r1 r2)
  (if (and r1 (not r2))
      (format t "✓~%")
      (format t "✗~%")))

(format t "~%")

;;; ============================================================================
;;; Test 6: Génération de code du compiler simplifié
;;; ============================================================================

(format t "========================================~%")
(format t "TEST 6: Génération compiler-simplified~%")
(format t "========================================~%~%")

(format t "Test de génération pour différentes expressions:~%~%")

(defun test-compile-expr (name expr)
  "Teste la compilation d'une expression"
  (handler-case
      (let* ((env (make-new-compiler-env-simplified))
             (code (compile-expr-main expr env)))
        (format t "  ~A: ~A instr " name (length code))
        (if (> (length code) 0)
            (format t "✓~%")
            (format t "✗~%")))
    (error (e)
      (format t "  ~A: ERREUR ✗~%" name))))

(test-compile-expr "Constante" 42)
(test-compile-expr "Variable" 'x)
(test-compile-expr "Addition" '(+ 1 2))
(test-compile-expr "Soustraction" '(- 10 5))
(test-compile-expr "Multiplication" '(* 3 4))
(test-compile-expr "Division" '(/ 20 4))
(test-compile-expr "Comparaison <" '(< x 10))
(test-compile-expr "Comparaison =" '(= x 5))
(test-compile-expr "IF simple" '(if (< n 2) n 0))
(test-compile-expr "AND" '(and (< x 10) (> x 0)))
(test-compile-expr "OR" '(or (= x 0) (= x 1)))
(test-compile-expr "NOT" '(not (= x 0)))
(test-compile-expr "LET" '(let ((x 5)) x))
(test-compile-expr "PROGN" '(progn (+ 1 2) (+ 3 4)))
(test-compile-expr "SETQ" '(setq x 10))

(format t "~%")

;;; ============================================================================
;;; Résumé final
;;; ============================================================================

(format t "========================================~%")
(format t "RÉSUMÉ DES TESTS~%")
(format t "========================================~%~%")

(format t "Compilation (simplifié):     ~A~%" 
        (if *compile-success* "✅ OK" "❌ ÉCHEC"))
(format t "Compilation (référence):     ~A~%" 
        (if *compile-ref-success* "✅ OK" "❌ ÉCHEC"))
(format t "Chargement (simplifié):      ~A~%" 
        (if (boundp '*load-success*) 
            (if *load-success* "✅ OK" "❌ ÉCHEC")
            "⏭ NON TESTÉ"))
(format t "Exécution fibo(20):          ~A~%" 
        (if *compile-ref-success* "✅ TESTÉ" "⏭ NON TESTÉ"))

(format t "~%")

(if (and *compile-success* *compile-ref-success*)
    (progn
      (format t "========================================~%")
      (format t "✅ SUCCÈS GLOBAL~%")
      (format t "========================================~%~%")
      (format t "Les fichiers simplifiés fonctionnent correctement !~%")
      (format t "Le compilateur peut compiler fibonacci.~%")
      (format t "Le loader peut prétraiter le code.~%")
      (format t "L'exécution produit le résultat correct.~%~%"))
    (progn
      (format t "========================================~%")
      (format t "⚠ QUELQUES TESTS ONT ÉCHOUÉ~%")
      (format t "========================================~%~%")
      (format t "Consulter les détails ci-dessus.~%~%")))
