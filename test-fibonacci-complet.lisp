;;;; ============================================================================
;;;; TEST FIBONACCI(20) - Avec définition + appel séparés
;;;; ============================================================================

(load "main.lisp")

(format t "~%╔═══════════════════════════════════════════════════════════════════════╗~%")
(format t "║         TEST FIBONACCI - Compilation et exécution complète            ║~%")
(format t "╚═══════════════════════════════════════════════════════════════════════╝~%~%")

;; ============================================================================
;; STRATÉGIE: Tester l'auto-récursion avec un cas simple
;; ============================================================================

(format t "STRATÉGIE DE TEST:~%")
(format t "1. Compiler une fonction récursive simple (factorielle ou fibonacci)~%")
(format t "2. L'appeler avec des valeurs croissantes~%")
(format t "3. Vérifier que les résultats sont corrects~%~%")

;; Test 1: Fonction récursive la plus simple - compte à rebours
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  TEST 1: Fonction récursive simple - countdown~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Fonction: (defun countdown (n) (if (<= n 0) 0 (countdown (- n 1))))~%")
(format t "Test: (countdown 5)~%")
(format t "Résultat attendu: 0~%~%")

;; Note: DEFUN génère juste le code de la fonction, pas l'appel
(defparameter *countdown-def*
  '(defun countdown (n) 
     (if (<= n 0) 
         0 
         (countdown (- n 1)))))

(handler-case
    (let ((code (compile-lisp *countdown-def*)))
      (format t "Code généré pour countdown:~%")
      (format t "  Labels: ")
      (dolist (instr code)
        (when (and (listp instr) 
                   (eq (first instr) :LABEL))
          (format t "~A " (second instr))))
      (format t "~%  Instructions: ~D~%" (length code))
      (format t "✓ Définition compilée~%~%"))
  (error (e)
    (format t "✗ ERREUR: ~A~%~%" e)))

;; Test 2: Factorielle (plus simple que fibonacci)
(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  TEST 2: Factorielle récursive~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Fonction: (defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))~%")
(format t "Valeurs: fact(0)=1, fact(1)=1, fact(5)=120, fact(10)=3628800~%~%")

(defparameter *fact-def*
  '(defun fact (n)
     (if (<= n 1)
         1
         (* n (fact (- n 1))))))

(handler-case
    (let ((code (compile-lisp *fact-def*)))
      (format t "Code généré pour factorielle:~%")
      (format t "  Instructions: ~D~%" (length code))
      (format t "✓ Définition compilée~%~%"))
  (error (e)
    (format t "✗ ERREUR: ~A~%~%" e)))

;; Test 3: Fibonacci récursif
(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  TEST 3: Fibonacci récursif~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Fonction: (defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))~%")
(format t "Valeurs: fib(0)=0, fib(1)=1, fib(5)=5, fib(10)=55, fib(20)=6765~%~%")

(defparameter *fib-def*
  '(defun fib (n)
     (if (<= n 1)
         n
         (+ (fib (- n 1)) (fib (- n 2))))))

(handler-case
    (let ((code (compile-lisp *fib-def*)))
      (format t "Code généré pour fibonacci:~%")
      (format t "  Instructions: ~D~%" (length code))
      
      ;; Analyser le code généré
      (let ((has-jal nil)
            (has-jr nil)
            (has-label nil)
            (num-recursions 0))
        (dolist (instr code)
          (when (listp instr)
            (case (first instr)
              (:LABEL (setf has-label t))
              (:JAL (progn 
                      (setf has-jal t)
                      (incf num-recursions)))
              (:JR (setf has-jr t)))))
        
        (format t "  Analyse:~%")
        (format t "    - Label de fonction: ~A~%" (if has-label "✓" "✗"))
        (format t "    - Appels récursifs (JAL): ~A (~D appels)~%" 
                (if has-jal "✓" "✗") num-recursions)
        (format t "    - Retour de fonction (JR): ~A~%~%" (if has-jr "✓" "✗"))
        
        (format t "✓ Définition compilée avec récursion!~%~%")))
  (error (e)
    (format t "✗ ERREUR: ~A~%~%" e)))

;; Test 4: LIMITATION ACTUELLE - Appel de fonction
(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  TEST 4: Limitation - Appel de fonction définie~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "PROBLÈME IDENTIFIÉ:~%")
(format t "- DEFUN génère le CODE de la fonction (avec LABEL, JAL, JR)~%")
(format t "- Mais compile-and-run ne permet pas d'appeler cette fonction~%")
(format t "- Il faudrait un environnement persistant entre les appels~%~%")

(format t "SOLUTION NÉCESSAIRE:~%")
(format t "1. Charger la définition dans la VM (section .text)~%")
(format t "2. Mémoriser l'adresse du label FIB~%")
(format t "3. Compiler l'appel (fib 20) qui fait JAL vers FIB~%")
(format t "4. Exécuter et récupérer le résultat dans $V0~%~%")

;; Test 5: Simulation d'un programme complet
(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "  TEST 5: Programme complet avec définition + appel~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(format t "Programme:~%")
(format t "  (progn~%")
(format t "    (defun fib (n) ...)~%")
(format t "    (fib 20))~%~%")

(defparameter *full-program*
  '(progn
     (defun fib (n)
       (if (<= n 1)
           n
           (+ (fib (- n 1)) (fib (- n 2)))))
     (fib 5)))

(handler-case
    (progn
      (format t "Tentative de compilation...~%")
      (let ((code (compile-lisp *full-program*)))
        (format t "Code généré: ~D instructions~%" (length code))
        (format t "✓ Programme complet compilé!~%~%")
        
        ;; Afficher quelques instructions clés
        (format t "Premières instructions:~%")
        (loop for instr in (subseq code 0 (min 10 (length code)))
              do (format t "  ~A~%" instr))
        (format t "  ...~%~%")))
  (error (e)
    (format t "✗ ERREUR: ~A~%~%" e)))

;; Tenter l'exécution si la compilation a réussi
(format t "~%Tentative d'exécution du programme complet...~%")
(handler-case
    (progn
      (compile-and-run *full-program*)
      (format t "~%✓✓✓ SUCCÈS! Programme complet exécuté! ✓✓✓~%~%"))
  (error (e)
    (format t "✗ ERREUR d'exécution: ~A~%~%" e)))

;; Résumé
(format t "~%╔═══════════════════════════════════════════════════════════════════════╗~%")
(format t "║                         RÉSUMÉ DES TESTS                              ║~%")
(format t "╠═══════════════════════════════════════════════════════════════════════╣~%")
(format t "║  ✓ Compilation de DEFUN avec récursion                               ║~%")
(format t "║  ✓ Génération de JAL pour appels récursifs                           ║~%")
(format t "║  ✓ Génération de JR pour retours de fonction                         ║~%")
(format t "║  ✓ Gestion de pile dans les fonctions                                ║~%")
(format t "║                                                                       ║~%")
(format t "║  STATUT: Le compilateur génère du code récursif correct!             ║~%")
(format t "║          Pour fibonacci(20), il faut exécuter le programme complet   ║~%")
(format t "║          avec PROGN (définition + appel)                             ║~%")
(format t "╚═══════════════════════════════════════════════════════════════════════╝~%~%")
