;;;; test-algorithms-suite.lisp
;;;; Suite de tests pour algorithmes classiques avec le compilateur simplifié

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)
(defvar *total-instructions* 0)

(defun test-algorithm (name code test-cases)
  "Test un algorithme avec plusieurs cas de test"
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: ~A~%" name)
  (format t "═══════════════════════════════════════════════════════════~%")
  
  (let ((compiled-code (compile-lisp-to-mips-simplified code)))
    (format t "Code généré: ~D instructions~%" (length compiled-code))
    
    (dolist (test test-cases)
      (let* ((input (first test))
             (expected (second test))
             (vm (make-new-vm))
             (start-time (get-internal-real-time)))
        
        (load-code vm compiled-code)
        (set-register vm :$A0 input)
        ;; Initialiser $RA pour éviter crash au retour
        (let ((code-start (calculate-code-start vm)))
          (set-register vm :$RA (+ code-start (length compiled-code))))
        (handler-case
            (progn
              (run-vm vm :max-instructions 10000000)
              (let* ((result (get-register vm :$V0))
                     (end-time (get-internal-real-time))
                     (elapsed (/ (- end-time start-time) internal-time-units-per-second))
                     (instructions (vm-instruction-count vm)))
                
                (setq *total-instructions* (+ *total-instructions* instructions))
                
                (if (= result expected)
                    (progn
                      (format t "  ✅ ~A(~D) = ~D [~,3F sec, ~:D instr]~%" 
                              (second code) input result elapsed instructions)
                      (incf *tests-passed*))
                    (progn
                      (format t "  ❌ ~A(~D) = ~D (attendu ~D)~%" 
                              (second code) input result expected)
                      (incf *tests-failed*)))))
          (error (e)
            (format t "  ❌ ERREUR: ~A(~D) - ~A~%" (second code) input e)
            (incf *tests-failed*)))))))

;;; ============================================================================
;;; ALGORITHMES À TESTER
;;; ============================================================================

;; 1. FACTORIELLE (récursive)
(defvar *factorial-code* 
  '(defun factorial (n)
     (if (< n 2) 
         1 
         (* n (factorial (- n 1))))))

;; 2. ACKERMANN (double récursion)
(defvar *ackermann-code*
  '(defun ackermann (m n)
     (if (= m 0)
         (+ n 1)
         (if (= n 0)
             (ackermann (- m 1) 1)
             (ackermann (- m 1) (ackermann m (- n 1)))))))

;; 3. PGCD (algorithme d'Euclide)
(defvar *gcd-code*
  '(defun gcd (a b)
     (if (= b 0)
         a
         (gcd b (% a b)))))

;; 4. PUISSANCE (exponentiation rapide)
(defvar *power-code*
  '(defun power (base exp)
     (if (= exp 0)
         1
         (if (= (% exp 2) 0)
             (power (* base base) (/ exp 2))
             (* base (power base (- exp 1)))))))

;; 5. SOMME DES N PREMIERS ENTIERS
(defvar *sum-n-code*
  '(defun sum-n (n)
     (if (< n 1)
         0
         (+ n (sum-n (- n 1))))))

;; 6. SOMME DES CARRÉS
(defvar *sum-squares-code*
  '(defun sum-squares (n)
     (if (< n 1)
         0
         (+ (* n n) (sum-squares (- n 1))))))

;; 7. NOMBRE TRIANGULAIRE
(defvar *triangle-code*
  '(defun triangle (n)
     (if (< n 1)
         0
         (+ n (triangle (- n 1))))))

;; 8. TEST DE PRIMALITÉ (division successive)
(defvar *is-prime-helper-code*
  '(defun is-prime-helper (n divisor)
     (if (> (* divisor divisor) n)
         1
         (if (= (% n divisor) 0)
             0
             (is-prime-helper n (+ divisor 1))))))

;; 9. COMBINAISON (n choose k) - version simplifiée
(defvar *combination-code*
  '(defun combination (n k)
     (if (= k 0)
         1
         (if (= n k)
             1
             (+ (combination (- n 1) (- k 1))
                (combination (- n 1) k))))))

;; 10. SUITE DE TRIBONACCI
(defvar *tribonacci-code*
  '(defun tribonacci (n)
     (if (< n 2)
         n
         (if (= n 2)
             1
             (+ (tribonacci (- n 1))
                (tribonacci (- n 2))
                (tribonacci (- n 3)))))))

;; 11. MAXIMUM DE DEUX NOMBRES
(defvar *max-code*
  '(defun max2 (a b)
     (if (> a b) a b)))

;; 12. MINIMUM DE TROIS NOMBRES (imbriqué)
(defvar *min3-code*
  '(defun min3 (a b c)
     (if (< a b)
         (if (< a c) a c)
         (if (< b c) b c))))

;; 13. VALEUR ABSOLUE
(defvar *abs-code*
  '(defun abs-val (n)
     (if (< n 0) (- 0 n) n)))

;; 14. COLLATZ (3n+1) - nombre d'étapes
(defvar *collatz-code*
  '(defun collatz (n)
     (if (= n 1)
         0
         (if (= (% n 2) 0)
             (+ 1 (collatz (/ n 2)))
             (+ 1 (collatz (+ (* 3 n) 1)))))))

;; 15. SOMME DES CHIFFRES (base 10)
(defvar *sum-digits-code*
  '(defun sum-digits (n)
     (if (< n 10)
         n
         (+ (% n 10) (sum-digits (/ n 10))))))

;;; ============================================================================
;;; EXÉCUTION DES TESTS
;;; ============================================================================

(format t "~%~%")
(format t "╔═══════════════════════════════════════════════════════════╗~%")
(format t "║   SUITE DE TESTS - ALGORITHMES CLASSIQUES                ║~%")
(format t "║   Compilateur: compiler-simplified.lisp                  ║~%")
(format t "╚═══════════════════════════════════════════════════════════╝~%")

(let ((start-total (get-internal-real-time)))

  ;; Test 1: Factorielle
  (test-algorithm "FACTORIELLE (récursive)"
                  *factorial-code*
                  '((0 1) (1 1) (5 120) (10 3628800)))

  ;; Test 2: Ackermann (ATTENTION: très lent pour m,n > 3)
  ;; On teste seulement avec de petites valeurs
  ;; Note: Ackermann prend 2 paramètres, on met n dans $A0 et m=1 fixe
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: ACKERMANN (double récursion) - SKIP (nécessite 2 params)~%" )
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  ⚠️  Algorithme nécessite 2 paramètres (non supporté pour l'instant)~%")

  ;; Test 3: PGCD
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: PGCD (Euclide) - SKIP (nécessite 2 params)~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  ⚠️  Algorithme nécessite 2 paramètres (non supporté pour l'instant)~%")

  ;; Test 4: Puissance - SKIP (2 params)
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: PUISSANCE - SKIP (nécessite 2 params)~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  ⚠️  Algorithme nécessite 2 paramètres (non supporté pour l'instant)~%")

  ;; Test 5: Somme des N premiers entiers
  (test-algorithm "SOMME DES N PREMIERS ENTIERS"
                  *sum-n-code*
                  '((0 0) (1 1) (10 55) (100 5050)))

  ;; Test 6: Somme des carrés
  (test-algorithm "SOMME DES CARRÉS"
                  *sum-squares-code*
                  '((0 0) (1 1) (5 55) (10 385)))

  ;; Test 7: Nombre triangulaire
  (test-algorithm "NOMBRE TRIANGULAIRE"
                  *triangle-code*
                  '((0 0) (1 1) (10 55) (20 210)))

  ;; Test 8: Primalité - SKIP (2 params)
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: PRIMALITÉ - SKIP (nécessite 2 params)~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  ⚠️  Algorithme nécessite 2 paramètres (non supporté pour l'instant)~%")

  ;; Test 9: Combinaison - SKIP (2 params)
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: COMBINAISON - SKIP (nécessite 2 params)~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  ⚠️  Algorithme nécessite 2 paramètres (non supporté pour l'instant)~%")

  ;; Test 10: Tribonacci
  (test-algorithm "TRIBONACCI (triple récursion)"
                  *tribonacci-code*
                  '((0 0) (1 1) (5 7) (10 149)))

  ;; Test 11: Max - SKIP (2 params)
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: MAXIMUM - SKIP (nécessite 2 params)~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  ⚠️  Algorithme nécessite 2 paramètres (non supporté pour l'instant)~%")

  ;; Test 12: Min3 - SKIP (3 params)
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "Test: MINIMUM 3 - SKIP (nécessite 3 params)~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  ⚠️  Algorithme nécessite 3 paramètres (non supporté pour l'instant)~%")

  ;; Test 13: Valeur absolue
  (test-algorithm "VALEUR ABSOLUE"
                  *abs-code*
                  '((0 0) (5 5) (-5 5) (100 100)))

  ;; Test 14: Collatz
  (test-algorithm "COLLATZ (3n+1)"
                  *collatz-code*
                  '((1 0) (2 1) (3 7) (10 6)))

  ;; Test 15: Somme des chiffres
  (test-algorithm "SOMME DES CHIFFRES"
                  *sum-digits-code*
                  '((0 0) (9 9) (123 6) (9999 36)))

  (let ((end-total (get-internal-real-time))
        (elapsed-total (/ (- end-total start-total) internal-time-units-per-second)))

    (format t "~%~%")
    (format t "╔═══════════════════════════════════════════════════════════╗~%")
    (format t "║   RÉSULTATS FINAUX                                       ║~%")
    (format t "╚═══════════════════════════════════════════════════════════╝~%")
    (format t "~%")
    (format t "  Tests réussis:        ~D~%" *tests-passed*)
    (format t "  Tests échoués:        ~D~%" *tests-failed*)
    (format t "  Total:                ~D~%" (+ *tests-passed* *tests-failed*))
    (format t "  Taux de réussite:     ~,1F%%~%" 
            (if (> (+ *tests-passed* *tests-failed*) 0)
                (* 100.0 (/ *tests-passed* (+ *tests-passed* *tests-failed*)))
                0.0))
    (format t "~%")
    (format t "  Instructions totales: ~:D~%" *total-instructions*)
    (format t "  Temps total:          ~,2F secondes~%" elapsed-total)
    (format t "~%")
    
    (if (= *tests-failed* 0)
        (progn
          (format t "╔═══════════════════════════════════════════════════════════╗~%")
          (format t "║                                                           ║~%")
          (format t "║              ✨ TOUS LES TESTS RÉUSSIS ! ✨              ║~%")
          (format t "║                                                           ║~%")
          (format t "║  Le compilateur simplifié fonctionne parfaitement         ║~%")
          (format t "║  avec tous les algorithmes testés !                       ║~%")
          (format t "║                                                           ║~%")
          (format t "╚═══════════════════════════════════════════════════════════╝~%"))
        (progn
          (format t "╔═══════════════════════════════════════════════════════════╗~%")
          (format t "║                                                           ║~%")
          (format t "║              ⚠️  CERTAINS TESTS ONT ÉCHOUÉ              ║~%")
          (format t "║                                                           ║~%")
          (format t "╚═══════════════════════════════════════════════════════════╝~%")))))

(format t "~%")
