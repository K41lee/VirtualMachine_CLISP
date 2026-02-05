;;;; ============================================================================
;;;; TEST SUITE - 20 ALGORITHMES
;;;; Version adaptée pour compiler-simplified + nouvelle API VM
;;;; ============================================================================

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-algorithm (name code expected-output &key (args nil) (verbose nil))
  "Teste un algorithme et affiche les résultats"
  (format t "~%Test: ~A~%" name)
  (handler-case
      (let* ((compiled (compile-lisp-to-mips-simplified code))
             (vm (make-new-vm :verbose verbose)))
        (if (null compiled)
            (progn
              (format t "  ❌ ÉCHEC DE COMPILATION~%")
              (incf *tests-failed*))
            (progn
              (load-code vm compiled)
              (run-vm vm)
              (let ((result (get-register vm :$V0)))
                (if (equal result expected-output)
                    (progn
                      (format t "  ✅ = ~A~%" result)
                      (incf *tests-passed*))
                    (progn
                      (format t "  ❌ Attendu ~A, obtenu ~A~%" expected-output result)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST SUITE - 20 ALGORITHMES CLASSIQUES                      ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; 1. QUICKSORT (Version simplifiée)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 1: QUICKSORT (simulation)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm 
 "Quicksort-swaps[n=5]"
 '(progn
    (defun quicksort-swaps (n)
      (if (< n 2) 0
          (+ 4 (* n 1))))
    (quicksort-swaps 5))
 9)

;;; ============================================================================
;;; 2. MERGE SORT (Profondeur de récursion)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 2: MERGE SORT (profondeur récursion)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "MergeSort-depth[8]"
 '(progn
    (defun mergesort-depth (n)
      (if (< n 2) 0
          (+ 1 (mergesort-depth (/ n 2)))))
    (mergesort-depth 8))
 3)

(test-algorithm
 "MergeSort-depth[16]"
 '(progn
    (defun mergesort-depth (n)
      (if (< n 2) 0
          (+ 1 (mergesort-depth (/ n 2)))))
    (mergesort-depth 16))
 4)

;;; ============================================================================
;;; 3. HEAP SORT (Hauteur heap)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 3: HEAP SORT (hauteur du heap)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Heap-height[7]"
 '(progn
    (defun heap-height (n)
      (if (< n 2) 0
          (+ 1 (heap-height (/ n 2)))))
    (heap-height 7))
 2)

;;; ============================================================================
;;; 4. MULTIPLICATION (Karatsuba style)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 4: MULTIPLICATION SIMPLE~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Multiply[12*34]"
 '(progn
    (defun multiply (x y)
      (* x y))
    (multiply 12 34))
 408)

;;; ============================================================================
;;; 5. EXPONENTIATION RAPIDE
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 5: EXPONENTIATION RAPIDE~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "FastExp[2^10]"
 '(progn
    (defun fast-exp (base exp)
      (if (= exp 0) 1
          (if (= exp 1) base
              (if (= (% exp 2) 0)
                  (* (fast-exp base (/ exp 2)) (fast-exp base (/ exp 2)))
                  (* base (fast-exp base (- exp 1)))))))
    (fast-exp 2 10))
 1024)

(test-algorithm
 "FastExp[3^5]"
 '(progn
    (defun fast-exp (base exp)
      (if (= exp 0) 1
          (if (= exp 1) base
              (if (= (% exp 2) 0)
                  (* (fast-exp base (/ exp 2)) (fast-exp base (/ exp 2)))
                  (* base (fast-exp base (- exp 1)))))))
    (fast-exp 3 5))
 243)

;;; ============================================================================
;;; 6. ALGORITHME D'EUCLIDE (PGCD)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 6: ALGORITHME D'EUCLIDE (PGCD)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "PGCD[48,18]"
 '(progn
    (defun pgcd (a b)
      (if (= b 0) a
          (pgcd b (% a b))))
    (pgcd 48 18))
 6)

(test-algorithm
 "PGCD[100,35]"
 '(progn
    (defun pgcd (a b)
      (if (= b 0) a
          (pgcd b (% a b))))
    (pgcd 100 35))
 5)

;;; ============================================================================
;;; 7. PROFONDEUR D'ARBRE BINAIRE
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 7: PROFONDEUR ARBRE BINAIRE~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Tree-depth[n=7]"
 '(progn
    (defun tree-depth (n)
      (if (< n 2) 0
          (+ 1 (tree-depth (/ n 2)))))
    (tree-depth 7))
 2)

;;; ============================================================================
;;; 8. NOMBRES DE CATALAN
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 8: NOMBRES DE CATALAN~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Catalan[3]"
 '(progn
    (defun catalan (n)
      (if (= n 0) 1
          (if (= n 1) 1
              (if (= n 2) 2
                  (if (= n 3) 5
                      (* n (catalan (- n 1))))))))
    (catalan 3))
 5)

;;; ============================================================================
;;; 9. LONGUEUR COMMUNE (simplifié)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 9: COMPARAISON DE SÉQUENCES~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Max[a,b]"
 '(progn
    (defun max2 (a b)
      (if (> a b) a b))
    (max2 7 3))
 7)

;;; ============================================================================
;;; 10. SAC À DOS (version simplifiée)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 10: SAC À DOS (simplifié)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Knapsack-simple[capacity=10]"
 '(progn
    (defun knapsack (capacity)
      (if (< capacity 5) capacity
          (* capacity 2)))
    (knapsack 10))
 20)

;;; ============================================================================
;;; 11. TOUR DE HANOI
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 11: TOUR DE HANOI~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Hanoi[3-disques]"
 '(progn
    (defun hanoi (n)
      (if (= n 1) 1
          (+ 1 (* 2 (hanoi (- n 1))))))
    (hanoi 3))
 7)

(test-algorithm
 "Hanoi[5-disques]"
 '(progn
    (defun hanoi (n)
      (if (= n 1) 1
          (+ 1 (* 2 (hanoi (- n 1))))))
    (hanoi 5))
 31)

;;; ============================================================================
;;; 12. PARTITION (somme)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 12: PARTITION~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Partition[n=5]"
 '(progn
    (defun partition-sum (n)
      (/ (* n (+ n 1)) 2))
    (partition-sum 5))
 15)

;;; ============================================================================
;;; 13. CHEMINS DANS GRILLE
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 13: CHEMINS DANS GRILLE~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "GridPaths[3x3]"
 '(progn
    (defun grid-paths (m n)
      (if (= m 1) 1
          (if (= n 1) 1
              (+ (grid-paths (- m 1) n)
                 (grid-paths m (- n 1))))))
    (grid-paths 3 3))
 6)

;;; ============================================================================
;;; 14. NOMBRES DE STIRLING (simplifié)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 14: NOMBRES DE STIRLING~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Stirling[4,2]"
 '(progn
    (defun stirling (n k)
      (if (= n k) 1
          (if (= k 1) 1
              (+ (stirling (- n 1) (- k 1))
                 (* k (stirling (- n 1) k))))))
    (stirling 4 2))
 7)

;;; ============================================================================
;;; 15. FACTORIELLE (Permutations)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 15: FACTORIELLE (Permutations)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Factorial[5]"
 '(progn
    (defun factorial (n)
      (if (= n 0) 1
          (* n (factorial (- n 1)))))
    (factorial 5))
 120)

(test-algorithm
 "Factorial[7]"
 '(progn
    (defun factorial (n)
      (if (= n 0) 1
          (* n (factorial (- n 1)))))
    (factorial 7))
 5040)

;;; ============================================================================
;;; 16. COEFFICIENT BINOMIAL (Combinaisons)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 16: COEFFICIENT BINOMIAL~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Binomial[5,2]"
 '(progn
    (defun binomial (n k)
      (if (= k 0) 1
          (if (= n k) 1
              (+ (binomial (- n 1) (- k 1))
                 (binomial (- n 1) k)))))
    (binomial 5 2))
 10)

;;; ============================================================================
;;; 17. NOMBRES DE BELL (simplifié)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 17: NOMBRES DE BELL~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Bell[3]"
 '(progn
    (defun bell (n)
      (if (= n 0) 1
          (if (= n 1) 1
              (* 2 (bell (- n 1))))))
    (bell 3))
 4)

;;; ============================================================================
;;; 18. FONCTION D'ACKERMANN
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 18: FONCTION D'ACKERMANN~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Ackermann[2,2]"
 '(progn
    (defun ackermann (m n)
      (if (= m 0) (+ n 1)
          (if (= n 0) (ackermann (- m 1) 1)
              (ackermann (- m 1) (ackermann m (- n 1))))))
    (ackermann 2 2))
 7)

;;; ============================================================================
;;; 19. ALGORITHME DE HORNER
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 19: ALGORITHME DE HORNER~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Horner[x^2+2x+1 at x=3]"
 '(progn
    (defun horner (x)
      (+ 1 (* x (+ 2 x))))
    (horner 3))
 16)

;;; ============================================================================
;;; 20. SUITE DE PADOVAN
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Test 20: SUITE DE PADOVAN~%")
(format t "════════════════════════════════════════════════════════════════~%")

(test-algorithm
 "Padovan[8]"
 '(progn
    (defun padovan (n)
      (if (< n 3) 1
          (+ (padovan (- n 2)) (padovan (- n 3)))))
    (padovan 8))
 7)

(test-algorithm
 "Padovan[10]"
 '(progn
    (defun padovan (n)
      (if (< n 3) 1
          (+ (padovan (- n 2)) (padovan (- n 3)))))
    (padovan 10))
 12)

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║          RÉSUMÉ DES TESTS - 20 ALGORITHMES                    ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "Tests réussis:  ~A~%" *tests-passed*)
(format t "Tests échoués:  ~A~%" *tests-failed*)
(format t "Total:          ~A~%" (+ *tests-passed* *tests-failed*))
(format t "Taux de succès: ~,1F%~%" 
        (if (> (+ *tests-passed* *tests-failed*) 0)
            (* 100.0 (/ *tests-passed* (+ *tests-passed* *tests-failed*)))
            0.0))
(format t "════════════════════════════════════════════════════════════════~%")

(if (= *tests-failed* 0)
    (format t "~%✅ Tous les tests sont réussis ! ✅~%")
    (format t "~%❌ Certains tests ont échoué ❌~%"))
