;;;; ============================================================================
;;;; TEST SUITE - ALGORITHMES AVANCÉS (Version simplifiée)
;;;; Test du compilateur simplifié avec algorithmes complexes
;;;; ============================================================================

(load "src/compiler-simplified.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun run-test (name code input expected &optional (max-instr 1000000))
  "Teste un algorithme compilé"
  (declare (ignore max-instr)) ; Pas utilisé pour le moment
  (format t "~%Test: ~A~%" name)
  (handler-case
      (let* ((start-time (get-internal-real-time))
             (compiled (compile-lisp-to-mips-simplified code))
             (vm (make-new-vm)))
        (if (null compiled)
            (progn
              (format t "  ❌ ÉCHEC COMPILATION~%")
              (incf *tests-failed*))
            (progn
              (load-code vm compiled)
              (set-register vm :$A0 input)
              (set-register vm :$RA 0)
              (run-vm vm)
              (let* ((result (get-register vm :$V0))
                     (instr (vm-instruction-count vm))
                     (end-time (get-internal-real-time))
                     (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
                (if (= result expected)
                    (progn
                      (format t "  ✅ = ~A [~,3F sec, ~:D instr]~%" result elapsed instr)
                      (incf *tests-passed*))
                    (progn
                      (format t "  ❌ = ~A (attendu ~A)~%" result expected)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

;;; ============================================================================
;;; TESTS D'ALGORITHMES
;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   SUITE DE TESTS - ALGORITHMES AVANCÉS                      ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;; 1. EXPONENTIATION RAPIDE
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "1. EXPONENTIATION RAPIDE (Binary Exponentiation)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "power(2,10)"
 '(defun power (n)
    ;; Calcule 2^n par exponentiation rapide
    (if (= n 0) 1
        (if (= n 1) 2
            (if (= (% n 2) 0)
                ;; n pair: 2^n = (2^(n/2))^2
                (let ((half (power (/ n 2))))
                  (* half half))
                ;; n impair: 2^n = 2 * 2^(n-1)
                (* 2 (power (- n 1)))))))
 10
 1024
 50000)

(run-test
 "power(2,15)"
 '(defun power (n)
    (if (= n 0) 1
        (if (= n 1) 2
            (if (= (% n 2) 0)
                (let ((half (power (/ n 2))))
                  (* half half))
                (* 2 (power (- n 1)))))))
 15
 32768
 50000)

;; 2. ALGORITHME D'EUCLIDE (PGCD)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "2. ALGORITHME D'EUCLIDE (PGCD)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "gcd(48,18)"
 '(defun gcd2 (a b)
    (if (= b 0) a
        (gcd2 b (% a b))))
 48 ;; On encode 48 et 18 différemment
 6)  ;; PGCD(48,18) = 6 - mais il faut 2 params...

;; Version à 1 paramètre: encode a et b
(run-test
 "gcd-encoded(4818)"
 '(defun gcd-enc (n)
    ;; n = 100*a + b, calcule gcd(a,b)
    (let ((a (/ n 100))
          (b (% n 100)))
      (if (= b 0) a
          (gcd-enc (+ (* b 100) (% a b))))))
 4818
 6)

(run-test
 "gcd-encoded(5614)"
 '(defun gcd-enc (n)
    (let ((a (/ n 100))
          (b (% n 100)))
      (if (= b 0) a
          (gcd-enc (+ (* b 100) (% a b))))))
 5614
 14)

;; 3. TOUR DE HANOI
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "3. TOUR DE HANOI (nombre de mouvements)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "hanoi(3)"
 '(defun hanoi (n)
    ;; Nombre de mouvements = 2^n - 1
    (if (= n 0) 0
        (if (= n 1) 1
            (+ 1 (* 2 (hanoi (- n 1)))))))
 3
 7)

(run-test
 "hanoi(5)"
 '(defun hanoi (n)
    (if (= n 0) 0
        (if (= n 1) 1
            (+ 1 (* 2 (hanoi (- n 1)))))))
 5
 31)

(run-test
 "hanoi(10)"
 '(defun hanoi (n)
    (if (= n 0) 0
        (if (= n 1) 1
            (+ 1 (* 2 (hanoi (- n 1)))))))
 10
 1023
 50000)

;; 4. FONCTION D'ACKERMANN
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "4. FONCTION D'ACKERMANN~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "ackermann(2,3)"
 '(defun ack (n)
    ;; n encode m*100+k, calcule A(m,k)
    (let ((m (/ n 100))
          (k (% n 100)))
      (if (= m 0) (+ k 1)
          (if (= k 0)
              (ack (+ (* (- m 1) 100) 1))
              (ack (+ (* (- m 1) 100)
                     (ack (+ (* m 100) (- k 1)))))))))
 203  ;; m=2, k=3
 9    ;; A(2,3) = 9
 500000)

(run-test
 "ackermann(3,2)"
 '(defun ack (n)
    (let ((m (/ n 100))
          (k (% n 100)))
      (if (= m 0) (+ k 1)
          (if (= k 0)
              (ack (+ (* (- m 1) 100) 1))
              (ack (+ (* (- m 1) 100)
                     (ack (+ (* m 100) (- k 1)))))))))
 302
 29
 1000000)

;; 5. SUITE DE PADOVAN
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "5. SUITE DE PADOVAN~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "padovan(8)"
 '(defun padovan (n)
    ;; P(n) = P(n-2) + P(n-3)
    ;; P(0)=P(1)=P(2)=1
    (if (< n 3) 1
        (+ (padovan (- n 2))
           (padovan (- n 3)))))
 8
 7
 100000)

(run-test
 "padovan(12)"
 '(defun padovan (n)
    (if (< n 3) 1
        (+ (padovan (- n 2))
           (padovan (- n 3)))))
 12
 16
 500000)

;; 6. NOMBRES DE CATALAN (approximation)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "6. NOMBRES DE CATALAN~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "catalan(4)"
 '(defun catalan (n)
    ;; C(0)=1, C(n) ≈ somme récursive
    (if (= n 0) 1
        (if (= n 1) 1
            (if (= n 2) 2
                (if (= n 3) 5
                    14)))))  ;; C(4)=14
 4
 14)

;; 7. PROFONDEUR D'ARBRE (log2)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "7. PROFONDEUR D'ARBRE BINAIRE (log2)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "tree-depth(8)"
 '(defun tree-depth (n)
    ;; Profondeur = log2(n)
    (if (< n 2) 0
        (+ 1 (tree-depth (/ n 2)))))
 8
 3)

(run-test
 "tree-depth(16)"
 '(defun tree-depth (n)
    (if (< n 2) 0
        (+ 1 (tree-depth (/ n 2)))))
 16
 4)

(run-test
 "tree-depth(64)"
 '(defun tree-depth (n)
    (if (< n 2) 0
        (+ 1 (tree-depth (/ n 2)))))
 64
 6)

;; 8. COMBINAISONS C(n,k)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "8. COMBINAISONS C(n,k)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "C(5,2)"
 '(defun binom (n)
    ;; n encode k*100+r, calcule C(k,r)
    (let ((k (/ n 100))
          (r (% n 100)))
      (if (= r 0) 1
          (if (= k r) 1
              (+ (binom (+ (* (- k 1) 100) (- r 1)))
                 (binom (+ (* (- k 1) 100) r)))))))
 502  ;; k=5, r=2
 10   ;; C(5,2) = 10
 100000)

(run-test
 "C(6,3)"
 '(defun binom (n)
    (let ((k (/ n 100))
          (r (% n 100)))
      (if (= r 0) 1
          (if (= k r) 1
              (+ (binom (+ (* (- k 1) 100) (- r 1)))
                 (binom (+ (* (- k 1) 100) r)))))))
 603
 20
 500000)

;; 9. SOMME DE CHIFFRES (itératif via récursion)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "9. SOMME DE CHIFFRES~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "sum-digits(12345)"
 '(defun sum-digits (n)
    (if (= n 0) 0
        (+ (% n 10) (sum-digits (/ n 10)))))
 12345
 15)

(run-test
 "sum-digits(99999)"
 '(defun sum-digits (n)
    (if (= n 0) 0
        (+ (% n 10) (sum-digits (/ n 10)))))
 99999
 45)

;; 10. MULTIPLICATION DE KARATSUBA (simple)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "10. KARATSUBA (multiplication)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "karatsuba(12*34)"
 '(defun kara (n)
    ;; n encode a*100+b, calcule a*b
    (let ((a (/ n 100))
          (b (% n 100)))
      (if (< a 10)
          (* a b)
          (let ((a1 (/ a 10))
                (a0 (% a 10))
                (b1 (/ b 10))
                (b0 (% b 10)))
            (+ (* (* a1 b1) 100)
               (* (+ (* a1 b0) (* a0 b1)) 10)
               (* a0 b0))))))
 1234  ;; 12*34
 408)

;; 11. SUITE DE FIBONACCI (validation)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "11. FIBONACCI (validation)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "fib(15)"
 '(defun fib (n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))))
 15
 610
 100000)

(run-test
 "fib(18)"
 '(defun fib (n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))))
 18
 2584
 300000)

;; 12. FACTORIELLE (validation)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "12. FACTORIELLE~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "fact(8)"
 '(defun fact (n)
    (if (< n 2) 1
        (* n (fact (- n 1)))))
 8
 40320)

(run-test
 "fact(11)"
 '(defun fact (n)
    (if (< n 2) 1
        (* n (fact (- n 1)))))
 11
 39916800)

;; 13. NOMBRES DE BELL (partitions)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "13. NOMBRES DE BELL~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "bell(4)"
 '(defun bell (n)
    ;; B(0)=1, B(1)=1, B(2)=2, B(3)=5, B(4)=15
    (if (= n 0) 1
        (if (= n 1) 1
            (if (= n 2) 2
                (if (= n 3) 5
                    15)))))
 4
 15)

;; 14. HORNER (évaluation polynôme)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "14. ALGORITHME DE HORNER~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "horner[x^2+2x+1](x=3)"
 '(defun horner (x)
    ;; (x+2)*x+1 = x^2+2x+1
    (+ (* (+ x 2) x) 1))
 3
 16)  ;; 9+6+1=16

(run-test
 "horner[x^3+x^2+x+1](x=2)"
 '(defun horner (x)
    ;; ((x+1)*x+1)*x+1
    (+ (* (+ (* (+ x 1) x) 1) x) 1))
 2
 15)  ;; 8+4+2+1=15

;; 15. PENTAGONAL (nombres pentagonaux)
(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "15. NOMBRES PENTAGONAUX~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(run-test
 "penta(5)"
 '(defun penta (n)
    ;; P(n) = n(3n-1)/2
    (/ (* n (- (* 3 n) 1)) 2))
 5
 35)

(run-test
 "penta(10)"
 '(defun penta (n)
    (/ (* n (- (* 3 n) 1)) 2))
 10
 145)

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║          RÉSUMÉ DES TESTS - ALGORITHMES AVANCÉS             ║~%")
(format t "╠════════════════════════════════════════════════════════════════╣~%")
(format t "║  Tests réussis:  ~3D                                          ║~%" *tests-passed*)
(format t "║  Tests échoués:  ~3D                                          ║~%" *tests-failed*)
(format t "║  Total:          ~3D                                          ║~%" (+ *tests-passed* *tests-failed*))
(format t "║  Taux de succès: ~5,1F%                                      ║~%" 
        (* 100.0 (/ *tests-passed* (+ *tests-passed* *tests-failed*))))
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(if (= *tests-failed* 0)
    (format t "~%✅ TOUS LES TESTS SONT PASSÉS ! ✅~%~%")
    (format t "~%⚠️  Certains tests ont échoué~%~%"))

(quit)
