;;;; TEST ALGORITHMES AVANCÉS - Version simplifiée sans LET
;;;;

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-algo (name code input expected)
  "Teste un algorithme"
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
                      (format t "  ❌ = ~A (attendu ~A) [~:D instr]~%" result expected instr)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST ALGORITHMES AVANCÉS - Sans structures complexes      ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; TOUR DE HANOI
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 1. TOUR DE HANOI (mouvements = 2^n - 1)~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "hanoi(3)"
 '(defun hanoi (n)
    (if (= n 0) 0
        (if (= n 1) 1
            (+ 1 (* 2 (hanoi (- n 1)))))))
 3
 7)

(test-algo
 "hanoi(5)"
 '(defun hanoi (n)
    (if (= n 0) 0
        (if (= n 1) 1
            (+ 1 (* 2 (hanoi (- n 1)))))))
 5
 31)

(test-algo
 "hanoi(8)"
 '(defun hanoi (n)
    (if (= n 0) 0
        (if (= n 1) 1
            (+ 1 (* 2 (hanoi (- n 1)))))))
 8
 255)

;;; PADOVAN
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 2. SUITE DE PADOVAN (P(n) = P(n-2) + P(n-3))~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "padovan(8)"
 '(defun padovan (n)
    (if (< n 3) 1
        (+ (padovan (- n 2))
           (padovan (- n 3)))))
 8
 7)

(test-algo
 "padovan(10)"
 '(defun padovan (n)
    (if (< n 3) 1
        (+ (padovan (- n 2))
           (padovan (- n 3)))))
 10
 12)

;;; PROFONDEUR ARBRE (log2)
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 3. PROFONDEUR D'ARBRE BINAIRE (log2)~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "depth(8)"
 '(defun depth (n)
    (if (< n 2) 0
        (+ 1 (depth (/ n 2)))))
 8
 3)

(test-algo
 "depth(16)"
 '(defun depth (n)
    (if (< n 2) 0
        (+ 1 (depth (/ n 2)))))
 16
 4)

(test-algo
 "depth(64)"
 '(defun depth (n)
    (if (< n 2) 0
        (+ 1 (depth (/ n 2)))))
 64
 6)

;;; SOMME CHIFFRES
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 4. SOMME DES CHIFFRES~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "sum-digits(12345)"
 '(defun sum-digits (n)
    (if (= n 0) 0
        (+ (% n 10) (sum-digits (/ n 10)))))
 12345
 15)

(test-algo
 "sum-digits(99999)"
 '(defun sum-digits (n)
    (if (= n 0) 0
        (+ (% n 10) (sum-digits (/ n 10)))))
 99999
 45)

;;; FACTORIELLE
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 5. FACTORIELLE~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "fact(8)"
 '(defun fact (n)
    (if (< n 2) 1
        (* n (fact (- n 1)))))
 8
 40320)

(test-algo
 "fact(11)"
 '(defun fact (n)
    (if (< n 2) 1
        (* n (fact (- n 1)))))
 11
 39916800)

;;; FIBONACCI
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 6. FIBONACCI~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "fib(15)"
 '(defun fib (n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))))
 15
 610)

(test-algo
 "fib(18)"
 '(defun fib (n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))))
 18
 2584)

;;; TRIANGULAIRE
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 7. NOMBRES TRIANGULAIRES (1+2+...+n)~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "triangle(20)"
 '(defun triangle (n)
    (if (= n 0) 0
        (+ n (triangle (- n 1)))))
 20
 210)

(test-algo
 "triangle(50)"
 '(defun triangle (n)
    (if (= n 0) 0
        (+ n (triangle (- n 1)))))
 50
 1275)

;;; NOMBRES PENTAGONAUX
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 8. NOMBRES PENTAGONAUX (n(3n-1)/2)~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "penta(5)"
 '(defun penta (n)
    (/ (* n (- (* 3 n) 1)) 2))
 5
 35)

(test-algo
 "penta(10)"
 '(defun penta (n)
    (/ (* n (- (* 3 n) 1)) 2))
 10
 145)

(test-algo
 "penta(20)"
 '(defun penta (n)
    (/ (* n (- (* 3 n) 1)) 2))
 20
 590)

;;; HORNER (évaluation polynôme)
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 9. ALGORITHME DE HORNER~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "horner[x^2+2x+1](3)"
 '(defun horner (x)
    (+ (* (+ x 2) x) 1))
 3
 16)

(test-algo
 "horner[x^3+x^2+x+1](2)"
 '(defun horner (x)
    (+ (* (+ (* (+ x 1) x) 1) x) 1))
 2
 15)

(test-algo
 "horner[2x^2+3x+4](5)"
 '(defun horner (x)
    (+ (* (+ (* 2 x) 3) x) 4))
 5
 69)

;;; TRIBONACCI
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " 10. TRIBONACCI (triple récursion)~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo
 "tribonacci(5)"
 '(defun tribonacci (n)
    (if (< n 2) n
        (if (= n 2) 1
            (+ (tribonacci (- n 1))
               (tribonacci (- n 2))
               (tribonacci (- n 3))))))
 5
 7)

(test-algo
 "tribonacci(8)"
 '(defun tribonacci (n)
    (if (< n 2) n
        (if (= n 2) 1
            (+ (tribonacci (- n 1))
               (tribonacci (- n 2))
               (tribonacci (- n 3))))))
 8
 44)

;;; RÉSUMÉ
(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                 RÉSUMÉ DES TESTS                             ║~%")
(format t "╠════════════════════════════════════════════════════════════════╣~%")
(format t "║  Tests réussis:  ~3D                                          ║~%" *tests-passed*)
(format t "║  Tests échoués:  ~3D                                          ║~%" *tests-failed*)
(format t "║  Total:          ~3D                                          ║~%" (+ *tests-passed* *tests-failed*))
(format t "║  Taux de succès: ~5,1F%                                      ║~%" 
        (* 100.0 (/ *tests-passed* (+ *tests-passed* *tests-failed*))))
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(if (= *tests-failed* 0)
    (format t "~%✅ TOUS LES TESTS SONT PASSÉS ! ✅~%~%")
    (format t "~%⚠️  ~A test(s) échoué(s)~%~%" *tests-failed*))

(quit)
