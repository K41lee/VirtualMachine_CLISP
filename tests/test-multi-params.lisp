;;; Test multi-paramètres pour vérifier que les fonctions avec plusieurs paramètres fonctionnent

(sb-ext:unlock-package :common-lisp)  ; Déverrouiller pour permettre la redéfinition

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%╔════════════════════════════════════════╗~%")
(format t "║   TESTS MULTI-PARAMÈTRES              ║~%")
(format t "╚════════════════════════════════════════╝~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-multi-param (name code expected)
  "Teste une fonction multi-paramètres"
  (format t "~%Test: ~A~%" name)
  (handler-case
      (let* ((compiled (compile-lisp-to-mips-simplified `(progn ,@code)))
             (vm (make-new-vm)))
        (if (null compiled)
            (progn
              (format t "  ❌ ÉCHEC COMPILATION~%")
              (incf *tests-failed*))
            (progn
              (load-code vm compiled)
              (run-vm vm)
              (let ((result (get-register vm :$V0)))
                (if (equal result expected)
                    (progn
                      (format t "  ✅ SUCCÈS: ~A~%" result)
                      (incf *tests-passed*))
                    (progn
                      (format t "  ❌ ÉCHEC: attendu ~A, obtenu ~A~%" expected result)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

;; Test 1: Fonction simple à 2 paramètres
(test-multi-param
  "max-of-two(42, 17)"
  '((defun max-of-two (a b)
      (if (> a b) a b))
    (max-of-two 42 17))
  42)

;; Test 2: PGCD avec récursion et 2 paramètres
(test-multi-param
  "gcd(48, 18)"
  '((defun gcd (a b)
      (if (= b 0)
          a
          (gcd b (mod a b))))
    (gcd 48 18))
  6)

;; Test 3: Addition simple
(test-multi-param
  "add(10, 32)"
  '((defun add (a b)
      (+ a b))
    (add 10 32))
  42)

;; Test 4: Puissance (récursif)
(test-multi-param
  "power(2, 10)"
  '((defun power (base exp)
      (if (= exp 0)
          1
          (* base (power base (- exp 1)))))
    (power 2 10))
  1024)

;; Test 5: Ackermann (double récursion)
(test-multi-param
  "ack(3, 2)"
  '((defun ack (m n)
      (cond
        ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (t (ack (- m 1) (ack m (- n 1))))))
    (ack 3 2))
  29)

;; Résultats
(format t "~%╔════════════════════════════════════════╗~%")
(format t "║   RÉSULTATS                           ║~%")
(format t "╚════════════════════════════════════════╝~%")
(format t "Tests réussis: ~A~%" *tests-passed*)
(format t "Tests échoués: ~A~%" *tests-failed*)
(format t "Total: ~A~%" (+ *tests-passed* *tests-failed*))
(format t "Taux de réussite: ~,1F%~%" 
        (* 100.0 (/ *tests-passed* (+ *tests-passed* *tests-failed*))))

