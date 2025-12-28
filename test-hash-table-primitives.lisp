;;;; test-hash-table-primitives.lisp
;;;; Test des primitives hash-table dans la VM

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║        TEST DES PRIMITIVES HASH-TABLE                            ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

;;; ============================================================================
;;; TEST 1 : Création et accès basique
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 1 : Création d'une hash-table et accès basique~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(defparameter *test1-source*
  '(defun test-hash-basic ()
     (let ((ht (vm-make-hash-table :test 'equal)))
       (vm-hash-set ht 42 100)
       (vm-gethash 42 ht))))

(format t "  → Compilation du test...~%")
(defparameter *test1-code* (compile-lisp *test1-source*))
(format t "  ✓ Compilé : ~A instructions~%~%" (length *test1-code*))

(format t "  → Exécution...~%")
(defparameter *vm1* (make-new-vm :verbose nil))
(load-code *vm1* *test1-code* :verbose nil)
(set-register *vm1* (get-reg :pc) (calculate-code-start *vm1*))
(run-vm *vm1* :max-instructions 10000)
(defparameter *result1* (get-register *vm1* *reg-v0*))

(format t "  → Résultat : ~A~%" *result1*)
(if (= *result1* 100)
    (format t "  ✅ TEST 1 RÉUSSI!~%~%")
    (format t "  ❌ TEST 1 ÉCHOUÉ (attendu: 100, obtenu: ~A)~%~%" *result1*))

;;; ============================================================================
;;; TEST 2 : Multiples clés
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 2 : Hash-table avec plusieurs clés~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(defparameter *test2-source*
  '(defun test-hash-multiple ()
     (let ((ht (vm-make-hash-table :test 'equal)))
       (vm-hash-set ht 1 10)
       (vm-hash-set ht 2 20)
       (vm-hash-set ht 3 30)
       (+ (+ (vm-gethash 1 ht) (vm-gethash 2 ht)) (vm-gethash 3 ht)))))

(format t "  → Compilation du test...~%")
(defparameter *test2-code* (compile-lisp *test2-source*))
(format t "  ✓ Compilé : ~A instructions~%~%" (length *test2-code*))

(format t "  → Exécution...~%")
(defparameter *vm2* (make-new-vm :verbose nil))
(load-code *vm2* *test2-code* :verbose nil)
(set-register *vm2* (get-reg :pc) (calculate-code-start *vm2*))
(run-vm *vm2* :max-instructions 10000)
(defparameter *result2* (get-register *vm2* *reg-v0*))

(format t "  → Résultat : ~A~%" *result2*)
(if (= *result2* 60)
    (format t "  ✅ TEST 2 RÉUSSI!~%~%")
    (format t "  ❌ TEST 2 ÉCHOUÉ (attendu: 60, obtenu: ~A)~%~%" *result2*))

;;; ============================================================================
;;; TEST 3 : Hash-table avec boucle
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 3 : Hash-table remplie dans une boucle~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(defparameter *test3-source*
  '(defun test-hash-loop ()
     (let ((ht (vm-make-hash-table :test 'equal))
           (i 0))
       (while (< i 5)
         (vm-hash-set ht i (* i 10))
         (setq i (+ i 1)))
       (vm-hash-table-count ht))))

(format t "  → Compilation du test...~%")
(defparameter *test3-code* (compile-lisp *test3-source*))
(format t "  ✓ Compilé : ~A instructions~%~%" (length *test3-code*))

(format t "  → Exécution...~%")
(defparameter *vm3* (make-new-vm :verbose nil))
(load-code *vm3* *test3-code* :verbose nil)
(set-register *vm3* (get-reg :pc) (calculate-code-start *vm3*))
(run-vm *vm3* :max-instructions 10000)
(defparameter *result3* (get-register *vm3* *reg-v0*))

(format t "  → Résultat : ~A~%" *result3*)
(if (= *result3* 5)
    (format t "  ✅ TEST 3 RÉUSSI!~%~%")
    (format t "  ❌ TEST 3 ÉCHOUÉ (attendu: 5, obtenu: ~A)~%~%" *result3*))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(defparameter *tests-passed* 0)
(when (= *result1* 100) (incf *tests-passed*))
(when (= *result2* 60) (incf *tests-passed*))
(when (= *result3* 5) (incf *tests-passed*))

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                      RÉSUMÉ DES TESTS                            ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║                                                                  ║~%")
(format t "║  Tests réussis : ~A / 3~48T║~%" *tests-passed*)
(format t "║                                                                  ║~%")

(if (= *tests-passed* 3)
    (progn
      (format t "║  ╔════════════════════════════════════════════════════════╗  ║~%")
      (format t "║  ║         ✓✓✓ TOUS LES TESTS RÉUSSIS! ✓✓✓               ║  ║~%")
      (format t "║  ║                                                        ║  ║~%")
      (format t "║  ║  Les primitives hash-table fonctionnent correctement! ║  ║~%")
      (format t "║  ╚════════════════════════════════════════════════════════╝  ║~%"))
    (progn
      (format t "║  ╔════════════════════════════════════════════════════════╗  ║~%")
      (format t "║  ║              ⚠ CERTAINS TESTS ONT ÉCHOUÉ              ║  ║~%")
      (format t "║  ╚════════════════════════════════════════════════════════╝  ║~%")))

(format t "║                                                                  ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Tests terminés.~%~%")
