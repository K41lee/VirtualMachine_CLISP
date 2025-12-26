;;;; test-compiler-bootstrap.lisp
;;;; Tests pour les nouvelles fonctionnalités du compilateur (Phase Bootstrap)
;;;;
;;;; Tests pour:
;;;; - LENGTH : Calcul de longueur de liste
;;;; - NTH : Accès indexé dans une liste
;;;; - Primitives VM : mem-write, mem-read, get-reg

;;; ============================================================================
;;; CHARGEMENT DES MODULES
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║     TESTS COMPILATEUR - PHASE BOOTSTRAP (LENGTH/NTH/VM)         ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/compiler.lisp")
(load "src/loader.lisp")

;;; ============================================================================
;;; TEST 1 : LENGTH - Longueur de liste
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 1 : LENGTH - Calcul de longueur de liste~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Test 1.1 : Liste vide
(format t "~%Test 1.1 : (length nil) = 0~%")
(defparameter *test-length-nil*
  '(progn
     (length nil)))

(defparameter *mips-length-nil* (compile-lisp *test-length-nil*))
(format t "  → Code généré : ~A instructions~%" (length *mips-length-nil*))

(defparameter *vm1* (make-new-vm))
(load-code *vm1* *mips-length-nil*)
(run-vm *vm1*)
(defparameter *result1* (get-register *vm1* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result1*)
(if (= *result1* 0)
    (format t "  ✅ Test 1.1 RÉUSSI~%")
    (format t "  ❌ Test 1.1 ÉCHOUÉ (attendu: 0, obtenu: ~A)~%" *result1*))

;; Test 1.2 : Liste avec quote
;; Note: Le compilateur actuel ne supporte pas directement les listes quotées
;; On va tester avec une liste construite dynamiquement
(format t "~%Test 1.2 : (length (cons 1 (cons 2 (cons 3 nil)))) = 3~%")
(defparameter *test-length-3*
  '(progn
     (length (cons 1 (cons 2 (cons 3 nil))))))

(defparameter *mips-length-3* (compile-lisp *test-length-3*))
(format t "  → Code généré : ~A instructions~%" (length *mips-length-3*))

(defparameter *vm2* (make-new-vm))
(load-code *vm2* *mips-length-3*)
(run-vm *vm2*)
(defparameter *result2* (get-register *vm2* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result2*)
(if (= *result2* 3)
    (format t "  ✅ Test 1.2 RÉUSSI~%")
    (format t "  ❌ Test 1.2 ÉCHOUÉ (attendu: 3, obtenu: ~A)~%" *result2*))

;;; ============================================================================
;;; TEST 2 : NTH - Accès indexé
;;; ============================================================================

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 2 : NTH - Accès au n-ième élément~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Test 2.1 : Premier élément (index 0)
(format t "~%Test 2.1 : (nth 0 (cons 10 (cons 20 (cons 30 nil)))) = 10~%")
(defparameter *test-nth-0*
  '(progn
     (nth 0 (cons 10 (cons 20 (cons 30 nil))))))

(defparameter *mips-nth-0* (compile-lisp *test-nth-0*))
(format t "  → Code généré : ~A instructions~%" (length *mips-nth-0*))

(defparameter *vm3* (make-new-vm))
(load-code *vm3* *mips-nth-0*)
(run-vm *vm3*)
(defparameter *result3* (get-register *vm3* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result3*)
(if (= *result3* 10)
    (format t "  ✅ Test 2.1 RÉUSSI~%")
    (format t "  ❌ Test 2.1 ÉCHOUÉ (attendu: 10, obtenu: ~A)~%" *result3*))

;; Test 2.2 : Troisième élément (index 2)
(format t "~%Test 2.2 : (nth 2 (cons 10 (cons 20 (cons 30 nil)))) = 30~%")
(defparameter *test-nth-2*
  '(progn
     (nth 2 (cons 10 (cons 20 (cons 30 nil))))))

(defparameter *mips-nth-2* (compile-lisp *test-nth-2*))
(format t "  → Code généré : ~A instructions~%" (length *mips-nth-2*))

(defparameter *vm4* (make-new-vm))
(load-code *vm4* *mips-nth-2*)
(run-vm *vm4*)
(defparameter *result4* (get-register *vm4* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result4*)
(if (= *result4* 30)
    (format t "  ✅ Test 2.2 RÉUSSI~%")
    (format t "  ❌ Test 2.2 ÉCHOUÉ (attendu: 30, obtenu: ~A)~%" *result4*))

;;; ============================================================================
;;; TEST 3 : Primitives VM - MEM-WRITE / MEM-READ
;;; ============================================================================

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 3 : Primitives VM - MEM-WRITE et MEM-READ~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Test 3.1 : Écrire puis lire une valeur en mémoire
(format t "~%Test 3.1 : (mem-write 1000 42) puis (mem-read 1000) = 42~%")
(defparameter *test-mem-write-read*
  '(progn
     (mem-write 1000 42)
     (mem-read 1000)))

(defparameter *mips-mem* (compile-lisp *test-mem-write-read*))
(format t "  → Code généré : ~A instructions~%" (length *mips-mem*))
(format t "  → Instructions:~%")
(dolist (instr *mips-mem*)
  (format t "      ~A~%" instr))

(defparameter *vm5* (make-new-vm))
(load-code *vm5* *mips-mem*)
(run-vm *vm5*)
(defparameter *result5* (get-register *vm5* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result5*)
(if (= *result5* 42)
    (format t "  ✅ Test 3.1 RÉUSSI~%")
    (format t "  ❌ Test 3.1 ÉCHOUÉ (attendu: 42, obtenu: ~A)~%" *result5*))

;;; ============================================================================
;;; TEST 4 : Intégration - Fonction utilisant LENGTH
;;; ============================================================================

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 4 : Intégration - Fonction avec LENGTH~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(format t "~%Test 4.1 : Fonction count-elements utilisant LENGTH~%")
(defparameter *test-func-length*
  '(progn
     (defun count-elements (lst)
       (length lst))
     (count-elements (cons 1 (cons 2 (cons 3 (cons 4 nil)))))))

(defparameter *mips-func-length* (compile-lisp *test-func-length*))
(format t "  → Code généré : ~A instructions~%" (length *mips-func-length*))

(defparameter *vm6* (make-new-vm))
(load-code *vm6* *mips-func-length*)
(run-vm *vm6*)
(defparameter *result6* (get-register *vm6* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result6*)
(if (= *result6* 4)
    (format t "  ✅ Test 4.1 RÉUSSI~%")
    (format t "  ❌ Test 4.1 ÉCHOUÉ (attendu: 4, obtenu: ~A)~%" *result6*))

;;; ============================================================================
;;; TEST 5 : Intégration - Fonction utilisant NTH
;;; ============================================================================

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 5 : Intégration - Fonction avec NTH~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(format t "~%Test 5.1 : Fonction get-second utilisant NTH~%")
(defparameter *test-func-nth*
  '(progn
     (defun get-second (lst)
       (nth 1 lst))
     (get-second (cons 100 (cons 200 (cons 300 nil))))))

(defparameter *mips-func-nth* (compile-lisp *test-func-nth*))
(format t "  → Code généré : ~A instructions~%" (length *mips-func-nth*))

(defparameter *vm7* (make-new-vm))
(load-code *vm7* *mips-func-nth*)
(run-vm *vm7*)
(defparameter *result7* (get-register *vm7* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result7*)
(if (= *result7* 200)
    (format t "  ✅ Test 5.1 RÉUSSI~%")
    (format t "  ❌ Test 5.1 ÉCHOUÉ (attendu: 200, obtenu: ~A)~%" *result7*))

;;; ============================================================================
;;; RÉSUMÉ DES TESTS
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                        RÉSUMÉ DES TESTS                          ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(defparameter *tests-passed* 0)
(defparameter *tests-total* 7)

(when (= *result1* 0) (incf *tests-passed*))
(when (= *result2* 3) (incf *tests-passed*))
(when (= *result3* 10) (incf *tests-passed*))
(when (= *result4* 30) (incf *tests-passed*))
(when (= *result5* 42) (incf *tests-passed*))
(when (= *result6* 4) (incf *tests-passed*))
(when (= *result7* 200) (incf *tests-passed*))

(format t "Tests réussis : ~A / ~A~%" *tests-passed* *tests-total*)
(format t "Taux de réussite : ~,1F%~%~%" (* 100.0 (/ *tests-passed* *tests-total*)))

(if (= *tests-passed* *tests-total*)
    (progn
      (format t "✅ TOUS LES TESTS ONT RÉUSSI !~%")
      (format t "~%Les fonctionnalités suivantes sont opérationnelles :~%")
      (format t "  • LENGTH : Calcul de longueur de liste ✓~%")
      (format t "  • NTH : Accès indexé dans une liste ✓~%")
      (format t "  • MEM-WRITE : Écriture en mémoire ✓~%")
      (format t "  • MEM-READ : Lecture en mémoire ✓~%")
      (format t "~%Prêt pour compiler le chargeur !~%"))
    (format t "⚠️  Certains tests ont échoué. Vérifier l'implémentation.~%"))

(format t "~%")
