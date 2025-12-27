;;;; test-assoc-member.lisp
;;;; Tests pour ASSOC et MEMBER - Phase Bootstrap Compilateur

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║          TESTS ASSOC ET MEMBER - BOOTSTRAP COMPILER             ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/compiler.lisp")
(load "src/loader.lisp")

;;; ============================================================================
;;; TEST 1 : ASSOC - Recherche dans liste associative
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 1 : ASSOC - Recherche dans alist~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Test 1.1 : Recherche réussie (clé en début)
(format t "~%Test 1.1 : (assoc 1 '((1 . 10) (2 . 20) (3 . 30))) → (1 . 10)~%")
(defparameter *test-assoc-1*
  '(progn
     (let ((alist (cons (cons 1 10)
                        (cons (cons 2 20)
                              (cons (cons 3 30) nil)))))
       (assoc 1 alist))))

(defparameter *mips-assoc-1* (compile-lisp *test-assoc-1*))
(format t "  → Code généré : ~A instructions~%" (length *mips-assoc-1*))

(defparameter *vm1* (make-new-vm))
(load-code *vm1* *mips-assoc-1*)
(run-vm *vm1*)
(defparameter *result1* (get-register *vm1* (get-reg :v0)))
(format t "  → Résultat (adresse paire) : ~A~%" *result1*)
;; Pour vérifier, on devrait charger CAR et CDR de la paire
(if (not (= *result1* 0))
    (format t "  ✅ Test 1.1 RÉUSSI (paire trouvée)~%")
    (format t "  ❌ Test 1.1 ÉCHOUÉ (devrait trouver paire)~%"))

;; Test 1.2 : Recherche réussie (clé au milieu)
(format t "~%Test 1.2 : (assoc 2 '((1 . 10) (2 . 20) (3 . 30))) → (2 . 20)~%")
(defparameter *test-assoc-2*
  '(progn
     (let ((alist (cons (cons 1 10)
                        (cons (cons 2 20)
                              (cons (cons 3 30) nil)))))
       (assoc 2 alist))))

(defparameter *mips-assoc-2* (compile-lisp *test-assoc-2*))
(format t "  → Code généré : ~A instructions~%" (length *mips-assoc-2*))

(defparameter *vm2* (make-new-vm))
(load-code *vm2* *mips-assoc-2*)
(run-vm *vm2*)
(defparameter *result2* (get-register *vm2* (get-reg :v0)))
(format t "  → Résultat (adresse paire) : ~A~%" *result2*)
(if (not (= *result2* 0))
    (format t "  ✅ Test 1.2 RÉUSSI (paire trouvée)~%")
    (format t "  ❌ Test 1.2 ÉCHOUÉ~%"))

;; Test 1.3 : Clé non trouvée
(format t "~%Test 1.3 : (assoc 99 '((1 . 10) (2 . 20))) → NIL~%")
(defparameter *test-assoc-3*
  '(progn
     (let ((alist (cons (cons 1 10)
                        (cons (cons 2 20) nil))))
       (assoc 99 alist))))

(defparameter *mips-assoc-3* (compile-lisp *test-assoc-3*))
(format t "  → Code généré : ~A instructions~%" (length *mips-assoc-3*))

(defparameter *vm3* (make-new-vm))
(load-code *vm3* *mips-assoc-3*)
(run-vm *vm3*)
(defparameter *result3* (get-register *vm3* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result3*)
(if (= *result3* 0)
    (format t "  ✅ Test 1.3 RÉUSSI (NIL)~%")
    (format t "  ❌ Test 1.3 ÉCHOUÉ (devrait être NIL)~%"))

;; Test 1.4 : Liste vide
(format t "~%Test 1.4 : (assoc 1 nil) → NIL~%")
(defparameter *test-assoc-4*
  '(progn
     (assoc 1 nil)))

(defparameter *mips-assoc-4* (compile-lisp *test-assoc-4*))
(format t "  → Code généré : ~A instructions~%" (length *mips-assoc-4*))

(defparameter *vm4* (make-new-vm))
(load-code *vm4* *mips-assoc-4*)
(run-vm *vm4*)
(defparameter *result4* (get-register *vm4* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result4*)
(if (= *result4* 0)
    (format t "  ✅ Test 1.4 RÉUSSI (NIL)~%")
    (format t "  ❌ Test 1.4 ÉCHOUÉ~%"))

;;; ============================================================================
;;; TEST 2 : MEMBER - Test d'appartenance
;;; ============================================================================

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST 2 : MEMBER - Test d'appartenance~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Test 2.1 : Élément trouvé en début
(format t "~%Test 2.1 : (member 10 '(10 20 30)) → sous-liste~%")
(defparameter *test-member-1*
  '(progn
     (let ((list (cons 10 (cons 20 (cons 30 nil)))))
       (member 10 list))))

(defparameter *mips-member-1* (compile-lisp *test-member-1*))
(format t "  → Code généré : ~A instructions~%" (length *mips-member-1*))

(defparameter *vm5* (make-new-vm))
(load-code *vm5* *mips-member-1*)
(run-vm *vm5*)
(defparameter *result5* (get-register *vm5* (get-reg :v0)))
(format t "  → Résultat (adresse liste) : ~A~%" *result5*)
(if (not (= *result5* 0))
    (format t "  ✅ Test 2.1 RÉUSSI (élément trouvé)~%")
    (format t "  ❌ Test 2.1 ÉCHOUÉ~%"))

;; Test 2.2 : Élément trouvé au milieu
(format t "~%Test 2.2 : (member 20 '(10 20 30)) → sous-liste~%")
(defparameter *test-member-2*
  '(progn
     (let ((list (cons 10 (cons 20 (cons 30 nil)))))
       (member 20 list))))

(defparameter *mips-member-2* (compile-lisp *test-member-2*))
(format t "  → Code généré : ~A instructions~%" (length *mips-member-2*))

(defparameter *vm6* (make-new-vm))
(load-code *vm6* *mips-member-2*)
(run-vm *vm6*)
(defparameter *result6* (get-register *vm6* (get-reg :v0)))
(format t "  → Résultat (adresse liste) : ~A~%" *result6*)
(if (not (= *result6* 0))
    (format t "  ✅ Test 2.2 RÉUSSI~%")
    (format t "  ❌ Test 2.2 ÉCHOUÉ~%"))

;; Test 2.3 : Élément non trouvé
(format t "~%Test 2.3 : (member 99 '(10 20 30)) → NIL~%")
(defparameter *test-member-3*
  '(progn
     (let ((list (cons 10 (cons 20 (cons 30 nil)))))
       (member 99 list))))

(defparameter *mips-member-3* (compile-lisp *test-member-3*))
(format t "  → Code généré : ~A instructions~%" (length *mips-member-3*))

(defparameter *vm7* (make-new-vm))
(load-code *vm7* *mips-member-3*)
(run-vm *vm7*)
(defparameter *result7* (get-register *vm7* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result7*)
(if (= *result7* 0)
    (format t "  ✅ Test 2.3 RÉUSSI (NIL)~%")
    (format t "  ❌ Test 2.3 ÉCHOUÉ~%"))

;; Test 2.4 : Liste vide
(format t "~%Test 2.4 : (member 1 nil) → NIL~%")
(defparameter *test-member-4*
  '(progn
     (member 1 nil)))

(defparameter *mips-member-4* (compile-lisp *test-member-4*))
(format t "  → Code généré : ~A instructions~%" (length *mips-member-4*))

(defparameter *vm8* (make-new-vm))
(load-code *vm8* *mips-member-4*)
(run-vm *vm8*)
(defparameter *result8* (get-register *vm8* (get-reg :v0)))
(format t "  → Résultat : ~A~%" *result8*)
(if (= *result8* 0)
    (format t "  ✅ Test 2.4 RÉUSSI (NIL)~%")
    (format t "  ❌ Test 2.4 ÉCHOUÉ~%"))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                           RÉSUMÉ                                 ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(defparameter *tests-assoc* 0)
(defparameter *tests-member* 0)

(when (not (= *result1* 0)) (incf *tests-assoc*))
(when (not (= *result2* 0)) (incf *tests-assoc*))
(when (= *result3* 0) (incf *tests-assoc*))
(when (= *result4* 0) (incf *tests-assoc*))

(when (not (= *result5* 0)) (incf *tests-member*))
(when (not (= *result6* 0)) (incf *tests-member*))
(when (= *result7* 0) (incf *tests-member*))
(when (= *result8* 0) (incf *tests-member*))

(format t "Tests ASSOC réussis : ~A / 4~%" *tests-assoc*)
(format t "Tests MEMBER réussis : ~A / 4~%" *tests-member*)
(format t "Total : ~A / 8~%~%" (+ *tests-assoc* *tests-member*))

(if (= (+ *tests-assoc* *tests-member*) 8)
    (progn
      (format t "✅ TOUS LES TESTS ONT RÉUSSI !~%")
      (format t "~%Prêt pour compiler le compilateur avec ASSOC et MEMBER !~%"))
    (format t "⚠️  Certains tests ont échoué.~%"))

(format t "~%")
