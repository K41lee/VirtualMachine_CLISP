;;;; ============================================================================
;;;; TEST: Vérification de la correction du bug while + paramètres
;;;; ============================================================================
;;;;
;;;; BUG IDENTIFIÉ: compile-comparison utilisait $S2 et $S3 comme registres
;;;; temporaires, ce qui écrasait les paramètres de fonction stockés dans ces
;;;; registres.
;;;;
;;;; CORRECTION: compile-comparison utilise maintenant $T0 et $T1, préservant
;;;; ainsi les paramètres dans $S0-$S3.
;;;;
;;;; Ce test vérifie que le bug est corrigé.
;;;; ============================================================================

(load "main.lisp")

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(format t "║  TEST: Bug while + paramètres (CORRIGÉ)               ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%~%")

;;; ----------------------------------------------------------------------------
;;; TEST 1: Boucle while avec constante (devait déjà fonctionner)
;;; ----------------------------------------------------------------------------

(format t "TEST 1: While avec constante~%")
(format t "────────────────────────────────────────────────────────~%")

(defparameter *test1-code*
  '(progn
     (defun count-to-5 ()
       (let ((i 0))
         (while (< i 5)
           (setq i (+ i 1)))
         i))
     (count-to-5)))

(defparameter *result1* (compile-and-run *test1-code*))
(defparameter *value1* (get-register *result1* *reg-v0*))

(format t "Résultat: ~A~%" *value1*)
(if (= *value1* 5)
    (format t "✓ TEST 1 RÉUSSI~%~%")
    (format t "✗ TEST 1 ÉCHOUÉ (attendu: 5)~%~%"))

;;; ----------------------------------------------------------------------------
;;; TEST 2: Boucle while avec paramètre (le bug)
;;; ----------------------------------------------------------------------------

(format t "TEST 2: While avec paramètre (BUG CORRIGÉ)~%")
(format t "────────────────────────────────────────────────────────~%")

(defparameter *test2-code*
  '(progn
     (defun count-to-n (n)
       (let ((i 0))
         (while (< i n)
           (setq i (+ i 1)))
         i))
     (count-to-n 7)))

(defparameter *result2* (compile-and-run *test2-code*))
(defparameter *value2* (get-register *result2* *reg-v0*))

(format t "Résultat: ~A~%" *value2*)
(if (= *value2* 7)
    (format t "✓ TEST 2 RÉUSSI (bug corrigé!)~%~%")
    (format t "✗ TEST 2 ÉCHOUÉ (attendu: 7, bug toujours présent)~%~%"))

;;; ----------------------------------------------------------------------------
;;; TEST 3: Loader simple compilable
;;; ----------------------------------------------------------------------------

(format t "TEST 3: Loader simple (mem-read/mem-write)~%")
(format t "────────────────────────────────────────────────────────~%")

(defparameter *test3-code*
  '(progn
     (defun simple-copy (dest src count)
       (let ((i 0))
         (while (< i count)
           (mem-write (+ dest i) (mem-read (+ src i)))
           (setq i (+ i 1)))
         count))
     (simple-copy 1000 2000 5)))

(defparameter *result3* (compile-and-run *test3-code*))
(defparameter *value3* (get-register *result3* *reg-v0*))

(format t "Résultat: ~A~%" *value3*)
(if (= *value3* 5)
    (format t "✓ TEST 3 RÉUSSI (loader compilable!)~%~%")
    (format t "✗ TEST 3 ÉCHOUÉ (attendu: 5)~%~%"))

;;; ----------------------------------------------------------------------------
;;; RÉSUMÉ
;;; ----------------------------------------------------------------------------

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(if (and (= *value1* 5) (= *value2* 7) (= *value3* 5))
    (format t "║  ✓✓✓ TOUS LES TESTS RÉUSSIS ✓✓✓                        ║~%")
    (format t "║  ✗✗✗ CERTAINS TESTS ONT ÉCHOUÉ ✗✗✗                     ║~%"))
(format t "╚════════════════════════════════════════════════════════╝~%~%")

(format t "CORRECTION APPLIQUÉE:~%")
(format t "  - src/compiler.lisp: compile-comparison~%")
(format t "  - Remplacé $S2/$S3 par $T0/$T1~%")
(format t "  - Les paramètres dans $S0-$S3 sont préservés~%~%")

(format t "Test terminé.~%")
