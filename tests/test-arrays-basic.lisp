(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TESTS BASIQUES DES OPÉRATIONS TABLEAUX                      ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defun run-test (name code expected)
  (format t "~%Test: ~A~%" name)
  (handler-case
      (let* ((compiled (compile-lisp-to-mips-simplified code))
             (vm (make-new-vm)))
        (load-code vm compiled)
        (run-vm vm)
        (let ((result (get-register vm :$V0)))
          (if (equal result expected)
              (format t "  ✅ = ~A~%" result)
              (format t "  ❌ Attendu ~A, obtenu ~A~%" expected result))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e))))

;; Test 1: Créer un tableau
(run-test "Créer tableau"
          '(make-array 5)
          21)

;; Test 2: Lire élément par défaut
(run-test "Lire élément par défaut"
          '(aref (make-array 3) 1)
          0)

;; Test 3: Écrire et lire
(run-test "Écrire et lire"
          '(let ((arr (make-array 3)))
             (setf (aref arr 1) 42)
             (aref arr 1))
          42)

;; Test 4: Écrire et lire différent index
(run-test "Indices différents"
          '(let ((arr (make-array 5)))
             (setf (aref arr 0) 10)
             (setf (aref arr 4) 50)
             (aref arr 4))
          50)

;; Test 5: Calculer une somme simple
(run-test "Somme de deux éléments"
          '(let ((arr (make-array 2)))
             (setf (aref arr 0) 5)
             (setf (aref arr 1) 7)
             (+ (aref arr 0) (aref arr 1)))
          12)

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "Tous les tests basiques des opérations tableaux sont terminés.~%")
(format t "═══════════════════════════════════════════════════════════════~%")
