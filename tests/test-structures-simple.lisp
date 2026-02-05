;;;; Test simple des opérations sur listes
;;;;

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST STRUCTURES - Exécution avec délégation à Lisp        ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; Test 1: Fonction retournant une constante
(format t "~%Test 1: Fonction retournant une constante numérique~%")
(defparameter test-const
  '(defun test-const (n)
     (if (= n 0) 42 99)))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-const))
           (vm (make-new-vm)))
      (load-code vm compiled)
      (set-register vm :$A0 0)
      (run-vm vm)
      (let ((result (get-register vm :$V0)))
        (format t "  test-const(0) = ~A (attendu 42)~%" result)
        (if (= result 42)
            (format t "  ✅ Test réussi~%")
            (format t "  ❌ Test échoué~%"))))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

;;; Test 2: Test NULL avec un nombre
(format t "~%Test 2: NULL avec un nombre (devrait retourner false=0)~%")
(defparameter test-null-num
  '(defun test-null (n)
     (if (null n) 1 0)))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-null-num))
           (vm (make-new-vm)))
      (load-code vm compiled)
      (set-register vm :$A0 42)
      (run-vm vm)
      (let ((result (get-register vm :$V0)))
        (format t "  test-null(42) = ~A (attendu 0 car 42 n'est pas NULL)~%" result)
        (if (= result 0)
            (format t "  ✅ Test réussi~%")
            (format t "  ❌ Test échoué~%"))))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

;;; Test 3: Test NULL avec 0 (représente NIL)
(format t "~%Test 3: NULL avec 0 (représente NIL, devrait retourner true=1)~%")
(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-null-num))
           (vm (make-new-vm)))
      (load-code vm compiled)
      (set-register vm :$A0 0)
      (run-vm vm)
      (let ((result (get-register vm :$V0)))
        (format t "  test-null(0) = ~A (attendu 1 car 0 représente NIL)~%" result)
        (if (= result 1)
            (format t "  ✅ Test réussi~%")
            (format t "  ❌ Test échoué~%"))))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                        FIN DES TESTS                         ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(format t "~%Note: Tests avec QUOTE, CONS, CAR, CDR nécessitent~%")
(format t "      l'instruction LISP-OBJECT dans le loader.~%~%")

(quit)
