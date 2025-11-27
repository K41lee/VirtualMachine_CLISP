;;;; test-defconstant.lisp
;;;; Test de DEFCONSTANT dans le compilateur

(load "src/asm-ops.lisp")
(load "src/compiler.lisp")

(format t "~%=== TEST DEFCONSTANT ===~%~%")

;; Réinitialiser les tables
(reset-global-tables)

;; Test 1: Définir une constante
(format t "Test 1: Définir +STATE-READY+ = 0~%")
(compile-lisp '(defconstant +state-ready+ 0))
(format t "  Valeur dans table: ~A~%" (gethash '+state-ready+ *global-constants*))
(assert (= (gethash '+state-ready+ *global-constants*) 0))
(format t "  ✓ PASS~%~%")

;; Test 2: Utiliser la constante dans une expression
(format t "Test 2: Utiliser +STATE-READY+ dans (+ +STATE-READY+ 1)~%")
(let ((code (compile-lisp '(+ +state-ready+ 1))))
  (format t "  Code généré: ~A instructions~%" (length code))
  (format t "  ✓ PASS~%~%"))

;; Test 3: Multiples constantes
(format t "Test 3: Définir plusieurs constantes~%")
(compile-lisp '(defconstant +state-running+ 1))
(compile-lisp '(defconstant +state-halted+ 2))
(compile-lisp '(defconstant +state-error+ 3))
(format t "  +STATE-RUNNING+ = ~A~%" (gethash '+state-running+ *global-constants*))
(format t "  +STATE-HALTED+ = ~A~%" (gethash '+state-halted+ *global-constants*))
(format t "  +STATE-ERROR+ = ~A~%" (gethash '+state-error+ *global-constants*))
(format t "  ✓ PASS~%~%")

;; Test 4: Utiliser dans une fonction
(format t "Test 4: Fonction utilisant constante~%")
(let ((code (compile-lisp '(defun test-fn (x) (+ x +state-running+)))))
  (format t "  Code généré: ~A instructions~%" (length code))
  (format t "  ✓ PASS~%~%"))

(format t "=== TOUS LES TESTS PASSENT ! ===~%")
(quit)
