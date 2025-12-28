;;; Test rapide des nouvelles primitives vm-equal et vm-cadr

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST PRIMITIVES: vm-equal et vm-cadr~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

;;; Test 1: vm-equal avec nombres
(format t "Test 1: (vm-equal 5 5)~%")
(defparameter *test1-src* '(defun test1 () (vm-equal 5 5)))
(defparameter *test1-mips* (compile-lisp *test1-src*))
(format t "  Compilé: ~A instructions~%~%" (length *test1-mips*))

;;; Test 2: vm-cadr sur liste
(format t "Test 2: (vm-cadr lst) - extrait second élément~%")
(defparameter *test2-src* '(defun test2 (lst) (vm-cadr lst)))
(defparameter *test2-mips* (compile-lisp *test2-src*))
(format t "  Compilé: ~A instructions~%~%" (length *test2-mips*))

;;; Test 3: Vérifier opcodes
(format t "Test 3: Vérification des opcodes générés~%")
(format t "  test1 contient :EQUAL ? ~A~%" 
        (if (some #'(lambda (i) (and (listp i) (eq (first i) :EQUAL))) *test1-mips*)
            "✓" "✗"))
(format t "  test2 contient :LIST-CADR ? ~A~%~%" 
        (if (some #'(lambda (i) (and (listp i) (eq (first i) :LIST-CADR))) *test2-mips*)
            "✓" "✗"))

(format t "✅ Compilation des primitives réussie!~%~%")
