;;;; test-if-simple.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

; Test 1: (defun test1 (n) (if (< n 2) 100 200))
(defvar *test1* '(defun test1 (n) (if (< n 2) 100 200)))
(defvar *code1* (compile-lisp-to-mips-simplified *test1*))

(format t "~%Test 1: test1(n) = (if (< n 2) 100 200)~%")
(defvar *vm1* (make-new-vm))
(load-code *vm1* *code1*)
(set-register *vm1* :$A0 0)
(run-vm *vm1* :max-instructions 100)
(format t "test1(0) = ~A (attendu 100)~%" (get-register *vm1* :$V0))

(defvar *vm2* (make-new-vm))
(load-code *vm2* *code1*)
(set-register *vm2* :$A0 3)
(run-vm *vm2* :max-instructions 100)
(format t "test1(3) = ~A (attendu 200)~%" (get-register *vm2* :$V0))
