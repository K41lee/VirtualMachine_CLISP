;;; Test simple pour débugger hash-table

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "~%Test simple hash-table~%~%")

(defparameter *test-code*
  '(defun test ()
     (let ((ht (vm-make-hash-table :test 'equal)))
       (vm-hash-set ht 42 100)
       (vm-gethash 42 ht))))

(format t "Code source:~%~A~%~%" *test-code*)

(defparameter *compiled* (compile-lisp *test-code*))

(format t "~%Code compilé (~A instructions):~%" (length *compiled*))
(dolist (instr *compiled*)
  (format t "  ~A~%" instr))

(format t "~%~%Exécution avec verbose:~%")
(defparameter *vm* (make-new-vm :verbose t))
(load-code *vm* *compiled* :verbose nil)
(set-register *vm* (get-reg :pc) (calculate-code-start *vm*))
(run-vm *vm* :max-instructions 1000)

(format t "~%~%Résultat dans $V0: ~A~%" (get-register *vm* :$v0*))
