;;; Test minimal pour arrays avec nouveau système

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%TEST: make-array simple avec verbose~%")

(let* ((code '(progn
                (defun test-create ()
                  (let ((arr (make-array 5)))
                    arr))
                (test-create)))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose t)))  ; VERBOSE activé
  
  (format t "~%Code compilé:~%")
  (dolist (instr compiled)
    (format t "  ~A~%" instr))
  
  (format t "~%Exécution:~%")
  (load-code vm compiled)
  (run-vm vm)
  
  (format t "~%Résultat: $V0 = ~A~%" (get-register vm :$V0)))
