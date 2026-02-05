(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

(initialize-compiler-symbols)
(load "src/compiler-simplified.lisp")

(let ((code (compile-lisp-to-mips-simplified 
              '(defun compile-constant-simplified (value env)
                 (list (list *instr-li* value *reg-v0*))))))
  (format t "~%Code MIPS de compile-constant-simplified:~%")
  (dolist (instr code)
    (format t "  ~A~%" instr)))
