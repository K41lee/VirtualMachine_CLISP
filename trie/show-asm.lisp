(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun show-assembly (name code)
  (format t "~%=== ~A ===~%" name)
  (let ((asm (compile-lisp-to-mips-simplified code)))
    (let ((i 0))
      (dolist (instr asm)
        (format t "[~3D] ~A~%" i instr)
        (incf i)))))

(show-assembly "sum-to-n function"
               '(progn
                  (defun sum-to-n (n)
                    (if (= n 0)
                        0
                        (+ n (sum-to-n (- n 1)))))
                  (sum-to-n 3)))
