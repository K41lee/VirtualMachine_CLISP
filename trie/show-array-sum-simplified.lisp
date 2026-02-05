(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(let* ((code '(progn
                (defun array-sum (arr n)
                  (if (= n 0)
                      0
                      (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
                ))
       (asm (compile-lisp-to-mips-simplified code)))
  
  (format t "=== array-sum function (SIMPLIFIED compiler) ===~%~%")
  (dotimes (i (min 100 (length asm)))
    (format t "[~3d] ~a~%" i (nth i asm))))
