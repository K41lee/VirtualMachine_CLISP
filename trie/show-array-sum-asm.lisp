(load "main.lisp")

(let* ((code '(progn
                (defun array-sum (arr n)
                  (if (= n 0)
                      0
                      (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
                ))
       (asm (compile-lisp code)))
  
  (format t "=== array-sum function ===~%")
  (dotimes (i (min 100 (length asm)))
    (format t "[~3d] ~a~%" i (nth i asm))))
