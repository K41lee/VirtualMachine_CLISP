(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun debug-compile (name code)
  (format t "~%=== ~A ===~%" name)
  (format t "Code: ~A~%~%" code)
  (let ((compiled (compile-lisp-to-mips-simplified code)))
    (format t "Assembly (~A instructions):~%" (length compiled))
    (let ((i 0))
      (dolist (instr compiled)
        (format t "  [~2D] ~A~%" i instr)
        (incf i)))
    compiled))

;; Test qui échoue  
(debug-compile "arr[0] + (arr[1] + arr[2])"
               '(let ((arr (make-array 3)))
                  (setf (aref arr 0) 10)
                  (setf (aref arr 1) 20)
                  (setf (aref arr 2) 30)
                  (+ (aref arr 0) (+ (aref arr 1) (aref arr 2)))))
