;;;; debug-defun-code.lisp
;;;; Afficher le code généré par compile-defun

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG: Code généré par compile-defun~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(let* ((env (make-new-compiler-env))
       (code '(progn
                (defun test-fn (n lst)
                  n)
                (test-fn 42 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  
  (format t "Code généré (~A instructions):~%" (length asm-code))
  (format t "~%Premières 50 instructions:~%")
  (loop for instr in (subseq asm-code 0 (min 50 (length asm-code)))
        for i from 0
        do (format t "[~3A] ~A~%" i instr)))

(format t "~%═══════════════════════════════════════════════════════════════~%")
