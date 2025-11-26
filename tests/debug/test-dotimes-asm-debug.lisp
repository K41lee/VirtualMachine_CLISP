;;;; test-dotimes-asm-debug.lisp
;;;; Afficher l'assembleur pour comprendre

(load "loader.lisp")
(load "compiler.lisp")

(format t "~%=== ASSEMBLEUR POUR TEST 3 ===~%")

(let* ((expr '(let ((n 3) (sum 0))
                (dotimes (i (+ n 2))
                  (setq sum (+ sum i)))
                sum))
       (asm (compile-lisp expr)))
  (format t "~%Code ASM:~%")
  (dolist (instr asm)
    (format t "~A~%" instr)))
