#!/usr/bin/env clisp
;; Test CDR - debug code généré

(load "main.lisp")

(format t "~%=== CODE GÉNÉRÉ pour (cdr (cons 1 2)) ===~%")
(let ((code (compile-lisp '(cdr (cons 1 2)))))
  (loop for instr in code
        for i from 0
        do (format t "~3D: ~A~%" i instr)))
