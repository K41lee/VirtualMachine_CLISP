#!/usr/bin/env clisp
;; Afficher code généré pour CAR et CDR

(load "main.lisp")

(format t "~%=== CODE: (car (cons 1 2)) ===~%")
(let ((code (compile-lisp '(car (cons 1 2)))))
  (loop for instr in code
        for i from 0
        do (format t "~3D: ~A~%" i instr)))

(format t "~%=== CODE: (cdr (cons 1 2)) ===~%")
(let ((code (compile-lisp '(cdr (cons 1 2)))))
  (loop for instr in code
        for i from 0
        do (format t "~3D: ~A~%" i instr)))
