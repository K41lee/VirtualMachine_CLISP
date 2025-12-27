#!/usr/bin/env clisp
;; Voir code généré pour APPEND simple

(load "main.lisp")

(format t "~%=== CODE: (append (list 1 2) (list 3 4)) ===~%")
(let ((code (compile-lisp '(append (list 1 2) (list 3 4)))))
  (format t "Nombre d'instructions : ~A~%~%" (length code))
  (loop for instr in code
        for i from 0
        when (< i 60)
        do (format t "~3D: ~A~%" i instr)))
