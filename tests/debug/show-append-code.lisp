#!/usr/bin/env clisp
;; Afficher tout le code APPEND

(load "main.lisp")

(let ((code (compile-lisp '(append (list 1) (list 2)))))
  (format t "~%Total instructions: ~A~%~%" (length code))
  (loop for instr in code
        for i from 0
        do (format t "~3D: ~A~%" i instr)))
