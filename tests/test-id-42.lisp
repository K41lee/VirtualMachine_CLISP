#!/usr/bin/env clisp
;;; Vérifier quel symbole a l'ID 42

(load "src/vm.lisp")
(initialize-compiler-symbols)

(format t "~%Symboles autour de l'ID 42:~%")
(loop for id from 38 to 46 do
  (let ((name (symbol-name-from-id id)))
    (format t "  ID ~2D: ~A~%" id (or name "non défini"))))
