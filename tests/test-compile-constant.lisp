#!/usr/bin/env clisp
;;; Test pour voir le code MIPS généré pour compile-constant-simplified

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles AVANT de charger le compilateur
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "src/compiler-bootstrap-ids.lisp")

(format t "~%~%════════════════════════════════════════════════════════════════~%")
(format t "Variables du compilateur:~%")
(format t "  *instr-li* = ~A~%" *instr-li*)
(format t "  *reg-v0* = ~A~%" *reg-v0*)
(format t "~%")

(format t "Code natif de compile-constant-simplified(42, NIL):~%")
(let ((code (compile-constant-simplified 42 nil)))
  (format t "  ~A~%" code))

(format t "~%")
(format t "Code MIPS compilé de la fonction compile-constant-simplified:~%")
(let ((mips-code (compile-lisp-with-ids '(defun compile-constant-simplified (value env)
                                            (list (list *instr-li* value *reg-v0*))))))
  (format t "Nombre d'instructions: ~A~%" (length mips-code))
  (format t "~%Instructions:~%")
  (loop for instr in mips-code
        for i from 0
        do (format t "  [~2D] ~A~%" i instr)))
