(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles AVANT de charger le compilateur
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")

;; Test simple
(format t "~%Valeur de *reg-v0*: ~A~%" *reg-v0*)
(format t "Valeur de *instr-li*: ~A~%" *instr-li*)

(let ((code (compile-constant-simplified 42 nil)))
  (format t "Code généré par natif: ~A~%" code))
