#!/usr/bin/env clisp
;;; Vérifier le code natif

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(initialize-compiler-symbols)
(load "src/compiler-simplified.lisp")

(format t "~%Code natif pour compile-constant-simplified(42, NIL):~%")
(let ((code (compile-constant-simplified 42 nil)))
  (format t "  Brut: ~A~%" code)
  (format t "  Premier élément: ~A~%" (first code))
  (format t "    [0] = ~A (type: ~A)~%" (first (first code)) (type-of (first (first code))))
  (format t "    [1] = ~A (type: ~A)~%" (second (first code)) (type-of (second (first code))))
  (format t "    [2] = ~A (type: ~A)~%" (third (first code)) (type-of (third (first code)))))

(format t "~%~%Code natif pour compile-constant-simplified(123, NIL):~%")
(let ((code (compile-constant-simplified 123 nil)))
  (format t "  Brut: ~A~%" code)
  (format t "  Premier élément: ~A~%" (first code))
  (format t "    [0] = ~A (type: ~A)~%" (first (first code)) (type-of (first (first code))))
  (format t "    [1] = ~A (type: ~A)~%" (second (first code)) (type-of (second (first code))))
  (format t "    [2] = ~A (type: ~A)~%" (third (first code)) (type-of (third (first code)))))
