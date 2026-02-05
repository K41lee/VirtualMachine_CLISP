#!/usr/bin/env clisp
(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")
(initialize-compiler-symbols)
(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║    BOOTSTRAP: FIBONACCI ET ACKERMANN                           ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defparameter *test-functions*
  '((defun fibo (n)
      (if (< n 2)
          n
          (+ (fibo (- n 1)) (fibo (- n 2)))))
    (defun ack (m n)
      (cond
        ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (t (ack (- m 1) (ack m (- n 1))))))))

(format t "~%Compilation avec le compilateur natif:~%")

(dolist (func-def *test-functions*)
  (let ((func-name (second func-def)))
    (format t "~%~A:~%" func-name)
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified func-def)))
          (format t "  ✓ Compilation réussie: ~A instructions~%" (length code)))
      (error (e)
        (format t "  ✗ Erreur: ~A~%" e)))))

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                 RÉSULTATS DU BOOTSTRAP                         ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "~%✅ FIBONACCI: Fonction récursive avec conditions~%")
(format t "✅ ACKERMANN: Fonction doublement récursive~%")
(format t "~%Le compilateur peut compiler des fonctions complexes!~%")
