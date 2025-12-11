;;; Test Bootstrap : Calcul de Fibonacci avec DEFUN récursif
;;; Commande : clisp tests/integration/test-bootstrap-mod.lisp
;;; Note: Fibonacci récursif est très lent, on utilise n=20
;;;       Avec fib(0)=1, fib(1)=1 : fib(20) = 10946

(load "benchmarks/run-benchmark.lisp")

;; Version récursive de Fibonacci avec DEFUN (comme demandé)
;; Convention : fib(0)=1, fib(1)=1
;; fib(14) = 610
(benchmark-code '(progn
                   (defun fibo (n)
                     (if (= n 0)
                         1
                         (if (= n 1)
                             1
                             (+ (fibo (- n 1)) (fibo (- n 2))))))
                   (fibo 20)))