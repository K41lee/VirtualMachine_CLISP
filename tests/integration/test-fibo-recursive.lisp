;;; Test de Fibonacci récursif
;;; Test rapide pour voir si la récursion fonctionne

(load "run-benchmark.lisp")

;; Version correcte: fib(0)=0, fib(1)=1
;; fib(10) = 55
;; fib(20) = 6765
(format t "~%Test: Fibonacci(20) avec DEFUN récursif~%")
(format t "Résultat attendu: 6765~%~%")

(benchmark-code '(progn
                   (defun fibo (n)
                     (if (= n 0)
                         0
                         (if (= n 1)
                             1
                             (+ (fibo (- n 1)) (fibo (- n 2))))))
                   (fibo 20))
                :scenarios '(:native :vm0))
