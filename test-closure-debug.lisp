;;; Test de débogage pour appels multiples de closures

(load "main.lisp")

(format t "~%=== TEST: Appel simple de closure ===~%")
(compile-and-run '(let ((y 10)) (let ((f (lambda (x) (+ x y)))) (f 1))))

(format t "~%~%=== TEST: Deux appels séquentiels ===~%")
(compile-and-run '(let ((y 10)) (let ((f (lambda (x) (+ x y)))) (let ((dummy (f 1))) (f 2)))))

(format t "~%~%=== TEST: Deux appels dans arithmétique ===~%")
(compile-and-run '(let ((y 10)) (let ((f (lambda (x) (+ x y)))) (+ (f 1) (f 2)))))

(format t "~%~%Tests terminés.~%")
