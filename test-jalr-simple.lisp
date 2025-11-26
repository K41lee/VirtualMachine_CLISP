(load "main.lisp")

(format t "~%=== TEST JALR/JR SIMPLE ===~%")
(let* ((code '(let ((y 10))
                (let ((f (lambda (x) (+ x y))))
                  (f 1))))
       (result (compile-and-run code)))
  (format t "~%Résultat: ~A~%" result))
