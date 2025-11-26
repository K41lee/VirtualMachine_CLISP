;;;; test-dotimes6-debug.lisp
;;;; Debug du test 6 de DOTIMES

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(format t "~%=== DEBUG TEST 6 : DOTIMES avec count expression ===~%")

(let* ((expr '(let ((n 3)
                     (sum 0))
                (dotimes (i (+ n 2))
                  (setq sum (+ sum i)))
                sum))
       (vm (compile-and-run expr))
       (result (get-register vm *reg-v0*)))
  
  (format t "~%Expression : ~A~%" expr)
  (format t "Résultat : ~A~%" result)
  (format t "Attendu  : 10 (0+1+2+3+4)~%")
  (format t "~%Explication:~%")
  (format t "  n = 3~%")
  (format t "  count = (+ n 2) = 5~%")
  (format t "  Boucle de i=0 à i=4~%")
  (format t "  sum = 0+1+2+3+4 = 10~%")
  (if (= result 10)
      (format t "~%✓ TEST RÉUSSI~%")
      (format t "~%✗ TEST ÉCHOUÉ~%")))
