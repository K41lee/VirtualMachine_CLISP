;;;; test-closure-trace.lisp
;;;; Tracer l'exécution pour comprendre

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(format t "~%=== TEST : Deux appels consécutifs à mult ===~%")

;; Simulons ce que fait twice: appeler mult deux fois
(let* ((expr '(labels ((outer (factor)
                         (labels ((mult (n)
                                    (* factor n)))
                           (let ((temp (mult 3)))
                             (mult temp)))))
                (outer 2)))
       (vm (compile-and-run expr))
       (result (get-register vm *reg-v0*)))
  (format t "Expression: (let ((temp (mult 3))) (mult temp))~%")
  (format t "  où mult(n) = factor * n et factor = 2~%")
  (format t "  temp = mult(3) = 2*3 = 6~%")
  (format t "  résultat = mult(6) = 2*6 = 12~%")
  (format t "Résultat: ~A (attendu: 12)~%" result)
  (if (= result 12)
      (format t "✓ TEST OK~%")
      (format t "✗ TEST ÉCHEC~%")))
