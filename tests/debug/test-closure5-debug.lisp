;;;; test-closure5-debug.lisp
;;;; Debug du test 5 de closures

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(format t "~%=== DEBUG TEST 5 : Closure avec opérations complexes ===~%")

(let* ((expr '(labels ((make-multiplier (factor)
                         (labels ((multiply (n)
                                    (* factor n))
                                  (apply-twice (n)
                                    (multiply (multiply n))))
                           (apply-twice 3))))
                (make-multiplier 2)))
       (vm (compile-and-run expr))
       (result (get-register vm *reg-v0*)))
  
  (format t "~%Expression : ~A~%" expr)
  (format t "Résultat : ~A~%" result)
  (format t "Attendu  : 12~%")
  (if (= result 12)
      (format t "✓ TEST RÉUSSI~%")
      (format t "✗ TEST ÉCHOUÉ~%")))
