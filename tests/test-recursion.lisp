(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-simple-recursion ()
  "Test d'une fonction récursive simple"
  (let* ((code '(progn
                  (defun sum-n (n)
                    (if (= n 0)
                        0
                        (+ n (sum-n (- n 1)))))
                  (sum-n 3)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: sum-n(3) = 3 + 2 + 1 = 6~%~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A~%" result)
      (format t "Attendu: 6~%")
      (if (= result 6)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ~%")))))

(test-simple-recursion)
