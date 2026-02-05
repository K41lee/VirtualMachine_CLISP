(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-two-params ()
  "Test fonction récursive avec 2 paramètres"
  (let* ((code '(progn
                  (defun add-twice (a b)
                    (if (= b 0)
                        0
                        (+ a (add-twice a (- b 1)))))
                  (add-twice 5 3)))  ; Devrait faire 5+5+5 = 15
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: add-twice(5, 3) = 5*3 = 15~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 1000)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 15)~%~%" result)
      (if (= result 15)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ~%")))))

(test-two-params)
