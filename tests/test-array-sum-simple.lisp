(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-array-sum-simple ()
  "Test array-sum avec seulement 2 éléments"
  (let* ((code '(progn
                  (defun array-sum (arr n)
                    (if (= n 0)
                        0
                        (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
                  
                  (defun test ()
                    (let ((arr (make-array 2)))
                      (setf (aref arr 0) 10)
                      (setf (aref arr 1) 20)
                      (array-sum arr 2)))  ; Devrait faire 10+20 = 30
                  
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: array-sum([10,20], 2) = 30~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 1500)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 30)~%~%" result)
      (if (= result 30)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ: obtenu ~A~%" result)))))

(test-array-sum-simple)
