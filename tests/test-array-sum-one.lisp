(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-array-sum-one ()
  "Test array-sum avec 1 seul élément"
  (let* ((code '(progn
                  (defun array-sum (arr n)
                    (if (= n 0)
                        0
                        (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
                  
                  (defun test ()
                    (let ((arr (make-array 1)))
                      (setf (aref arr 0) 42)
                      (array-sum arr 1)))  ; Devrait faire 42 + 0 = 42
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: array-sum([42], 1) = 42~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 1000)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 42)~%~%" result)
      (if (= result 42)
          (format t "✅ RÉUSSI~%")
          (format t "❌ ÉCHOUÉ~%")))))

(test-array-sum-one)
