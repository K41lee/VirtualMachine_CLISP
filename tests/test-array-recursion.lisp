(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-array-sum ()
  "Test de la somme avec tableaux"
  (let* ((code '(progn
                  (defun array-sum (arr n)
                    (if (= n 0)
                        0
                        (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
                  
                  (defun test-sum ()
                    (let ((arr (make-array 4)))
                      (setf (aref arr 0) 1)
                      (setf (aref arr 1) 2)
                      (setf (aref arr 2) 3)
                      (setf (aref arr 3) 4)
                      (array-sum arr 4)))
                  
                  (test-sum)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm :verbose nil)))
    
    (format t "Test: array-sum([1,2,3,4], 4) devrait être 1+2+3+4 = 10~%~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 2000)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A~%" result)
      (format t "Attendu: 10~%")
      (if (= result 10)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ~%")))))

(test-array-sum)
