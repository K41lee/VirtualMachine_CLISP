(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-array-sum ()
  "Test array-sum"
  (let* ((code '(progn
                  (defun array-sum (arr n)
                    (if (= n 0)
                        0
                        (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
                  
                  (defun test ()
                    (let ((arr (make-array 4)))
                      (setf (aref arr 0) 1)
                      (setf (aref arr 1) 2)
                      (setf (aref arr 2) 3)
                      (setf (aref arr 3) 4)
                      (array-sum arr 4)))  ; Devrait faire 1+2+3+4 = 10
                  
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: array-sum([1,2,3,4], 4) = 10~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 2000)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 10)~%~%" result)
      (if (= result 10)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ~%"))
      
      ; Afficher l'état de la mémoire
      (format t "~%État du heap:~%")
      (format t "  Heap start: 21~%")
      (loop for i from 0 to 6
            do (format t "  mem[~A] = ~A~%"
                      (+ 21 i)
                      (mem-read vm (+ 21 i)))))))

(test-array-sum)
