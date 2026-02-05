(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-pass-array ()
  "Test passer un tableau à une fonction"
  (let* ((code '(progn
                  (defun get-first (arr)
                    (aref arr 0))
                  
                  (defun test ()
                    (let ((arr (make-array 2)))
                      (setf (aref arr 0) 99)
                      (setf (aref arr 1) 88)
                      (get-first arr)))
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: get-first([99, 88])~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 99)~%~%" result)
      (if (= result 99)
          (format t "✅ RÉUSSI~%")
          (format t "❌ ÉCHOUÉ~%")))))

(defun test-pass-array-with-add ()
  "Test passer un tableau et faire une addition"
  (let* ((code '(progn
                  (defun add-to-first (arr val)
                    (+ (aref arr 0) val))
                  
                  (defun test ()
                    (let ((arr (make-array 2)))
                      (setf (aref arr 0) 10)
                      (add-to-first arr 5)))  ; 10 + 5 = 15
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: add-to-first([10, ...], 5)~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 15)~%~%" result)
      (if (= result 15)
          (format t "✅ RÉUSSI~%")
          (format t "❌ ÉCHOUÉ~%")))))

(test-pass-array)
(test-pass-array-with-add)
