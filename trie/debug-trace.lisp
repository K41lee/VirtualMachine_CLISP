(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-with-trace (name code expected)
  (format t "~%=== TEST: ~A ===~%" name)
  (format t "Code: ~A~%" code)
  (format t "Attendu: ~A~%~%" expected)
  
  (let* ((compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm :verbose t)))
    (load-code vm compiled)
    (format t "~%Exécution:~%")
    (run-vm vm :max-instructions 50)
    
    (let ((result (get-register vm :$V0)))
      (format t "~%Résultat: ~A~%" result)
      (if (equal result expected)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ (attendu ~A)~%" expected)))))

(test-with-trace "Somme de deux éléments"
                 '(let ((arr (make-array 2)))
                    (setf (aref arr 0) 5)
                    (setf (aref arr 1) 7)
                    (+ (aref arr 0) (aref arr 1)))
                 12)
