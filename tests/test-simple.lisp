(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-simple-make-array ()
  "Test make-array avec defun"
  (let* ((code '(progn
                  (defun test-create ()
                    (let ((arr (make-array 5)))
                      arr))
                  (test-create)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (format t "Code Lisp: ~A~%" code)
    (format t "~%Code MIPS (~A instructions):~%" (length compiled))
    (let ((i 0))
      (dolist (instr compiled)
        (format t "  [~A] ~A~%" i instr)
        (incf i)))
    
    (load-code vm compiled)
    (run-vm vm)
    
    (let ((result (get-register vm :$V0)))
      (format t "~%Résultat dans $V0: ~A~%" result)
      (format t "Attendu: 21~%")
      (if (= result 21)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ~%")))))

(test-simple-make-array)
