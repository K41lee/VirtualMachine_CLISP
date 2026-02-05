(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-simple-if-rec ()
  "Test if dans récursion avec 2 params"
  (let* ((code '(progn
                  (defun count-down (x n)
                    (if (= n 0)
                        x
                        (count-down x (- n 1))))
                  
                  (defun test ()
                    (count-down 99 3))  ; Devrait retourner 99
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: count-down(99, 3) = 99~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 99)~%~%" result)
      (if (= result 99)
          (format t "✅ RÉUSSI~%")
          (format t "❌ ÉCHOUÉ~%")))))

(test-simple-if-rec)
