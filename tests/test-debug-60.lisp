(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-direct (name code expected)
  (format t "~%=== TEST: ~A ===~%" name)
  (format t "Code: ~A~%" code)
  (format t "Attendu: ~A~%~%" expected)
  
  (let* ((compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (load-code vm compiled)
    (run-vm vm :max-instructions 1000)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A~%" result)
      (if (equal result expected)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ~%")))))

;; Test simple d'abord
(test-direct "Trois éléments sans fonction"
             '(let ((arr (make-array 3)))
                (setf (aref arr 0) 10)
                (setf (aref arr 1) 20)
                (setf (aref arr 2) 30)
                (+ (aref arr 0) (+ (aref arr 1) (aref arr 2))))
             60)

;; Test avec fonction
(test-direct "Trois éléments avec fonction"
             '(progn
                (defun test-multiple ()
                  (let ((arr (make-array 5)))
                    (setf (aref arr 0) 10)
                    (setf (aref arr 1) 20)
                    (setf (aref arr 2) 30)
                    (+ (aref arr 0) (+ (aref arr 1) (aref arr 2)))))
                (test-multiple))
             60)
