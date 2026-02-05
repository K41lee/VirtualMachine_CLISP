(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun minimal-test ()
  "Test minimal: (+ 3 2) devrait donner 5"
  (let* ((code '(progn
                  (defun test () (+ 3 2))
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test minimal: (+ 3 2)~%")
    (format t "Code assembleur:~%")
    (dotimes (i (min 30 (length compiled)))
      (format t "  [~2d] ~a~%" i (nth i compiled)))
    
    (load-code vm compiled)
    (run-vm vm :max-instructions 100)
    
    (let ((result (get-register vm :$V0)))
      (format t "~%Résultat: ~A (attendu 5)~%" result))))

(defun simple-add-test ()
  "Test: (+ n 1) où n=3"
  (let* ((code '(progn
                  (defun add-one (n) (+ n 1))
                  (add-one 3)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "~%~%Test: (+ n 1) avec n=3~%")
    
    (load-code vm compiled)
    (run-vm vm :max-instructions 100)
    
    (let ((result (get-register vm :$V0)))
      (format t "~%Résultat: ~A (attendu 4)~%" result))))

(defun recursive-test ()
  "Test: sum-to-n(2) = 2+1+0 = 3"
  (let* ((code '(progn
                  (defun sum-to-n (n)
                    (if (= n 0)
                        0
                        (+ n (sum-to-n (- n 1)))))
                  (sum-to-n 2)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "~%~%Test: sum-to-n(2)~%")
    
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "~%Résultat: ~A (attendu 3)~%" result)
      (format t "$T0 final: ~A~%" (get-register vm :$T0))
      (format t "$S0 final: ~A~%" (get-register vm :$S0)))))

(minimal-test)
(simple-add-test)
(recursive-test)
