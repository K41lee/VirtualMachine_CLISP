(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-array-sum-trace ()
  "Test array-sum avec traces manuelles"
  (let* ((code '(progn
                  (defun array-sum (arr n)
                    (if (= n 0)
                        0
                        (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
                  
                  (defun test ()
                    (let ((arr (make-array 1)))
                      (setf (aref arr 0) 42)
                      ; Test manuel: charger arr[0]
                      (let ((val (aref arr 0)))
                        ; Test: appeler array-sum
                        (array-sum arr 1))))
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: array-sum([42], 1)~%")
    (format t "Code assembleur (premières 80 lignes):~%")
    (dotimes (i (min 80 (length compiled)))
      (format t "[~2d] ~a~%" i (nth i compiled)))
    
    (format t "~%Exécution...~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 1500)
    
    (let ((result (get-register vm :$V0)))
      (format t "~%Résultat: ~A (attendu 42)~%" result))))

(test-array-sum-trace)
