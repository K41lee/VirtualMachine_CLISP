(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-values (name code)
  (format t "~%=== TEST: ~A ===~%" name)
  (let* ((compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (load-code vm compiled)
    (run-vm vm)
    (get-register vm :$V0)))

;; Tester chaque élément individuellement
(format t "arr[0] = ~A (attendu 10)~%" 
        (test-values "Element 0"
                     '(let ((arr (make-array 3)))
                        (setf (aref arr 0) 10)
                        (setf (aref arr 1) 20)
                        (setf (aref arr 2) 30)
                        (aref arr 0))))

(format t "arr[1] = ~A (attendu 20)~%" 
        (test-values "Element 1"
                     '(let ((arr (make-array 3)))
                        (setf (aref arr 0) 10)
                        (setf (aref arr 1) 20)
                        (setf (aref arr 2) 30)
                        (aref arr 1))))

(format t "arr[2] = ~A (attendu 30)~%" 
        (test-values "Element 2"
                     '(let ((arr (make-array 3)))
                        (setf (aref arr 0) 10)
                        (setf (aref arr 1) 20)
                        (setf (aref arr 2) 30)
                        (aref arr 2))))

(format t "20 + 30 = ~A (attendu 50)~%" 
        (test-values "Addition simple"
                     '(+ 20 30)))

(format t "10 + 50 = ~A (attendu 60)~%" 
        (test-values "Addition simple 2"
                     '(+ 10 50)))

(format t "arr[1] + arr[2] = ~A (attendu 50)~%" 
        (test-values "Somme deux éléments"
                     '(let ((arr (make-array 3)))
                        (setf (aref arr 0) 10)
                        (setf (aref arr 1) 20)
                        (setf (aref arr 2) 30)
                        (+ (aref arr 1) (aref arr 2)))))

(format t "arr[0] + 50 = ~A (attendu 60)~%" 
        (test-values "Element + constante"
                     '(let ((arr (make-array 3)))
                        (setf (aref arr 0) 10)
                        (setf (aref arr 1) 20)
                        (setf (aref arr 2) 30)
                        (+ (aref arr 0) 50))))

(format t "arr[0] + (arr[1] + arr[2]) = ~A (attendu 60)~%" 
        (test-values "Expression complète"
                     '(let ((arr (make-array 3)))
                        (setf (aref arr 0) 10)
                        (setf (aref arr 1) 20)
                        (setf (aref arr 2) 30)
                        (+ (aref arr 0) (+ (aref arr 1) (aref arr 2))))))
