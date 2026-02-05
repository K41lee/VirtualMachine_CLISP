(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-just-aref ()
  "Test juste aref avec une expression pour l'index"
  (let* ((code '(progn
                  (defun test ()
                    (let ((arr (make-array 2)))
                      (setf (aref arr 0) 10)
                      (setf (aref arr 1) 20)
                      (aref arr (- 2 1))))  ; Devrait retourner 20
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: aref(arr, 2-1) où arr = [10, 20]~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 20)~%~%" result))))

(defun test-add-aref ()
  "Test addition avec aref"
  (let* ((code '(progn
                  (defun test ()
                    (let ((arr (make-array 2)))
                      (setf (aref arr 0) 10)
                      (setf (aref arr 1) 20)
                      (+ 5 (aref arr (- 2 1)))))  ; Devrait retourner 5 + 20 = 25
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: 5 + aref(arr, 2-1)~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 25)~%~%" result))))

(defun test-simple-rec ()
  "Test fonction récursive simple avec tableau SANS accumulation"
  (let* ((code '(progn
                  (defun get-elem (arr n)
                    (if (= n 0)
                        (aref arr 0)
                        (get-elem arr (- n 1))))
                  
                  (defun test ()
                    (let ((arr (make-array 3)))
                      (setf (aref arr 0) 100)
                      (setf (aref arr 1) 200)
                      (setf (aref arr 2) 300)
                      (get-elem arr 2)))
                  (test)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: get-elem(arr, 2) - récursion simple~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 1000)
    
    (let ((result (get-register vm :$V0)))
      (format t "Résultat: ~A (attendu 100)~%~%" result))))

(test-just-aref)
(test-add-aref)
(test-simple-rec)
