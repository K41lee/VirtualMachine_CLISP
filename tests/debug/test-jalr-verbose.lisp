(load "main.lisp")

(format t "~%=== TEST JALR/JR DEBUG ===~%")

(let* ((code '(let ((y 10))
                (let ((f (lambda (x) (+ x y))))
                  (f 1))))
       (vm (make-new-vm))
       (asm-code (compile-lisp code)))
  
  ;; Enable verbose mode
  (setf (vm-verbose vm) t)
  
  (format t "~%=== CODE ASSEMBLEUR ===~%")
  (dolist (instr asm-code)
    (format t "~A~%" instr))
  
  (format t "~%=== EXÉCUTION (VERBOSE) ===~%")
  (load-code vm asm-code :verbose nil)
  (run-vm vm)
  
  (format t "~%Résultat: ~A~%" (get-register vm (get-reg :v0))))
