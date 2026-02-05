(load "main.lisp")

(defun test-sum-n-debug ()
  (reset-vm)
  
  (let* ((code '(
    (defun sum-to-n (n)
      (if (= n 0)
          0
          (+ n (sum-to-n (- n 1)))))
    
    (sum-to-n 3)
    ))
         (asm (compile-toplevel-simplified code)))
    
    ;; Afficher le code assembleur
    (format t "~%=== Code assembleur ===~%")
    (dotimes (i (length asm))
      (format t "[~3d] ~a~%" i (nth i asm)))
    
    ;; Exécuter avec traces
    (format t "~%~%=== Exécution ===~%")
    (load-program-vector asm)
    (setf *trace-execution* t)   ; Activer les traces si disponible
    (setf *vm-running* t)
    (vm-execute)
    
    (format t "~%Résultat: ~a~%" (register-get *reg-v0*))
    (format t "Attendu: 6~%")))

(test-sum-n-debug)
