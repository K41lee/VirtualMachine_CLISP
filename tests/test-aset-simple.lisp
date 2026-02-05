;;; Test ASET ultra simple

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%TEST: ASET directement~%")

(let* ((code '(defun test-aset-simple ()
                (let ((arr (make-array 3)))
                  (setf (aref arr 1) 42)
                  (aref arr 1))))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose nil)))
  
  (format t "~%Code compilé (~A instructions):~%" (length compiled))
  (when (< (length compiled) 100)
    (dolist (instr compiled)
      (format t "  ~A~%" instr)))
  
  (format t "~%Exécution:~%")
  (load-code vm compiled)
  
  ;; Appeler la fonction via un autre code
  (let* ((call-code '(test-aset-simple))
         (call-compiled (compile-lisp-to-mips-simplified call-code))
         (vm2 (make-new-vm :verbose t)))
    (load-code vm2 compiled)  ; Charger la définition
    (load-code vm2 call-compiled)  ; Charger l'appel
    (handler-case
        (progn
          (run-vm vm2)
          (format t "~%✅ Résultat: $V0 = ~A (devrait être 42)~%" (get-register vm2 :$V0)))
      (error (e)
        (format t "~%❌ ERREUR: ~A~%" e)))))
