;;; Test encore plus minimal

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%TEST 1: make-array sans LET~%")

(let* ((code '(progn
                (defun test-create ()
                  (make-array 5))
                (test-create)))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose nil)))
  
  (format t "Code compilé:~%")
  (dolist (instr compiled)
    (format t "  ~A~%" instr))
  
  (format t "~%Exécution:~%")
  (load-code vm compiled)
  (handler-case
      (progn
        (run-vm vm)
        (format t "✅ Résultat: $V0 = ~A (handle du tableau)~%" (get-register vm :$V0)))
    (error (e)
      (format t "❌ ERREUR: ~A~%" e))))

(format t "~%~%TEST 2: aref sans récursion~%")

(let* ((code '(progn
                (defun test-aref ()
                  (let ((arr (make-array 3)))
                    (aref arr 0)))
                (test-aref)))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose t)))
  
  (format t "~%Exécution:~%")
  (load-code vm compiled)
  (handler-case
      (progn
        (run-vm vm)
        (format t "✅ Résultat: $V0 = ~A (devrait être 0)~%" (get-register vm :$V0)))
    (error (e)
      (format t "❌ ERREUR: ~A~%" e))))
