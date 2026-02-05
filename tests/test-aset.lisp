;;; Test ASET

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%TEST: ASET (modifier un élément)~%")

(let* ((code '(progn
                (defparameter *test-array* (make-array 3))
                (setf (aref *test-array* 1) 42)
                (aref *test-array* 1)))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose t)))
  
  (format t "~%Exécution:~%")
  (load-code vm compiled)
  (handler-case
      (progn
        (run-vm vm)
        (format t "~%✅ Résultat: $V0 = ~A (devrait être 42)~%" (get-register vm :$V0)))
    (error (e)
      (format t "~%❌ ERREUR: ~A~%" e))))
