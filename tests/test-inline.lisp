;;; Test ASET inline (pas de fonction)

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%TEST 1: MAKE-ARRAY simple inline~%")
(let* ((code '(make-array 3))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose nil)))
  (format t "Code: ~A~%Compilé:~%" code)
  (dolist (instr compiled)
    (format t "  ~A~%" instr))
  (load-code vm compiled)
  (run-vm vm)
  (format t "✅ Handle du tableau: ~A~%" (get-register vm :$V0)))

(format t "~%TEST 2: AREF inline (accès à élément)~%")  
(let* ((code '(progn
                (defparameter *arr* (make-array 3))
                (aref *arr* 1)))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose t)))
  (format t "~%Exécution avec verbose:~%")
  (load-code vm compiled)
  (handler-case
      (progn
        (run-vm vm)
        (format t "~%✅ Valeur lue: ~A (devrait être 0)~%" (get-register vm :$V0)))
    (error (e)
      (format t "~%❌ ERREUR: ~A~%" e))))

(format t "~%TEST 3: ASET + AREF inline~%")
(let* ((code '(progn
                (defparameter *arr2* (make-array 3))
                (setf (aref *arr2* 1) 99)
                (aref *arr2* 1)))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose t)))
  (format t "~%Exécution avec verbose:~%")
  (load-code vm compiled)
  (handler-case
      (progn
        (run-vm vm)
        (format t "~%✅ Valeur lue: ~A (devrait être 99)~%" (get-register vm :$V0)))
    (error (e)
      (format t "~%❌ ERREUR: ~A~%" e))))
