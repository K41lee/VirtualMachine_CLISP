;;; Test FastExp minimal

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%TEST: Fast-exp simple~%")

(let* ((code '(progn
                (defun fast-exp (base exp)
                  (if (= exp 0) 1
                      (if (= exp 1) base
                          (if (= (% exp 2) 0)
                              (let ((half (fast-exp base (/ exp 2))))
                                (* half half))
                              (* base (fast-exp base (- exp 1)))))))
                (fast-exp 2 4)))
       (compiled (compile-lisp-to-mips-simplified code))
       (vm (make-new-vm :verbose nil)))
  
  (format t "Code compilé (~A instructions):~%" (length compiled))
  (when (< (length compiled) 150)
    (dolist (instr compiled)
      (format t "  ~A~%" instr)))
  
  (format t "~%Exécution:~%")
  (load-code vm compiled)
  (handler-case
      (progn
        (run-vm vm)
        (format t "✅ Résultat: $V0 = ~A (attendu 16)~%" (get-register vm :$V0)))
    (error (e)
      (format t "❌ ERREUR: ~A~%" e))))
