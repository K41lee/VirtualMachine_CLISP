(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

; Fonction pour afficher l'état de la pile
(defun show-stack-at (vm addr what)
  (let ((val (mem-read vm addr)))
    (format t "  ~A: mem[~A] = ~A~%" what addr val)))

; Patch de ADD pour tracer
(defparameter *original-add* nil)

(defun trace-add (vm)
  (unless *original-add*
    (setf *original-add* (symbol-function 'vm-add)))
  
  (setf (symbol-function 'vm-add)
        (lambda (vm rd rs rt)
          (let ((rs-val (register-get vm rs))
                (rt-val (register-get vm rt)))
            (format t "~%[TRACE ADD] $~A = $~A(~A) + $~A(~A)~%"
                    (register-name rd) 
                    (register-name rs) rs-val
                    (register-name rt) rt-val)
            (funcall *original-add* vm rd rs rt)
            (format t "          -> $~A = ~A~%"
                    (register-name rd)
                    (register-get vm rd))))))

(defun register-name (reg)
  (cond
    ((= reg 2) "V0")
    ((= reg 8) "T0")
    (t (format nil "R~A" reg))))

(defun test-with-trace ()
  "Test avec traçage de ADD"
  (let* ((code '(progn
                  (defun sum-n (n)
                    (if (= n 0)
                        0
                        (+ n (sum-n (- n 1)))))
                  (sum-n 2)))  ; Commencer avec 2 pour moins de sortie
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: sum-n(2) = 2 + 1 = 3~%~%")
    
    ; Activer le traçage
    (trace-add vm)
    
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0)))
      (format t "~%Résultat final: ~A~%" result)
      (format t "Attendu: 3~%")
      (if (= result 3)
          (format t "✅ TEST RÉUSSI~%")
          (format t "❌ TEST ÉCHOUÉ~%")))))

(test-with-trace)
