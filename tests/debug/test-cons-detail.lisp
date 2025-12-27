(load "main.lisp")

(format t "~%=== TEST CONS DETAILLE ===~%~%")

(defparameter *code* (compile-lisp '(cons 1 2)))

(format t "Code genere:~%")
(let ((i 0))
  (dolist (instr *code*)
    (format t "  [~A] ~A~%" i instr)
    (incf i)))

(defparameter *vm* (make-new-vm))

(format t "~%Avant load-code:~%")
(format t "  $V0 = ~A~%" (get-register *vm* (get-reg :v0)))
(format t "  $GP = ~A~%" (get-register *vm* (get-reg :gp)))
(format t "  $SP = ~A~%" (get-register *vm* (get-reg :sp)))

(load-code *vm* *code*)

(format t "~%Apres load-code:~%")
(format t "  $V0 = ~A~%" (get-register *vm* (get-reg :v0)))
(format t "  $GP = ~A~%" (get-register *vm* (get-reg :gp)))

(format t "~%Execution...~%")
(run-vm *vm*)

(format t "~%Apres execution:~%")
(format t "  $V0 = ~A~%" (get-register *vm* (get-reg :v0)))
(format t "  $GP = ~A~%" (get-register *vm* (get-reg :gp)))
(format t "  $SP = ~A~%" (get-register *vm* (get-reg :sp)))

(when (> (get-register *vm* (get-reg :v0)) 0)
  (let ((addr (get-register *vm* (get-reg :v0))))
    (format t "~%CONS cell a l'adresse ~A:~%" addr)
    (format t "  CAR = ~A~%" (mem-read *vm* addr 0))
    (format t "  CDR = ~A~%" (mem-read *vm* addr 1))))
