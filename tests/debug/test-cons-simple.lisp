(load "main.lisp")

(format t "~%Test CONS simple~%")

(defparameter *test* '(progn (cons 1 2)))
(defparameter *code* (compile-lisp *test*))

(format t "Code : ~A instructions~%~%" *code*)

(defparameter *vm* (make-new-vm))
(load-code *vm* *code*)
(run-vm *vm*)

(defparameter *result* (get-register *vm* '$V0))
(format t "~%Résultat : ~A~%" *result*)

(when (> *result* 0)
  (format t "CAR : ~A~%" (mem-read *vm* *result* 0))
  (format t "CDR : ~A~%" (mem-read *vm* *result* 1)))
