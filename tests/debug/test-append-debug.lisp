;;;; Test APPEND minimal pour debug

(load "main.lisp")

(format t "~%Test APPEND simple : (append '(1) '(2))~%~%")

(defparameter *test-simple*
  '(progn
     (append (cons 1 nil) (cons 2 nil))))

(defparameter *code* (compile-lisp *test-simple*))

(format t "Code généré (~A instructions) :~%" (length *code*))
(format t "~%")

;; Afficher les 20 premières instructions
(let ((count 0))
  (dolist (instr *code*)
    (when (< count 20)
      (format t "~A: ~A~%" count instr)
      (incf count))))

(format t "~%Exécution...~%")
(defparameter *vm* (make-new-vm))
(load-code *vm* *code*)
(run-vm *vm*)

(defparameter *result* (get-register *vm* '$V0))
(format t "~%Résultat : ~A~%" *result*)

(if (> *result* 0)
    (progn
      (format t "CAR : ~A~%" (mem-read *vm* *result* 0))
      (format t "CDR : ~A~%" (mem-read *vm* *result* 1)))
    (format t "Résultat NIL~%"))
