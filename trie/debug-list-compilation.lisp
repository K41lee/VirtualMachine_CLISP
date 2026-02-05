;;;; Vérifier comment LIST est compilé
(load "src/compiler-simplified.lisp")

(format t "~%Test de compilation de LIST~%")
(format t "════════════════════════════════~%~%")

(defparameter *test1* '(defun test-list () (list 1 2 3)))
(format t "Code 1: ~A~%" *test1*)
(defparameter *compiled1* (compile-lisp-to-mips-simplified *test1*))
(format t "Résultat: ~A instructions~%" (length *compiled1*))
(format t "~%Premières instructions:~%")
(dolist (instr (subseq *compiled1* 0 (min 10 (length *compiled1*))))
  (format t "  ~A~%" instr))

(format t "~%~%")

(defparameter *test2* '(defun test-cons () (cons 1 2)))
(format t "Code 2: ~A~%" *test2*)
(defparameter *compiled2* (compile-lisp-to-mips-simplified *test2*))
(format t "Résultat: ~A instructions~%" (length *compiled2*))
(format t "~%Premières instructions:~%")
(dolist (instr (subseq *compiled2* 0 (min 10 (length *compiled2*))))
  (format t "  ~A~%" instr))
