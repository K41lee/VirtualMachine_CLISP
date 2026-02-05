(load "main.lisp")

;; Test 1: While avec constante (devrait fonctionner)
(format t "~%TEST 1: While avec constante~%")
(defparameter *test1-source*
  '(defun test1 ()
     (let ((i 0))
       (while (< i 3)
         (setq i (+ i 1)))
       i)))

(defparameter *test1-code* (compile-lisp *test1-source*))
(format t "Compilé: ~A instructions~%" (length *test1-code*))

(defparameter *vm1* (make-new-vm))
(load-code *vm1* *test1-code*)
(run-vm *vm1*)
(format t "Résultat: ~A (attendu: 3)~%~%" (get-register *vm1* *reg-v0*))

;; Test 2: While avec paramètre (le bug)
(format t "TEST 2: While avec paramètre~%")
(defparameter *test2-source*
  '(defun test2 (n)
     (let ((i 0))
       (while (< i n)
         (setq i (+ i 1)))
       i)))

(defparameter *test2-code* 
  (append (compile-lisp *test2-source*)
          (list (list :LI 3 *reg-a0*)
                (list :JAL :TEST2)
                (list :HALT))))

(format t "Compilé: ~A instructions~%" (length *test2-code*))

(defparameter *vm2* (make-new-vm))
(load-code *vm2* *test2-code*)
(run-vm *vm2*)
(format t "Résultat: ~A (attendu: 3)~%~%" (get-register *vm2* *reg-v0*))

(format t "FIN DES TESTS~%")
