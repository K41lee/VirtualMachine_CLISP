(defparameter *function-definition*
  '(defun fact (n)
     (if (<= n 1)
         1
         (* n (fact (- n 1)))))
  "Factorielle")

(defparameter *function-name* 'FACT)
(defparameter *function-args* '(10))
(defparameter *expected-result* 3628800)

(format t "~%Code chargé depuis code.lisp:~%")
(format t "  Fonction: ~A~%" (second *function-definition*))
(format t "  Arguments: ~A~%" *function-args*)
(format t "  Résultat attendu: ~A~%~%" *expected-result*)
