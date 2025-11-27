;;; Test avec backtrace complète

(load "src/compiler.lisp")

(format t "~%=== TEST DEFUN + LET avec backtrace ===~%")
(let ((code '(defun test2 (x) (let ((y 10)) y))))
  (format t "Code: ~A~%" code)
  (handler-case
      (let ((result (compile-lisp code)))
        (format t "✓ Résultat: ~A instructions~%" (length result)))
    (error (e)
      (format t "✗ ERREUR: ~A~%" e)
      (format t "~%Backtrace disponible~%"))))
