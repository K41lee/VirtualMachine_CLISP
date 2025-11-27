;;; Test avec debug détaillé pour tracer bindings

(load "src/compiler.lisp")

;; Temporairement remplacer compile-let avec debug
(defun compile-let-debug (bindings body env)
  (format t "~%>>> DEBUG compile-let appelé:~%")
  (format t "    bindings type: ~A~%" (type-of bindings))
  (format t "    bindings value: ~A~%" bindings)
  (format t "    body type: ~A~%" (type-of body))
  (format t "    body value: ~A~%~%" body)
  
  (format t ">>> Début dolist sur bindings:~%")
  (let ((counter 0))
    (dolist (binding bindings)
      (format t "    [~A] binding type: ~A~%" counter (type-of binding))
      (format t "    [~A] binding value: ~A~%" counter binding)
      (format t "    [~A] (first binding): ~A~%" counter (first binding))
      (format t "    [~A] (second binding): ~A~%~%" counter (second binding))
      (incf counter)))
  
  (format t ">>> Test SECOND sur bindings lui-même:~%")
  (format t "    (second bindings): ~A~%~%" (second bindings))
  
  '(:debug-ok))

;; Redéfinir temporairement
(setf (symbol-function 'compile-let) #'compile-let-debug)

(format t "~%=== TEST DEFUN + LET avec DEBUG ===~%")
(let ((code '(defun test2 (x) (let ((y 10)) y))))
  (format t "Code: ~A~%" code)
  (handler-case
      (let ((result (compile-lisp code)))
        (format t "✓ Résultat: ~A instructions~%" (length result)))
    (error (e)
      (format t "✗ ERREUR: ~A~%" e))))
