;;;; debug-verbose.lisp
;;;; Test avec verbose pour voir les registres

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG VERBOSE: test3 avec traces VM~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(let* ((vm (make-new-vm :verbose t))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test3 (n lst)
                  (if (= n 0)
                      (car lst)
                      999))
                (test3 0 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  
  (format t "~%Chargement et exécution...~%")
  (format t "══════════════════════════════════════════════════════════════~%~%")
  
  (load-and-run vm asm-code :verbose t :include-runtime t)
  
  (format t "~%~%══════════════════════════════════════════════════════════════~%")
  (format t "Résultat final: $V0 = ~A~%" (get-register vm :$V0))
  (format t "Attendu: 10~%"))

(format t "~%═══════════════════════════════════════════════════════════════~%")
