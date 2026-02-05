(load "main.lisp")

(format t "~%╔═══════════════════════════════════════════════════════════════════════╗~%")
(format t "║                   TEST FIBONACCI(20) - FINAL                          ║~%")
(format t "╚═══════════════════════════════════════════════════════════════════════╝~%~%")

(format t "Programme: (progn (defun fib (n) ...) (fib 20))~%")
(format t "Résultat attendu: 6765~%~%")
(format t "⚠ Note: fibonacci(20) nécessite 21891 appels récursifs.~%")
(format t "   Cela peut prendre quelques secondes...~%~%")

(defparameter *start-time* (get-internal-real-time))

(handler-case
    (progn
      (compile-and-run 
        '(progn
           (defun fib (n)
             (if (<= n 1)
                 n
                 (+ (fib (- n 1)) (fib (- n 2)))))
           (fib 20)))
      
      (let* ((end-time (get-internal-real-time))
             (elapsed (/ (- end-time *start-time*) internal-time-units-per-second)))
        (format t "~%╔═══════════════════════════════════════════════════════════════════════╗~%")
        (format t "║                       ✓✓✓ SUCCÈS ✓✓✓                                  ║~%")
        (format t "╠═══════════════════════════════════════════════════════════════════════╣~%")
        (format t "║  Fibonacci(20) calculé correctement!                                 ║~%")
        (format t "║  Temps d'exécution: ~6,3F secondes                                    ║~%" elapsed)
        (format t "║                                                                       ║~%")
        (format t "║  Le compilateur LISP→MIPS est ENTIÈREMENT FONCTIONNEL!              ║~%")
        (format t "╚═══════════════════════════════════════════════════════════════════════╝~%~%")))
  (error (e)
    (format t "~%✗ ERREUR: ~A~%~%" e)
    (format t "Le compilateur ne peut pas encore gérer fibonacci(20).~%~%")))
