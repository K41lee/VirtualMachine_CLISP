(load "main.lisp")

(format t "=== Test: retourne juste la valeur de n ===~%")
(let* ((code '((defun return-n (n) n) (return-n 5)))
       (result (compile-and-run code)))
  (format t "Résultat: ~a (attendu 5)~%~%" result))

(format t "=== Test: double de n ===~%")
(let* ((code '((defun double-n (n) (+ n n)) (double-n 5)))
       (result (compile-and-run code)))
  (format t "Résultat: ~a (attendu 10)~%~%" result))

(format t "=== Test: n + valeur fixe ===~%")
(let* ((code '((defun add-one (n) (+ n 1)) (add-one 5)))
       (result (compile-and-run code)))
  (format t "Résultat: ~a (attendu 6)~%~%" result))

(format t "=== Test: récursion sans accumulation ===~%")
(let* ((code '((defun countdown (n)
                 (if (= n 0)
                     0
                     (countdown (- n 1))))
               (countdown 3)))
       (result (compile-and-run code)))
  (format t "Résultat: ~a (attendu 0)~%~%" result))

(format t "=== Test: récursion avec accumulation (problème) ===~%")
(let* ((code '((defun sum-to-n (n)
                 (if (= n 0)
                     0
                     (+ n (sum-to-n (- n 1)))))
               (sum-to-n 3)))
       (result (compile-and-run code)))
  (format t "Résultat: ~a (attendu 6)~%~%" result))

(format t "=== Test: récursion avec accumulation via LET ===~%")
(let* ((code '((defun sum-let (n)
                 (if (= n 0)
                     0
                     (let ((rec (sum-let (- n 1))))
                       (+ n rec))))
               (sum-let 3)))
       (result (compile-and-run code)))
  (format t "Résultat: ~a (attendu 6)~%~%" result))
