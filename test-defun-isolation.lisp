;; Test isolation du problème SECOND

(load "src/compiler.lisp")
(setf *global-constants* (make-hash-table))
(setf *global-variables* (make-hash-table))
(setf *global-data-offset* 32)

;; Test: DEFUN sans corps
(format t "~%Test 1: DEFUN vide~%")
(handler-case
    (let ((result (compile-lisp '(defun test1 (x) 42))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

;; Test: DEFUN avec LET sans corps
(format t "~%Test 2: DEFUN avec LET vide~%")
(handler-case
    (let ((result (compile-lisp '(defun test2 (x) (let ((y 10)) y)))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

;; Test: DEFUN avec LET et get-register
(format t "~%Test 3: DEFUN avec LET + GET-REGISTER~%")
(handler-case
    (let ((result (compile-lisp '(defun test3 (x) (let ((addr (get-register :$gp))) addr)))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

;; Test: DEFUN avec multiple expressions dans le corps
(format t "~%Test 4: DEFUN avec plusieurs expressions~%")
(handler-case
    (let ((result (compile-lisp '(defun test4 (x) 
                                   (let ((addr (get-register :$gp))) 
                                     42
                                     addr)))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

;; Test: DEFUN avec WHEN
(format t "~%Test 5: DEFUN avec WHEN~%")
(handler-case
    (let ((result (compile-lisp '(defun test5 (x) 
                                   (when (>= x 10) 
                                     42)
                                   x))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

;; Test: DEFUN avec LET + WHEN
(format t "~%Test 6: DEFUN avec LET + WHEN~%")
(handler-case
    (let ((result (compile-lisp '(defun test6 (x) 
                                   (let ((addr (get-register :$gp))) 
                                     (when (>= addr 10) 
                                       42)
                                     addr)))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

;; Test: DEFUN avec LET + WHEN + SET-REGISTER
(format t "~%Test 7: DEFUN avec LET + WHEN + SET-REGISTER~%")
(handler-case
    (let ((result (compile-lisp '(defun test7 (x) 
                                   (let ((addr (get-register :$gp))) 
                                     (when (>= addr 10) 
                                       nil)
                                     (set-register :$gp (+ addr 10))
                                     addr)))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

;; Test: DEFUN exact d'ALLOC-MEMORY mais param "size" et référence dans body
(format t "~%Test 8: ALLOC-MEMORY avec param SIZE utilisé~%")
(handler-case
    (let ((result (compile-lisp '(defun test8 (size) 
                                   (let ((addr (get-register :$gp))) 
                                     (when (>= (+ addr size) (get-register :$sp)) 
                                       nil)
                                     (set-register :$gp (+ addr size))
                                     addr)))))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e) (format t "✗ ERREUR: ~A~%" e)))

(format t "~%Terminé.~%")
