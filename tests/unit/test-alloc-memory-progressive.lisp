;; Test progressif d'ALLOC-MEMORY

(load "src/compiler.lisp")

;; Initialiser
(setf *global-constants* (make-hash-table))
(setf *global-variables* (make-hash-table))
(setf *global-data-offset* 32)

;; Test 1: Juste le LET
(format t "~%=== Test 1: LET simple ===~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp))) addr))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 2: LET + arithmétique
(format t "~%=== Test 2: LET + arithmétique ===~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp)))
                    (+ addr 10)))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 3: LET + deux appels get-register
(format t "~%=== Test 3: LET + deux get-register ===~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp)))
                    (get-register :$sp)))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 4: LET + comparaison
(format t "~%=== Test 4: LET + >= ===~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp)))
                    (>= addr 100)))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 5: LET + >= avec deux appels
(format t "~%=== Test 5: LET + >= avec deux get-register ===~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp)))
                    (>= (+ addr 10) (get-register :$sp))))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 6: LET + WHEN
(format t "~%=== Test 6: LET + WHEN ===~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp)))
                    (when (>= addr 100)
                      42)))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 7: LET + SET-REGISTER
(format t "~%=== Test 7: LET + SET-REGISTER ===~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp)))
                    (set-register :$gp (+ addr 10))))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 8: ALLOC-MEMORY complet mais simplifié (sans ERROR)
(format t "~%=== Test 8: ALLOC-MEMORY simplifié (sans ERROR) ===~%")
(handler-case
    (let* ((code '(defun alloc-memory (size)
                    (let ((addr (get-register :$gp)))
                      (when (>= (+ addr size) (get-register :$sp))
                        nil)
                      (set-register :$gp (+ addr size))
                      addr)))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 9: ALLOC-MEMORY complet avec ERROR
(format t "~%=== Test 9: ALLOC-MEMORY complet avec ERROR ===~%")
(handler-case
    (let* ((code '(defun alloc-memory (size)
                    (let ((addr (get-register :$gp)))
                      (when (>= (+ addr size) (get-register :$sp))
                        (error "Dépassement"))
                      (set-register :$gp (+ addr size))
                      addr)))
           (result (compile-lisp code)))
      (format t "✓ ~A instructions~%" (length result)))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

(format t "~%=== Tests terminés ===~%")
