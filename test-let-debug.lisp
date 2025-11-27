;; Test minimal pour reproduire l'erreur SECOND: 0 is not a list

(load "src/compiler.lisp")

;; Initialiser les tables globales
(setf *global-constants* (make-hash-table))
(setf *global-variables* (make-hash-table))
(setf *global-data-offset* 32)

;; Test 1: Simple LET sans appel
(format t "~%=== Test 1: LET simple ===~%")
(handler-case
    (let ((code '(let ((x 42)) x)))
      (format t "Code: ~S~%" code)
      (let ((result (compile-lisp-to-mips code)))
        (format t "✓ SUCCESS: ~A instructions~%" (length result))))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 2: LET avec appel get-register
(format t "~%=== Test 2: LET avec GET-REGISTER ===~%")
(handler-case
    (let ((code '(let ((addr (get-register :$gp))) addr)))
      (format t "Code: ~S~%" code)
      (let ((result (compile-lisp-to-mips code)))
        (format t "✓ SUCCESS: ~A instructions~%" (length result))))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 3: Simple comparaison
(format t "~%=== Test 3: Comparaison >= ===~%")
(handler-case
    (let ((code '(>= 5 3)))
      (format t "Code: ~S~%" code)
      (let ((result (compile-lisp-to-mips code)))
        (format t "✓ SUCCESS: ~A instructions~%" (length result))))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 4: WHEN simple
(format t "~%=== Test 4: WHEN simple ===~%")
(handler-case
    (let ((code '(when (>= 5 3) 42)))
      (format t "Code: ~S~%" code)
      (let ((result (compile-lisp-to-mips code)))
        (format t "✓ SUCCESS: ~A instructions~%" (length result))))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 5: LET + WHEN + comparaison (comme dans ALLOC-MEMORY)
(format t "~%=== Test 5: LET + WHEN + >= ===~%")
(handler-case
    (let ((code '(let ((addr 100) (size 10))
                   (when (>= (+ addr size) 200)
                     42))))
      (format t "Code: ~S~%" code)
      (let ((result (compile-lisp-to-mips code)))
        (format t "✓ SUCCESS: ~A instructions~%" (length result))))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

;; Test 6: Le code complet d'ALLOC-MEMORY simplifié
(format t "~%=== Test 6: ALLOC-MEMORY simplifié ===~%")
(handler-case
    (let ((code '(let ((addr (get-register :$gp)))
                   (when (>= addr 100)
                     (error "test"))
                   addr)))
      (format t "Code: ~S~%" code)
      (let ((result (compile-lisp-to-mips code)))
        (format t "✓ SUCCESS: ~A instructions~%" (length result))))
  (error (e)
    (format t "✗ ERREUR: ~A~%" e)))

(format t "~%=== Tests terminés ===~%")
