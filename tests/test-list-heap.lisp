;;;; test-list-heap.lisp
;;;; Tests pour vérifier que les listes sont maintenant allouées dans le heap

(load "main.lisp")

(defun compile-and-run (expr)
  "Compile et exécute une expression, retourne la valeur de $V0"
  (let* ((vm (make-new-vm :verbose nil))
         (env (make-new-compiler-env))
         (asm-code (compile-expr expr env)))
    (when asm-code
      (load-and-run vm asm-code :verbose nil :include-runtime t)
      (get-register vm :$V0))))

(format t "~%=== TESTS LISTES DANS HEAP ===~%")
(format t "Test que les listes sont maintenant allouées en mémoire~%~%")

;; Test 1 : NIL
(format t "Test 1 - NIL : ")
(let ((result (compile-and-run '(quote ()))))
  (if (= result 0)
      (format t "✓ PASS (NIL = 0)~%")
      (format t "✗ FAIL (NIL = ~A au lieu de 0)~%" result)))

;; Test 2 : Liste simple - vérifier qu'on obtient une adresse
(format t "Test 2 - Liste (a b c) donne adresse : ")
(let ((result (compile-and-run '(quote (a b c)))))
  (if (and (numberp result) (> result 0))
      (format t "✓ PASS (adresse = ~A)~%" result)
      (format t "✗ FAIL (résultat = ~A, pas une adresse)~%" result)))

;; Test 3 : CAR d'une liste simple
(format t "Test 3 - (car '(1 2 3)) : ")
(handler-case
    (let ((result (compile-and-run '(car (quote (1 2 3))))))
      ;; 'a devrait être converti en hash
      (if (numberp result)
          (format t "✓ PASS (car = ~A)~%" result)
          (format t "✗ FAIL (car = ~A, pas un nombre)~%" result)))
  (error (e)
    (format t "✗ FAIL (erreur: ~A)~%" e)))

;; Test 4 : CDR d'une liste
(format t "Test 4 - (cdr '(1 2 3)) donne adresse : ")
(handler-case
    (let ((result (compile-and-run '(cdr (quote (1 2 3))))))
      (if (and (numberp result) (> result 0))
          (format t "✓ PASS (cdr = adresse ~A)~%" result)
          (format t "✗ FAIL (cdr = ~A)~%" result)))
  (error (e)
    (format t "✗ FAIL (erreur: ~A)~%" e)))

;; Test 5 : NULL sur NIL
(format t "Test 5 - (null '()) : ")
(handler-case
    (let ((result (compile-and-run '(null (quote ())))))
      (if (= result 1)
          (format t "✓ PASS (null NIL = 1)~%")
          (format t "✗ FAIL (null NIL = ~A au lieu de 1)~%" result)))
  (error (e)
    (format t "✗ FAIL (erreur: ~A)~%" e)))

;; Test 6 : NULL sur liste non-vide
(format t "Test 6 - (null '(a)) : ")
(handler-case
    (let ((result (compile-and-run '(null (quote (a))))))
      (if (= result 0)
          (format t "✓ PASS (null liste = 0)~%")
          (format t "✗ FAIL (null liste = ~A au lieu de 0)~%" result)))
  (error (e)
    (format t "✗ FAIL (erreur: ~A)~%" e)))

;; Test 7 : CAR(CDR liste)
(format t "Test 7 - (car (cdr '(1 2 3))) : ")
(handler-case
    (let ((result (compile-and-run '(car (cdr (quote (1 2 3)))))))
      (if (= result 2)
          (format t "✓ PASS (second = 2)~%")
          (format t "✗ FAIL (second = ~A au lieu de 2)~%" result)))
  (error (e)
    (format t "✗ FAIL (erreur: ~A)~%" e)))

;; Test 8 : Fonction récursive count-elements
(format t "~%Test 8 - Fonction récursive count-elements :~%")
(handler-case
    (let* ((vm (make-new-vm :verbose nil))
           (env (make-new-compiler-env))
           (code '(progn
                    (defun count-elements (lst)
                      (if (null lst)
                          0
                          (+ 1 (count-elements (rest lst)))))
                    (count-elements (quote (a b c d e)))))
           (asm-code (compile-expr code env)))
      (load-and-run vm asm-code :verbose nil :include-runtime t)
      (let ((result (get-register vm :$V0)))
        (if (= result 5)
            (format t "✓ PASS (count-elements = 5)~%")
            (format t "✗ FAIL (count-elements = ~A au lieu de 5)~%" result))))
  (error (e)
    (format t "✗ FAIL (erreur: ~A)~%" e)))

(format t "~%=== FIN DES TESTS HEAP ===~%")
