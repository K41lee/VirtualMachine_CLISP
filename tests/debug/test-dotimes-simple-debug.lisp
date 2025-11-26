;;;; test-dotimes-simple-debug.lisp
;;;; Test simplifié pour comprendre le problème

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(format t "~%=== TEST SIMPLE : LET + DOTIMES ===~%")

;; Test 1: Sans count expression
(format t "~%Test 1: (let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum)~%")
(let* ((expr1 '(let ((sum 0))
                 (dotimes (i 5)
                   (setq sum (+ sum i)))
                 sum))
       (vm1 (compile-and-run expr1))
       (result1 (get-register vm1 *reg-v0*)))
  (format t "Résultat: ~A (attendu: 10)~%" result1)
  (if (= result1 10)
      (format t "✓ TEST 1 OK~%")
      (format t "✗ TEST 1 ÉCHEC~%")))

;; Test 2: Avec count expression simple  
(format t "~%Test 2: (let ((n 5) (sum 0)) (dotimes (i n) (setq sum (+ sum i))) sum)~%")
(let* ((expr2 '(let ((n 5) (sum 0))
                 (dotimes (i n)
                   (setq sum (+ sum i)))
                 sum))
       (vm2 (compile-and-run expr2))
       (result2 (get-register vm2 *reg-v0*)))
  (format t "Résultat: ~A (attendu: 10)~%" result2)
  (if (= result2 10)
      (format t "✓ TEST 2 OK~%")
      (format t "✗ TEST 2 ÉCHEC~%")))

;; Test 3: Avec count expression composée
(format t "~%Test 3: (let ((n 3) (sum 0)) (dotimes (i (+ n 2)) (setq sum (+ sum i))) sum)~%")
(let* ((expr3 '(let ((n 3) (sum 0))
                 (dotimes (i (+ n 2))
                   (setq sum (+ sum i)))
                 sum))
       (vm3 (compile-and-run expr3))
       (result3 (get-register vm3 *reg-v0*)))
  (format t "Résultat: ~A (attendu: 10)~%" result3)
  (if (= result3 10)
      (format t "✓ TEST 3 OK~%")
      (format t "✗ TEST 3 ÉCHEC~%")))
