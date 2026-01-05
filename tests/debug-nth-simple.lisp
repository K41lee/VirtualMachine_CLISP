;;;; debug-nth-simple.lisp
;;;; Tests progressifs pour identifier le problème avec nth-element

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG PROGRESSIF: nth-element~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

;; Test 1: Fonction qui retourne directement (car lst)
(format t "Test 1: Fonction retournant (car lst)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun get-first (lst)
                  (car lst))
                (get-first (quote (10 20 30)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 10) " result)
    (if (= result 10)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 2: Fonction avec IF qui retourne (car lst)
(format t "Test 2: Fonction avec IF retournant (car lst)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun get-first-if (lst)
                  (if (null lst)
                      0
                      (car lst)))
                (get-first-if (quote (10 20 30)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 10) " result)
    (if (= result 10)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 3: Test de (= 0 0)
(format t "Test 3: (= 0 0)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(= 0 0))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 1) " result)
    (if (= result 1)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 4: Fonction testant (= n 0)
(format t "Test 4: Fonction testant (= n 0) avec n=0~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test-zero (n)
                  (if (= n 0)
                      100
                      200))
                (test-zero 0)))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 100) " result)
    (if (= result 100)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 5: Fonction complète mais sans récursion
(format t "Test 5: nth-element sans récursion (n=0)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun nth-no-rec (n lst)
                  (if (= n 0)
                      (car lst)
                      999))
                (nth-no-rec 0 (quote (10 20 30)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 10) " result)
    (if (= result 10)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 6: Test avec récursion simple
(format t "Test 6: Décrémentation simple sans liste~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun countdown (n)
                  (if (= n 0)
                      100
                      (countdown (- n 1))))
                (countdown 2)))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 100) " result)
    (if (= result 100)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 7: member-p simplifié
(format t "Test 7: member-p avec liste vide~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun member-simple (x lst)
                  (if (null lst)
                      0
                      1))
                (member-simple 40 (quote ()))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 0) " result)
    (if (= result 0)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 8: member-p avec liste non-vide mais sans match
(format t "Test 8: member-p sans récursion (pas de match)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun member-norec (x lst)
                  (if (null lst)
                      0
                      (if (= x (car lst))
                          1
                          999)))
                (member-norec 40 (quote (10)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 999) " result)
    (if (= result 999)
        (format t "✓~%")
        (format t "✗~%"))))

(format t "~%═══════════════════════════════════════════════════════════════~%")
