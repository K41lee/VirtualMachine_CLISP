;;;; debug-registers.lisp
;;;; Test pour afficher les registres après chaque étape

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG: Affichage des registres~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

;; Test 1: Fonction simple qui retourne n
(format t "Test 1: Fonction retournant n directement~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun retour-n (n)
                  n)
                (retour-n 42)))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (format t "  $V0 = ~A (attendu: 42) " (get-register vm :$V0))
  (if (= (get-register vm :$V0) 42)
      (format t "✓~%")
      (format t "✗~%")))

;; Test 2: Fonction qui retourne lst
(format t "Test 2: Fonction retournant lst directement~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun retour-lst (lst)
                  lst)
                (retour-lst (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  $V0 = ~A (attendu: adresse > 0) " result)
    (if (> result 0)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 3: Fonction qui retourne second paramètre  
(format t "Test 3: Fonction à 2 params retournant le 2ème~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun retour-second (n lst)
                  lst)
                (retour-second 42 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  $V0 = ~A (attendu: adresse > 0) " result)
    (if (> result 0)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 4: Fonction à 2 params retournant le 1er
(format t "Test 4: Fonction à 2 params retournant le 1er~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun retour-first (n lst)
                  n)
                (retour-first 42 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (format t "  $V0 = ~A (attendu: 42) " (get-register vm :$V0))
  (if (= (get-register vm :$V0) 42)
      (format t "✓~%")
      (format t "✗~%")))

;; Test 5: Fonction testant (= n 42)
(format t "Test 5: Fonction testant (= n 42) avec n=42~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test-egal (n lst)
                  (= n 42))
                (test-egal 42 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (format t "  $V0 = ~A (attendu: 1) " (get-register vm :$V0))
  (if (= (get-register vm :$V0) 1)
      (format t "✓~%")
      (format t "✗~%")))

;; Test 6: Fonction avec IF simple
(format t "Test 6: Fonction avec IF (= n 42)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test-if-simple (n lst)
                  (if (= n 42)
                      100
                      200))
                (test-if-simple 42 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (format t "  $V0 = ~A (attendu: 100) " (get-register vm :$V0))
  (if (= (get-register vm :$V0) 100)
      (format t "✓~%")
      (format t "✗~%")))

;; Test 7: Fonction avec IF retournant (car lst)
(format t "Test 7: Fonction avec IF retournant (car lst)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test-if-car (n lst)
                  (if (= n 42)
                      (car lst)
                      999))
                (test-if-car 42 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (format t "  $V0 = ~A (attendu: 10) " (get-register vm :$V0))
  (if (= (get-register vm :$V0) 10)
      (format t "✓~%")
      (format t "✗~%")))

(format t "~%═══════════════════════════════════════════════════════════════~%")
