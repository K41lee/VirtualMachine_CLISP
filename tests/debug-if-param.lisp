;;;; debug-if-param.lisp
;;;; Test spécifique: IF avec paramètres de fonction

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG: IF avec paramètres~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

;; Test 1: IF simple avec paramètre numérique
(format t "Test 1: IF (= n 0) avec n=0, retour valeur simple~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test1 (n)
                  (if (= n 0)
                      100
                      200))
                (test1 0)))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (format t "  Résultat: ~A (attendu: 100) " (get-register vm :$V0))
  (if (= (get-register vm :$V0) 100)
      (format t "✓~%")
      (format t "✗~%")))

;; Test 2: IF avec deux paramètres
(format t "Test 2: IF avec (= n 0) et retour de lst~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test2 (n lst)
                  (if (= n 0)
                      lst
                      999))
                (test2 0 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: adresse de liste) " result)
    (if (> result 0)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 3: IF avec (= n 0) et retour (car lst)
(format t "Test 3: IF avec (= n 0) et retour (car lst)~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test3 (n lst)
                  (if (= n 0)
                      (car lst)
                      999))
                (test3 0 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (let ((result (get-register vm :$V0)))
    (format t "  Résultat: ~A (attendu: 10) " result)
    (if (= result 10)
        (format t "✓~%")
        (format t "✗~%"))))

;; Test 4: IF avec (= n 1) pour voir si c'est spécifique à 0
(format t "Test 4: IF avec (= n 1) et n=1~%")
(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun test4 (n)
                  (if (= n 1)
                      100
                      200))
                (test4 1)))
       (asm-code (compile-expr code env)))
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  (format t "  Résultat: ~A (attendu: 100) " (get-register vm :$V0))
  (if (= (get-register vm :$V0) 100)
      (format t "✓~%")
      (format t "✗~%")))

;; Test 5: Afficher le code compilé pour test3
(format t "~%Test 5: Code ASM pour test3~%")
(let* ((env (make-new-compiler-env))
       (code '(progn
                (defun test3 (n lst)
                  (if (= n 0)
                      (car lst)
                      999))
                (test3 0 (quote (10 20)))))
       (asm-code (compile-expr code env)))
  (format t "Nombre d'instructions: ~A~%" (length asm-code))
  (format t "Premières 30 instructions:~%")
  (loop for instr in (subseq asm-code 0 (min 30 (length asm-code)))
        for i from 0
        do (format t "  [~2A] ~A~%" i instr)))

(format t "~%═══════════════════════════════════════════════════════════════~%")
