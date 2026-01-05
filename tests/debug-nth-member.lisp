;;;; debug-nth-member.lisp
;;;; Debug des fonctions nth-element et member-p qui échouent

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG: nth-element et member-p~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

;; Test nth-element avec traces
(format t "Test 1: nth-element avec traces~%")
(format t "────────────────────────────────────────────────────────────────~%")

(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun nth-element (n lst)
                  (if (= n 0)
                      (car lst)
                      (nth-element (- n 1) (cdr lst))))
                (nth-element 0 (quote (10 20 30)))))
       (asm-code (compile-expr code env)))
  
  (format t "Code compilé (~A instructions)~%" (length asm-code))
  (format t "~%Exécution...~%")
  
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  
  (let ((result (get-register vm :$V0)))
    (format t "~%Résultat: ~A~%" result)
    (format t "Attendu: 10~%")
    (if (= result 10)
        (format t "✓ PASS~%")
        (format t "✗ FAIL~%"))))

(format t "~%")

;; Test avec n=1
(format t "Test 2: nth-element avec n=1~%")
(format t "────────────────────────────────────────────────────────────────~%")

(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun nth-element (n lst)
                  (if (= n 0)
                      (car lst)
                      (nth-element (- n 1) (cdr lst))))
                (nth-element 1 (quote (10 20 30)))))
       (asm-code (compile-expr code env)))
  
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  
  (let ((result (get-register vm :$V0)))
    (format t "~%Résultat: ~A~%" result)
    (format t "Attendu: 20~%")
    (if (= result 20)
        (format t "✓ PASS~%")
        (format t "✗ FAIL~%"))))

(format t "~%")

;; Test plus simple: juste (car (quote (10 20 30)))
(format t "Test 3: Simple (car '(10 20 30))~%")
(format t "────────────────────────────────────────────────────────────────~%")

(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(car (quote (10 20 30))))
       (asm-code (compile-expr code env)))
  
  (format t "Code: ~A~%" code)
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  
  (let ((result (get-register vm :$V0)))
    (format t "Résultat: ~A~%" result)
    (format t "Attendu: 10~%")
    (if (= result 10)
        (format t "✓ PASS~%")
        (format t "✗ FAIL~%"))))

(format t "~%")

;; Test member-p cas absent
(format t "Test 4: member-p cas absent~%")
(format t "────────────────────────────────────────────────────────────────~%")

(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(progn
                (defun member-p (x lst)
                  (if (null lst)
                      0
                      (if (= x (car lst))
                          1
                          (member-p x (cdr lst)))))
                (member-p 40 (quote (10 20 30)))))
       (asm-code (compile-expr code env)))
  
  (format t "Code compilé (~A instructions)~%" (length asm-code))
  (format t "~%Exécution...~%")
  
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  
  (let ((result (get-register vm :$V0)))
    (format t "~%Résultat: ~A~%" result)
    (format t "Attendu: 0~%")
    (if (= result 0)
        (format t "✓ PASS~%")
        (format t "✗ FAIL~%"))))

(format t "~%")

;; Test (null (quote (10 20)))
(format t "Test 5: (null '(10 20))~%")
(format t "────────────────────────────────────────────────────────────────~%")

(let* ((vm (make-new-vm :verbose nil))
       (env (make-new-compiler-env))
       (code '(null (quote (10 20))))
       (asm-code (compile-expr code env)))
  
  (load-and-run vm asm-code :verbose nil :include-runtime t)
  
  (let ((result (get-register vm :$V0)))
    (format t "Résultat: ~A~%" result)
    (format t "Attendu: 0~%")
    (if (= result 0)
        (format t "✓ PASS~%")
        (format t "✗ FAIL~%"))))

(format t "~%═══════════════════════════════════════════════════════════════~%")
