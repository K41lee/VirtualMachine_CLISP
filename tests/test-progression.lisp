(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

;; Test 1: Addition simple
(defun test1 ()
  (let* ((code '(+ 1 2))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (format t "Test 1: (+ 1 2)~%")
    (load-code vm compiled)
    (run-vm vm)
    (format t "  Résultat: ~A (attendu 3)~%~%" (get-register vm :$V0))))

;; Test 2: Fonction simple
(defun test2 ()
  (let* ((code '(progn
                  (defun add-two (x)
                    (+ x 2))
                  (add-two 5)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (format t "Test 2: (add-two 5) où add-two = x + 2~%")
    (load-code vm compiled)
    (run-vm vm)
    (format t "  Résultat: ~A (attendu 7)~%~%" (get-register vm :$V0))))

;; Test 3: Récursion très simple
(defun test3 ()
  (let* ((code '(progn
                  (defun countdown (n)
                    (if (= n 0)
                        100
                        (countdown (- n 1))))
                  (countdown 2)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (format t "Test 3: countdown(2) qui devrait retourner 100~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 200)
    (format t "  Résultat: ~A (attendu 100)~%~%" (get-register vm :$V0))))

;; Test 4: Récursion avec accumulation
(defun test4 ()
  (let* ((code '(progn
                  (defun sum-to-n (n)
                    (if (= n 0)
                        0
                        (+ n (sum-to-n (- n 1)))))
                  (sum-to-n 3)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (format t "Test 4: sum-to-n(3) = 3 + 2 + 1 + 0~%")
    (load-code vm compiled)
    (run-vm vm :max-instructions 500)
    (format t "  Résultat: ~A (attendu 6)~%~%" (get-register vm :$V0))))

(test1)
(test2)
(test3)
(test4)
