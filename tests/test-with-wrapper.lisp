;;;; test-with-wrapper.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(defvar *factorial* '(defun factorial (n) (if (< n 2) 1 (* n (factorial (- n 1))))))
(defvar *code* (compile-lisp-to-mips-simplified *factorial*))

;; Ajouter un wrapper qui appelle factorial puis HALT
(defvar *wrapped-code* 
  (append 
    (list '(JAL FACTORIAL)
          '(HALT))
    *code*))

(defun test-fact (n expected)
  (let ((vm (make-new-vm)))
    (load-code vm *wrapped-code*)
    (set-register vm :$A0 n)
    (run-vm vm :max-instructions 10000)
    (let ((result (get-register vm :$V0)))
      (if (= result expected)
          (format t "  ✅ factorial(~D) = ~D~%" n result)
          (format t "  ❌ factorial(~D) = ~D (attendu ~D)~%" n result expected)))))

(format t "~%Test factorial avec wrapper:~%")
(test-fact 0 1)
(test-fact 1 1)
(test-fact 2 2)
(test-fact 3 6)
(test-fact 4 24)
(test-fact 5 120)
