;;;; test-condition.lisp
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(defvar *test1* '(defun test1 (n) (if (< n 2) 100 200)))
(defvar *code1* (compile-lisp-to-mips-simplified *test1*))

(defun test-cond (n expected)
  (let ((vm (make-new-vm)))
    (load-code vm *code1*)
    (set-register vm :$A0 n)
    (run-vm vm :max-instructions 100)
    (let ((result (get-register vm :$V0)))
      (if (= result expected)
          (format t "  ✅ test1(~D) = ~D~%" n result)
          (format t "  ❌ test1(~D) = ~D (attendu ~D)~%" n result expected)))))

(format t "~%Test conditions (< n 2):~%")
(test-cond 0 100)  ; 0 < 2 TRUE → 100
(test-cond 1 100)  ; 1 < 2 TRUE → 100
(test-cond 2 200)  ; 2 < 2 FALSE → 200
(test-cond 3 200)  ; 3 < 2 FALSE → 200
