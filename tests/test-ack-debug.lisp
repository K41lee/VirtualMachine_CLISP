;;; Debug Ackermann

(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG ACKERMANN~%")
(format t "═══════════════════════════════════════════════════════════════~%")

;; Test cas simples d'abord
(defun test-ack (m n expected)
  (let* ((code `(progn
                  (defun ack (m n)
                    (cond
                      ((= m 0) (+ n 1))
                      ((= n 0) (ack (- m 1) 1))
                      (t (ack (- m 1) (ack m (- n 1))))))
                  (ack ,m ,n)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    (if (null compiled)
        (format t "~%❌ ack(~A,~A): ÉCHEC COMPILATION~%" m n)
        (progn
          (load-code vm compiled)
          (run-vm vm)
          (let ((result (get-register vm :$V0)))
            (format t "~%ack(~A,~A) = ~A (attendu: ~A) " m n result expected)
            (if (= result expected)
                (format t "✅~%")
                (format t "❌~%")))))))

;; Tests progressifs
(test-ack 0 0 1)    ; = 0 + 1 = 1
(test-ack 0 1 2)    ; = 1 + 1 = 2
(test-ack 0 5 6)    ; = 5 + 1 = 6
(test-ack 1 0 2)    ; = ack(0, 1) = 2
(test-ack 1 1 3)    ; = ack(0, ack(1, 0)) = ack(0, 2) = 3
(test-ack 1 2 4)    ; = ack(0, ack(1, 1)) = ack(0, 3) = 4
(test-ack 2 0 3)    ; = ack(1, 1) = 3
(test-ack 2 1 5)    ; = ack(1, ack(2, 0)) = ack(1, 3) = 5
(test-ack 2 2 7)    ; = ack(1, ack(2, 1)) = ack(1, 5) = 7
(test-ack 3 0 5)    ; = ack(2, 1) = 5
(test-ack 3 1 13)   ; = ack(2, ack(3, 0)) = ack(2, 5) = 13
(test-ack 3 2 29)   ; = ack(2, ack(3, 1)) = ack(2, 13) = 29
