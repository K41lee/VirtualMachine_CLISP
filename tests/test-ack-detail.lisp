;;; Debug détaillé ack(2,1)

(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "DEBUG DÉTAILLÉ ack(2, 1)~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(defvar *code* '(progn
                  (defun ack (m n)
                    (cond
                      ((= m 0) (+ n 1))
                      ((= n 0) (ack (- m 1) 1))
                      (t (ack (- m 1) (ack m (- n 1))))))
                  (ack 2 1)))

(defvar *compiled* (compile-lisp-to-mips-simplified *code*))

(format t "~%Code compilé (~A instructions)~%" (length *compiled*))
(format t "~%Instructions 0-30:~%")
(dotimes (i (min 30 (length *compiled*)))
  (format t "~3D: ~A~%" i (nth i *compiled*)))

(format t "~%Exécution...~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *compiled*)
(run-vm *vm*)

(defvar *result* (get-register *vm* :$V0))
(format t "~%Résultat: ~A (attendu: 5)~%" *result)

;; Calcul manuel:
;; ack(2, 1) = ack(1, ack(2, 0))
;; ack(2, 0) = ack(1, 1)
;; ack(1, 1) = ack(0, ack(1, 0))
;; ack(1, 0) = ack(0, 1) = 2
;; ack(1, 1) = ack(0, 2) = 3
;; ack(2, 0) = 3
;; ack(2, 1) = ack(1, 3)
;; ack(1, 3) = ack(0, ack(1, 2))
;; ack(1, 2) = ack(0, ack(1, 1)) = ack(0, 3) = 4
;; ack(1, 3) = ack(0, 4) = 5
(format t "~%Donc ack(2, 1) devrait être 5~%")
