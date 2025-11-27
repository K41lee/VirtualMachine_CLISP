;;; Test de compilation de vm-compilable.lisp

(load "src/compiler.lisp")
(load "src/vm-compilable.lisp")

(format t "~%=== COMPILATION DE VM-COMPILABLE.LISP ===~%~%")

(defparameter *functions-to-test*
  '(MAKE-NEW-VM GET-REGISTER SET-REGISTER REGISTER-P GET-REG
    ALLOC-MEMORY PUSH-STACK POP-STACK PEEK-STACK
    LOAD-PROGRAM EXECUTE-ONCE RUN
    GET-MEMORY SET-MEMORY
    HALT-VM? PRINT-STATE
    LOAD-AND-RUN
    LI-INSTR MOVE-INSTR ADD-INSTR ADDI-INSTR
    SUB-INSTR MUL-INSTR DIV-INSTR
    BEQ-INSTR BNE-INSTR J-INSTR JAL-INSTR JR-INSTR
    SW-INSTR LW-INSTR))

(defparameter *total-functions* 0)
(defparameter *compiled-functions* 0)
(defparameter *total-lines* 0)

(format t "Test de compilation des fonctions:~%~%")

(dolist (fn-name *functions-to-test*)
  (incf *total-functions*)
  (let ((fn-def (symbol-function fn-name)))
    (handler-case
        (progn
          ;; Essayer de compiler (cela testera le parsing au minimum)
          (let* ((source (function-lambda-expression fn-def))
                 (result (if source
                            (compile-lisp source)
                            nil)))
            (when result
              (incf *compiled-functions*)
              (incf *total-lines* (length result))
              (format t "✓ ~A: ~A instructions~%" fn-name (length result)))
            (unless result
              (format t "✗ ~A: pas de source disponible~%" fn-name))))
      (error (e)
        (format t "✗ ~A: ~A~%" fn-name e)))))

(format t "~%=== RÉSULTAT ===~%")
(format t "Fonctions compilées: ~A/~A (~A%)~%" 
        *compiled-functions* *total-functions*
        (round (* 100 (/ *compiled-functions* *total-functions*))))
(format t "Total lignes MIPS: ~A~%" *total-lines*)
