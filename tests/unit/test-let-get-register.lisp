;; Test LET avec get-register

(load "src/compiler.lisp")

;; Initialiser
(setf *global-constants* (make-hash-table))
(setf *global-variables* (make-hash-table))
(setf *global-data-offset* 32)

;; Test: LET avec appel get-register
(format t "~%Test LET avec GET-REGISTER: (let ((addr (get-register :$gp))) addr)~%")
(handler-case
    (let* ((code '(let ((addr (get-register :$gp))) addr))
           (result (compile-lisp code)))
      (format t "✓ Résultat: ~A instructions~%" (length result))
      (format t "Code MIPS:~%")
      (dolist (instr result)
        (format t "  ~S~%" instr)))
  (error (e)
    (format t "✗ ERREUR: ~A~%~A~%" e (type-of e))))

(format t "~%Fait.~%")
