;; Test encore plus simple

(load "src/compiler.lisp")

;; Initialiser
(setf *global-constants* (make-hash-table))
(setf *global-variables* (make-hash-table))
(setf *global-data-offset* 32)

;; Test ultra-simple: juste un LET
(format t "~%Test LET basique: (let ((x 42)) x)~%")
(handler-case
    (let* ((code '(let ((x 42)) x))
           (result (compile-lisp code)))
      (format t "✓ Résultat: ~A instructions~%" (length result))
      (format t "Code MIPS:~%")
      (dolist (instr result)
        (format t "  ~S~%" instr)))
  (error (e)
    (format t "✗ ERREUR: ~A~%~A~%" e (type-of e))))

(format t "~%Fait.~%")
