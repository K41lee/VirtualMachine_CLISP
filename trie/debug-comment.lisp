;;;; Debug: trouver d'où vient "COMMENT"
(load "src/vm.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%Recherche de 'COMMENT' dans le code généré~%")
(format t "═══════════════════════════════════════════════~%~%")

(defun read-file-as-sexps (filename)
  (with-open-file (stream filename :direction :input)
    (let ((sexps nil))
      (handler-case
          (loop
            (let ((sexp (read stream nil :eof)))
              (if (eq sexp :eof)
                  (return (nreverse sexps))
                  (when (listp sexp) (push sexp sexps)))))
        (end-of-file () (nreverse sexps))))))

(defparameter *compiler-sexps* (read-file-as-sexps "src/compiler-simplified.lisp"))
(defparameter *compiler-defuns* 
  (remove-if-not (lambda (sexp) 
                   (and (listp sexp) (eq (first sexp) 'defun)))
                 *compiler-sexps*))

(defun contains-comment (code)
  "Vérifie si le code contient 'COMMENT'"
  (cond
    ((null code) nil)
    ((stringp code) (search "COMMENT" code))
    ((symbolp code) (search "COMMENT" (symbol-name code)))
    ((listp code) 
     (or (contains-comment (car code))
         (contains-comment (cdr code))))
    (t nil)))

(format t "Compilation et recherche dans les 10 premières fonctions:~%~%")

(dolist (defun-form (subseq *compiler-defuns* 0 10))
  (let* ((fn-name (second defun-form))
         (code (compile-lisp-to-mips-simplified defun-form)))
    (if (contains-comment code)
        (progn
          (format t "⚠️  ~A contient 'COMMENT'!~%" fn-name)
          (format t "Code:~%")
          (dolist (instr code)
            (when (contains-comment instr)
              (format t "  → ~A~%" instr))))
        (format t "✓ ~A: OK (pas de COMMENT)~%" fn-name))))

(format t "~%~%Recherche terminée.~%")
