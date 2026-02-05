#!/usr/bin/env clisp
(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(initialize-compiler-symbols)
(load "src/compiler-simplified.lisp")

(defun read-file-as-sexps (filename)
  (with-open-file (stream filename :direction :input)
    (let ((sexps nil))
      (handler-case
          (loop
            (let ((sexp (read stream nil :eof)))
              (if (eq sexp :eof)
                  (return (nreverse sexps))
                  (when (listp sexp)
                    (push sexp sexps)))))
        (end-of-file () (nreverse sexps))))))

(defparameter *utils-sexps* (read-file-as-sexps "utils-bootstrap.lisp"))
(defparameter *utils-defuns* 
  (remove-if-not (lambda (sexp) 
                   (and (listp sexp) 
                        (eq (first sexp) 'defun)))
                 *utils-sexps*))

(format t "~%Fonctions dans utils-bootstrap.lisp:~%")
(dolist (defun-form *utils-defuns*)
  (format t "  - ~A~%" (second defun-form)))

(format t "~%Compilation de compile-from-handle...~%")
(defparameter *cfh-code* 
  (compile-lisp-to-mips-simplified 
    (find 'compile-from-handle *utils-defuns* :key #'second)))

(format t "Code: ~A instructions~%~%" (length *cfh-code*))

(format t "Labels dans le code compile-from-handle:~%")
(dolist (instr *cfh-code*)
  (when (and (listp instr) (eq (first instr) :LABEL))
    (format t "  → ~A (type: ~A)~%" (second instr) (type-of (second instr)))))
